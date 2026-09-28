# Step 5: run the calculation and present results -------------------------------

mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::h5("Calculation"),
      shiny::textInput(ns("title"), "Analysis title", placeholder = "e.g. Appendicitis, Ecuador 2024"),
      bslib::layout_columns(
        col_widths = c(6, 6), gap = "0.5rem",
        shiny::numericInput(ns("n_iter"), "Iterations", value = 10000, min = 1000,
                            max = 100000, step = 1000),
        shiny::numericInput(ns("seed"), "Random seed", value = 2026, min = 1, step = 1)
      ),
      shiny::h6("Checklist"),
      shiny::uiOutput(ns("checklist")),
      shiny::uiOutput(ns("button")),
      shiny::uiOutput(ns("stale"))
    ),
    shiny::uiOutput(ns("body"))
  )
}

mod_results_server <- function(id, population, deaths, health_states, economics,
                               life_table) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Readiness ---------------------------------------------------------------
    checks <- shiny::reactive({
      p <- population(); d <- deaths(); h <- health_states(); e <- economics()
      list(
        list(label = "Population file", ok = !is.null(p) && p$ok),
        list(label = "Deaths file", ok = !is.null(d) && d$ok),
        list(label = sprintf("Health states (%d complete)", length(h$states)), ok = h$ok,
             detail = h$issues),
        list(label = "Economic inputs", ok = e$ok, detail = e$issues),
        list(label = "Iterations between 1,000 and 100,000",
             ok = !is.na(input$n_iter) && input$n_iter >= 1000 && input$n_iter <= 100000)
      )
    })
    ready <- shiny::reactive(all(vapply(checks(), function(x) isTRUE(x$ok), logical(1))))

    output$checklist <- shiny::renderUI({
      shiny::tags$ul(class = "list-unstyled small",
        lapply(checks(), function(x) shiny::tags$li(
          class = "mb-1",
          shiny::icon(if (isTRUE(x$ok)) "circle-check" else "circle-xmark",
                      class = if (isTRUE(x$ok)) "text-success" else "text-danger"),
          " ", x$label,
          if (!isTRUE(x$ok) && length(x$detail))
            shiny::div(class = "text-muted ms-3", paste(x$detail, collapse = " "))
        )))
    })

    output$button <- shiny::renderUI({
      if (ready()) {
        shiny::actionButton(ns("calculate"), "Calculate burden", icon = shiny::icon("play"),
                            class = "btn-primary w-100")
      } else {
        shiny::tags$button(class = "btn btn-primary w-100", disabled = NA,
                           shiny::icon("play"), " Calculate burden")
      }
    })

    # Snapshot of inputs, to flag results that no longer match them ------------
    signature <- shiny::reactive({
      list(population()$data, deaths()$data,
           lapply(health_states()$states, function(s) s[c("name", "cases", "dw_mean",
                                                            "dw_lower", "dw_upper", "duration_years")]),
           economics()[c("currency", "gdp", "cost_case", "cost_death")],
           input$n_iter, input$seed)
    })

    results <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$calculate, {
      shiny::req(ready())
      h <- health_states(); e <- economics()
      res <- tryCatch(
        shiny::withProgress(message = "Running Monte Carlo simulation", value = 0, {
          run_burden_simulation(
            population = population()$data, deaths = deaths()$data,
            health_states = h$states, life_table = life_table,
            gdp_per_capita = e$gdp, cost_per_case = e$cost_case, cost_per_death = e$cost_death,
            n_iter = input$n_iter, seed = input$seed,
            progress = function(f, d) shiny::setProgress(f, detail = d))
        }),
        error = function(err) {
          shiny::showNotification(paste("Calculation failed:", conditionMessage(err)),
                                  type = "error", duration = NULL)
          NULL
        })
      if (!is.null(res)) {
        res$settings$currency <- e$currency
        res$settings$title <- trimws(input$title)
        res$signature <- signature()
        results(res)
      }
    })

    output$stale <- shiny::renderUI({
      r <- results()
      if (is.null(r) || !ready()) return(NULL)
      if (!identical(r$signature, signature())) {
        shiny::div(class = "alert alert-warning small mt-3",
                   shiny::icon("triangle-exclamation"),
                   " Inputs changed since the last calculation. Click Calculate to update the results.")
      }
    })

    # Result body ---------------------------------------------------------------
    output$body <- shiny::renderUI({
      r <- results()
      if (is.null(r)) {
        return(bslib::card(bslib::card_body(
          class = "text-center text-muted py-5",
          shiny::icon("calculator", class = "fa-2x mb-3"),
          shiny::p("Complete steps 1 to 4, then click", shiny::strong("Calculate burden"), "."))))
      }
      tot <- r$totals; cur <- r$settings$currency
      ci <- round(r$settings$conf * 100)
      vb <- function(measure, title, icon, money = FALSE) {
        row <- tot[tot$measure == measure, ]
        f <- if (money) function(x) format_money(x, cur) else function(x) format_num(x, 0)
        bslib::value_box(title = title, value = f(row$estimate), showcase = shiny::icon(icon),
                         showcase_layout = "left center", theme = "light",
                         shiny::p(class = "small", sprintf("%d%% UI %s – %s", ci,
                                                           f(row$lower), f(row$upper))))
      }
      shiny::tagList(
        if (nzchar(r$settings$title)) shiny::h4(r$settings$title),
        bslib::layout_columns(
          col_widths = c(6, 6, 6, 6), fill = FALSE,
          vb("DALY", "DALYs", "heart-pulse"),
          vb("YLL", "Years of life lost", "cross"),
          vb("YLD", "Years lived with disability", "wheelchair"),
          vb("Total economic burden", "Total economic burden", "coins", money = TRUE)
        ),
        bslib::navset_card_tab(
          bslib::nav_panel("Summary", DT::DTOutput(ns("totals"))),
          bslib::nav_panel(
            "By age and sex",
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::div(shiny::h6("DALYs split into YLL and YLD"),
                         shiny::plotOutput(ns("daly_pyramid"), height = "520px")),
              shiny::div(
                shiny::selectInput(ns("stratum_measure"), NULL,
                                   choices = c("DALY", "YLL", "YLD", "DALY per 100,000",
                                               "Deaths", "Cases"), width = "220px"),
                shiny::plotOutput(ns("uncertainty"), height = "470px"))
            ),
            DT::DTOutput(ns("stratum_table"))
          ),
          bslib::nav_panel("By health state", DT::DTOutput(ns("state_table"))),
          bslib::nav_panel(
            "Economic burden",
            bslib::layout_columns(
              col_widths = c(5, 7),
              shiny::plotOutput(ns("econ_plot"), height = "380px"),
              shiny::div(DT::DTOutput(ns("econ_table")), shiny::uiOutput(ns("econ_inputs")))
            )
          ),
          bslib::nav_panel(
            "Download",
            shiny::p("Download the results for further analysis or reporting."),
            shiny::div(class = "d-flex flex-wrap gap-2",
              shiny::downloadButton(ns("dl_xlsx"), "All tables (Excel)", class = "btn-primary"),
              shiny::downloadButton(ns("dl_csv"), "Age-sex results (CSV)", class = "btn-outline-primary"),
              shiny::downloadButton(ns("dl_report"), "Report (HTML)", class = "btn-outline-primary")),
            shiny::p(class = "small text-muted mt-3",
                     sprintf("%s iterations, seed %s, calculated %s.",
                             format_num(r$settings$n_iter), r$settings$seed,
                             format(r$settings$timestamp, "%Y-%m-%d %H:%M")))
          )
        )
      )
    })

    dt_plain <- function(df) DT::datatable(df, rownames = FALSE, class = "compact stripe",
                                           options = list(dom = "t", paging = FALSE, ordering = FALSE))

    output$totals <- DT::renderDT({
      r <- shiny::req(results())
      dt_plain(format_totals_table(r$totals, r$settings$currency, r$settings$conf))
    })
    output$daly_pyramid <- shiny::renderPlot(plot_daly_pyramid(shiny::req(results())$by_stratum))
    output$uncertainty <- shiny::renderPlot({
      r <- shiny::req(results())
      plot_measure_uncertainty(r$by_stratum, shiny::req(input$stratum_measure), r$settings$conf)
    })
    output$stratum_table <- DT::renderDT({
      r <- shiny::req(results())
      DT::datatable(format_stratum_table(r$by_stratum, r$settings$conf), rownames = FALSE,
                    class = "compact stripe",
                    options = list(pageLength = 25, scrollX = TRUE))
    })
    output$state_table <- DT::renderDT({
      r <- shiny::req(results())
      dt_plain(format_state_table(r$by_state, r$settings$conf))
    })
    output$econ_plot <- shiny::renderPlot({
      r <- shiny::req(results())
      plot_economic_burden(r$totals, r$settings$currency)
    })
    output$econ_table <- DT::renderDT({
      r <- shiny::req(results())
      tb <- r$totals[r$totals$measure %in% c("Direct costs", "Productivity loss",
                                             "Total economic burden"), ]
      dt_plain(format_totals_table(tb, r$settings$currency, r$settings$conf))
    })
    output$econ_inputs <- shiny::renderUI({
      r <- shiny::req(results()); s <- r$settings; cur <- s$currency
      shiny::tags$ul(class = "small text-muted mt-3",
        shiny::tags$li("GDP per capita: ", format_money(s$gdp_per_capita, cur, 2)),
        shiny::tags$li("Average cost per case: ", format_money(s$cost_per_case, cur, 2)),
        shiny::tags$li("Average cost per death: ", format_money(s$cost_per_death, cur, 2)))
    })

    # Downloads ---------------------------------------------------------------
    stamp <- function() format(Sys.time(), "%Y%m%d_%H%M")
    output$dl_csv <- shiny::downloadHandler(
      filename = function() paste0("bod_results_age_sex_", stamp(), ".csv"),
      content = function(file) {
        r <- shiny::req(results())
        utils::write.csv(r$by_stratum, file, row.names = FALSE)
      })
    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() paste0("bod_results_", stamp(), ".xlsx"),
      content = function(file) {
        r <- shiny::req(results())
        writexl::write_xlsx(results_workbook(r), file)
      })
    output$dl_report <- shiny::downloadHandler(
      filename = function() paste0("bod_report_", stamp(), ".html"),
      content = function(file) {
        r <- shiny::req(results())
        if (!rmarkdown::pandoc_available()) {
          shiny::showNotification(
            "The HTML report needs Pandoc (bundled with RStudio). Run the app from RStudio or install Pandoc.",
            type = "error", duration = NULL)
          stop("Pandoc not available")
        }
        shiny::withProgress(message = "Rendering report", {
          render_report(r, file)
        })
      })
  })
}

# Table formatting ---------------------------------------------------------------

measure_digits <- function(measure) {
  ifelse(measure %in% c("Deaths", "Cases"), 0,
         ifelse(grepl("per 100,000", measure), 2, 1))
}
is_money <- function(measure) measure %in% c("Direct costs", "Productivity loss",
                                             "Total economic burden")

format_value <- function(x, measure, currency) {
  vapply(seq_along(x), function(i) {
    if (is_money(measure[i])) format_money(x[i], currency)
    else format_num(x[i], measure_digits(measure[i]))
  }, character(1))
}

format_totals_table <- function(tot, currency, conf = 0.95) {
  ci <- round(conf * 100)
  out <- data.frame(
    Measure = tot$measure,
    Estimate = format_value(tot$estimate, tot$measure, currency),
    Median = format_value(tot$median, tot$measure, currency),
    lower = format_value(tot$lower, tot$measure, currency),
    upper = format_value(tot$upper, tot$measure, currency),
    check.names = FALSE)
  names(out)[4:5] <- paste0(ci, "% UI ", c("lower", "upper"))
  out
}

format_stratum_table <- function(bs, conf = 0.95) {
  ci <- round(conf * 100)
  d <- measure_digits(bs$measure)
  out <- data.frame(
    `Age group` = factor(age_group_labels(unique(bs$age))[match(bs$age, sort(unique(bs$age)))],
                         levels = age_group_labels(unique(bs$age))),
    Sex = factor(bs$sex, levels = c("Males", "Females")),
    Measure = factor(bs$measure, levels = unique(bs$measure)),
    Estimate = round(bs$estimate, d), Median = round(bs$median, d),
    lower = round(bs$lower, d), upper = round(bs$upper, d), check.names = FALSE)
  names(out)[6:7] <- paste0(ci, "% UI ", c("lower", "upper"))
  out
}

format_state_table <- function(st, conf = 0.95) {
  ci <- round(conf * 100)
  out <- data.frame(
    `Health state` = st$health_state, Measure = st$measure,
    Estimate = format_value(st$estimate, st$measure, ""),
    Median = format_value(st$median, st$measure, ""),
    lower = format_value(st$lower, st$measure, ""),
    upper = format_value(st$upper, st$measure, ""), check.names = FALSE)
  names(out)[5:6] <- paste0(ci, "% UI ", c("lower", "upper"))
  out
}

#' Sheets for the Excel download
results_workbook <- function(r) {
  s <- r$settings
  list(
    Summary = cbind(r$totals, currency = ifelse(is_money(r$totals$measure), s$currency, "")),
    `By age and sex` = r$by_stratum,
    `By health state` = r$by_state,
    `Health states` = s$health_states,
    `Life table used` = s$life_expectancy,
    Settings = data.frame(
      setting = c("Analysis title", "Iterations", "Seed", "Uncertainty interval",
                  "Currency (label only)", "GDP per capita", "Average cost per case",
                  "Average cost per death", "Total population", "Calculated at"),
      value = c(s$title, s$n_iter, s$seed, paste0(round(s$conf * 100), "%"), s$currency,
                s$gdp_per_capita, s$cost_per_case, s$cost_per_death, s$population_total,
                format(s$timestamp, "%Y-%m-%d %H:%M:%S")))
  )
}

#' Render the HTML report to `file`
render_report <- function(r, file) {
  tmp <- tempfile("bod_report_"); dir.create(tmp)
  rmd <- file.path(tmp, "report.Rmd")
  file.copy(file.path("report", "report.Rmd"), rmd)
  out <- rmarkdown::render(rmd, output_file = "report.html", output_dir = tmp,
                           params = list(res = r), quiet = TRUE,
                           envir = new.env(parent = environment(render_report)))
  file.copy(out, file, overwrite = TRUE)
}
