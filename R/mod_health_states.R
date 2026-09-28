# Step 3: health states (repeatable units) -------------------------------------
# Each health state is a severity level or complication of the ONE disease
# analysed: name + cases CSV + disability weight (mean, 95% CI) + duration.

# One health state card -------------------------------------------------------

mod_health_state_item_ui <- function(id, number) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("card"),
    bslib::card(
      class = "mb-3",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span(shiny::strong(paste0("Health state ", number)),
                    shiny::textOutput(ns("title"), inline = TRUE)),
        shiny::actionLink(ns("remove"), "Remove", icon = shiny::icon("trash"),
                          class = "text-danger small")
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::div(
          shiny::textInput(ns("name"), "Name", placeholder = "e.g. complicated appendicitis"),
          mod_age_sex_upload_ui(ns("cases"), "Cases CSV")
        ),
        shiny::div(
          shiny::tags$label(class = "form-label", "Disability weight"),
          bslib::layout_columns(
            col_widths = c(4, 4, 4), gap = "0.5rem",
            shiny::numericInput(ns("dw_mean"), "Mean", value = NA, min = 0, max = 1, step = 0.001),
            shiny::numericInput(ns("dw_lower"), "Lower 95%", value = NA, min = 0, max = 1, step = 0.001),
            shiny::numericInput(ns("dw_upper"), "Upper 95%", value = NA, min = 0, max = 1, step = 0.001)
          ),
          shiny::p(class = "small text-muted",
                   "Values between 0 (full health) and 1 (equivalent to death).")
        ),
        shiny::div(
          shiny::tags$label(class = "form-label", "Duration"),
          bslib::layout_columns(
            col_widths = c(6, 6), gap = "0.5rem",
            shiny::numericInput(ns("duration_value"), "Value", value = NA, min = 0),
            shiny::selectInput(ns("duration_unit"), "Unit", choices = DURATION_UNITS,
                               selected = "days", selectize = FALSE)
          ),
          shiny::div(class = "small text-muted", shiny::textOutput(ns("duration_echo")))
        )
      )
    )
  )
}

mod_health_state_item_server <- function(id, default_ages, reference_ages,
                                         population, on_remove) {
  shiny::moduleServer(id, function(input, output, session) {
    cases <- mod_age_sex_upload_server("cases", "Cases", default_ages,
                                       reference_ages, population)

    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("name", shinyvalidate::sv_required())
    for (f in c("dw_mean", "dw_lower", "dw_upper")) {
      iv$add_rule(f, shinyvalidate::sv_required())
      iv$add_rule(f, shinyvalidate::sv_between(0, 1))
    }
    iv$add_rule("dw_mean", function(value) {
      lo <- input$dw_lower; hi <- input$dw_upper
      if (!is.na(lo) && !is.na(hi) && !is.na(value) && !(lo <= value && value <= hi))
        "Mean must lie between the lower and upper bounds"
    })
    iv$add_rule("duration_value", shinyvalidate::sv_required())
    iv$add_rule("duration_value", shinyvalidate::sv_gt(0))
    iv$enable()

    output$title <- shiny::renderText({
      if (!is.null(input$name) && nzchar(trimws(input$name))) paste0(": ", input$name) else ""
    })
    output$duration_echo <- shiny::renderText({
      v <- input$duration_value
      if (is.null(v) || is.na(v) || v <= 0) return("")
      format_duration(v, input$duration_unit)
    })

    shiny::observeEvent(input$remove, on_remove(), ignoreInit = TRUE, once = TRUE)

    shiny::reactive({
      up <- cases()
      # inputs are NULL until the inserted card is bound in the browser
      num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)
      name <- if (is.null(input$name)) "" else trimws(input$name)
      unit <- if (is.null(input$duration_unit)) "days" else input$duration_unit
      dw_mean <- num(input$dw_mean); dw_lower <- num(input$dw_lower)
      dw_upper <- num(input$dw_upper); dur <- num(input$duration_value)
      issues <- validate_health_state_params(name, dw_mean, dw_lower, dw_upper, dur)
      if (is.null(up)) issues <- c(issues, "Cases file not uploaded.")
      else if (!up$ok) issues <- c(issues, "Cases file has errors.")
      dur_years <- if (!is.na(dur) && dur > 0) duration_to_years(dur, unit) else NA_real_
      list(
        name = name,
        cases = if (!is.null(up) && up$ok) up$data else NULL,
        total_cases = if (!is.null(up) && up$ok) sum(up$data$value) else NA_real_,
        dw_mean = dw_mean, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_value = dur, duration_unit = unit,
        duration_years = dur_years,
        issues = issues, ok = length(issues) == 0
      )
    })
  })
}

# Container ---------------------------------------------------------------------

mod_health_states_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::h5("Health states"),
      shiny::p(class = "small text-muted",
               "Add one health state per severity level or complication of the disease",
               "(for example uncomplicated and complicated appendicitis). Each needs its",
               "own cases file, disability weight and duration."),
      shiny::actionButton(ns("add"), "Add health state", icon = shiny::icon("plus"),
                          class = "btn-primary w-100 mb-3"),
      shiny::h6("Summary"),
      shiny::uiOutput(ns("summary"))
    ),
    shiny::div(id = ns("items"))
  )
}

#' @return reactive: list(states = list of complete health states, ok, issues,
#'   table = summary data frame)
mod_health_states_server <- function(id, default_ages, reference_ages, population) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    active <- shiny::reactiveVal(character())
    results <- list()
    counter <- 0

    add_item <- function() {
      counter <<- counter + 1
      item_id <- paste0("hs", counter)
      shiny::insertUI(paste0("#", ns("items")), "beforeEnd",
                      mod_health_state_item_ui(ns(item_id), counter))
      results[[item_id]] <<- mod_health_state_item_server(
        item_id, default_ages, reference_ages, population,
        on_remove = function() {
          shiny::removeUI(paste0("#", ns(item_id), "-card"))
          active(setdiff(active(), item_id))
        })
      active(c(shiny::isolate(active()), item_id))
    }
    add_item()
    shiny::observeEvent(input$add, add_item())

    states <- shiny::reactive({
      lapply(active(), function(i) results[[i]]())
    })

    combined <- shiny::reactive({
      st <- states()
      issues <- character()
      if (length(st) == 0) issues <- "Add at least one health state."
      nm <- vapply(st, function(s) s$name, character(1))
      dup <- unique(nm[duplicated(nm) & nzchar(nm)])
      if (length(dup)) issues <- c(issues, paste("Duplicated health state name(s):",
                                                 paste(dup, collapse = ", ")))
      for (k in seq_along(st)) {
        if (!st[[k]]$ok) issues <- c(issues, sprintf("Health state %s: %s",
                                                    if (nzchar(st[[k]]$name)) st[[k]]$name else k,
                                                    paste(st[[k]]$issues, collapse = " ")))
      }
      table <- if (length(st)) data.frame(
        `Health state` = ifelse(nzchar(nm), nm, "(unnamed)"),
        Cases = vapply(st, function(s) format_num(s$total_cases), character(1)),
        `DW (95% CI)` = vapply(st, function(s) if (anyNA(c(s$dw_mean, s$dw_lower, s$dw_upper))) "" else
          sprintf("%.3f (%.3f–%.3f)", s$dw_mean, s$dw_lower, s$dw_upper), character(1)),
        Duration = vapply(st, function(s) if (is.na(s$duration_years)) "" else
          sprintf("%s %s (%.4f y)", format(s$duration_value), s$duration_unit, s$duration_years), character(1)),
        Status = vapply(st, function(s) if (s$ok) "Complete" else "Incomplete", character(1)),
        check.names = FALSE) else NULL
      list(states = Filter(function(s) s$ok, st), ok = length(issues) == 0,
           issues = issues, table = table)
    })

    output$summary <- shiny::renderUI({
      tb <- combined()$table
      if (is.null(tb)) return(shiny::p(class = "small text-muted", "No health states."))
      shiny::tags$ul(class = "list-unstyled small",
        lapply(seq_len(nrow(tb)), function(i) shiny::tags$li(
          class = "mb-2",
          shiny::icon(if (tb$Status[i] == "Complete") "circle-check" else "circle",
                      class = if (tb$Status[i] == "Complete") "text-success" else "text-muted"),
          shiny::strong(tb$`Health state`[i]), shiny::br(),
          shiny::span(class = "text-muted",
                      paste(c(if (nzchar(tb$Cases[i])) paste(tb$Cases[i], "cases"),
                              if (nzchar(tb$`DW (95% CI)`[i])) paste("DW", tb$`DW (95% CI)`[i]),
                              if (nzchar(tb$Duration[i])) tb$Duration[i]), collapse = " · "))
        )))
    })

    combined
  })
}
