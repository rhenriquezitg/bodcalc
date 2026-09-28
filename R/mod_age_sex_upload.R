# Shared upload widget for an age-sex CSV --------------------------------------
# Used by mod_population, mod_deaths and each health state in
# mod_health_states: template download + file input + validation messages.

mod_age_sex_upload_ui <- function(id, label = "Upload CSV") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::downloadButton(ns("template"), "Download template",
                          class = "btn-outline-secondary btn-sm mb-2", icon = shiny::icon("download")),
    shiny::fileInput(ns("file"), label, accept = c(".csv", "text/csv", "text/plain"),
                     placeholder = "age, males, females"),
    shiny::uiOutput(ns("status"))
  )
}

#' @param what label used in messages and the template name
#' @param reference_ages reactive returning the population age grid, or NULL
#'   for the population upload itself
#' @param population reactive returning the population long df (optional; used
#'   to flag counts larger than the population)
#' @param default_ages age grid for the template when no population exists yet
#' @return reactive: list(ok, data, wide, errors, warnings, filename) or NULL
#'   before a file is uploaded
mod_age_sex_upload_server <- function(id, what, default_ages,
                                      reference_ages = NULL, population = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ref <- function() if (is.null(reference_ages)) NULL else reference_ages()

    output$template <- shiny::downloadHandler(
      filename = function() template_filename(what),
      content = function(file) {
        ages <- ref(); if (is.null(ages)) ages <- default_ages
        write_csv_template(ages, file)
      }
    )

    result <- shiny::reactive({
      f <- input$file
      if (is.null(f)) return(NULL)
      if (!is.null(reference_ages) && is.null(ref())) {
        return(list(ok = FALSE, data = NULL, wide = NULL, filename = f$name,
                    errors = "Upload a valid population file first (step 1); this file is checked against its age groups.",
                    warnings = character()))
      }
      pop <- if (is.null(population)) NULL else population()
      res <- load_age_sex_upload(f$datapath, what, ref(), pop)
      res$filename <- f$name
      res
    })

    output$status <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(shiny::div(class = "text-muted small",
                                        "No file uploaded yet."))
      shiny::tagList(
        if (r$ok) shiny::div(class = "alert alert-success py-2 small mb-2",
                             shiny::icon("check"), sprintf(" %s loaded: %d age groups.",
                                                           r$filename, nrow(r$wide))),
        lapply(r$errors, function(e) shiny::div(class = "alert alert-danger py-2 small mb-2",
                                                shiny::icon("xmark"), " ", e)),
        lapply(r$warnings, function(w) shiny::div(class = "alert alert-warning py-2 small mb-2",
                                                  shiny::icon("triangle-exclamation"), " ", w)),
        lapply(r$notes, function(n) shiny::div(class = "small text-muted mb-2",
                                               shiny::icon("circle-info"), " ", n))
      )
    })

    result
  })
}

#' Preview table for a validated wide table, with a totals row
age_sex_preview_table <- function(wide, digits = 0) {
  df <- data.frame(`Age group` = age_group_labels(wide$age),
                   Males = wide$males, Females = wide$females,
                   Both = wide$males + wide$females, check.names = FALSE)
  df <- rbind(df, data.frame(`Age group` = "Total", Males = sum(wide$males),
                             Females = sum(wide$females),
                             Both = sum(wide$males + wide$females), check.names = FALSE))
  DT::formatRound(DT::datatable(df, rownames = FALSE, class = "compact stripe",
                                options = list(dom = "t", paging = FALSE, ordering = FALSE)),
                  c("Males", "Females", "Both"), digits = digits)
}
