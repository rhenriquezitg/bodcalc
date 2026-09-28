# Step 1: population by age and sex --------------------------------------------
# This upload sets the reference age grid for the whole run.

mod_population_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::h5("Population"),
      shiny::p(class = "small text-muted",
               "Population by age group and sex. The age groups in this file define",
               "the age groups every other file must use."),
      mod_age_sex_upload_ui(ns("upload"), "Population CSV")
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(bslib::card_header("Uploaded data"),
                  DT::DTOutput(ns("table"))),
      bslib::card(bslib::card_header("Population pyramid"),
                  shiny::plotOutput(ns("pyramid"), height = "520px"))
    )
  )
}

#' @return reactive: validated upload result (see mod_age_sex_upload_server)
mod_population_server <- function(id, default_ages) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- mod_age_sex_upload_server("upload", "Population", default_ages)
    ok <- shiny::reactive({ r <- res(); shiny::req(r, r$ok); r })
    output$table <- DT::renderDT(age_sex_preview_table(ok()$wide))
    output$pyramid <- shiny::renderPlot(plot_age_sex_pyramid(ok()$data, "Population"))
    res
  })
}
