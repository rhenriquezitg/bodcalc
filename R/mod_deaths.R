# Step 2: deaths by age and sex ------------------------------------------------

mod_deaths_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::h5("Deaths"),
      shiny::p(class = "small text-muted",
               "Deaths due to the disease by age group and sex. The age groups must",
               "be identical to those of the population file."),
      mod_age_sex_upload_ui(ns("upload"), "Deaths CSV")
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(bslib::card_header("Uploaded data"),
                  DT::DTOutput(ns("table"))),
      bslib::card(bslib::card_header("Deaths by age and sex"),
                  shiny::plotOutput(ns("pyramid"), height = "520px"))
    )
  )
}

mod_deaths_server <- function(id, default_ages, reference_ages, population) {
  shiny::moduleServer(id, function(input, output, session) {
    res <- mod_age_sex_upload_server("upload", "Deaths", default_ages,
                                     reference_ages, population)
    ok <- shiny::reactive({ r <- res(); shiny::req(r, r$ok); r })
    output$table <- DT::renderDT(age_sex_preview_table(ok()$wide))
    output$pyramid <- shiny::renderPlot(plot_age_sex_pyramid(ok()$data, "Deaths"))
    res
  })
}
