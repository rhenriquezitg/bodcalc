# Step 4: economic inputs ------------------------------------------------------
# Currency is a display label only (v1): all amounts must be in that currency.

mod_economics_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(-2, 8, -2),
    bslib::card(
      bslib::card_header("Economic inputs"),
      shiny::p(class = "small text-muted",
               "Enter every amount in the currency selected below. The currency is used as",
               "a label only: no exchange-rate or PPP conversion is applied."),
      shiny::selectInput(ns("currency"), "Currency", choices = CURRENCIES, selected = "USD",
                         width = "100%"),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::numericInput(ns("gdp"), "GDP per capita", value = NA, min = 0),
        shiny::numericInput(ns("cost_case"), "Average cost per case", value = NA, min = 0),
        shiny::numericInput(ns("cost_death"), "Average cost per death", value = NA, min = 0)
      ),
      shiny::tags$ul(class = "small text-muted mt-2",
        shiny::tags$li(shiny::strong("Direct costs"),
                       " = cases × cost per case + deaths × cost per death"),
        shiny::tags$li(shiny::strong("Productivity loss"), " = DALYs × GDP per capita"),
        shiny::tags$li(shiny::strong("Total economic burden"),
                       " = direct costs + productivity loss"))
    )
  )
}

#' @return reactive: list(currency, gdp, cost_case, cost_death, ok, issues)
mod_economics_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    for (f in c("gdp", "cost_case", "cost_death")) {
      iv$add_rule(f, shinyvalidate::sv_required())
      iv$add_rule(f, shinyvalidate::sv_gte(0))
    }
    iv$enable()

    shiny::reactive({
      vals <- list(`GDP per capita` = input$gdp, `Cost per case` = input$cost_case,
                   `Cost per death` = input$cost_death)
      issues <- unlist(lapply(names(vals), function(n) {
        v <- vals[[n]]
        if (is.null(v) || is.na(v)) paste(n, "is required.")
        else if (v < 0) paste(n, "cannot be negative.")
      }))
      list(currency = input$currency, gdp = input$gdp, cost_case = input$cost_case,
           cost_death = input$cost_death, ok = length(issues) == 0,
           issues = as.character(issues))
    })
  })
}
