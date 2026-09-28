# Burden of Disease Calculator -------------------------------------------------
# Run from R with the working directory set to this folder:
#   shiny::runApp()
# Files in R/ are loaded automatically by Shiny.

required <- c("shiny", "bslib", "shinyvalidate", "DT", "ggplot2", "writexl",
              "rmarkdown", "knitr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install the missing packages first:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
}

life_table <- load_life_table(file.path("data", "gbd_life_table.rds"))

shiny::shinyApp(
  ui = app_ui(),
  server = function(input, output, session) app_server(input, output, session, life_table)
)
