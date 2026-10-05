# Load app functions and data for the tests. Run all tests from the app folder:
#   testthat::test_dir("tests/testthat")
app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
life_table <- load_life_table(file.path(app_dir, "data", "gbd_life_table.rds"))
example_file <- function(name) file.path(app_dir, "datatables", name)

# Appendicitis example (datatables/), shared by the calculation tests
appendicitis <- function() {
  p <- load_age_sex_upload(example_file("population_pyramid.csv"), "Population")
  ages <- sort(unique(p$data$age))
  d <- load_age_sex_upload(example_file("appendicitis_deaths.csv"), "Deaths", ages)
  u <- load_age_sex_upload(example_file("appendicitis_uncomplicated_cases.csv"), "Cases", ages)
  c <- load_age_sex_upload(example_file("appendicitis_complicated_cases.csv"), "Cases", ages)
  list(pop = p$data, deaths = d$data, hs = list(
    list(name = "Uncomplicated", cases = u$data, dw_mean = 0.32, dw_lower = 0.22,
         dw_upper = 0.44, duration_years = 0.038),
    list(name = "Complicated", cases = c$data, dw_mean = 0.32, dw_lower = 0.22,
         dw_upper = 0.44, duration_years = 0.038)))
}
