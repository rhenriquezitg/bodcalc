# Load app functions and data for the tests. Run all tests from the app folder:
#   testthat::test_dir("tests/testthat")
app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
life_table <- load_life_table(file.path(app_dir, "data", "gbd_life_table.rds"))
example_file <- function(name) file.path(app_dir, "datatables", name)
