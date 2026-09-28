# Preloaded GBD reference life table ------------------------------------------
# Built by data-raw/build_life_table.R from datatables/life_expectancy.csv.

#' Load the preloaded life table
#' @return data.frame with columns age, life_expectancy
load_life_table <- function(path = file.path("data", "gbd_life_table.rds")) {
  if (!file.exists(path)) {
    stop("Life table not found at '", path,
         "'. Run data-raw/build_life_table.R from the app folder first.")
  }
  lt <- readRDS(path)
  stopifnot(all(c("age", "life_expectancy") %in% names(lt)))
  lt[order(lt$age), c("age", "life_expectancy")]
}

#' Standard life expectancy at the lower bound of each age group.
#' The life table is linearly interpolated onto the user's age grid; ages beyond
#' the last life-table age take the last value.
life_expectancy_at <- function(ages, life_table) {
  stats::approx(life_table$age, life_table$life_expectancy,
                xout = ages, rule = 2)$y
}

#' Default age grid (used for templates before a population file is uploaded)
default_age_grid <- function(life_table) sort(life_table$age)
