# Years of Life Lost ----------------------------------------------------------
# YLL = deaths x standard life expectancy at the age of death.
# By default there is no discounting and no age weighting (GBD convention since
# GBD 2010). Optionally the life expectancy is replaced by its discounted and/or
# age-weighted equivalent (see fct_discount_weight.R).

#' @param deaths numeric vector (or matrix: iterations x strata) of deaths
#' @param life_expectancy numeric vector, one value per stratum
#' @param age exact age at death per stratum (midpoint of the age group); only
#'   needed when discounting or age weighting is on
#' @param discount_rate annual discount rate as a fraction (0 = none)
#' @param age_weighting TRUE to apply GBD 1990 age weights
#' @return same shape as `deaths`
calc_yll <- function(deaths, life_expectancy, age = NULL,
                     discount_rate = 0, age_weighting = FALSE) {
  years <- if (no_weighting(discount_rate, age_weighting)) {
    life_expectancy
  } else {
    stopifnot(!is.null(age), length(age) == length(life_expectancy))
    weighted_years(age, life_expectancy, discount_rate, age_weighting)
  }
  if (is.matrix(deaths)) {
    stopifnot(ncol(deaths) == length(years))
    return(sweep(deaths, 2, years, `*`))
  }
  stopifnot(length(deaths) == length(years))
  deaths * years
}
