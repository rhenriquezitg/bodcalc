# Years of Life Lost ----------------------------------------------------------
# YLL = deaths x standard life expectancy at the age of death.
# No discounting, no age-weighting (GBD convention since GBD 2010).

#' @param deaths numeric vector (or matrix: iterations x strata) of deaths
#' @param life_expectancy numeric vector, one value per stratum
#' @return same shape as `deaths`
calc_yll <- function(deaths, life_expectancy) {
  if (is.matrix(deaths)) {
    stopifnot(ncol(deaths) == length(life_expectancy))
    return(sweep(deaths, 2, life_expectancy, `*`))
  }
  stopifnot(length(deaths) == length(life_expectancy))
  deaths * life_expectancy
}
