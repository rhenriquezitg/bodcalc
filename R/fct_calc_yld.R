# Years Lived with Disability -------------------------------------------------
# Incidence-based: YLD = cases x disability weight x duration (years).

#' @param cases numeric vector of cases per stratum, or a matrix
#'   (iterations x strata)
#' @param disability_weight scalar, or one value per iteration when `cases` is a
#'   matrix
#' @param duration_years scalar duration in years
#' @return same shape as `cases`
calc_yld <- function(cases, disability_weight, duration_years) {
  if (is.matrix(cases)) {
    stopifnot(length(disability_weight) %in% c(1, nrow(cases)))
    # a length-nrow vector recycles down each column: row i gets weight i
    return(cases * disability_weight * duration_years)
  }
  stopifnot(length(disability_weight) == 1)
  cases * disability_weight * duration_years
}
