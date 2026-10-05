# Years Lived with Disability -------------------------------------------------
# Incidence-based: YLD = cases x disability weight x duration (years).
# Optionally the duration is replaced by its discounted and/or age-weighted
# equivalent from the age of onset (see fct_discount_weight.R).

#' @param cases numeric vector of cases per stratum, or a matrix
#'   (iterations x strata)
#' @param disability_weight scalar, or one value per iteration when `cases` is a
#'   matrix
#' @param duration_years scalar duration in years
#' @param age exact age at onset per stratum (midpoint of the age group); only
#'   needed when discounting or age weighting is on
#' @param discount_rate annual discount rate as a fraction (0 = none)
#' @param age_weighting TRUE to apply GBD 1990 age weights
#' @return same shape as `cases`
calc_yld <- function(cases, disability_weight, duration_years, age = NULL,
                     discount_rate = 0, age_weighting = FALSE) {
  n_strata <- if (is.matrix(cases)) ncol(cases) else length(cases)
  years <- if (no_weighting(discount_rate, age_weighting)) {
    rep(duration_years, n_strata)
  } else {
    stopifnot(!is.null(age), length(age) == n_strata)
    weighted_years(age, duration_years, discount_rate, age_weighting)
  }
  if (is.matrix(cases)) {
    stopifnot(length(disability_weight) %in% c(1, nrow(cases)))
    # a length-nrow vector recycles down each column: row i gets weight i
    return(sweep(cases * disability_weight, 2, years, `*`))
  }
  stopifnot(length(disability_weight) == 1)
  cases * disability_weight * years
}
