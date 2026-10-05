# Discounting and age weighting ------------------------------------------------
# Standard GBD 1990 formula (Murray 1994) for the number of healthy years
# represented by `span` years lived from exact age `age`, with continuous
# discounting at rate r and optional age weighting:
#
#   K * C * exp(r*a) / (beta + r)^2 *
#       { exp(-(beta + r)(L + a)) * (-(beta + r)(L + a) - 1)
#         - exp(-(beta + r) a)    * (-(beta + r) a - 1) }
#   + (1 - K) / r * (1 - exp(-r L))
#
# a = age, L = span, K = 1 with age weighting and 0 without, C = 0.1658 and
# beta = 0.04 (GBD 1990 age-weighting constants). With r = 0 and K = 0 the
# result is L itself, i.e. no discounting and no age weighting.
#
# For YLL, `span` is the remaining life expectancy at the age of death.
# For YLD, `span` is the duration of the health state from the age of onset.

AGE_WEIGHT_C    <- 0.1658
AGE_WEIGHT_BETA <- 0.04

#' @param age exact age (years) at death or onset; vector
#' @param span years of life lost or lived with disability; vector or scalar
#' @param discount_rate annual rate as a fraction (0.03 = 3%); 0 = none
#' @param age_weighting TRUE to apply GBD 1990 age weights
#' @return numeric vector, same length as the longer of `age` and `span`
weighted_years <- function(age, span, discount_rate = 0, age_weighting = FALSE,
                           C = AGE_WEIGHT_C, beta = AGE_WEIGHT_BETA) {
  stopifnot(length(discount_rate) == 1, !is.na(discount_rate), discount_rate >= 0)
  n <- max(length(age), length(span))
  age <- rep_len(age, n); span <- rep_len(span, n)
  r <- discount_rate
  if (!isTRUE(age_weighting)) {
    return(if (r == 0) span else (1 - exp(-r * span)) / r)
  }
  b <- beta + r
  C * exp(r * age) / b^2 *
    (exp(-b * (span + age)) * (-b * (span + age) - 1) -
       exp(-b * age) * (-b * age - 1))
}

#' TRUE when neither discounting nor age weighting is applied
no_weighting <- function(discount_rate = 0, age_weighting = FALSE) {
  discount_rate == 0 && !isTRUE(age_weighting)
}

#' One-line description of the settings, for tables, Excel and the report
describe_weighting <- function(discount_rate = 0, age_weighting = FALSE) {
  sprintf("discount rate %s%%, age weighting %s",
          format(round(discount_rate * 100, 2), nsmall = 0, trim = TRUE),
          if (isTRUE(age_weighting)) "yes" else "no")
}
