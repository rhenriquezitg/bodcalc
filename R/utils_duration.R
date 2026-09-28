# Duration helpers ------------------------------------------------------------
# Health-state durations are entered as a number plus a unit and converted to
# years before YLD is computed.

DURATION_UNITS <- c("Days" = "days", "Weeks" = "weeks",
                    "Months" = "months", "Years" = "years")

DAYS_PER_YEAR  <- 365.25
WEEKS_PER_YEAR <- 365.25 / 7   # 52.1786
MONTHS_PER_YEAR <- 12

#' Convert a duration to years
#' @param value numeric duration
#' @param unit one of "days", "weeks", "months", "years"
#' @return numeric duration in years
duration_to_years <- function(value, unit) {
  unit <- tolower(unit)
  if (!all(unit %in% DURATION_UNITS)) {
    stop("Unknown duration unit: ", paste(setdiff(unit, DURATION_UNITS), collapse = ", "))
  }
  divisor <- c(days = DAYS_PER_YEAR, weeks = WEEKS_PER_YEAR,
               months = MONTHS_PER_YEAR, years = 1)[unit]
  unname(value / divisor)
}

#' Human-readable echo of a duration conversion, e.g. "14 days = 0.0383 years"
format_duration <- function(value, unit) {
  if (is.null(value) || is.na(value)) return("")
  yrs <- duration_to_years(value, unit)
  sprintf("%s %s = %s years", format(value, big.mark = ","), tolower(unit),
          formatC(yrs, format = "g", digits = 4))
}
