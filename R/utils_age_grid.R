# Age-grid helpers ------------------------------------------------------------
# Age groups are identified by their lower bound (0, 1, 5, 10, ... 95), the same
# convention as the GBD life table and the sample CSVs.

#' Parse age labels to their numeric lower bound.
#' Accepts "5", "5-9", "5 to 9", "95+", "<1" (-> 0).
#' @return numeric vector, NA where unparseable
parse_age_lower <- function(x) {
  x <- trimws(as.character(x))
  x[grepl("^<\\s*1", x)] <- "0"
  out <- suppressWarnings(as.numeric(sub("^(\\d+(\\.\\d+)?).*$", "\\1", x)))
  out[!grepl("^\\d", x)] <- NA_real_
  out
}

#' Human-readable age-group labels from lower bounds: "0", "1-4", "5-9", ..., "95+"
age_group_labels <- function(ages) {
  ages <- sort(unique(ages))
  upper <- c(ages[-1] - 1, NA)
  ifelse(is.na(upper), paste0(ages, "+"),
         ifelse(upper == ages, as.character(ages), paste0(ages, "-", upper)))
}

#' Midpoint (exact age, in years) of each age group, from the lower bounds.
#' Group i covers [lower_i, lower_i+1), so "5-9" has midpoint 7.5, "1-4" has 3
#' and "<1" has 0.5. The open-ended last group is assumed to be `open_width`
#' years wide (95+ is treated as 95-100, midpoint 97.5).
#' Used as the age at death or onset for discounting and age weighting.
#' @return numeric vector aligned with `ages`
age_midpoints <- function(ages, open_width = 5) {
  u <- sort(unique(ages))
  upper <- c(u[-1], u[length(u)] + open_width)
  mid <- (u + upper) / 2
  mid[match(ages, u)]
}

#' Compare an uploaded age grid against the reference grid (population upload).
#' @return NULL when identical, otherwise a character error message.
compare_age_grids <- function(reference, candidate, what = "This file") {
  reference <- sort(unique(reference))
  candidate <- sort(unique(candidate))
  if (identical(reference, candidate)) return(NULL)
  missing <- setdiff(reference, candidate)
  extra   <- setdiff(candidate, reference)
  parts <- c(
    if (length(missing)) paste0("missing age group(s) ", paste(missing, collapse = ", ")),
    if (length(extra))   paste0("unexpected age group(s) ", paste(extra, collapse = ", "))
  )
  paste0(what, " does not use the same age groups as the population file: ",
         paste(parts, collapse = "; "),
         ". Download the template from this step to get the correct age groups.")
}
