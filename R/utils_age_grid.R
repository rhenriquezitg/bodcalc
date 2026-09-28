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
