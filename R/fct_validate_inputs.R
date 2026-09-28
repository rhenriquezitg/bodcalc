# Reading and validating uploads ----------------------------------------------
# Every age-sex table (population, deaths, cases) has the columns
#   age, males, females
# with one row per age group. A trailing "total" row is allowed and ignored.

REQUIRED_COLUMNS <- c("age", "males", "females")

#' Read an age-sex CSV. Handles comma- or semicolon-separated files
#' (semicolon files, as saved by Excel in many European locales, are read with
#' a decimal comma).
read_age_sex_csv <- function(path) {
  first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  first <- sub("^﻿", "", first)
  semicolon <- grepl(";", first) && !grepl(",", first)
  raw <- utils::read.csv(path, sep = if (semicolon) ";" else ",",
                         colClasses = "character", check.names = FALSE,
                         strip.white = TRUE, na.strings = c("", "NA"),
                         fileEncoding = "UTF-8-BOM")
  if (semicolon) {
    # decimal comma -> decimal point (columns are read as text)
    raw[] <- lapply(raw, function(x) gsub(",", ".", x, fixed = TRUE))
  }
  raw
}

#' Validate a wide age-sex table.
#' @param raw data.frame as read by read_age_sex_csv()
#' @param what label used in messages ("Population", "Deaths", ...)
#' @param reference_ages numeric age grid to match exactly (NULL for population)
#' @return list(ok, data = long df [age, sex, value], wide, errors, warnings)
validate_age_sex_table <- function(raw, what = "This file", reference_ages = NULL,
                                   population = NULL) {
  errors <- character(); warnings <- character(); notes <- character()
  result <- function() list(ok = length(errors) == 0,
                            data = if (length(errors) == 0) long else NULL,
                            wide = if (length(errors) == 0) wide else NULL,
                            errors = errors, warnings = warnings, notes = notes)
  long <- NULL; wide <- NULL

  names(raw) <- tolower(trimws(names(raw)))
  missing_cols <- setdiff(REQUIRED_COLUMNS, names(raw))
  if (length(missing_cols)) {
    errors <- c(errors, sprintf(
      "%s is missing column(s): %s. The file must have the columns age, males, females.",
      what, paste(missing_cols, collapse = ", ")))
    return(result())
  }
  extra_cols <- setdiff(names(raw), REQUIRED_COLUMNS)
  if (length(extra_cols)) {
    notes <- c(notes, sprintf("Ignored extra column(s): %s.",
                                    paste(extra_cols, collapse = ", ")))
  }
  raw <- raw[, REQUIRED_COLUMNS]

  # Drop fully empty rows and a "total" row
  empty <- apply(raw, 1, function(r) all(is.na(r) | r == ""))
  raw <- raw[!empty, , drop = FALSE]
  is_total <- tolower(trimws(raw$age)) %in% c("total", "totals", "all ages", "all")
  if (any(is_total)) {
    notes <- c(notes, "The 'total' row was ignored; totals are recomputed from the age groups.")
    raw <- raw[!is_total, , drop = FALSE]
  }
  if (nrow(raw) == 0) {
    errors <- c(errors, sprintf("%s has no data rows.", what))
    return(result())
  }

  age <- parse_age_lower(raw$age)
  bad_age <- which(is.na(age))
  if (length(bad_age)) {
    errors <- c(errors, sprintf("%s: unreadable age value(s) %s. Use the lower bound of each age group (0, 1, 5, 10, ...).",
                                what, paste0("'", raw$age[bad_age], "'", collapse = ", ")))
  }
  dup <- unique(age[duplicated(age) & !is.na(age)])
  if (length(dup)) {
    errors <- c(errors, sprintf("%s: duplicated age group(s) %s.", what, paste(dup, collapse = ", ")))
  }

  to_num <- function(x) suppressWarnings(as.numeric(gsub("[[:space:]]", "", x)))
  males <- to_num(raw$males); females <- to_num(raw$females)
  for (sx in c("males", "females")) {
    v <- if (sx == "males") males else females
    bad <- which(is.na(v))
    if (length(bad)) {
      errors <- c(errors, sprintf("%s: empty or non-numeric '%s' value(s) at age(s) %s.",
                                  what, sx, paste(raw$age[bad], collapse = ", ")))
    }
    neg <- which(!is.na(v) & v < 0)
    if (length(neg)) {
      errors <- c(errors, sprintf("%s: negative '%s' value(s) at age(s) %s.",
                                  what, sx, paste(raw$age[neg], collapse = ", ")))
    }
  }
  if (length(errors)) return(result())

  if (!is.null(reference_ages)) {
    grid_error <- compare_age_grids(reference_ages, age, what)
    if (!is.null(grid_error)) {
      errors <- c(errors, grid_error)
      return(result())
    }
  }

  ord <- order(age)
  wide <- data.frame(age = age[ord], males = males[ord], females = females[ord])
  long <- to_long(wide)

  if (!is.null(population)) {
    merged <- merge(long, population, by = c("age", "sex"), suffixes = c("", "_pop"))
    over <- merged[merged$value > merged$value_pop, ]
    if (nrow(over)) {
      warnings <- c(warnings, sprintf(
        "%s exceed the population in %d age-sex group(s) (e.g. %s, age %s). Check the figures.",
        what, nrow(over), over$sex[1], over$age[1]))
    }
  }
  result()
}

#' Wide (age, males, females) -> long (age, sex, value), sorted by sex then age
to_long <- function(wide) {
  out <- rbind(
    data.frame(age = wide$age, sex = "Males",   value = wide$males),
    data.frame(age = wide$age, sex = "Females", value = wide$females)
  )
  out[order(out$sex != "Males", out$age), , drop = FALSE]
}

#' Read + validate in one step, catching read errors
load_age_sex_upload <- function(path, what, reference_ages = NULL, population = NULL) {
  raw <- tryCatch(read_age_sex_csv(path), error = function(e) e)
  if (inherits(raw, "error")) {
    return(list(ok = FALSE, data = NULL, wide = NULL,
                errors = sprintf("%s could not be read as a CSV file (%s).", what, conditionMessage(raw)),
                warnings = character()))
  }
  validate_age_sex_table(raw, what, reference_ages, population)
}

#' Validate the disability weight and duration of one health state
#' @return character vector of problems (empty when valid)
validate_health_state_params <- function(name, dw_mean, dw_lower, dw_upper,
                                         duration_value) {
  msgs <- character()
  if (is.null(name) || !nzchar(trimws(name))) msgs <- c(msgs, "Name is required.")
  vals <- list(`DW mean` = dw_mean, `DW lower` = dw_lower, `DW upper` = dw_upper)
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (is.null(v) || is.na(v)) msgs <- c(msgs, paste(nm, "is required."))
    else if (v < 0 || v > 1) msgs <- c(msgs, paste(nm, "must be between 0 and 1."))
  }
  if (!any(vapply(vals, function(v) is.null(v) || is.na(v), logical(1)))) {
    if (!(dw_lower <= dw_mean && dw_mean <= dw_upper)) {
      msgs <- c(msgs, "Disability weights must satisfy lower ≤ mean ≤ upper.")
    }
  }
  if (is.null(duration_value) || is.na(duration_value)) msgs <- c(msgs, "Duration is required.")
  else if (duration_value <= 0) msgs <- c(msgs, "Duration must be greater than 0.")
  msgs
}
