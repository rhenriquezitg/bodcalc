# Empty CSV templates ---------------------------------------------------------

#' Build an empty template for an age-sex upload
#' @param ages numeric age grid (lower bounds)
#' @return data.frame with columns age, males, females (values empty)
csv_template <- function(ages) {
  data.frame(age = sort(unique(ages)), males = NA_real_, females = NA_real_)
}

#' Write the template to a file (empty cells for values)
write_csv_template <- function(ages, file) {
  utils::write.csv(csv_template(ages), file, row.names = FALSE, na = "")
}

#' File name for a template, e.g. "template_deaths.csv"
template_filename <- function(what) {
  paste0("template_", gsub("[^a-z0-9]+", "_", tolower(trimws(what))), ".csv")
}
