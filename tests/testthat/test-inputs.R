test_that("duration units convert to years", {
  expect_equal(duration_to_years(365.25, "days"), 1)
  expect_equal(duration_to_years(6, "months"), 0.5)
  expect_equal(duration_to_years(365.25 / 7, "weeks"), 1)
  expect_equal(duration_to_years(2, "years"), 2)
  expect_error(duration_to_years(1, "fortnights"))
})

test_that("sample population file loads and the total row is ignored", {
  p <- load_age_sex_upload(example_file("population_pyramid.csv"), "Population")
  expect_true(p$ok)
  expect_equal(nrow(p$wide), 21)
  expect_equal(sum(p$wide$males), 7128000)
  expect_match(p$notes, "total", all = FALSE)
})

test_that("age-group mismatch is rejected with a clear error", {
  p <- load_age_sex_upload(example_file("population_pyramid.csv"), "Population")
  tmp <- tempfile(fileext = ".csv")
  d <- utils::read.csv(example_file("appendicitis_deaths.csv"))
  d <- d[d$age != "95" & d$age != "total", ]
  d$age[d$age == "90"] <- "90"
  d <- rbind(d, data.frame(age = "100", males = 1, females = 1))
  utils::write.csv(d, tmp, row.names = FALSE)
  res <- load_age_sex_upload(tmp, "Deaths", sort(unique(p$data$age)))
  expect_false(res$ok)
  expect_match(res$errors, "missing age group\\(s\\) 95")
  expect_match(res$errors, "unexpected age group\\(s\\) 100")
})

test_that("missing columns, non-numeric and negative values are rejected", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("age,men,women", "0,1,2"), tmp)
  expect_match(load_age_sex_upload(tmp, "X")$errors, "missing column")
  writeLines(c("age,males,females", "0,1,", "5,-2,3"), tmp)
  res <- load_age_sex_upload(tmp, "X")
  expect_false(res$ok)
  expect_true(any(grepl("empty or non-numeric", res$errors)))
  expect_true(any(grepl("negative", res$errors)))
})

test_that("semicolon files with decimal commas are read", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("age;males;females", "0;1,5;2", "5;3;4,25"), tmp)
  res <- load_age_sex_upload(tmp, "X")
  expect_true(res$ok)
  expect_equal(res$wide$males, c(1.5, 3))
  expect_equal(res$wide$females, c(2, 4.25))
})

test_that("templates carry the reference age grid and empty values", {
  tmp <- tempfile(fileext = ".csv")
  write_csv_template(c(0, 1, 5), tmp)
  expect_equal(readLines(tmp), c('"age","males","females"', "0,,", "1,,", "5,,"))
})

test_that("health-state parameters are validated", {
  expect_length(validate_health_state_params("A", 0.3, 0.2, 0.4, 10), 0)
  expect_match(validate_health_state_params("A", 0.5, 0.2, 0.4, 10), "lower")
  expect_match(validate_health_state_params("", 0.3, 0.2, 0.4, 10), "Name")
  expect_match(validate_health_state_params("A", 0.3, 0.2, 1.4, 10), "between 0 and 1")
  expect_match(validate_health_state_params("A", 0.3, 0.2, 0.4, 0), "greater than 0")
})
