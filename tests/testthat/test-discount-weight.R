# Independent check: integrate the age weight x discount factor numerically.
#   years = integral over x in [a, a + L] of (K C x e^(-beta x) + (1 - K)) e^(-r (x - a)) dx
numeric_years <- function(a, L, r, K) {
  f <- function(x) (K * 0.1658 * x * exp(-0.04 * x) + (1 - K)) * exp(-r * (x - a))
  stats::integrate(f, lower = a, upper = a + L, rel.tol = 1e-10)$value
}

test_that("defaults (0% discount, no age weighting) return the span unchanged", {
  expect_equal(weighted_years(c(0.5, 7.5, 97.5), c(80, 50, 5)), c(80, 50, 5))
  expect_true(no_weighting())
  expect_false(no_weighting(0.03, FALSE))
  expect_false(no_weighting(0, TRUE))
})

test_that("discounting alone follows (1 - exp(-rL)) / r", {
  expect_equal(weighted_years(30, 10, 0.03), (1 - exp(-0.3)) / 0.03)
  expect_equal(weighted_years(30, 10, 0.03), numeric_years(30, 10, 0.03, K = 0))
  expect_lt(weighted_years(30, 10, 0.03), 10)
})

test_that("age weighting matches numerical integration, with and without discounting", {
  for (r in c(0, 0.03, 0.05)) {
    for (a in c(0.5, 7.5, 40, 97.5)) {
      for (L in c(0.0383, 5, 40, 88)) {
        expect_equal(weighted_years(a, L, r, TRUE), numeric_years(a, L, r, K = 1),
                     tolerance = 1e-8, info = paste("r", r, "age", a, "span", L))
      }
    }
  }
})

test_that("a zero span is worth nothing and bad rates are rejected", {
  expect_equal(weighted_years(30, 0, 0.03, TRUE), 0)
  expect_error(weighted_years(30, 10, -0.01))
  expect_error(weighted_years(30, 10, NA))
})

test_that("age midpoints use the interval [lower, next lower)", {
  ages <- c(0, 1, seq(5, 95, 5))
  expect_equal(age_midpoints(ages), c(0.5, 3, seq(7.5, 92.5, 5), 97.5))
  # aligned with repeated and unsorted input (age-sex strata)
  expect_equal(age_midpoints(c(5, 0, 5, 0, 1, 1)), c(7.5, 0.5, 7.5, 0.5, 3, 3))
})

test_that("calc_yll and calc_yld apply the options per stratum, vector and matrix alike", {
  age <- c(0.5, 3, 7.5, 97.5); le <- c(88, 85, 80, 5)
  deaths <- c(10, 20, 30, 40)
  expect_equal(calc_yll(deaths, le), deaths * le)
  expect_equal(calc_yll(deaths, le, age, 0.03, TRUE),
               deaths * weighted_years(age, le, 0.03, TRUE))
  m <- rbind(deaths, deaths * 2)
  expect_equal(calc_yll(m, le, age, 0.03, FALSE),
               rbind(deaths, deaths * 2) * rep(weighted_years(age, le, 0.03, FALSE), each = 2))
  cases <- c(100, 200, 300, 400)
  expect_equal(calc_yld(cases, 0.3, 0.5), cases * 0.3 * 0.5)
  expect_equal(calc_yld(cases, 0.3, 0.5, age, 0.03, TRUE),
               cases * 0.3 * weighted_years(age, 0.5, 0.03, TRUE))
  cm <- rbind(cases, cases * 3); dw <- c(0.2, 0.4)
  expect_equal(calc_yld(cm, dw, 0.5, age, 0.05, TRUE)[2, ],
               cases * 3 * 0.4 * weighted_years(age, 0.5, 0.05, TRUE))
  expect_error(calc_yll(deaths, le, NULL, 0.03))
})

test_that("default settings leave the appendicitis results unchanged", {
  a <- appendicitis()
  base <- calc_daly_deterministic(a$pop, a$deaths, a$hs, life_table)
  same <- calc_daly_deterministic(a$pop, a$deaths, a$hs, life_table, 0, FALSE)
  expect_identical(base$by_stratum, same$by_stratum)
  expect_equal(sum(base$by_stratum$yll), 9971.8, tolerance = 1e-4)
})

test_that("discounting and age weighting change the deterministic results as specified", {
  a <- appendicitis()
  base <- calc_daly_deterministic(a$pop, a$deaths, a$hs, life_table)
  disc <- calc_daly_deterministic(a$pop, a$deaths, a$hs, life_table, 0.03, FALSE)
  aw   <- calc_daly_deterministic(a$pop, a$deaths, a$hs, life_table, 0, TRUE)
  both <- calc_daly_deterministic(a$pop, a$deaths, a$hs, life_table, 0.03, TRUE)
  expect_lt(sum(disc$by_stratum$yll), sum(base$by_stratum$yll))
  expect_lt(sum(disc$by_stratum$yld), sum(base$by_stratum$yld))
  expect_false(isTRUE(all.equal(sum(aw$by_stratum$yll), sum(base$by_stratum$yll))))
  expect_lt(sum(both$by_stratum$daly), sum(aw$by_stratum$daly))
  # independent hand calculation for every stratum
  bs <- both$by_stratum
  mid <- age_midpoints(bs$age)
  expect_equal(bs$yll, mapply(function(d, le, x) d * numeric_years(x, le, 0.03, 1),
                              bs$deaths, bs$life_expectancy, mid), tolerance = 1e-6)
  yld_hand <- Reduce(`+`, lapply(a$hs, function(hs) {
    cs <- align_to_reference(hs$cases, a$pop)
    mapply(function(c, x) c * hs$dw_mean * numeric_years(x, hs$duration_years, 0.03, 1), cs, mid)
  }))
  expect_equal(bs$yld, yld_hand, tolerance = 1e-6)
})

test_that("the simulation carries the options through and stays reproducible", {
  a <- appendicitis()
  run <- function(...) run_burden_simulation(a$pop, a$deaths, a$hs, life_table, 6000, 1500, 3000,
                                             n_iter = 1000, seed = 7, ...)
  base <- run()
  expect_equal(base$settings$discount_rate, 0)
  expect_false(base$settings$age_weighting)
  both <- run(discount_rate = 0.03, age_weighting = TRUE)
  expect_equal(both$settings$discount_rate, 0.03)
  expect_true(both$settings$age_weighting)
  expect_identical(both$totals, run(discount_rate = 0.03, age_weighting = TRUE)$totals)
  est <- function(r, m) r$totals$estimate[r$totals$measure == m]
  expect_equal(est(base, "Deaths"), est(both, "Deaths"))
  expect_equal(est(base, "Cases"), est(both, "Cases"))
  expect_lt(est(run(discount_rate = 0.03), "DALY"), est(base, "DALY"))
  expect_equal(est(both, "Productivity loss"), est(both, "DALY") * 6000)
  row <- both$totals[both$totals$measure == "DALY", ]
  expect_true(row$lower <= row$estimate && row$estimate <= row$upper)
  expect_error(run(discount_rate = -0.01))
})

test_that("the settings are described and exported", {
  expect_equal(describe_weighting(0, FALSE), "discount rate 0%, age weighting no")
  expect_equal(describe_weighting(0.03, TRUE), "discount rate 3%, age weighting yes")
  a <- appendicitis()
  r <- run_burden_simulation(a$pop, a$deaths, a$hs, life_table, 6000, 1500, 3000,
                             n_iter = 1000, seed = 1, discount_rate = 0.03, age_weighting = TRUE)
  r$settings$currency <- "USD"; r$settings$title <- ""
  st <- results_workbook(r)$Settings
  expect_equal(st$value[st$setting == "Discount rate"], "3%")
  expect_equal(st$value[st$setting == "Age weighting"], "Yes")
})
