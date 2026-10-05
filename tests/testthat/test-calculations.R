# The `appendicitis()` example data helper lives in setup.R.

test_that("deterministic YLL, YLD and DALY match a hand calculation", {
  a <- appendicitis()
  det <- calc_daly_deterministic(a$pop, a$deaths, a$hs, life_table)
  # hand calculation from the raw CSVs
  lt <- utils::read.csv(example_file("life_expectancy.csv"))
  raw_d <- utils::read.csv(example_file("appendicitis_deaths.csv"))
  raw_d <- raw_d[raw_d$age != "total", ]
  yll <- sum((raw_d$males + raw_d$females) * lt$life_expectancy[match(as.numeric(raw_d$age), lt$age)])
  yld <- (35811 + 9414) * 0.32 * 0.038
  expect_equal(sum(det$by_stratum$yll), yll)
  expect_equal(sum(det$by_stratum$yld), yld)
  expect_equal(sum(det$by_stratum$daly), yll + yld)
})

test_that("life table is interpolated onto other age grids", {
  expect_equal(life_expectancy_at(c(0, 95), life_table),
               life_table$life_expectancy[c(1, nrow(life_table))])
  mid <- life_expectancy_at(2.5, life_table)
  expect_true(mid < life_table$life_expectancy[2] && mid > life_table$life_expectancy[3])
})

test_that("simulation is reproducible and brackets the point estimate", {
  a <- appendicitis()
  run <- function() run_burden_simulation(a$pop, a$deaths, a$hs, life_table,
                                          gdp_per_capita = 6000, cost_per_case = 1500,
                                          cost_per_death = 3000, n_iter = 2000, seed = 42)
  r1 <- run(); r2 <- run()
  expect_identical(r1$totals, r2$totals)
  for (m in c("DALY", "YLL", "YLD", "Deaths", "Cases", "Total economic burden")) {
    row <- r1$totals[r1$totals$measure == m, ]
    expect_true(row$lower <= row$estimate && row$estimate <= row$upper, info = m)
  }
  # Poisson uncertainty on deaths: the UI must have non-zero width
  dth <- r1$totals[r1$totals$measure == "Deaths", ]
  expect_gt(dth$upper - dth$lower, 0)
})

test_that("economic burden follows the specified formulas", {
  e <- calc_economic_burden(total_cases = 100, total_deaths = 2, total_daly = 50,
                            cost_per_case = 10, cost_per_death = 1000, gdp_per_capita = 200)
  expect_equal(e$direct_costs, 100 * 10 + 2 * 1000)
  expect_equal(e$productivity_loss, 50 * 200)
  expect_equal(e$total_economic_burden, 3000 + 10000)
  a <- appendicitis()
  r <- run_burden_simulation(a$pop, a$deaths, a$hs, life_table, 6000, 1500, 3000,
                             n_iter = 1000, seed = 1)
  dal <- r$totals$estimate[r$totals$measure == "DALY"]
  expect_equal(r$totals$estimate[r$totals$measure == "Productivity loss"], dal * 6000)
  expect_equal(r$totals$estimate[r$totals$measure == "Direct costs"], 45225 * 1500 + 539 * 3000)
})

test_that("a zero-width disability weight interval gives constant weights", {
  expect_true(all(rpert(50, 0.3, 0.3, 0.3) == 0.3))
  x <- rpert(5000, 0.22, 0.32, 0.44)
  expect_true(all(x >= 0.22 & x <= 0.44))
})
