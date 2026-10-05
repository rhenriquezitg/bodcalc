# Monte Carlo uncertainty propagation ----------------------------------------
# Each iteration draws, independently:
#   - the disability weight of each health state ~ PERT(lower, mean, upper)
#   - the cases in each age-sex cell of each health state ~ Poisson(observed)
#   - the deaths in each age-sex cell ~ Poisson(observed)
# and pushes them through YLL, YLD, DALY and the economic burden.

#' Random draws from a PERT distribution (lambda = 4), with the mean input
#' used as the mode.
rpert <- function(n, min, mode, max, lambda = 4) {
  if (max <= min) return(rep(mode, n))
  a <- 1 + lambda * (mode - min) / (max - min)
  b <- 1 + lambda * (max - mode) / (max - min)
  min + stats::rbeta(n, a, b) * (max - min)
}

#' n x K matrix of Poisson draws, column k with mean lambda[k]
rpois_matrix <- function(n, lambda) {
  matrix(stats::rpois(n * length(lambda), rep(lambda, each = n)),
         nrow = n, ncol = length(lambda))
}

#' Summarise a point estimate and its draws
summarise_draws <- function(estimate, draws, conf = 0.95) {
  p <- c((1 - conf) / 2, 0.5, 1 - (1 - conf) / 2)
  if (is.matrix(draws)) {
    q <- apply(draws, 2, stats::quantile, probs = p, names = FALSE, na.rm = TRUE)
    data.frame(estimate = estimate, median = q[2, ], lower = q[1, ], upper = q[3, ])
  } else {
    q <- stats::quantile(draws, probs = p, names = FALSE, na.rm = TRUE)
    data.frame(estimate = estimate, median = q[2], lower = q[1], upper = q[3])
  }
}

#' Full burden calculation with uncertainty
#'
#' @param population,deaths long data frames (age, sex, value)
#' @param health_states list of lists: name, cases (long df), dw_mean, dw_lower,
#'   dw_upper, duration_years
#' @param life_table data.frame (age, life_expectancy)
#' @param gdp_per_capita,cost_per_case,cost_per_death numeric scalars
#' @param n_iter number of Monte Carlo iterations
#' @param seed optional integer seed for reproducibility
#' @param conf width of the uncertainty interval
#' @param progress optional function(fraction, detail) for progress reporting
#' @param discount_rate annual discount rate as a fraction (default 0 = none)
#' @param age_weighting TRUE to apply GBD 1990 age weights (default FALSE)
#' @return list(totals, by_stratum, by_state, draws, settings)
run_burden_simulation <- function(population, deaths, health_states, life_table,
                                  gdp_per_capita, cost_per_case, cost_per_death,
                                  n_iter = 10000, seed = NULL, conf = 0.95,
                                  progress = NULL, discount_rate = 0,
                                  age_weighting = FALSE) {
  stopifnot(length(health_states) >= 1, n_iter >= 100,
            !is.na(discount_rate), discount_rate >= 0)
  if (!is.null(seed) && !is.na(seed)) set.seed(seed)
  tick <- function(f, d) if (is.function(progress)) progress(f, d)

  det <- calc_daly_deterministic(population, deaths, health_states, life_table,
                                 discount_rate, age_weighting)
  age_mid <- det$age_mid
  bs  <- det$by_stratum
  K   <- nrow(bs); n <- n_iter
  pop <- bs$population

  # Deaths and YLL ----------------------------------------------------------
  tick(0.1, "Simulating deaths")
  D   <- rpois_matrix(n, bs$deaths)
  YLL <- calc_yll(D, det$life_expectancy, age_mid, discount_rate, age_weighting)

  # Cases and YLD per health state --------------------------------------------
  C_total <- matrix(0, n, K); YLD <- matrix(0, n, K)
  state_rows <- list()
  for (i in seq_along(health_states)) {
    hs <- health_states[[i]]
    tick(0.1 + 0.6 * i / length(health_states), paste("Simulating", hs$name))
    cases <- det$by_state[[i]]$cases
    C  <- rpois_matrix(n, cases)
    dw <- rpert(n, hs$dw_lower, hs$dw_mean, hs$dw_upper)
    Y  <- calc_yld(C, dw, hs$duration_years, age_mid, discount_rate, age_weighting)
    C_total <- C_total + C
    YLD <- YLD + Y
    state_rows[[length(state_rows) + 1]] <- cbind(
      health_state = hs$name,
      measure = c("Cases", "YLD"),
      rbind(summarise_draws(sum(cases), rowSums(C), conf),
            summarise_draws(sum(det$by_state[[i]]$yld), rowSums(Y), conf))
    )
    rm(C, Y)
  }
  DALY <- YLL + YLD

  # Stratum-level summaries -------------------------------------------------
  tick(0.8, "Summarising")
  rate <- function(M) sweep(M, 2, ifelse(pop > 0, 1e5 / pop, NA_real_), `*`)
  stratum_measures <- list(
    "Deaths"           = list(bs$deaths, D),
    "Cases"            = list(bs$cases, C_total),
    "YLL"              = list(bs$yll, YLL),
    "YLD"              = list(bs$yld, YLD),
    "DALY"             = list(bs$daly, DALY),
    "DALY per 100,000" = list(bs$daly_per_100k, rate(DALY))
  )
  by_stratum <- do.call(rbind, lapply(names(stratum_measures), function(m) {
    x <- stratum_measures[[m]]
    cbind(bs[, c("age", "sex")], measure = m, summarise_draws(x[[1]], x[[2]], conf))
  }))

  # Totals ----------------------------------------------------------------
  tot_pop <- sum(pop)
  draws <- data.frame(
    deaths = rowSums(D), cases = rowSums(C_total),
    yll = rowSums(YLL), yld = rowSums(YLD), daly = rowSums(DALY)
  )
  econ_draws <- calc_economic_burden(draws$cases, draws$deaths, draws$daly,
                                     cost_per_case, cost_per_death, gdp_per_capita)
  draws <- cbind(draws, econ_draws)
  point <- c(deaths = sum(bs$deaths), cases = sum(bs$cases), yll = sum(bs$yll),
             yld = sum(bs$yld), daly = sum(bs$daly))
  econ_point <- calc_economic_burden(point[["cases"]], point[["deaths"]], point[["daly"]],
                                     cost_per_case, cost_per_death, gdp_per_capita)

  total_measures <- list(
    "Deaths"                = list(point[["deaths"]], draws$deaths),
    "Cases"                 = list(point[["cases"]], draws$cases),
    "YLL"                   = list(point[["yll"]], draws$yll),
    "YLD"                   = list(point[["yld"]], draws$yld),
    "DALY"                  = list(point[["daly"]], draws$daly),
    "YLL per 100,000"       = list(point[["yll"]] / tot_pop * 1e5, draws$yll / tot_pop * 1e5),
    "YLD per 100,000"       = list(point[["yld"]] / tot_pop * 1e5, draws$yld / tot_pop * 1e5),
    "DALY per 100,000"      = list(point[["daly"]] / tot_pop * 1e5, draws$daly / tot_pop * 1e5),
    "Direct costs"          = list(econ_point$direct_costs, draws$direct_costs),
    "Productivity loss"     = list(econ_point$productivity_loss, draws$productivity_loss),
    "Total economic burden" = list(econ_point$total_economic_burden, draws$total_economic_burden)
  )
  totals <- do.call(rbind, lapply(names(total_measures), function(m) {
    cbind(measure = m, summarise_draws(total_measures[[m]][[1]], total_measures[[m]][[2]], conf))
  }))
  tick(1, "Done")

  list(
    totals = totals,
    by_stratum = by_stratum,
    by_state = do.call(rbind, state_rows),
    draws = draws,
    settings = list(n_iter = n_iter, seed = seed, conf = conf,
                    discount_rate = discount_rate, age_weighting = age_weighting,
                    population_total = tot_pop,
                    gdp_per_capita = gdp_per_capita,
                    cost_per_case = cost_per_case, cost_per_death = cost_per_death,
                    life_expectancy = data.frame(age = unique(bs$age),
                                                 life_expectancy = det$life_expectancy[!duplicated(bs$age)]),
                    health_states = do.call(rbind, lapply(health_states, function(hs) data.frame(
                      health_state = hs$name, dw_mean = hs$dw_mean, dw_lower = hs$dw_lower,
                      dw_upper = hs$dw_upper,
                      duration = if (!is.null(hs$duration_value)) hs$duration_value else hs$duration_years,
                      duration_unit = if (!is.null(hs$duration_unit)) hs$duration_unit else "years",
                      duration_years = hs$duration_years))),
                    timestamp = Sys.time())
  )
}
