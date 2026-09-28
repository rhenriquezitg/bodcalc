# DALY = YLL + YLD (point estimates) -------------------------------------------

#' Return the values of `df` (age, sex, value) in the row order of `reference`
align_to_reference <- function(df, reference) {
  key_ref <- paste(reference$age, reference$sex)
  key_df  <- paste(df$age, df$sex)
  idx <- match(key_ref, key_df)
  if (anyNA(idx)) stop("Age-sex groups do not match the population table.")
  df$value[idx]
}

#' Deterministic DALY calculation using observed counts and mean disability
#' weights.
#' @param population long df (age, sex, value)
#' @param deaths long df (age, sex, value)
#' @param health_states list of lists with name, cases (long df), dw_mean,
#'   duration_years
#' @param life_table data.frame (age, life_expectancy)
#' @return list(by_stratum, by_state, life_expectancy)
calc_daly_deterministic <- function(population, deaths, health_states, life_table) {
  strata <- population[, c("age", "sex")]
  pop <- population$value
  le  <- life_expectancy_at(strata$age, life_table)
  d   <- align_to_reference(deaths, population)

  yll <- calc_yll(d, le)
  cases_total <- numeric(nrow(strata)); yld <- numeric(nrow(strata))
  by_state <- lapply(health_states, function(hs) {
    cs <- align_to_reference(hs$cases, population)
    y  <- calc_yld(cs, hs$dw_mean, hs$duration_years)
    list(name = hs$name, cases = cs, yld = y)
  })
  for (s in by_state) {
    cases_total <- cases_total + s$cases
    yld <- yld + s$yld
  }
  daly <- yll + yld
  by_stratum <- data.frame(
    strata, population = pop, life_expectancy = le, deaths = d,
    cases = cases_total, yll = yll, yld = yld, daly = daly,
    daly_per_100k = ifelse(pop > 0, daly / pop * 1e5, NA_real_)
  )
  list(by_stratum = by_stratum, by_state = by_state, life_expectancy = le)
}
