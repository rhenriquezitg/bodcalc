# Economic burden -------------------------------------------------------------
# Direct costs      = cases x cost per case + deaths x cost per death
# Productivity loss = DALYs x GDP per capita (human-capital approach)
# Currency is a label only (v1).

#' Vectorised: works on point estimates or on vectors of simulation draws.
#' @return data.frame with direct_costs, productivity_loss, total_economic_burden
calc_economic_burden <- function(total_cases, total_deaths, total_daly,
                                 cost_per_case, cost_per_death, gdp_per_capita) {
  direct <- total_cases * cost_per_case + total_deaths * cost_per_death
  productivity <- total_daly * gdp_per_capita
  data.frame(direct_costs = direct,
             productivity_loss = productivity,
             total_economic_burden = direct + productivity)
}
