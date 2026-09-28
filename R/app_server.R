# Top-level server: wires the modules together ----------------------------------

app_server <- function(input, output, session, life_table) {
  default_ages <- default_age_grid(life_table)

  population <- mod_population_server("population", default_ages)
  pop_data <- shiny::reactive({ p <- population(); if (!is.null(p) && p$ok) p$data else NULL })
  ref_ages <- shiny::reactive({ d <- pop_data(); if (is.null(d)) NULL else sort(unique(d$age)) })

  deaths <- mod_deaths_server("deaths", default_ages, ref_ages, pop_data)
  health_states <- mod_health_states_server("health_states", default_ages, ref_ages, pop_data)
  economics <- mod_economics_server("economics")

  mod_results_server("results", population, deaths, health_states, economics, life_table)
}
