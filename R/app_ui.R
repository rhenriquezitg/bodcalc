# Top-level UI -----------------------------------------------------------------

app_ui <- function() {
  bslib::page_navbar(
    title = "Burden of Disease Calculator",
    id = "steps",
    theme = bslib::bs_theme(version = 5, preset = "flatly",
                            primary = "#2A6F97", base_font = bslib::font_google("Inter", local = FALSE)),
    header = shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "styles.css")),
    fillable = FALSE,
    bslib::nav_panel("1 · Population", mod_population_ui("population")),
    bslib::nav_panel("2 · Deaths", mod_deaths_ui("deaths")),
    bslib::nav_panel("3 · Health states", mod_health_states_ui("health_states")),
    bslib::nav_panel("4 · Economic inputs", mod_economics_ui("economics")),
    bslib::nav_panel("5 · Results", mod_results_ui("results")),
    bslib::nav_spacer(),
    bslib::nav_panel("Methods", methods_ui())
  )
}

methods_ui <- function() {
  bslib::layout_columns(
    col_widths = c(-2, 8, -2),
    bslib::card(
      bslib::card_header("Methods"),
      shiny::markdown("
**DALY = YLL + YLD**, calculated for one disease per run. The disease's health
states (severity levels or complications) are entered in step 3.

**Years of life lost (YLL)** = deaths × standard life expectancy at the lower
bound of the age group. Life expectancy comes from the preloaded GBD reference life
table, interpolated onto the age groups of the population file. No discounting and
no age weighting are applied.

**Years lived with disability (YLD)** = cases × disability weight × duration
(incidence-based), summed over all health states. Durations entered in days, weeks
or months are converted to years (365.25 days per year).

**Uncertainty.** Each Monte Carlo iteration draws the disability weight of every
health state from a PERT distribution (mode = mean, bounds = 95% CI) and the cases
and deaths in each age-sex group from Poisson distributions with the observed counts
as means. The 95% uncertainty interval (UI) is the 2.5th–97.5th percentile of the
draws. The *estimate* column uses observed counts and mean disability weights.

**Economic burden.**
- Direct costs = cases × average cost per case + deaths × average cost per death
- Productivity loss = DALYs × GDP per capita (human-capital approach)
- Total economic burden = direct costs + productivity loss

The currency is a label only; no exchange-rate or PPP conversion is applied.

**Input files.** Population, deaths and each health state's cases are CSV files
with the columns `age, males, females` and one row per age group, where `age` is the
lower bound of the group (0, 1, 5, 10, …). A `total` row is ignored. All files
must use exactly the same age groups as the population file. Semicolon-separated
files with decimal commas (Excel in many European locales) are accepted. Use the
*Download template* buttons to get correctly formatted empty files.

**References.** Devleesschauwer B, et al. Calculating disability-adjusted life
years to quantify burden of disease. *Int J Public Health* 2014;59:565–69.
Devleesschauwer B, et al. DALY calculation in practice: a stepwise approach.
*Int J Public Health* 2014;59:571–74.
")
    )
  )
}
