# Top-level UI -----------------------------------------------------------------

app_ui <- function() {
  bslib::page_navbar(
    title = "Burden of Disease Calculator",
    id = "steps",
    theme = bslib::bs_theme(version = 5, preset = "flatly",
                            primary = "#2A6F97", base_font = bslib::font_google("Inter", local = FALSE)),
    header = shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "styles.css")),
    footer = shiny::div(
      class = "app-footer text-center text-muted small py-3",
      "Created by Rodrigo Henriquez, TB Unit, ITM Antwerp, 2026. For educational purposes only."),
    fillable = FALSE,
    bslib::nav_panel("1 · Population", mod_population_ui("population")),
    bslib::nav_panel("2 · Deaths", mod_deaths_ui("deaths")),
    bslib::nav_panel("3 · Health states", mod_health_states_ui("health_states")),
    bslib::nav_panel("4 · Economic inputs", mod_economics_ui("economics")),
    bslib::nav_panel("5 · Results", mod_results_ui("results")),
    bslib::nav_spacer(),
    bslib::nav_panel("Methods", methods_ui()),
    bslib::nav_panel("About", about_ui())
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

**Years of life lost (YLL)** = deaths × standard life expectancy at the average
age of the age group's lower and upper bound. Life expectancy comes from the preloaded
GBD reference life table, interpolated onto the age groups of the population file. By
default no discounting and no age weighting are applied (see below).

**Years lived with disability (YLD)** = cases × disability weight × duration
(incidence-based), summed over all health states. Durations entered in days, weeks
or months are converted to years (365.25 days per year).

**Discounting and age weighting.** Both are optional and are set on the Results page.
The defaults, a 0% discount rate and no age weighting, count every year equally (the GBD
convention since 2010).
- *Discounting.* With an annual rate r, future years count less. A span of L years
  is worth (1 - e^(-rL)) / r years (continuous discounting), counted from the age at
  death or at onset.
- *Age weighting.* A year lived at age x is weighted by C · x · e^(-beta · x), with
  C = 0.1658 and beta = 0.04 (GBD 1990). The weights are applied to the same span.
- *Age used.* The age x of each age group is its midpoint: 7.5 for ages 5-9, 3 for
  ages 1-4, 0.5 for under 1. The last group (95+) is treated as 95-100, midpoint 97.5.
- *Where they apply.* For YLL the span is the remaining life expectancy at death. For
  YLD the span is the duration of the health state from the age at onset. The economic
  burden uses the DALYs as calculated under the chosen settings.

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
lower bound of the group (0, 1, 5, 10, …), given as a single number rather than a
range (not `5-9`) so Excel doesn't autoconvert it to a date. A `total` row is
ignored. All files must use exactly the same age groups as the population file.
Semicolon-separated files with decimal commas (Excel in many European locales) are
accepted. Use the *Download template* buttons to get correctly formatted empty files.

**References.** Devleesschauwer B, et al. Calculating disability-adjusted life
years to quantify burden of disease. *Int J Public Health* 2014;59:565–69.
Devleesschauwer B, et al. DALY calculation in practice: a stepwise approach.
*Int J Public Health* 2014;59:571–74.
Murray CJL. Quantifying the burden of disease: the technical basis for
disability-adjusted life years. *Bull World Health Organ* 1994;72:429–45.
")
    )
  )
}

about_ui <- function() {
  bslib::layout_columns(
    col_widths = c(-2, 8, -2),
    shiny::div(
      bslib::card(
        bslib::card_header("About"),
        shiny::markdown("
**Burden of Disease Calculator** calculates the disability-adjusted life years (DALYs)
and the economic burden of one disease from population, deaths, cases and disability
weights, with Monte Carlo uncertainty intervals.

Created by **Rodrigo Henriquez**, TB Unit, Institute of Tropical Medicine (ITM)
Antwerp, 2026.

**For educational purposes only.**

Source code: [github.com/rhenriquezitg/bodcalc](https://github.com/rhenriquezitg/bodcalc)
")
      ),
      bslib::card(
        bslib::card_header("Pull the app from GitHub"),
        shiny::markdown("
The code is public. To get your own copy, use either option:

1. **Download a ZIP.** Open the
   [repository](https://github.com/rhenriquezitg/bodcalc), click **Code**, then
   **Download ZIP**, and unzip it.
2. **Clone with Git.** In a terminal, run:

```
git clone https://github.com/rhenriquezitg/bodcalc.git
```

To run it on your own computer, open `bodcalc.Rproj` in RStudio, install the packages
once, and start the app:

```
install.packages(c('shiny', 'bslib', 'shinyvalidate', 'DT', 'ggplot2',
                   'writexl', 'rmarkdown', 'knitr', 'testthat'))
shiny::runApp()
```
")
      ),
      bslib::card(
        bslib::card_header("Get bodcalc running on Posit Cloud"),
        shiny::markdown("
Anyone can get an independent, running copy on Posit Cloud in a few minutes, with no
local R installation.

1. Create a free account at [posit.cloud](https://posit.cloud) if you don't have one.
2. From your workspace, click **New Project**, then **New Project from Git Repository**.
3. Paste the repository URL: `https://github.com/rhenriquezitg/bodcalc.git`. Posit Cloud
   clones it into a new RStudio session. This is your own copy; nothing you do in it
   changes the original repository.
4. In the console, install the required packages once:

```
install.packages(c('shiny', 'bslib', 'shinyvalidate', 'DT', 'ggplot2',
                   'writexl', 'rmarkdown', 'knitr', 'testthat'))
```

5. Open `app.R` and click **Run App** (or run `shiny::runApp()` in the console).
6. Try it with the example data in the `datatables` folder. The README lists the
   expected results (539 deaths, DALY 10,526.5) when discounting and age weighting are off.

The downloadable HTML report needs Pandoc, which the RStudio on Posit Cloud already
includes.

To pick up later updates, run `system('git pull')` in the console. If it complains
about local changes, commit or discard them first.
")
      )
    )
  )
}
