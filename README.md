# Burden of Disease Calculator (Shiny)

Calculates DALYs (YLL + YLD) and the economic burden of one disease from
user-supplied population, deaths, cases and disability weights, with Monte Carlo
uncertainty intervals. The GBD reference life table is preloaded. Optional
discounting and age weighting can be set on the Results page.

Created by Rodrigo Henriquez, TB Unit, Institute of Tropical Medicine (ITM) Antwerp, 2026.
**For educational purposes only.**

## Get the app from GitHub

The code is at <https://github.com/rhenriquezitg/bodcalc>. Either download it as a ZIP
(**Code**, then **Download ZIP**) or clone it:

```
git clone https://github.com/rhenriquezitg/bodcalc.git
```

## Run on your computer

1. Open `bodcalc.Rproj` in RStudio (or `setwd()` to this folder).
2. Install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "shinyvalidate", "DT", "ggplot2",
                      "writexl", "rmarkdown", "knitr", "testthat"))
   ```
3. Start the app:

   ```r
   shiny::runApp()
   ```

The HTML report download needs Pandoc, which ships with RStudio.

## Run on Posit Cloud

Anyone can get an independent, running copy on Posit Cloud in a few minutes, with no
local R installation.

1. Create a free account at <https://posit.cloud> if you don't have one.
2. From your workspace, click **New Project**, then **New Project from Git Repository**.
3. Paste the repository URL: `https://github.com/rhenriquezitg/bodcalc.git`. Posit Cloud
   clones it into a new RStudio session. This is your own copy; nothing you do in it
   changes the original repository.
4. In the console, install the required packages once:

   ```r
   install.packages(c("shiny", "bslib", "shinyvalidate", "DT", "ggplot2",
                      "writexl", "rmarkdown", "knitr", "testthat"))
   ```
5. Open `app.R` and click **Run App** (or run `shiny::runApp()` in the console).
6. Try it with the example data below to check that everything loaded.

The downloadable HTML report needs Pandoc, which the RStudio on Posit Cloud already
includes.

To pick up later updates from GitHub, run `system("git pull")` in the console. If it
complains about local changes, commit or discard them first.

## Try it with the example data

The files in `datatables/` are a complete appendicitis example:

| Step | File / value |
|---|---|
| 1 Population | `population_pyramid.csv` |
| 2 Deaths | `appendicitis_deaths.csv` |
| 3 Health state 1 | name "Uncomplicated appendicitis", `appendicitis_uncomplicated_cases.csv`, DW 0.32 (0.22–0.44), duration 14 days |
| 3 Health state 2 | name "Complicated appendicitis", `appendicitis_complicated_cases.csv`, DW 0.32 (0.22–0.44), duration 14 days |
| 4 Economic inputs | any currency, GDP per capita, cost per case, cost per death |

Expected point estimates with the default settings (0% discount rate, no age
weighting): 539 deaths, 45,225 cases, YLL 9,971.8, YLD 554.7, DALY 10,526.5 (with
14 days = 0.0383 years).

## Input files

Population, deaths and cases files are CSVs with the columns `age, males, females`,
one row per age group, `age` being the lower bound of the group (0, 1, 5, 10, …),
given as a single number rather than a range (not `5-9`) so Excel doesn't
autoconvert it to a date. A `total` row is ignored. Semicolon-separated files
with decimal commas are accepted. All files must use exactly the same age groups
as the population file; a mismatch is rejected with an error. Each upload step has
a **Download template** button.

## Methods (summary)

- YLL = deaths × GBD reference life expectancy at the average age of the age
  group's lower and upper bound (interpolated onto the population's age groups).
- YLD = cases × disability weight × duration in years, summed over health states.
- Discounting and age weighting (Results page, optional). Defaults: 0% discount rate,
  no age weighting, so every year counts equally. With discount rate r, a span of
  L years is worth (1 − e^(−rL)) / r years. With age weighting, a year at age x is
  weighted by 0.1658 · x · e^(−0.04 x) (GBD 1990). The same formula, applied from the
  age at death (YLL, span = remaining life expectancy) or at onset (YLD, span =
  duration), uses the midpoint of each age group as the age: 7.5 for ages 5–9, 3 for
  1–4, 0.5 for under 1, and 97.5 for the open-ended 95+ group (treated as 95–100).
- Uncertainty: per iteration, disability weights ~ PERT(lower, mean, upper);
  cases and deaths per age-sex group ~ Poisson(observed). Default 10,000 iterations,
  seed settable for reproducibility. 95% UI = 2.5th–97.5th percentiles.
- Direct costs = cases × cost per case + deaths × cost per death.
- Productivity loss = DALYs × GDP per capita, using the DALYs as calculated under the
  chosen settings. Currency is a label only.

## Structure

```
app.R                     entry point (checks packages, loads life table)
R/app_ui.R, app_server.R  top-level UI/server, Methods and About pages
R/mod_population.R        step 1 (sets the reference age grid)
R/mod_deaths.R            step 2
R/mod_health_states.R     step 3, repeatable health-state cards
R/mod_economics.R         step 4, currency + GDP + unit costs
R/mod_results.R           step 5, calculation, tables, plots, downloads
R/mod_age_sex_upload.R    shared upload + template + validation widget
R/fct_*.R                 calculation engine (life table, YLL, YLD, DALY,
                          discounting/age weighting, Monte Carlo, economic
                          burden, validation, templates)
R/utils_*.R               age grid, duration, currency, plots
data/gbd_life_table.rds   preloaded life table (built by data-raw/build_life_table.R)
report/report.Rmd         downloadable HTML report
tests/testthat/           unit tests
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

## Updating the life table

Replace `datatables/life_expectancy.csv` (columns `age, life_expectancy`) and run
`source("data-raw/build_life_table.R")`.
