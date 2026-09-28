# Build the preloaded GBD reference life table --------------------------------
# Source: datatables/life_expectancy.csv (GBD standard/reference life table:
# life expectancy at the lower bound of each age group, 0, 1, 5, ..., 95).
# Run from the app folder whenever the source table is updated:
#   source("data-raw/build_life_table.R")

lt <- utils::read.csv(file.path("datatables", "life_expectancy.csv"))
names(lt) <- tolower(trimws(names(lt)))
stopifnot(all(c("age", "life_expectancy") %in% names(lt)))
lt <- lt[order(lt$age), c("age", "life_expectancy")]
stopifnot(!anyNA(lt), !anyDuplicated(lt$age), all(diff(lt$life_expectancy) < 0))

dir.create("data", showWarnings = FALSE)
saveRDS(lt, file.path("data", "gbd_life_table.rds"))
message("Saved data/gbd_life_table.rds (", nrow(lt), " age groups, LE at birth = ",
        round(lt$life_expectancy[1], 2), ")")
