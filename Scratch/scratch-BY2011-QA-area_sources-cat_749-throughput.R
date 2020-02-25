library(Ingres)

#
# Abhinav    749
# Annie     1010
# Michael     66
# Sukarn     284
# Tan       1902
# Yuan      1759
#
# Pick your favorite area-source category.
#
QA_CATEGORY <- 66

#
# Pull throughput estimates and metadata from `t1325` (aka "FID 1325").
# The "current working" would be `t0325`; `t1` signifies "previous".
# Note: `p` is typically "primary key"; `s` is "secondary key".
# Also: `p` may signify "plant"; `s` may signify "source".
#
QA_t1325_data <- filter(t1325, p == QA_CATEGORY)
show(QA_t1325_data)
glimpse(QA_t1325_data)
help(t0325)

# Emission factors, for area source categories only, from `t1326`.
QA_t1326_data <- filter(t1326, p == QA_CATEGORY)
show(QA_t1326_data)
help(t0326) # FIXME: this documentation is wrong!

# Control factors, from `t1328`. (There may be none.)
QA_t1328_data <- filter(t1328, p == QA_CATEGORY)
show(QA_t1328_data)
help(t0328)

#'-----------------------------------------------------------------------------

library(BY2011)

QA_BY2011_annual_emission_data <-
  BY2011_annual %>%
  filter(
    cat_id == QA_CATEGORY)

show(QA_BY2011_annual_emission_data)
glimpse(QA_BY2011_annual_emission_data)

#'-----------------------------------------------------------------------------

library(inventory) # imports `Ingres`
library(janitor)   # may soon be imported by `inventory`
library(glue)      # may soon be imported by `inventory`

QA_pollutant_x_year_tbl  <-
  QA_BY2011_annual_emission_data %>%
  filter(
    elide_year(year) %>% between(2011, 2015)) %>%
  tabulate_emissions_by(
    pol_abbr, year)

show(QA_pollutant_x_year_tbl)

#'-----------------------------------------------------------------------------

QA_county_x_pollutant_tbl <-
  QA_BY2011_annual_emission_data %>%
  filter(
    year == 2011) %>%
  tabulate_emissions_by(
    year, cnty_abbr, pol_abbr)

QA_county_x_pollutant_tbl %>%
  janitor::adorn_totals("row")

#'-----------------------------------------------------------------------------

QA_pollutant_x_county_tbl <-
  QA_BY2011_annual_emission_data %>%
  filter(
    year == 2011) %>%
  tabulate_emissions_by(
    pol_abbr, cnty_abbr)

show(QA_pollutant_x_county_tbl)

QA_pollutant_x_county_tbl %>%
  mutate(
    TOT = ALA + CC + MAR + NAP + SM + SNC + SOL + SON) %>%
  select(
    TOT, everything()) %>%
  mutate_at(
    vars(ALA:SON),
    ~ . / TOT) %>%
  drop_vars(TOT) %>%
  adorn_pct_formatting()

#'-----------------------------------------------------------------------------

DB_throughput_data <-
  BY(2011) %>%
  DB_area_source_throughputs(
    verbose = TRUE) %>%
  filter(
    cat_id == QA_CATEGORY)

show(DB_throughput_data)

#'-----------------------------------------------------------------------------

DB_BY2011_annual_area_source_data <-
  BY(2011) %>%
  area_source_projections(
    verbose = TRUE) %>%
  filter(
    cat_id == QA_CATEGORY)

show(DB_BY2011_annual_area_source_data)
glimpse(DB_BY2011_annual_area_source_data)

DB_county_x_pollutant_tbl <-
  DB_BY2011_annual_area_source_data %>%
  filter(
    year == 2011) %>%
  tabulate_emissions_by(
    year, cnty_abbr, pol_abbr)

#
# Compare this ...
#
DB_county_x_pollutant_tbl %>%
  janitor::adorn_totals("row")

#
# ... to this:
#
QA_county_x_pollutant_tbl %>%
  janitor::adorn_totals("row")
