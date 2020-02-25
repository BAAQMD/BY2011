make_BY2011_annual_motor_vehicle_emission_data <- function (
  csv_path,
  verbose = TRUE
) {

  msg <- function (...) {
    if(isTRUE(verbose)) {
      message("[make_BY2011_annual_motor_vehicle_emission_data] ", ...)
    }
  }

  csv_content <-
    csv_path %>%
    read_csv() %>%
    ensure(
      all_true(.$ems_unit == "tons_per_day"))

  msg("recoding")
  recoded <-
    csv_content %>%
    mutate(
      pol_abbr = recode(pol_abbr, PM25 = "PM2.5"),
      ems_unit = "ton/day",
      year = as.integer(year),
      cat_id = as.integer(cat_id),
      cnty_abbr = str_extract(cnty_abbr, "[A-Z]+"))

  msg("filtering")
  filtered <-
    recoded %>%
    filter(
      ems_qty > 0) %>%
    filter(
      elide_year(year) %>% between(1990, 2030)) %>%
    filter(
      cnty_abbr != "TOT") %>%
    sum_annual_emissions_by(
      cnty_abbr,
      cat_id,
      pol_abbr)

  msg("validating")
  validated <-
    filtered %>%
    ensure(
      all_true(elide_year(.$year) %>% between(1990, 2030)),
      all_true(.$cat_id > 0),
      all_true(!is.na(.$pol_abbr)),
      all_true(.$ems_qty > 0))

  BY2011_annual_motor_vehicle_emission_data <-
    validated %>%
    # mutate_at(
    #   vars(year),
    #   ~ if_else(
    #     elide_year(year) == 2011,
    #     as.character(BY(2011)),
    #     as.character(CY(elide_year(year))))) %>%
    mutate_at(
      vars(year),
      ~ as.character(CY(elide_year(.)))) %>%
    convert_emission_units(
      to = "ton/yr")

  class(BY2011_annual_motor_vehicle_emission_data) <-
    union(
      c("inventory", "annual"),
      class(BY2011_annual_motor_vehicle_emission_data))

  return(BY2011_annual_motor_vehicle_emission_data)

}

