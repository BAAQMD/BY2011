make_BY2011_annual_motor_vehicle_emission_data <- function (
  csv_path,
  verbose = TRUE
) {

  msg <- function (...) {
    if(isTRUE(verbose)) {
      message("[make_BY2011_annual_motor_vehicle_emission_data] ", ...)
    }
  }

  csv_data <-
    csv_path %>%
    read_csv() %>%
    ensure(
      all_true(.$ems_unit == "tons_per_day"))

  msg("recoding PM25 as PM2.5")
  recoded_data <-
    csv_data %>%
    mutate(
      pol_abbr = recode(pol_abbr, PM25 = "PM2.5"),
      ems_unit = "ton/day",
      year = as.integer(year),
      cat_id = as.integer(cat_id),
      cnty_abbr = str_extract(cnty_abbr, "[A-Z]+")) %>%
    drop_vars(
      empl_abbr,
      rev_date,
      comments)

  msg("dropping rows where `cnty_abbr == \"TOT\"`")
  county_specific_data <-
    recoded_data %>%
    filter(
      elide_year(year) %>% between(1990, 2030)) %>%
    filter(
      cnty_abbr != "TOT")

  msg("validating")
  validated_data <-
    county_specific_data %>%
    ensure(
      all_true(elide_year(.$year) %>% between(1990, 2030)),
      all_true(.$cat_id > 0),
      all_true(!is.na(.$pol_abbr)),
      all_true(.$ems_qty > 0))


  BY2011_annual_motor_vehicle_emission_data <-
    validated_data %>%
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

