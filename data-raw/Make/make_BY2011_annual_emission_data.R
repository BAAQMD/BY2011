make_BY2011_annual_emission_data <- function (
  BY2011_annual_point_source_emission_data,
  BY2011_annual_area_source_emission_data,
  BY2011_annual_motor_vehicle_emission_data,
  BY2011_TOG_speciation_data,
  BY2011_PM_speciation_data,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[make_BY2011_annual_emission_data] ", ...)

  stacked_speciated_data <- local({

    stacked_unspeciated_data <-
      bind_rows(
        BY2011_annual_point_source_emission_data,
        BY2011_annual_area_source_emission_data)

    msg("speciating TOG and PM")
    stacked_speciated_data <-
      stacked_unspeciated_data %>%
      speciate_TOG_and_PM(
        BY2011_TOG_speciation_data,
        BY2011_PM_speciation_data)

    bind_rows(
      stacked_speciated_data,
      BY2011_annual_motor_vehicle_emission_data)

  })

  msg("dropping where ems_qty == 0")
  filtered_data <-
    stacked_speciated_data %>%
    filter(ems_qty != 0) %>%
    ensure(
      all_true(.$ems_qty > 0))

  BY2011_COUNTY_LEVELS <-
    c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SON", "SOL", "SNC")

  BY2011_POLLUTANT_LEVELS <-
    c("TOG", "ROG",
      "TSP", "PM", "PM10", "PM2.5",
      "NOx", "SO2", "HFC+PFC",
      "CO", "CO2", "CH4", "N2O", "SF6", "CO2_bio")

  msg("recoding")
  recoded_data <-
    filtered_data %>%
    mutate_at(
      vars(cnty_abbr),
      ~ factor(., levels = BY2011_COUNTY_LEVELS)) %>%
    mutate_at(
      vars(pol_abbr),
      ~ factor(., levels = BY2011_POLLUTANT_LEVELS)) %>%
    mutate_at(
      vars(year),
      ~ as.character(CY(elide_year(.)))) %>%
    mutate_at(
      vars(cat_id),
      ~ as.integer(.))

  msg("validating")
  validated_data <-
    filtered_data %>%
    ensure_distinct(
      year, cat_id, pol_abbr, cnty_abbr) %>%
    ensure(
      #all_true(elide_year(.$year) %>% between(1990, 2030)),
      all_true(
        is.na(.$cnty_abbr) | .$cnty_abbr %in% BY2011_COUNTY_LEVELS),
      all_true(.$cat_id > 0),
      all_true(!is.na(.$pol_abbr)),
      all_true(.$ems_qty >= 0))

  BY2011_annual_emission_data <-
    validated_data %>%
    mutate_at(
      vars(cat_id),
      ~ as.integer(.)) %>%
    select(
      year,
      cnty_abbr,
      cat_id,
      pol_abbr,
      ems_qty,
      ems_unit) %>%
    arrange(
      year,
      cnty_abbr,
      cat_id,
      pol_abbr)

  class(BY2011_annual_emission_data) <-
    union(
      c("inventory", "annual"),
      class(BY2011_annual_emission_data))

  return(BY2011_annual_emission_data)

}
