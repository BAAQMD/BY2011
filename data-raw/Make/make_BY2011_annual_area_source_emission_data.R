#
# Subtotals for PM, TOG, NOx, SO2, CO, CO2, CH4, N2O, and CO2_bio.
#
make_BY2011_annual_area_source_CAP_and_GHG_emission_data <- function (
  csv_path,
  verbose = TRUE
) {

  msg <- function (...) {
    if(isTRUE(verbose)) {
      message("[make_BY2011_annual_area_source_CAP_and_GHG_emission_data] ", ...)
    }
  }

  csv_content <-
    csv_path %>%
    tbltools::read_csv(
      col_types = "icicdddddddddd",
      verbose = TRUE) %>%
    ensurer::ensure(
      all_true(
        .$season == "Annual",
        .$cat_type == "Area"))

  BY2011_annual_area_source_CAP_and_GHG_emission_data <- local({

    renamed_data <-
      csv_content %>%
      select(
        -season,
        -cat_type) %>%
      rename(
        year = yr,
        cat_id = cat_no,
        CO2_bio = BCO2,
        `HFC+PFC` = HFC,
        SO2 = SOx)

    validated_data <-
      renamed_data %>%
      ensure(
        all_true(elide_year(.$year) %>% between(1990, 2030))) %>%
      ensure_distinct(
        year,
        cat_id)

    BY2011_POLLUTANT_LEVELS <- c(
      "PM", "TOG", "NOx", "SO2", "HFC+PFC",
      "CO", "CO2", "CH4", "N2O", "CO2_bio")

    msg("BY2011_POLLUTANT_LEVELS is: ",
        str_csv(BY2011_POLLUTANT_LEVELS))

    gathered_data <-
      validated_data %>%
      gather(
        pol_abbr,
        ems_qty,
        !!BY2011_POLLUTANT_LEVELS)

    tidied_data <-
      gathered_data %>%
      select(
        year,
        cat_id,
        pol_abbr,
        ems_qty) %>%
      ensure_distinct(
        year,
        cat_id,
        pol_abbr) %>%
      mutate_at(
        vars(cat_id),
        ~ as.integer(.)) %>%
      mutate_at(
        vars(pol_abbr),
        ~ factor(., levels = BY2011_POLLUTANT_LEVELS))

    msg("converting units")
    converted_data <-
      tidied_data %>%
      mutate(
        ems_unit = "ton/day") %>% # TODO: use set_emission_units()
      convert_emission_units(
        from = "ton/day",
        to = "ton/yr")

  })

  class(BY2011_annual_area_source_CAP_and_GHG_emission_data) <-
    union(
      c("inventory", "annual"),
      class(BY2011_annual_area_source_CAP_and_GHG_emission_data))

  return(BY2011_annual_area_source_CAP_and_GHG_emission_data)

}

#'-----------------------------------------------------------------------------

make_BY2011_annual_area_source_SF6_emission_data <- function (
  XLSX_path,
  verbose = TRUE
) {

  msg <- function (...) {
    if(isTRUE(verbose)) {
      message("[make_BY2011_annual_area_source_SF6_emission_data] ", ...)
    }
  }

  XLSX_content <-
    XLSX_path %>%
    read_xls()

  BY2011_annual_area_source_SF6_emission_data <- local({

    transmuted_data <-
      XLSX_content %>%
      transmute(
        year = as.integer(Year),
        cat_id = as.integer(cat_id),
        pol_abbr = "SF6",
        ems_qty = SF6,
        ems_unit = "ton/day") %>%
      filter(
        year >= 1990)

    converted_data <-
      transmuted_data %>%
      convert_emission_units(
        from = "ton/day",
        to = "ton/yr")

  })

  class(BY2011_annual_area_source_SF6_emission_data) <-
    union(
      c("inventory", "annual"),
      class(BY2011_annual_area_source_SF6_emission_data))

  return(BY2011_annual_area_source_SF6_emission_data)

}

#'-----------------------------------------------------------------------------

make_BY2011_annual_area_source_emission_data <- function (
  BY2011_annual_area_source_CAP_and_GHG_emission_data,
  BY2011_annual_area_source_SF6_emission_data,
  BY2011_county_fraction_data,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[make_BY2011_annual_area_source_emission_data] ", ...)

  stacked_emission_data <-
    BY2011_annual_area_source_CAP_and_GHG_emission_data %>%
    mutate_at(
      vars(pol_abbr),
      ~ as.character(.)) %>%
    bind_rows(
      BY2011_annual_area_source_SF6_emission_data)

  msg("apportioning across counties")
  apportioned_data <-
    stacked_emission_data %>%
    apportion_to_counties(
      using = BY2011_county_fraction_data) %>%
    drop_vars(
      cnty_frac)

  BY2011_annual_area_source_emission_data <-
    apportioned_data %>%
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

  comment(BY2011_annual_area_source_emission_data) <-
    "BY2011 area source emissions, by category and county, 1990—2030."

  class(BY2011_annual_area_source_emission_data) <-
    union(
      c("inventory", "annual"),
      class(BY2011_annual_area_source_emission_data))

  return(BY2011_annual_area_source_emission_data)

}
