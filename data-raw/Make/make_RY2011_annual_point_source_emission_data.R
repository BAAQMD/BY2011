make_RY2011_annual_point_source_emission_data <- function (
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[make_BY2011_point_data] ", ...)

  DB_YEAR <- RY(2011)

  msg("pulling ", DB_YEAR, " data from DataBank")
  DB_data <-
    DataBank::DB_point_source_emissions(
      DB_YEAR,
      na.rm = FALSE) %>% # include "defunct" (closed) facilities
    filter(
      !is.na(cnty_abbr)) %>%
    mutate_at(
      vars(pol_abbr),
      ~ as.character(fct_collapse(., "HFC+PFC" = c("HFCs", "PFCs"))))

  RY2011_POLLUTANT_LEVELS <-
    c("TOG", "ROG", "TSP", "PM", "PM10", "PM2.5", "NOx", "SO2", "HFC+PFC",
      "CO", "CO2", "CH4", "N2O", "CO2_bio", "SF6")

  msg("keeping only ", str_csv(RY2011_POLLUTANT_LEVELS))
  filtered <-
    DB_data %>%
    filter(
      pol_abbr %in% RY2011_POLLUTANT_LEVELS)

  summed_by_county <-
    filtered %>%
    sum_annual_emissions_by(
      pol_abbr, cat_id, cnty_abbr)

  msg("filtering for ems_qty > 0")
  filtered <-
    summed_by_county %>%
    mutate(
      year = parse_number(year)) %>%
    filter(
      ems_qty > 0,
      elide_year(year) %>% between(1990, 2030))

  validated <-
    filtered %>%
    ensure_distinct(
      year, cat_id, pol_abbr, cnty_abbr) %>%
    ensure(
      all_true(elide_year(.$year) %>% between(1990, 2030)),
      #all_true(
      #  .$cnty_abbr %in% c(BY2011_COUNTY_LEVELS) |
      #    is.na(.$cnty_abbr)),
      all_true(.$cat_id > 0),
      all_true(!is.na(.$pol_abbr)),
      all_true(.$ems_qty >= 0))

  RY2011_annual_point_source_emission_data <-
    validated %>%
    mutate_at(
      vars(year),
      ~ as.character(RY(elide_year(.))))

  class(RY2011_annual_point_source_emission_data) <-
    union(
      c("inventory", "annual"),
      class(RY2011_annual_point_source_emission_data))

  return(RY2011_annual_point_source_emission_data)

}

#'-----------------------------------------------------------------------------

# make_BY2011_point_SF6_emission_data <- function (
#   XLSX_path,
#   BY2011_county_fraction_data,
#   verbose = TRUE
# ) {
#
#   XLSX_content <-
#     XLSX_path %>%
#     read_xls()
#
#   transmuted <-
#     XLSX_content %>%
#     transmute(
#       year = Year,
#       cat_id = cat_id,
#       pol_abbr = "SF6",
#       ems_qty = SF6,
#       ems_unit = "ton/day") %>%
#     filter(
#       year >= 1990) %>%
#     convert_emission_units(
#       from = "ton/day",
#       to = "ton/yr")
#
#   BY2011_point_SF6_emission_data <-
#     transmuted
#
#   return(BY2011_point_SF6_emission_data)
#
# }
