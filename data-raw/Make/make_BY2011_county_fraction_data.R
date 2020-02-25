make_BY2011_county_fraction_data <- function (
  verbose = TRUE
) {

  msg <- function (...) {
    if(isTRUE(verbose)) {
      message("[make_BY2011_county_fraction_data] ", ...)
    }
  }

  msg("pulling from DataBank:::DB_county_fractions_()")

  DB_BY2011_county_fraction_data <-
    DataBank:::DB_county_fractions_(
      base_year = DataBank::BY(2011),
      na.rm = FALSE)

  # historical_patches <- local({
  #
  #   BY2011_COUNTY_LEVELS <-
  #     c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SON", "SOL", "SNC")
  #
  #   BY2011_POLLUTANT_LEVELS <-
  #     c("TOG", "ROG",
  #       "TSP", "PM", "PM10", "PM2.5",
  #       "NOx", "SO2", "HFC+PFC",
  #       "CO", "CO2", "CH4", "N2O", "SF6", "CO2_bio")
  #
  #   patch_cases <-
  #     bind_rows(
  #
  #       # All emissions from coal combustion at plant #17 Lehigh
  #       # (no Ingres records after 2009)
  #       tibble(pol_abbr = BY2011_POLLUTANT_LEVELS, cat_id = 1748, cnty_abbr = "SNC"),
  #
  #       # All emissions from oil fired boiler(s) at plant #15263 Bottling Group LLC
  #       # (no Ingres records after 2008)
  #       tibble(pol_abbr = BY2011_POLLUTANT_LEVELS, cat_id = 294, cnty_abbr = "ALA"),
  #
  #       # NOx from "Other Refining Processes" at plant #14630 Tesoro Refining and Marketing Company
  #       # (no Ingres records after 2010)
  #       tibble(pol_abbr = "NOx", cat_id = 16, cnty_abbr = "CC"),
  #
  #       # NOx from "Basic Refining Processes" at plant #14628 Tesoro Refining & Marketing Company LLC
  #       # (no Ingres records after 2008 *but* estimates for 2009 in P file(s))
  #       tibble(pol_abbr = "NOx", cat_id = 10, cnty_abbr = "CC"),
  #
  #       # All emissions from "Reciprocating Engines, Gasoline" at plant #7416 Clean Air Vehicle Test Center
  #       # (Ingres records exist only (?) for 2008, *but* there are estimates for 2008 and 2009 in P file(s))
  #       tibble(pol_abbr = BY2011_POLLUTANT_LEVELS, cat_id = 302, cnty_abbr = "ALA"),
  #
  #       # TOG from "Wineries, Fermentation" at plant #606 Anheuser-Busch LLC
  #       # (reclassified as #33 "Other Food & Agricultural Processes" after 2010)
  #       # FIXME: this omits plant #1648 Sonoma Wine Company (minor)
  #       tibble(pol_abbr = "TOG", cat_id = 31, cnty_abbr = "SOL"),
  #
  #       # TOG from "Vacuum Producing Systems" at refineries
  #       # (no Ingres records after 1997)
  #       tibble(pol_abbr = "TOG", cat_id = 14, cnty_abbr = "CC"),
  #
  #       # "Dry Cleaners, Other Synthetic Solvents"
  #       # FIXME: for 2007, there are actually only Ingres records for SNC, MAR, and ALA
  #       tibble(pol_abbr = "TOG", cat_id = 106, cnty_abbr = BY2011_COUNTY_LEVELS)
  #
  #     )
  #
  #   patch_cases %>%
  #     group_by(pol_abbr, cat_id) %>%
  #     mutate(cnty_frac = 1 / length(cnty_abbr)) %>%
  #     ungroup()
  #
  # })

  BY2011_county_fraction_data <-
    DB_BY2011_county_fraction_data %>%
    select(
      -year) %>%
    # mutate(
    #   pol_abbr = map(1:n(), ~ BY2011_POLLUTANT_LEVELS)) %>%
    # unnest() %>%
    # bind_rows(
    #   historical_patches) %>%
    arrange(
      cat_id, cnty_abbr)

  return(BY2011_county_fraction_data)

}
