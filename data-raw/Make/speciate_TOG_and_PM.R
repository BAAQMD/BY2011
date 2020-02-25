speciate_TOG_and_PM <- function (
  emission_data,
  TOG_speciation_data,
  PM_speciation_data,
  verbose = TRUE
) {

  complete_TOG_speciation_data <-
    TOG_speciation_data %>%
    right_join(
      distinct(emission_data, cat_id),
      by = "cat_id") %>%
    replace_na(
      list(
        ROG = 1.000))

  complete_PM_speciation_data <-
    PM_speciation_data %>%
    right_join(
      distinct(emission_data, cat_id),
      by = "cat_id") %>%
    replace_na(
      list(
        TSP = 1.000,
        PM10 = 1.000,
        PM2.5 = 1.000))

  #
  # FIXME: `speciate_pollutant.R` is in `data-raw/Make/`. `speciate_pollutant()`
  # is currently living in `inventory`, but should  live in a more foundational
  # package, so that we avoid a circular dependency between `BY2011` and
  # `inventory` (which imports `BY2011`).
  #
  speciated <-
    emission_data %>%
    mutate_at(
      vars(pol_abbr),
      ~ replace(as.character(.), . == "PM", "TSP")) %>%
    speciate_pollutant(
      pollutant = "TOG",
      into = c("TOG", "ROG"),
      using = complete_TOG_speciation_data,
      verbose = TRUE) %>%
    speciate_pollutant(
      pollutant = "TSP",
      into = c("TSP", "PM10", "PM2.5"),
      using = complete_PM_speciation_data,
      verbose = TRUE)

  return(speciated)

}
