make_BY2011_GHG_pollutant_data <- function (
  BY2011_legacy_env,
  verbose = TRUE
) {

  BY2011_pollutant_GWP_data <-
    get("BY2011_GWPs", envir = BY2011_legacy_env) %>%
    enframe(
      name = "pol_abbr",
      value = "pol_GWP100")

  BY2011_pollutant_data <-
    BY2011_pollutant_GWP_data

  return(BY2011_pollutant_data)

}
