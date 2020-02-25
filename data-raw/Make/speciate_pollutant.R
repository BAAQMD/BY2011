speciate_pollutant <- function (
  input_data,
  into,
  using,
  pollutant,
  verbose = getOption("verbose")
) {

  msg <- function(...) if (isTRUE(verbose)) message("[speciate_pollutant] ", ...)

  target_pollutants <-
    setdiff(into, pollutant)

  already_in_data <-
    input_data %>%
    distinct(pol_abbr) %>%
    pull(pol_abbr) %>%
    intersect(target_pollutants)

  if (length(already_in_data) > 0) {

    stop_msg <- glue::glue(
      "[speciate_pollutant] declining to speciate ",
      "{pollutant} into {str_csv(into)} because ",
      "{str_csv(already_in_data)} is/are already in your data. ",
      "Maybe you want to filter out {pollutant} from your data first?")

    stop(stop_msg)

  }

  speciated_data <-
    input_data %>%
    filter(pol_abbr == pollutant) %>%
    select(-pol_abbr) %>%
    qtytools::apply_scalars(
      using = using,
      value_col = "ems_qty",
      key_col = "pol_abbr") %>%
    filter(pol_abbr %in% union(pollutant, into))

  unspeciated_data <-
    input_data %>%
    filter(
      (is.na(pol_abbr)) | (pol_abbr != pollutant))

  tidied <-
    list(speciated_data, unspeciated_data) %>%
    map_df(~ as_tibble(.)) %>%
    select(
      !!names(input_data))

  return(tidied)

}
