apportion_to_counties <- function (
  emission_data,
  using,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[apportion_to_counties] ", ...)

  corrected_fraction_data <- local({

    problem_set <-
      using %>%
      group_by(cat_id) %>%
      summarise(
        frac_sum = sum(cnty_frac, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(
        round(frac_sum, digits = 2) != 1.00) %>%
      distinct(cat_id)

    if (isTRUE(nrow(problem_set) > 0)) {
      msg("rescaling fractions so that they always sum to 1")
    }

    using %>%
      drop_vars(year) %>%
      group_by(cat_id) %>%
      mutate_at(
        vars(cnty_frac),
        ~ . / sum(.)) %>%
      ungroup()

  })

  joined <-
    left_join(
      emission_data,
      corrected_fraction_data,
      by = "cat_id")

  apportioned <-
    joined %>%
    replace_na(
      list(cnty_frac = 1/9)) %>% # default: distribute equally
    mutate_at(
      vars(ems_qty),
      ~ cnty_frac * .)

  # tidied <-
  #   apportioned %>%
  #   mutate_at(
  #     vars(cnty_abbr),
  #     ~ factor(., levels = BY2011_COUNTY_LEVELS))

  return(apportioned)

}
