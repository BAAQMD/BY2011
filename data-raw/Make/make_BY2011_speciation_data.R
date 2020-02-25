make_BY2011_TOG_speciation_data <- function (
  csv_path,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[make_BY2011_TOG_speciation_data] ", ...)

  csv_data <-
    csv_path %>%
    read_csv()

  expected_names <-
    c("cat_id", "TOG", "ROG")

  stopifnot(
    setequal(
      names(csv_data),
      expected_names))

  BY2011_TOG_speciation_data <-
    csv_data %>%
    mutate_at(
      vars(tidyselect::matches("_id$")),
      ~ as.integer(.)) %>%
    ensure(
      all_true(.$TOG == 1))

  return(BY2011_TOG_speciation_data)

}

#'-----------------------------------------------------------------------------

make_BY2011_PM_speciation_data <- function (
  csv_path,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[make_BY2011_PM_speciation_data] ", ...)

  csv_data <-
    csv_path %>%
    read_csv()

  expected_names <-
    c("cat_id", "TSP", "PM10", "PM2.5")

  stopifnot(
    setequal(
      names(csv_data),
      expected_names))

  BY2011_PM_speciation_data <-
    csv_data %>%
    mutate_at(
      vars(tidyselect::matches("_id$")),
      ~ as.integer(.)) %>%
    ensure(
      all_true(.$TSP == 1))

  return(BY2011_PM_speciation_data)

}
