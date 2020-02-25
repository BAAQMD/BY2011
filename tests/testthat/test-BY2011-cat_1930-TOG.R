test_that("BY2011 category #1930 (TOG)", {

  cat_1930_TOG_data <-
    BY2011_annual_emission_data %>%
    filter(
      cat_id == 1930) %>%
    filter(
      pol_abbr == "TOG")

  #
  # CY2005-2008 should be **missing**
  #
  expect_true(
    all_true(
      CY(2005:2008) %not_in% cat_1930_TOG_data[["year"]]))

  # CY1990-2004 should be present
  expect_true(
    all_true(
      CY(1990:2004) %in% cat_1930_TOG_data[["year"]]))

  # CY2009-2030 should be present
  expect_true(
    all_true(
      CY(2009:2030) %in% cat_1930_TOG_data[["year"]]))

  expect_equal_to_reference(
    cat_1930_TOG_data,
    here::here(
      "tests",
      "testthat",
      "expected",
      "expected-BY2011_annual_emission_data-cat_1930-TOG.Rds"))

})
