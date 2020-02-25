context("BY2011 annual totals")

library(tidyverse)

test_that("cat_id is integer", {
  BY2011_annual_emission_data %>%
    pull(cat_id) %>%
    is.integer() %>%
    expect_true()
})

#'-----------------------------------------------------------------------------

test_that("year is four digits prefixed with CY", {
  BY2011_annual_emission_data %>%
    pull(year) %>%
    stringr::str_detect("^CY[0-9]{4}$") %>%
    all_true() %>%
    expect_true()
})

#'-----------------------------------------------------------------------------

test_that("2011 NOx total", {

  expected_NOx <-
    tibble(
      year = "CY2011",
      pol_abbr = "NOx",
      ems_qty = 129000,
      ems_unit = "ton/yr")

  BY2011_NOx <-
    BY2011_annual_emission_data %>%
    filter(
      pol_abbr == "NOx") %>%
    filter(
      elide_year(year) == 2011) %>%
    sum_annual_emissions_by(
      pol_abbr,
      digits = -3)

  testthat::expect_equal(
    expected_NOx,
    BY2011_NOx)

})

#'-----------------------------------------------------------------------------

test_that("2011 CH4 total", {

  expected_CH4 <-
    tibble(
      year = "CY2011",
      pol_abbr = "CH4",
      ems_qty = 138000,
      ems_unit = "ton/yr")

  BY2011_CH4 <-
    BY2011_annual_emission_data %>%
    filter(
      pol_abbr == "CH4") %>%
    filter(
      elide_year(year) == 2011) %>%
    sum_annual_emissions_by(
      pol_abbr,
      digits = -3)

  testthat::expect_equal(
    expected_CH4,
    BY2011_CH4)

})
