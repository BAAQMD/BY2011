context("BY2011-cat_66-TOG")

test_that("BY2011 category #66 (TOG)", {

  cat_66_TOG_data <-
    BY2011_annual_emission_data %>%
    filter(
      cat_id == 66) %>%
    filter(
      pol_abbr == "TOG")

  expect_equal_to_reference(
    cat_66_TOG_data,
    here::here(
      "tests",
      "testthat",
      "expected",
      "expected-BY2011_annual_emission_data-cat_66-TOG.Rds"))

})
