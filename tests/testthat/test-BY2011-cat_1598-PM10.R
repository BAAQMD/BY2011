context("BY2011-cat_1598-PM10")

test_that("BY2011 category #1598 (PM10)", {

  cat_1598_PM10_data <-
    BY2011_annual_emission_data %>%
    filter(
      cat_id == 1598) %>%
    filter(
      pol_abbr == "PM10")

  cat_1598_PM10_data %>%
    sum_annual_emissions_by(
      cat_id,
      pol_abbr) %>%
    arrange(
      year) %>%
    select(
      year,
      ems_qty) %>%
    deframe() %>%
    round(
      digits = 2) %>%
    expect_equal(
      set_names(
        c(rep(0, 11), 17.53, 14.03, 7.01, 1.75, rep(0, 26)), # lots of zeros
        CY(1990:2030)))

  expect_equal_to_reference(
    cat_1598_PM10_data,
    here::here(
      "tests",
      "testthat",
      "expected",
      "expected-BY2011_annual_emission_data-cat_1598-PM10.Rds"))

})
