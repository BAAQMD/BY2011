create_bindings()
reset_bindings()

library(ggtools)

BY2011_annual <-
  get("BY2011_annual", BY2011_legacy_env)

QA_category_set <-
  BY2011_category_hierarchy %>%
  filter(cat_h0 %>% str_detect("Area")) %>%
  semi_join(
    filter(BY2011_category_staff_data, cat_staff == "Schultz")) %>%
  distinct(cat_id)

#'-----------------------------------------------------------------------------

#
# Visual diff of subtotals, by county and pollutant
#

then_by_county <-
  BY2011_annual %>%
  semi_join(BY2011_area_source_category_set) %>%
  filter(year == 2011) %>%
  filter(ems_qty > 0) %>%
  tabulate_emissions_by(cnty_abbr, pol_abbr) %>%
  mutate_if(is.numeric, signif, digits = 4) %>%
  glimpse()

now_by_county <-
  BY2011_annual_emission_data %>%
  semi_join(BY2011_area_source_category_set) %>%
  filter(year == 2011) %>%
  tabulate_emissions_by(cnty_abbr, pol_abbr) %>%
  mutate_if(is.numeric, signif, digits = 4) %>%
  glimpse()

library(daff)
diff_by_county <- diff_data(then_by_county, now_by_county)
render_diff(diff_by_county)
dplyr::all_equal(then_by_county, now_by_county)

#'-----------------------------------------------------------------------------

#
# Visual diff, by category instead
#

then_by_category <-
  BY2011_annual %>%
  semi_join(BY2011_area_source_category_set) %>%
  filter(year == 2011) %>%
  filter(ems_qty > 0) %>%
  tabulate_emissions_by(cat_id, pol_abbr) %>%
  mutate_if(is.numeric, signif, digits = 3) %>%
  glimpse()

now_by_category <-
  BY2011_annual_emission_data %>%
  semi_join(BY2011_area_source_category_set) %>%
  filter(year == 2011) %>%
  tabulate_emissions_by(cat_id, pol_abbr) %>%
  mutate_if(is.numeric, signif, digits = 3) %>%
  glimpse()

library(daff)
diff_by_category <- diff_data(then_by_category, now_by_category)
render_diff(diff_by_category)
dplyr::all_equal(then_by_category, now_by_category)

#'-----------------------------------------------------------------------------

#
# Scatterplots by year and pollutant
#
make_scatterplot_data <- function (..., value_var = "ems_qty") {
  msg <- function (...) if(isTRUE(verbose)) message("[join_emissions] ", ...)
  data_list <- list(...)
  data_names <- names(data_list)
  common_vars <- data_list %>% map(names) %>% reduce(intersect)
  join_vars <- setdiff(common_vars, value_var)
  msg("value_var is: ", value_var)
  msg("joining by: ", str_csv(join_vars))
  suffix <- str_c(".", data_names)
  joined <- reduce(data_list, full_join, by = join_vars, suffix = suffix)
  return(joined)
}

QA_emission_scatterplot_data <-
  make_scatterplot_data(
    BY2011_annual = BY2011_annual,
    BY2011_annual_emission_data = BY2011_annual_emission_data,
    value_var = "ems_qty")

make_QA_pollutant_scatterplot <- function (
  scatterplot_data,
  pollutant,
  category,
  value_var = "ems_qty",
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[make_QA_pollutant_scatterplot] ", ...)

  chart_data <-
    scatterplot_data %>%
    filter(
      pol_abbr == pollutant,
      cat_id == category) %>%
    glimpse()

  value_vars <-
    tidyselect::vars_select(
      names(chart_data),
      tidyselect::matches(!!value_var))

  msg("value_vars is: ", str_csv(value_vars))

  stopifnot(
    length(value_vars) == 2)

  humanize_category <- function (category) {
    cat_h_data <- BY2011_category_hierarchy %>% filter(cat_id == category) %>% select(starts_with("cat_h"))
    cat_h_data %>% unlist() %>% discard(is.na) %>% str_c(collapse = "\n")
  }

  chart_subtitle <-
    glue::glue(
      '{humanize_category(category)}',
      '\n{pollutant} ({unique(pull(chart_data, "ems_unit"))})',
      .sep = "\n")

  x_var <- value_vars[1]
  y_var <- value_vars[2]

  sanitize_var <- function (x) {
    str_remove_all(x, fixed(str_c(value_var, ".")))
  }

  value_max <-
    c(x_var, y_var) %>%
    map_dbl(., ~ max(chart_data[[.]], na.rm = TRUE)) %>%
    max()

  chart_object <-
    chart_data %>%
    ggplot() +
    aes_string(x = x_var, y = y_var) +
    theme_simple() +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    scale_x_quantity(
      name = glue::glue('{sanitize_var(x_var)}'),
      limits = c(0, value_max)) +
    scale_y_quantity(
      name = glue::glue('{sanitize_var(y_var)}'),
      limits = c(0, value_max)) +
    coord_equal() +
    geom_point(
      aes(alpha = I(0.5))) +
    labs(
      subtitle = chart_subtitle,
      caption = glue::glue(
        'DRAFT {format(Sys.Date(), "%Y%m%d")}',
        .sep = "\n"))

  return(chart_object)

}

#'-----------------------------------------------------------------------------

QA_emission_scatterplot_data %>%
  filter(year == 2011) %>%
  make_QA_pollutant_scatterplot(
    pollutant = "TOG",
    category = 54) +
  geom_label_repel(
    aes(label = cnty_abbr)) +
  labs(
    title = "Category #54")

