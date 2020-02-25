make_BY2011_category_hierarchy <- function (
  BY2011_legacy_env,
  verbose = TRUE
) {

  cat_h0_list <- list(
    "Area sources",
    "Point sources",
    "Motor vehicles")

  cat_h0_data <- local({

    BY2011_sets <-
      get("BY2011_sets", envir = BY2011_legacy_env)

    cat_h0_list %>%
      set_names() %>%
      map(~ BY2011_sets[[.]]) %>%
      map(~ tibble(cat_id = .)) %>%
      bind_rows(.id = "cat_h0")

  })

  BY2011_category_hierarchy <-
    get("BY2011_categories", envir = BY2011_legacy_env) %>%
    left_join(
      cat_h0_data,
      by = "cat_id") %>%
    select(
      cat_id,
      num_range(0:6, prefix = "cat_h")) %>%
    arrange(cat_id)

  class(BY2011_category_hierarchy) <-
    union(
      c("hierarchy"),
      class(BY2011_category_hierarchy))

  return(BY2011_category_hierarchy)

}

#'-----------------------------------------------------------------------------

make_BY2011_category_staff_data <- function (
  BY2011_category_hierarchy,
  verbose = TRUE
) {

  BY2011_category_set <-
    BY2011_category_hierarchy %>%
    distinct(cat_id)

  BY2011_category_staff_data <-
    BY2011_category_set %>%
    DataBank::with_category_staff(
      by = "cat_id",
      using = Ingres::t1325) %>%
    arrange(cat_id) %>%
    ensure_distinct(cat_id)

  return(BY2011_category_staff_data)

}
