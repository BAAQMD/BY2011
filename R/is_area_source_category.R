#' is_area_source_category
#'
#' @note NOT exported because BY2008 and BY2005 contain functions with the same
#'   name. Invoked within the `DataBank` package like
#'   `BY2011:::is_area_source_category()`.
#'
is_area_source_category <- function (ids) {
  is_true <- function (x) (x == TRUE) & !is.na(x)
  BY2011_area_source_category_ids <- BY2011_sets[["Area sources"]]
  is_true(ids %in% BY2011_area_source_category_ids)
}
