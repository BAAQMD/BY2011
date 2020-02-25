#' is_point_source_category
#'
#' @note NOT exported because BY2008 and BY2005 contain functions with the same
#'   name. Invoked within the `DataBank` package like
#'   `BY2011:::is_point_source_category()`.
#'
is_point_source_category <- function (ids) {
  is_true <- function (x) (x == TRUE) & !is.na(x)
  BY2011_point_source_category_ids <- BY2011_sets[["Point sources"]]
  is_true(ids %in% BY2011_point_source_category_ids)
}
