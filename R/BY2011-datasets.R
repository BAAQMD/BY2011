#' DEPRECATED: BY2011 annual emissions (all categories)
#'
#' @note Deprecated in favor of \code{BY2011_annual}.
#'
#' @seealso BY2011_annual
#'
"BY2011_data"

#' BY2011 annual emissions (all categories)
#'
#' A dataset containing estimated emissions by year, county,
#' category, and pollutant. BY2011 means "Base Year 2011".
#'
#' @note Please use this instead of \code{BY2011_data}. Thanks!
#'
#' @format A data frame with 990,621 rows and 6 columns:
#' \describe{
#'   \item{year}{(int) four-digit year}
#'   \item{cnty_abbr}{(chr) county identifier: ALA, CC, MAR, NAP, SF, SM, SNC, SOL, SON}
#'   \item{cat_id}{(int) category identifier}
#'   \item{pol_abbr}{(chr) pollutant identifier: PM, PM10, PM2.5, TOG, ROG, NOx, SO2, CO, CO2, CO2_bio, CH4, N2O, HFC+PFC, SF6}
#'   \item{ems_qty}{(int) estimated emissions}
#'   \item{ems_unit}{(chr) ton/yr}
#' }
#'
#' @seealso BY2011_sets BY2011_categories with_hierarchy sum_annual_emissions_by
#'
#' @examples
#' library(inventory)
#' BY2011_annual %>% filter(cat_id %in% BY2011_sets[["Area sources"]])
#' BY2011_annual %>% filter(cat_id %not_in% BY2011_sets[["Unreported"]])
#'
"BY2011_annual"

#' BY2011 category IDs and descriptors (nested headings)
#'
#' A named list containing useful sets of BY2011 category identifiers (IDs).
#' To see them in the console, type `BY2011_sets` (no quotes), then hit Enter.
#'
#' @format A data frame with columns \code{cat_id}, \code{cat_h1}, \code{cat_h2}, ... \code{cat_h6}
#'
#' @seealso BY2011_annual BY2011_sets with_hierarchy
#'
#' @examples
#' library(inventory)
#' View(BY2011_categories)
#' BY2011_annual %>% with_hierarchy(BY2011_categories)
#'
"BY2011_categories"

#' Notable sets of BY2011 categories
#'
#' A named list containing useful sets of BY2011 category identifiers (IDs).
#' To see them in the console, type \code{show(BY2011_sets)} (no quotes), then hit Enter.
#'
#' @format A data frame with 990,621 rows and 6 columns:
#' \describe{
#'   \item{"Area sources"}{(int) area source category IDs}
#'   \item{"Point sources"}{(int) point source category IDs}
#'   \item{"Motor vehicles"}{(int) motor vehicle category IDs}
#'   \item{"Unreported"}{(int) Category IDs corresponding to ships > 3 nautical miles, as well as those corresponding to biogenics}
#' }
#'
#' @seealso BY2011_annual
#'
#' @examples
#' library(inventory)
#' options(width = 80)
#' show(BY2011_sets)
#' BY2011_annual %>% filter(cat_id %in% BY2011_sets[["Area sources"]])
#' BY2011_annual %>% filter(cat_id %not_in% BY2011_sets[["Unreported"]])
#'
"BY2011_sets"

#' Global Warming Potential (GWP) factors used for/in/with BY2011
#'
#' Type \code{show(BY2011_GWPs)} to see the values.
#'
#' @format A named vector, with pollutant abbreviations used as names.
#'
#' @seealso BY2011_annual as_MMTCO2eq
#'
#' @examples
#' library(inventory)
#' show(BY2011_GWPs)
#'
"BY2011_GWPs"
