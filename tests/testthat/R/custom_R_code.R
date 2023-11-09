
# Put any functions you use in custom R code here

#' Replace duplicate values with NA
#'
#' `replace_duplicates_with_NA` replaces duplicate values in x with NA.
#'
#' @param x Vector containing values
#'
#' @return Vector with duplicate values as NA
#' @importFrom rlang .data
replace_duplicates_with_NA <- function(x) {
  base::replace(x, duplicated(x), NA)
}
