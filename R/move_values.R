#' Move values of one trait into another trait's column
#'
#' For a dataset's `custom_R_code`, on data with one column per trait. Where
#' `original_trait` holds one of `original_values`, the matching element of
#' `values_for_new_trait` is written to `new_trait`, and the value in
#' `original_trait` is replaced by the matching element of `values_to_keep`.
#' Use it when a source records in one column information that belongs to
#' more than one trait -- a growth form column that also says a plant is an
#' epiphyte, say.
#'
#' `new_trait` is created, filled with `NA`, if `data` has no such column, so
#' there is no need to create it first with `mutate()`. Name an existing column
#' to add values to it instead: rows not matched keep what they held.
#'
#' `original_values` are matched exactly and in the order given, each against
#' the data as the one before left it, so a value written to `original_trait`
#' by `values_to_keep` can be matched by a later element of `original_values`.
#' `NA` never matches, and rows not matched are left unchanged.
#'
#' @param data Data frame of the dataset, typically `data.csv` as read by the
#'   build
#' @param original_trait Name of the column holding the values to move
#' @param new_trait Name of the column to write the moved values to. Created if
#'   `data` has no such column
#' @param original_values Values of `original_trait` to move
#' @param values_for_new_trait Values written to `new_trait`, one per element
#'   of `original_values`. A single value is used for all of them
#' @param values_to_keep Values left in `original_trait`, one per element of
#'   `original_values`: the same value to leave it as it was, another to
#'   rewrite it, or `""` or `NA` to clear it. A single value is used for all of
#'   them
#' @param blank_to_na Convert `""` in `original_trait` to `NA` once the values
#'   have been moved? Use it with `values_to_keep = ""`: the build treats `""`
#'   as a value rather than as missing, and reports it among the excluded data.
#'   Default `FALSE`.
#' @param to_lower Lower-case `original_trait` before matching, and match
#'   `original_values` ignoring case? The build lower-cases trait values after
#'   `custom_R_code` runs in any case, so for a trait column this changes which
#'   values match rather than the values stored. Default `FALSE`.
#'
#' @return `data`, with values moved and `new_trait` added if it was absent
#' @seealso [move_values_to_new_trait_long()] for data with one row per trait
#'   value
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   growth_form = c("shrub", "Epiphyte", "herb", "epiphytic herb", NA)
#' )
#'
#' # Record epiphytes as a plant_growth_substrate, leaving the growth form
#' # where there is one
#' data %>%
#'   move_values_to_new_trait(
#'     "growth_form", "plant_growth_substrate",
#'     c("epiphyte", "epiphytic herb"), "epiphyte", c("", "herb"),
#'     blank_to_na = TRUE, to_lower = TRUE
#'   )
move_values_to_new_trait <- function(data, original_trait, new_trait, original_values, values_for_new_trait,
                                     values_to_keep, blank_to_na = FALSE, to_lower = FALSE) {
  if (!original_trait %in% names(data)) {
    stop("`original_trait` is not a column of `data`: ", original_trait, call. = FALSE)
  }

  values_for_new_trait <- recycle_to_match(values_for_new_trait, original_values, "values_for_new_trait")
  values_to_keep <- recycle_to_match(values_to_keep, original_values, "values_to_keep")

  # Creating an absent column changes nothing for callers who already create it: a
  # missing column made the loop below error, unless every row matched the first
  # value, in which case the column was filled without its old contents being read
  if (!new_trait %in% names(data)) {
    data[[new_trait]] <- rep(NA_character_, nrow(data))
  }

  if (to_lower) {
    data[[original_trait]] <- tolower(data[[original_trait]])
    original_values <- tolower(original_values)
  }

  # `ifelse()`, not subassignment, so a column's type changes exactly as it always has
  for (j in seq_along(original_values)) {
    matched <- (data[[original_trait]] == original_values[[j]]) %in% TRUE

    data[[new_trait]] <- ifelse(matched, values_for_new_trait[[j]], data[[new_trait]])
    data[[original_trait]] <- ifelse(matched, values_to_keep[[j]], data[[original_trait]])
  }

  if (blank_to_na) {
    data[[original_trait]][data[[original_trait]] %in% ""] <- NA
  }

  data
}

#' Move values of one trait to another trait, for data in long format
#'
#' For a dataset's `custom_R_code`, on data with one row per trait value. Every
#' row whose value in `values_column` is one of `original_values` is reassigned
#' to `new_trait`, by rewriting its `traits_column`. Values themselves are not
#' changed.
#'
#' Rows are matched on their value alone, whatever trait they belong to:
#' `original_trait` is not used. It is kept so that existing calls, which pass
#' it, still work, and so that a call reads as the trait the values are taken
#' from.
#'
#' @param data Data frame of the dataset, typically `data.csv` as read by the
#'   build
#' @param original_trait Name of the trait the values are taken from. Not used
#'   for matching: see Details
#' @param new_trait Name of the trait to move the matched rows to
#' @param traits_column Name of the column holding trait names
#' @param values_column Name of the column holding trait values
#' @param original_values Values to move
#' @param to_lower Match `original_values` ignoring case? Values are compared
#'   lower-cased but left as they are. Default `FALSE`.
#'
#' @return `data`, with the matched rows reassigned to `new_trait`
#' @seealso [move_values_to_new_trait()] for data with one column per trait
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   trait = c("Texture", "Texture", "Texture"),
#'   value = c("plumose", "smooth", "Tomentose")
#' )
#'
#' data %>%
#'   move_values_to_new_trait_long(
#'     "Texture", "seed_surface_hairs", "trait", "value",
#'     c("plumose", "tomentose"),
#'     to_lower = TRUE
#'   )
move_values_to_new_trait_long <- function(data, original_trait, new_trait, traits_column, values_column,
                                          original_values, to_lower = FALSE) {
  for (column in c(traits_column, values_column)) {
    if (!column %in% names(data)) {
      stop("Not a column of `data`: ", column, call. = FALSE)
    }
  }

  values <- data[[values_column]]

  if (to_lower) {
    values <- tolower(values)
    original_values <- tolower(original_values)
  }

  matched <- values %in% original_values

  data[[traits_column]] <- ifelse(matched, new_trait, data[[traits_column]])

  data
}

#' Recycle a single value to the length of `original_values`
#'
#' @param x Values given for each of `original_values`
#' @param original_values Values being matched
#' @param arg Name of the argument `x` was passed as, for the error message
#'
#' @return `x`, repeated to the length of `original_values` if it was a single value
#' @noRd
recycle_to_match <- function(x, original_values, arg) {
  n <- length(original_values)

  if (length(x) == 1 && n > 1) {
    return(rep(x, n))
  }

  if (length(x) != n) {
    stop(
      sprintf("`%s` has %d values; it needs one for each of the %d `original_values`, or a single value",
              arg, length(x), n),
      call. = FALSE
    )
  }

  x
}
