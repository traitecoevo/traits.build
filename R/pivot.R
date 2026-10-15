#' @title Test whether a dataset can pivot wider
#'
#' @description Test whether the traits table of a dataset can pivot wider with the minimum required columns.
#'
#' @param dataset Built dataset with `test_build_dataset`
#'
#' @return Number of rows with duplicates preventing pivoting wider

check_pivot_wider <- function(dataset) {

  duplicates <- util_pivot_duplicates(dataset$traits) %>% nrow()

  if (duplicates == 0) {
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }

}

# Columns that, together with `trait_name`, must identify a single `value` for
# the traits table to pivot wider. Shared by `check_pivot_wider()`, which
# `dataset_test()` runs, and `dataset_find_pivot_duplicates()`, so the two
# always agree on what counts as a duplicate.
pivot_id_cols <- c(
  "dataset_id", "observation_id", "value_type",
  "repeat_measurements_id", "method_id", "method_context_id"
)

# One row per combination of `pivot_id_cols` and `trait_name` holding more than
# one value, with the count in `number_of_duplicates`. Equivalent to pivoting
# wider with `values_fn = length` and keeping the cells greater than 1.
# `extra_cols` are grouped on as well, to carry them into the result.
util_pivot_duplicates <- function(traits, extra_cols = character()) {
  traits %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(c(pivot_id_cols, extra_cols, "trait_name"))),
      name = "number_of_duplicates"
    ) %>%
    dplyr::filter(.data$number_of_duplicates > 1)
}

#' Find the rows that stop a dataset pivoting wider
#'
#' Builds each dataset exactly as [dataset_test()] does and returns the traits
#' rows that make its "`traits` table cannot pivot wider" check fail.
#'
#' Use this rather than [check_pivot_duplicates()] on a compiled database to
#' chase that failure. [austraits::bind_databases()] drops rows that are
#' identical in every column, so a dataset whose duplicates are exact copies
#' fails [dataset_test()] but shows no duplicates once compiled.
#'
#' @inheritParams dataset_test
#'
#' @return Tibble of the offending rows from each dataset's `traits` table, with
#' every column, grouped so that rows sharing a pivot cell are adjacent.
#' `number_of_duplicates` is the number of rows sharing that cell, and
#' `exact_duplicate` is `TRUE` for a row identical to another in every column --
#' the rows a compiled database silently drops. Zero rows means every dataset
#' passes the check.
#' @export
dataset_find_pivot_duplicates <- function(
  dataset_ids,
  path_config = "config",
  path_data = "data"
) {

  # Easy to call like `check_pivot_duplicates(austraits, ...)`, which would
  # otherwise shift the ids into `path_config` and fail on a missing file
  if (!is.character(dataset_ids)) {
    stop(
      "`dataset_ids` must be a character vector of dataset ids. This function builds datasets ",
      "from their source files and takes no database object -- call it as ",
      "`dataset_find_pivot_duplicates(dataset_ids)`.",
      call. = FALSE
    )
  }

  # Same inputs `dataset_test_worker()` passes to `test_build_dataset()`
  definitions <- get_schema(file.path(path_config, "traits.yml"), "traits")
  unit_conversions <- get_unit_conversions(file.path(path_config, "unit_conversions.csv"))
  schema <- get_schema()
  resource_metadata <- get_schema(file.path(path_config, "metadata.yml"), "metadata")
  taxon_list <- read_csv_char(file.path(path_config, "taxon_list.csv"))

  purrr::map(dataset_ids, function(dataset_id) {

    traits <-
      dataset_configure(file.path(path_data, dataset_id, "metadata.yml"), definitions) %>%
      dataset_process(
        file.path(path_data, dataset_id, "data.csv"), ., schema, resource_metadata, unit_conversions
      ) %>%
      dataset_update_taxonomy(taxon_list) %>%
      purrr::pluck("traits")

    traits %>%
      dplyr::mutate(exact_duplicate = duplicated(.) | duplicated(., fromLast = TRUE)) %>%
      dplyr::inner_join(
        util_pivot_duplicates(traits),
        by = c(pivot_id_cols, "trait_name"),
        na_matches = "na"
      ) %>%
      dplyr::select(
        dplyr::all_of(c("dataset_id", "trait_name", "number_of_duplicates", "exact_duplicate",
        "taxon_name", "original_name", "observation_id", "value_type", "value")), dplyr::everything()
      ) %>%
      dplyr::arrange(.data$observation_id, .data$trait_name, .data$value_type)

  }) %>%
    dplyr::bind_rows()

}
