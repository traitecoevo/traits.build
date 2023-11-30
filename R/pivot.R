#' @title Test whether a dataset can pivot wider
#'
#' @description Test whether the traits table of a dataset can pivot wider with the minimum required columns.
#'
#' @param dataset Built dataset with `test_build_dataset`
#'
#' @return Number of rows with duplicates preventing pivoting wider

check_pivot_wider <- function(dataset) {

  duplicates <- dataset$traits %>%
    dplyr::select(
      dplyr::all_of(c("dataset_id", "trait_name", "value", "observation_id", "value_type",
      "repeat_measurements_id", "method_id", "method_context_id"))
    ) %>%
    tidyr::pivot_wider(names_from = "trait_name", values_from = "value", values_fn = length) %>%
    tidyr::pivot_longer(cols = 7:ncol(.)) %>%
    dplyr::rename(dplyr::all_of(c("trait_name" = "name", "number_of_duplicates" = "value"))) %>%
    dplyr::select(
      dplyr::all_of(c("dataset_id", "trait_name", "number_of_duplicates", "observation_id",
      "value_type")), everything()
    ) %>%
    dplyr::filter(.data$number_of_duplicates > 1) %>%
    nrow()

  if (duplicates == 0) {
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }

}


#' @title Pivot long format data into a wide format
#'
#' @description `trait_pivot_wider` "widens" long format data ("tidy data").
#'
#' Databases built with `traits.build` are organised in a long format where observations are on different rows and the
#' type of observation is denoted by various identifying columns (e.g `trait_name`, `dataset_id`,
#' `observation_id`, etc.).
#' This function converts the data into wide format so that each trait in its own column.
#'
#' @param traits The traits table from database (list object)
#' @return A tibble in wide format
#' @details `trait_pivot_wider` will return a single wide tibble; note that some meta-data columns
#' (unit, replicates, measurement_remarks, basis_of_record, basis_of_value) will be excluded to
#' produce a useful wide tibble.
#' @examples
#' \dontrun{
#' data <- austraits$traits %>% filter(dataset_id == "Falster_2003")
#' data # Long format
#' traits_wide <- trait_pivot_wider(data)
#' traits_wide # Wide format
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
db_traits_pivot_wider <- function(traits) {

  metadata_cols <- c("unit", "replicates", "measurement_remarks", "basis_of_value")

  # A check for if there are more than 1 value_type for a given taxon_name, observation_id and method
  check_value_type <- traits %>%
    dplyr::select(dplyr::all_of(c(
      "trait_name", "value", "dataset_id", "observation_id", "method_id", "method_context_id",
      "repeat_measurements_id", "value_type"))) %>%
    dplyr::group_by(
      .data$dataset_id, .data$observation_id, .data$method_id,
      .data$method_context_id, .data$repeat_measurements_id) %>%
    dplyr::summarise(n_value_type = length(unique(.data$value_type))) %>%
    dplyr::arrange(.data$observation_id) %>%
    dplyr::filter(.data$n_value_type > 1)

  if (nrow(check_value_type) > 1) {

    traits %>%
      tidyr::pivot_wider(
        names_from = "trait_name",
        values_from = "value",
        id_cols = -dplyr::all_of(metadata_cols)
      )

  } else {

    metadata_cols <- c(metadata_cols, "value_type")

    traits %>%
      tidyr::pivot_wider(
        names_from = "trait_name",
        values_from = "value",
        id_cols = -dplyr::all_of(metadata_cols)
      )
  }

}


#' @title Pivot wide format data into a long format
#'
#' @description `trait_pivot_longer` "gathers" wide format data into a "tidy" format.
#'
#' This function converts the data into long format where observations are on different rows and the type of
#' observation is denoted by the `trait_name` column.
#' In other words, `trait_pivot_longer` reverts the actions of `trait_pivot_wider`.
#' @param wide_data Output from `trait_pivot_wider` (a tibble of wide data)
#' @return A tibble in long format
#' @details
#' `trait_pivot_longer` will return a tibble with fewer columns than the original traits table
#' The excluded columns include: "unit", "replicates", "measurement_remarks", "basis_of_record",
#' "basis_of_value" # Double check #TODO
#'
#' @examples
#' \dontrun{
#' data <- austraits$traits %>%
#' filter(dataset_id == "Falster_2003")
#' data # Long format
#' traits_wide <- trait_pivot_wider(data)
#' traits_wide # Wide format
#'
#' values_long <- trait_pivot_longer(traits_wide)
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @author Fonti Kar - fonti.kar@unsw.edu.au
#' @export
db_traits_pivot_longer <- function(wide_data) {

  # The start of the trait columns is after `original_name`
  start_of_trait_cols <- which(names(wide_data) == "original_name") + 1

  wide_data %>%
    tidyr::pivot_longer(
      cols = start_of_trait_cols:ncol(.),
      names_to = "trait_name",
      values_drop_na = TRUE
    )

}
