#' Test whether a dataset can pivot wider
#'
#' Test whether the traits table of a dataset can pivot wider with the minimum required columns.
#'
#' @param dataset Built dataset with `test_build_dataset`
#'
#' @return Number of rows with duplicates preventing pivoting wider
#' @export

check_pivot_wider <- function(dataset) {

  duplicates <- dataset$traits %>%
    select(
      dplyr::all_of(c("dataset_id", "trait_name", "value", "observation_id", "value_type",
      "repeat_measurements_id", "method_id", "method_context_id"))
    ) %>%
    tidyr::pivot_wider(names_from = "trait_name", values_from = "value", values_fn = length) %>%
    tidyr::pivot_longer(cols = 7:ncol(.)) %>%
    dplyr::rename(dplyr::all_of(c("trait_name" = "name", "number_of_duplicates" = "value"))) %>%
    select(
      dplyr::all_of(c("dataset_id", "trait_name", "number_of_duplicates", "observation_id",
      "value_type")), everything()
    ) %>%
    filter(.data$number_of_duplicates > 1) %>%
    nrow()

  if (duplicates == 0) {
    invisible(TRUE)
  } else {
     invisible(FALSE)
  }

}
