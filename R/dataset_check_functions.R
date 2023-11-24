#' Summarise needed categorical trait value substitutions
#'
#' @param database  Trait database
#' @param dataset   Dataset being checked
#'
#' @return Tibble of categorical trait values requiring substitutions
#' @export
#'
#' @examples
#' dataset_check_categorical_substitutions(austraits, "NSWFRD_2014")
#' 
dataset_check_categorical_substitutions <- function(database, dataset) {
  
  required_substitutions <-
    database$excluded_data %>%
    dplyr::filter(
      dataset_id == dataset,
      error == "Unsupported trait value"
    ) %>%
    dplyr::distinct(dataset_id, trait_name, value) %>%
    dplyr::rename(find = value) %>%
    dplyr::select(-dataset_id) %>%
    dplyr::mutate(replace = NA)
  
  required_substitutions
}

#' Summarise excluded numeric trait values
#'
#' @param database  Trait database
#' @param dataset   Dataset being checked
#'
#' @return Tibble of numeric traits values out of allowable range
#' @export
#'
#' @examples
#' dataset_check_numeric_values(austraits, "Bloomfield_2018")
#' 
dataset_check_numeric_values <- function(database, dataset) {

  out_of_range_values <-
    database$excluded_data %>%
      dplyr::filter(
        dataset_id == dataset,
        error == "Value out of allowable range"
      ) %>%
      dplyr::select(dataset_id, trait_name, value)
  
  out_of_range_values
}
```

#' Summarise needed taxonomic updates
#'
#' @param taxon_list  Taxon list for database
#' @param database    Trait database
#' @param dataset     Dataset being checked
#'
#' @return Tibble with taxon names requiring updating
#' @export
#'
#' @examples
#' dataset_check_taxonomic_updates(taxon_list, austraits, "ANBG_2019")
#' 
dataset_check_taxonomic_updates <- function(taxon_list, database, dataset) {

  names_to_align <- 
    database$taxonomic_updates %>%
    dplyr::filter(dataset_id == current_study) %>%
    dplyr::filter(!aligned_name %in% taxon_list$aligned_name & !aligned_name %in% taxon_list$taxon_name) %>%
    dplyr::filter(is.na(taxonomic_resolution)) %>%
    dplyr::distinct(original_name)
  
  names_to_align
}


#' Identify duplicates preventing pivoting wider
#'
#' @param database  Trait database
#' @param dataset   Dataset being checked
#'
#' @return Tibble with duplicates and pivot columns
#' @export
check_pivot_duplicates <- function(
    database,
    dataset
) {
  
  # Check for duplicates
  rows_cannot_pivot <-
    database$traits %>%
    filter(.data$dataset_id %in% dataset) %>%
    select(
      # `taxon_name` and `original_name` are not needed for pivoting but are included for informative purposes
      dplyr::all_of(
        c("dataset_id", "trait_name", "value", "taxon_name", "original_name", "observation_id",
          "value_type", "repeat_measurements_id", "method_id", "method_context_id"))
    ) %>%
    tidyr::pivot_wider(names_from = "trait_name", values_from = "value", values_fn = length) %>%
    tidyr::pivot_longer(cols = 9:ncol(.)) %>%
    dplyr::rename(dplyr::all_of(c("trait_name" = "name", "number_of_duplicates" = "value"))) %>%
    select(
      dplyr::all_of(c("dataset_id", "trait_name", "number_of_duplicates",
                      "taxon_name", "original_name", "observation_id", "value_type")), everything()
    ) %>%
    filter(.data$number_of_duplicates > 1)
  
  rows_cannot_pivot
}


#' Trait value outliers at the species-level
#'
#' @param database     Trait database
#' @param dataset      Dataset being checked
#' @param trait        A specific trait concept
#' @param multiplier   Value indicating range above/below mean taxon by trait value that is flagged as an outlier
#'
#' @return Tibble of potential outliers
#' @export

dataset_check_outlier_by_species <- function(database, dataset, trait, multiplier) {
  
  to_compare <- 
    database$traits %>% 
    dplyr::filter(dataset_id == dataset)
  
  comparisons <- database$traits %>%
    dplyr::filter(trait_name == trait) %>%
    dplyr::filter(dataset_id != dataset) %>%
    dplyr::filter(taxon_name %in% to_compare$taxon_name) %>%
    dplyr::select(taxon_name, trait_name, value) %>%
    dplyr::group_by(taxon_name) %>%
    dplyr::mutate(
      count = n(),
      value = as.numeric(value)
    ) %>%
    dplyr::filter(count > 5) %>%
    dplyr::summarise(
      trait_name = first(trait_name),
      mean_value = mean(value),
      std_dev = sd(value),
      min_value = min(value),
      max_value = max(value),
      count = first(count)
    ) %>%
    dplyr::ungroup() 
  
  need_review <- to_compare %>%
    dplyr::filter(trait_name == trait) %>%
    dplyr::filter(taxon_name %in% comparisons$taxon_name) %>%
    dplyr::select(taxon_name, trait_name, value) %>%
    dplyr::left_join(
      by = c("taxon_name", "trait_name"),
      comparisons) %>%
    dplyr::filter(as.numeric(value) > multiplier*mean_value | as.numeric(value) < (1/multiplier)*mean_value) %>%
    dplyr::mutate(value_ratio = as.numeric(value)/mean_value) %>%
    dplyr::arrange(value_ratio)
  
  need_review
  
}

#' Trait value outliers at the genus-level
#'
#' @param database     Trait database
#' @param dataset      Dataset being checked
#' @param trait        A specific trait concept
#' @param multiplier   Value indicating range above/below mean taxon by trait value that is flagged as an outlier
#'
#' @return Tibble of potential outliers
#' @export
dataset_check_outlier_by_genus <- function(database, dataset, trait, multiplier) {
  
  to_compare <- 
    database$traits %>% 
    dplyr::filter(dataset_id == dataset) %>%
    dplyr::left_join(by = c("taxon_name"), database$taxa %>% select(taxon_name, genus))

  comparisons <- database$traits %>%
    dplyr::filter(trait_name == trait) %>%
    dplyr::filter(dataset_id != dataset) %>%
    dplyr::left_join(by = c("taxon_name"), database$taxa %>% select(taxon_name, genus)) %>%
    dplyr::filter(genus %in% to_compare$genus) %>%
    dplyr::select(genus, trait_name, value) %>%
    dplyr::group_by(genus) %>%
    dplyr::mutate(
      count = n(),
      value = as.numeric(value)
    ) %>%
    dplyr::filter(count > 5) %>%
    dplyr::summarise(
      trait_name = first(trait_name),
      mean_value = mean(value),
      std_dev = sd(value),
      min_value = min(value),
      max_value = max(value),
      count = first(count)
    ) %>%
    dplyr::ungroup() 
  
  need_review <- to_compare %>%
    dplyr::filter(trait_name == trait) %>%
    dplyr::filter(genus %in% comparisons$genus) %>%
    dplyr::select(taxon_name, trait_name, value, genus) %>%
    dplyr::left_join(
      by = c("genus", "trait_name"),
      comparisons) %>%
    dplyr::filter(as.numeric(value) > multiplier*mean_value | as.numeric(value) < (1/multiplier)*mean_value) %>%
    dplyr::mutate(value_ratio = as.numeric(value)/mean_value) %>%
    dplyr::arrange(value_ratio)
  
  need_review
  
}


#' Summary of duplicates within a dataset
#'
#' @param database     Trait database
#' @param dataset      Dataset being checked
#'
#' @return Tibble of trait values (by taxon) that occur more than once in a dataset
#' @export
#'
#' @examples
dataset_check_duplicates_within_dataset <- function(database, dataset) {

  duplicates_within_dataset <-
    database$traits %>%
      dplyr::filter(dataset_id == dataset) %>%
      dplyr::select(dplyr::all_of(c("taxon_name", "trait_name", "value", "entity_type"))) %>%
      dplyr::group_by(taxon_name, trait_name, entity_type, value) %>%
        dplyr::summarise(
          n_duplicates = n()
        ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n_duplicates > 1)
    
  duplicates_within_dataset
}
