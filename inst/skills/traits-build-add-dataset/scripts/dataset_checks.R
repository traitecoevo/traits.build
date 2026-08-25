#!/usr/bin/env Rscript
# dataset_checks.R -- GATE 2 helpers for the traits-build-add-dataset skill.
#
# These seven functions are taken close to verbatim from
# `check_dataset_functions.qmd` in traits.build-book, where they exist only
# as code for a curator to copy into a session -- the package itself exports
# none of them. Sourcing this file makes them available without
# copy-pasting.
#
# Each is defined only if a function of the same name doesn't already exist
# on the search path, so a host repo that has its own copy (or a future
# improved version) wins rather than being silently shadowed.
#
# All operate on a built `database` list (the output of a `traits.build`
# build, e.g. `remake::make(current_study)` or a full rebuild) and a
# `dataset` (a `dataset_id` string), and return a data frame -- empty when
# there is nothing to review.
#
# `dataset_check_duplicates_across_datasets()` is *not* included here: the
# book documents it as "TO BE WRITTEN" and unimplemented, and that remains
# true; nothing to source.
#
# Usage: `source("inst/skills/traits-build-add-dataset/scripts/dataset_checks.R")`
# from a database repo's root, after building (at least) `dataset` into
# `database`.

#' Categorical trait values excluded for having no matching allowed value
#'
#' @param database A built traits.build database (list with `excluded_data`)
#' @param dataset A `dataset_id`
#' @return A data frame with `trait_name`, `find` (the offending value), and
#'   an empty `replace` column for the curator to fill in.
if (!exists("dataset_check_categorical_substitutions", mode = "function")) {
  dataset_check_categorical_substitutions <- function(database, dataset) {
    database$excluded_data %>%
      dplyr::filter(
        .data$dataset_id == dataset,
        .data$error == "Unsupported trait value"
      ) %>%
      dplyr::distinct(.data$dataset_id, .data$trait_name, .data$value) %>%
      dplyr::rename(find = "value") %>%
      dplyr::select(-"dataset_id") %>%
      dplyr::mutate(replace = NA)
  }
}

#' Numeric trait values excluded for falling outside the allowed range
#'
#' A long table here is almost always a units error, not real outliers --
#' confirm the unit mapping before treating any of these as legitimate
#' exclusions.
#'
#' @inheritParams dataset_check_categorical_substitutions
#' @return A data frame of the excluded values and their context.
if (!exists("dataset_check_numeric_values", mode = "function")) {
  dataset_check_numeric_values <- function(database, dataset) {
    database$excluded_data %>%
      dplyr::filter(
        .data$dataset_id == dataset,
        .data$error == "Value out of allowable range"
      ) %>%
      dplyr::select(dplyr::all_of(c(
        "dataset_id", "trait_name", "value", "observation_id", "unit", "original_name"
      )))
  }
}

#' Taxon names in `taxonomic_updates` not yet resolved against `taxon_list.csv`
#'
#' @param taxon_list The database's `config/taxon_list.csv`, already read in
#' @inheritParams dataset_check_categorical_substitutions
#' @return A data frame of `original_name`s needing further alignment.
if (!exists("dataset_check_taxonomic_updates", mode = "function")) {
  dataset_check_taxonomic_updates <- function(taxon_list, database, dataset) {
    database$taxonomic_updates %>%
      dplyr::filter(.data$dataset_id == dataset) %>%
      dplyr::filter(
        !.data$aligned_name %in% taxon_list$aligned_name,
        !.data$aligned_name %in% taxon_list$taxon_name
      ) %>%
      dplyr::filter(is.na(.data$taxonomic_resolution)) %>%
      dplyr::distinct(.data$original_name)
  }
}

#' Trait measurements preventing the dataset from pivoting wider
#'
#' `dataset_test()`'s pivot check requires each row to have a unique
#' combination of `dataset_id`, `trait_name`, `observation_id`, `value_type`,
#' `repeat_measurements_id`, `method_id`, `method_context_id`. A failure
#' usually traces back to `observation_id` not being parsed as intended --
#' typically a missing context mapping or `source_id` column. This narrows a
#' pivot failure down to the offending rows instead of the full traits table.
#'
#' @inheritParams dataset_check_categorical_substitutions
#' @return A data frame of the rows with `number_of_duplicates > 1`.
if (!exists("dataset_check_not_pivoting", mode = "function")) {
  dataset_check_not_pivoting <- function(database, dataset) {
    database$traits %>%
      dplyr::filter(.data$dataset_id %in% dataset) %>%
      dplyr::select(dplyr::all_of(c(
        # `taxon_name` and `original_name` are not needed for pivoting but
        # are included for informative purposes.
        "dataset_id", "trait_name", "value", "taxon_name", "original_name",
        "observation_id", "value_type", "repeat_measurements_id", "method_id",
        "method_context_id"
      ))) %>%
      tidyr::pivot_wider(names_from = "trait_name", values_from = "value", values_fn = length) %>%
      tidyr::pivot_longer(cols = 9:ncol(.)) %>%
      dplyr::rename(dplyr::all_of(c("trait_name" = "name", "number_of_duplicates" = "value"))) %>%
      dplyr::select(
        dplyr::all_of(c("dataset_id", "trait_name", "number_of_duplicates",
                        "taxon_name", "original_name", "observation_id", "value_type")),
        dplyr::everything()
      ) %>%
      dplyr::filter(.data$number_of_duplicates > 1)
  }
}

#' Numeric trait values that are outliers relative to the same taxon elsewhere in the database
#'
#' Only useful once the database already holds other observations of `trait`
#' for the same taxa -- there is no "correct" value to compare against
#' otherwise. A `multiplier` of 100-1000 tends to flag genuine outliers; 10
#' is likely to flag legitimate values too.
#'
#' @inheritParams dataset_check_categorical_substitutions
#' @param trait A `trait_name`
#' @param multiplier How many-fold above/below the taxon's mean counts as an outlier
#' @return A data frame of candidate outliers, most extreme first.
if (!exists("dataset_check_outlier_by_species", mode = "function")) {
  dataset_check_outlier_by_species <- function(database, dataset, trait, multiplier) {
    to_compare <- database$traits %>% dplyr::filter(.data$dataset_id == dataset)

    comparisons <- database$traits %>%
      dplyr::filter(.data$trait_name == trait) %>%
      dplyr::filter(.data$dataset_id != dataset) %>%
      dplyr::filter(.data$taxon_name %in% to_compare$taxon_name) %>%
      dplyr::select(dplyr::all_of(c("taxon_name", "trait_name", "value"))) %>%
      dplyr::group_by(.data$taxon_name) %>%
      dplyr::mutate(count = dplyr::n(), value = as.numeric(.data$value)) %>%
      dplyr::filter(.data$count > 5) %>%
      dplyr::summarise(
        trait_name = dplyr::first(.data$trait_name),
        mean_value = mean(.data$value),
        std_dev = stats::sd(.data$value),
        min_value = min(.data$value),
        max_value = max(.data$value),
        count = dplyr::first(.data$count)
      ) %>%
      dplyr::ungroup()

    to_compare %>%
      dplyr::filter(.data$trait_name == trait) %>%
      dplyr::filter(.data$taxon_name %in% comparisons$taxon_name) %>%
      dplyr::select(dplyr::all_of(c(
        "taxon_name", "trait_name", "value", "observation_id", "unit", "original_name"
      ))) %>%
      dplyr::left_join(comparisons, by = c("taxon_name", "trait_name")) %>%
      dplyr::filter(
        as.numeric(.data$value) > multiplier * .data$mean_value |
          as.numeric(.data$value) < (1 / multiplier) * .data$mean_value
      ) %>%
      dplyr::mutate(value_ratio = as.numeric(.data$value) / .data$mean_value) %>%
      dplyr::arrange(dplyr::desc(.data$value_ratio))
  }
}

#' Numeric trait values that are outliers relative to the same genus elsewhere in the database
#'
#' As `dataset_check_outlier_by_species()`, but compared against all taxa
#' sharing a genus. Weaker for genera whose species legitimately span a wide
#' trait range -- it will flag correct values as "outliers" there.
#'
#' @inheritParams dataset_check_outlier_by_species
#' @return A data frame of candidate outliers, most extreme first.
if (!exists("dataset_check_outlier_by_genus", mode = "function")) {
  dataset_check_outlier_by_genus <- function(database, dataset, trait, multiplier) {
    taxa_genus <- database$taxa %>% dplyr::select(dplyr::all_of(c("taxon_name", "genus")))

    to_compare <- database$traits %>%
      dplyr::filter(.data$dataset_id == dataset) %>%
      dplyr::left_join(taxa_genus, by = "taxon_name")

    comparisons <- database$traits %>%
      dplyr::filter(.data$trait_name == trait) %>%
      dplyr::filter(.data$dataset_id != dataset) %>%
      dplyr::left_join(taxa_genus, by = "taxon_name") %>%
      dplyr::filter(.data$genus %in% to_compare$genus) %>%
      dplyr::select(dplyr::all_of(c("genus", "trait_name", "value"))) %>%
      dplyr::group_by(.data$genus) %>%
      dplyr::mutate(count = dplyr::n(), value = as.numeric(.data$value)) %>%
      dplyr::filter(.data$count > 5) %>%
      dplyr::summarise(
        trait_name = dplyr::first(.data$trait_name),
        mean_value = mean(.data$value),
        std_dev = stats::sd(.data$value),
        min_value = min(.data$value),
        max_value = max(.data$value),
        count = dplyr::first(.data$count)
      ) %>%
      dplyr::ungroup()

    to_compare %>%
      dplyr::filter(.data$trait_name == trait) %>%
      dplyr::filter(.data$genus %in% comparisons$genus) %>%
      dplyr::select(dplyr::all_of(c(
        "taxon_name", "trait_name", "value", "genus", "observation_id", "unit", "original_name"
      ))) %>%
      dplyr::left_join(comparisons, by = c("genus", "trait_name")) %>%
      dplyr::filter(
        as.numeric(.data$value) > multiplier * .data$mean_value |
          as.numeric(.data$value) < (1 / multiplier) * .data$mean_value
      ) %>%
      dplyr::mutate(value_ratio = as.numeric(.data$value) / .data$mean_value) %>%
      dplyr::arrange(dplyr::desc(.data$value_ratio))
  }
}

#' Duplicate taxon x trait values within a single dataset
#'
#' Expected, not suspicious, for numeric traits reported to few significant
#' figures (nutrient contents) or where one bulked measurement is reported
#' against every contributing individual. Worth investigating when
#' `n_duplicates` is large, or identical across every taxon for one trait --
#' in which case the fix is a `custom_R_code` de-duplication, not a metadata
#' edit.
#'
#' @inheritParams dataset_check_categorical_substitutions
#' @return A data frame of `(taxon_name, trait_name, entity_type, value)`
#'   combinations occurring more than once, with a count.
if (!exists("dataset_check_duplicates_within_dataset", mode = "function")) {
  dataset_check_duplicates_within_dataset <- function(database, dataset) {
    database$traits %>%
      dplyr::filter(.data$dataset_id == dataset) %>%
      dplyr::select(dplyr::all_of(c("taxon_name", "trait_name", "value", "entity_type"))) %>%
      dplyr::group_by(.data$taxon_name, .data$trait_name, .data$entity_type, .data$value) %>%
      dplyr::summarise(n_duplicates = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(.data$n_duplicates > 1)
  }
}
