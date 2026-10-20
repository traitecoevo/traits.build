#' Find numeric values far from what other datasets report for the same species
#'
#' Compares each numeric value in a dataset with the values other datasets in a
#' compiled database report for the same taxon and trait, and lists those that
#' differ by at least `min_fold`-fold. The question it answers is whether a
#' value is plausible given everything else known about the species, which the
#' allowed range in `traits.yml` cannot: a seed mass of 0.005 mg is in range for
#' plants but not for an *Angophora*. This is a curation aid, not a test --
#' species do vary -- but a value 10, 100 or 1000 times what everyone else
#' reports is most often a unit slip, a misplaced decimal point or a value
#' mapped to the wrong trait.
#'
#' **How values are compared.** Each value is compared with `other_median`, the
#' median, across the *other* datasets with values for that taxon and trait, of
#' each dataset's own median. Every other dataset gets one vote however many
#' values it holds, so a dataset of 30 individual measurements does not outvote
#' three datasets of one mean each -- which matters when the 30 values are the
#' ones in error. The dataset being checked is never part of its own
#' comparison. Values are compared as ratios, `value / other_median`, since
#' errors such as unit slips are multiplicative and traits like seed mass vary
#' over orders of magnitude, so only numeric traits that cannot be negative
#' (`allowed_values_min` of 0 or more) are checked, and zeros are left out:
#' a fold-difference is undefined for them. Traits that can be negative, such
#' as water potentials and isotope ratios, are not checked.
#'
#' **Value types.** Only values of compatible `value_type` are compared:
#' `value_types` lists the groups, and a value is compared only with values in
#' its own group. By default a minimum is compared only with other minima and a
#' maximum only with other maxima -- comparing a flora's maximum height with
#' field means would flag every tall species -- while `mean`, `median`, `mode`
#' and `raw` values, which all estimate a typical value, are compared with one
#' another. A value whose `value_type` is in no group, such as a `bin`, is not
#' checked. To compare them all together, pass
#' `value_types = list(all = c("mean", "median", "mode", "raw", "minimum", "maximum"))`;
#' to check means alone, `value_types = list("mean")`.
#'
#' Other attributes of a value, such as `life_stage` and `entity_type`, are
#' not matched, and are returned so that a seedling measured against adults
#' can be recognised as such.
#'
#' **Reading the output.** A value with a `ratio` near 1000 or 0.001 -- or 100
#' or 10 for traits such as lengths recorded in mm, cm or m -- is worth checking
#' first. A dataset whose values for a trait are all shifted, so that
#' `median_ratio` in `summary` is far from 1 across many taxa, is more likely to
#' have a unit or mapping error than one with a single flagged value. Traits
#' that vary widely within a species, such as `bark_thickness` (with stem size)
#' and `plant_height` (with age), produce the most flags that are not errors.
#'
#' @param database Compiled database object, e.g. `austraits`
#' @param dataset_ids `dataset_id`'s to check. The default, `NULL`, checks every
#' dataset in the database against the rest.
#' @param trait_names Traits to check. The default, `NULL`, checks every numeric
#' trait that cannot be negative.
#' @param min_fold Values that differ from `other_median` by at least this
#' factor, in either direction, are listed in `records`. The default, 10,
#' catches unit slips while leaving most natural variation alone.
#' @param min_other_datasets Minimum number of other datasets with values for
#' the taxon and trait for a value to be compared. The default, 2, means a
#' value is only flagged when it disagrees with the consensus of at least two
#' other datasets, rather than with a single one that might be the one in
#' error. Set it to 1 to also check taxa only one other dataset has measured.
#' @param value_types List of groups of `value_type`s to compare with each
#' other; see Details. A value whose `value_type` is in no group is not
#' checked.
#'
#' @return A list of two tibbles:
#' \describe{
#'   \item{`summary`}{One row per dataset, trait and group of value types with
#'   values that could be compared, so a dataset or trait missing from it had
#'   nothing to compare with. `n_taxa` and `n_values` count the taxa and values
#'   compared, `n_flagged` the values listed in `records`, and `median_ratio` is
#'   the median across taxa of each taxon's median `ratio` -- far from 1 when
#'   the whole trait is shifted. Sorted with the largest shifts first.}
#'   \item{`records`}{The values at least `min_fold` from `other_median`, with
#'   `ratio` (`value / other_median`, so below 1 for a value lower than the
#'   others) and `fold_difference` (the larger of `ratio` and `1 / ratio`), the
#'   range of the other datasets' values (`other_min`, `other_max`), how many
#'   values and datasets they comprise, and the record's details. Sorted with
#'   the largest `fold_difference` first.}
#' }
#' @seealso [dataset_check_outlier_by_genus()] to compare with the rest of the
#' genus, for taxa too few datasets have measured.
#' @examples
#' \dontrun{
#' outliers <- dataset_check_outlier_by_species(austraits, "Ooi_2007")
#' outliers$summary
#' outliers$records
#' }
#' @export
dataset_check_outlier_by_species <- function(
  database,
  dataset_ids = NULL,
  trait_names = NULL,
  min_fold = 10,
  min_other_datasets = 2,
  value_types = list(central = c("mean", "median", "mode", "raw"), minimum = "minimum", maximum = "maximum")
) {
  util_outlier_check(database, "taxon_name", dataset_ids, trait_names, min_fold, min_other_datasets, value_types)
}


#' Find numeric values far from what other datasets report for the same genus
#'
#' As [dataset_check_outlier_by_species()], but compares each value with the
#' values other datasets report for any taxon in the same genus, as given by
#' the `genus` column of the database's `taxa` table. Use it for taxa that too
#' few other datasets have measured for the species-level check, bearing in
#' mind that species within a genus can legitimately differ by far more than
#' individuals within a species: a genus of herbs and trees will flag every
#' tree. Taxa without a genus, such as names resolved only to family, are not
#' checked.
#'
#' `other_median` is the median, across every taxon in the genus and every
#' other dataset, of the median of each taxon's values in each dataset, so each
#' taxon a dataset measured counts once. `n_other_taxa` is the number of taxa
#' those values come from.
#'
#' @inheritParams dataset_check_outlier_by_species
#' @param min_fold Values that differ from `other_median` by at least this
#' factor, in either direction, are listed in `records`. The default, 100, is
#' higher than for species: in a sample from AusTraits, values 30- to 100-fold
#' from the rest of their genus were often species that genuinely differ,
#' while most beyond 100-fold were errors.
#'
#' @return As [dataset_check_outlier_by_species()], with `genus` and
#' `n_other_taxa` added to `records`.
#' @examples
#' \dontrun{
#' dataset_check_outlier_by_genus(austraits, "Ooi_2007")$records
#' }
#' @export
dataset_check_outlier_by_genus <- function(
  database,
  dataset_ids = NULL,
  trait_names = NULL,
  min_fold = 100,
  min_other_datasets = 2,
  value_types = list(central = c("mean", "median", "mode", "raw"), minimum = "minimum", maximum = "maximum")
) {
  util_outlier_check(database, "genus", dataset_ids, trait_names, min_fold, min_other_datasets, value_types)
}


# Shared engine for `dataset_check_outlier_by_species()` (`by = "taxon_name"`)
# and `dataset_check_outlier_by_genus()` (`by = "genus"`). Values are summarised
# into units -- one per dataset for a species, one per taxon and dataset for a
# genus -- and each value is compared with the median of the units from other
# datasets in its group, trait and group of value types.
util_outlier_check <- function(database, by, dataset_ids, trait_names, min_fold, min_other_datasets, value_types) {

  util_check_dataset_ids(database, dataset_ids)

  if (!is.numeric(min_fold) || length(min_fold) != 1 || !(min_fold > 1)) {
    stop("`min_fold` must be a single number greater than 1.", call. = FALSE)
  }

  if (!is.list(value_types) || length(value_types) == 0 || anyDuplicated(unlist(value_types))) {
    stop("`value_types` must be a list of groups of value types, each value type in one group only.", call. = FALSE)
  }
  if (is.null(names(value_types))) names(value_types) <- rep("", length(value_types))
  unnamed <- names(value_types) == ""
  names(value_types)[unnamed] <- purrr::map_chr(value_types[unnamed], paste, collapse = "/")
  value_type_group <- rlang::set_names(rep(names(value_types), lengths(value_types)), unlist(value_types))

  traits_checked <- util_ratio_scale_traits(database)
  if (!is.null(trait_names)) {
    not_checked <- setdiff(trait_names, traits_checked)
    if (length(not_checked) > 0) {
      stop(
        "Only numeric traits that cannot be negative can be checked, so not: ",
        paste(not_checked, collapse = ", "), call. = FALSE
      )
    }
    traits_checked <- trait_names
  }

  records <-
    database$traits %>%
    dplyr::filter(
      .data$trait_name %in% traits_checked,
      .data$value_type %in% names(value_type_group)
    )

  if (by == "genus") {
    if (!"genus" %in% names(database$taxa)) {
      stop("The database's `taxa` table has no `genus` column to group taxa by.", call. = FALSE)
    }
    records$genus <- database$taxa$genus[match(records$taxon_name, database$taxa$taxon_name)]
    records <- records %>% dplyr::filter(!is.na(.data$genus))
  }

  records <-
    records %>%
    dplyr::mutate(
      value_type_group = unname(value_type_group[.data$value_type]),
      x = suppressWarnings(as.numeric(.data$value))
    ) %>%
    dplyr::filter(!is.na(.data$x), .data$x > 0) %>%
    dplyr::mutate(log_x = log10(.data$x))

  group_cols <- c(by, "trait_name", "value_type_group")
  unit_cols <- unique(c(group_cols, "taxon_name", "dataset_id"))

  checked <- records
  if (!is.null(dataset_ids)) {
    checked <- records %>% dplyr::filter(.data$dataset_id %in% dataset_ids)
    # Only the groups the checked datasets have values in are needed
    records <- records %>% dplyr::semi_join(checked, by = group_cols)
  }

  # Per-group statistics come from `util_group_stat()` rather than
  # `summarise()`, which evaluates its expressions once per group: a sweep of
  # every dataset in AusTraits has over a million groups, and took over a minute.
  unit_ids <- util_group_ids(records, unit_cols)
  units <- unit_ids$keys
  units$n <- util_group_stat(records$x, unit_ids$id, nrow(units), "n")
  units$log_median <- util_group_stat(records$log_x, unit_ids$id, nrow(units), "median")
  units$log_min <- util_group_stat(records$log_x, unit_ids$id, nrow(units), "min")
  units$log_max <- util_group_stat(records$log_x, unit_ids$id, nrow(units), "max")

  # Each checked dataset's groups, paired with the units of every other dataset
  pairs <-
    checked %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols, "dataset_id")))) %>%
    dplyr::rename(checked_id = "dataset_id") %>%
    dplyr::inner_join(units, by = group_cols, relationship = "many-to-many") %>%
    dplyr::filter(.data$dataset_id != .data$checked_id)

  pair_ids <- util_group_ids(pairs, c(group_cols, "checked_id"))
  others <- pair_ids$keys
  others$other_median <- 10^util_group_stat(pairs$log_median, pair_ids$id, nrow(others), "median")
  others$other_min <- 10^util_group_stat(pairs$log_min, pair_ids$id, nrow(others), "min")
  others$other_max <- 10^util_group_stat(pairs$log_max, pair_ids$id, nrow(others), "max")
  others$n_other_values <- util_group_stat(pairs$n, pair_ids$id, nrow(others), "sum")
  others$n_other_datasets <- util_group_stat(pairs$dataset_id, pair_ids$id, nrow(others), "n_distinct")
  others$n_other_taxa <- util_group_stat(pairs$taxon_name, pair_ids$id, nrow(others), "n_distinct")
  others <- others %>% dplyr::filter(.data$n_other_datasets >= min_other_datasets)

  compared <-
    checked %>%
    dplyr::inner_join(others, by = c(group_cols, dataset_id = "checked_id")) %>%
    dplyr::mutate(
      ratio = .data$x / .data$other_median,
      fold_difference = pmax(.data$ratio, 1 / .data$ratio),
      flagged = as.integer(.data$fold_difference >= min_fold)
    )

  # Each taxon's median ratio, then the median of those across taxa, so a
  # taxon with many values counts no more than one with a single value
  taxon_ids <- util_group_ids(compared, c("dataset_id", "trait_name", "value_type_group", "taxon_name"))
  taxa <- taxon_ids$keys
  taxa$n <- util_group_stat(compared$x, taxon_ids$id, nrow(taxa), "n")
  taxa$n_flagged <- util_group_stat(compared$flagged, taxon_ids$id, nrow(taxa), "sum")
  taxa$log_ratio <- util_group_stat(log10(compared$ratio), taxon_ids$id, nrow(taxa), "median")

  trait_ids <- util_group_ids(taxa, c("dataset_id", "trait_name", "value_type_group"))
  summary <- trait_ids$keys
  summary$n_taxa <- util_group_stat(taxa$n, trait_ids$id, nrow(summary), "n")
  summary$n_values <- util_group_stat(taxa$n, trait_ids$id, nrow(summary), "sum")
  summary$n_flagged <- util_group_stat(taxa$n_flagged, trait_ids$id, nrow(summary), "sum")
  summary$median_ratio <- 10^util_group_stat(taxa$log_ratio, trait_ids$id, nrow(summary), "median")
  summary <- summary %>%
    dplyr::arrange(dplyr::desc(abs(log10(.data$median_ratio))), dplyr::desc(.data$n_taxa))

  records_cols <- c(
    "dataset_id", "taxon_name", if (by == "genus") "genus", "trait_name", "value", "unit", "value_type",
    "ratio", "fold_difference", "other_median", "other_min", "other_max", "n_other_values",
    "n_other_datasets", if (by == "genus") "n_other_taxa", "entity_type", "life_stage", "basis_of_record",
    "observation_id", "original_name"
  )

  list(
    summary = summary,
    records = compared %>%
      dplyr::filter(.data$fold_difference >= min_fold) %>%
      dplyr::arrange(dplyr::desc(.data$fold_difference), .data$dataset_id, .data$taxon_name) %>%
      dplyr::select(dplyr::all_of(records_cols))
  )
}


#' Find taxa whose values for a trait are all the same within a dataset
#'
#' Lists the taxa that have several values for a trait in a dataset, all of
#' them identical. This is usually a value recorded once for the species, such
#' as its growth form, repeated on every row of individual measurements, or a
#' single bulked measurement reported for each individual that contributed to
#' it. Neither is disallowed, so [dataset_test()] does not check for it, but a
#' value repeated this way counts as several observations when it is one, and
#' is usually better entered once, with the `entity_type` it describes.
#'
#' Values are compared within a taxon as the source names it, by
#' `original_name`, at a single location. Values for names the source gives
#' separately but the taxonomy merges into one `taxon_name`, as for synonyms in
#' a flora, are separate records of the source rather than repeats; and a
#' taxon scored once at each of several sites, as in a survey of fire
#' responses, has one value per population, as it should. Neither is counted.
#' What remains is the same value given more than once for one name at one
#' place: copied down to individuals, or repeated across `method_context`s,
#' `temporal_context`s or sources, any of which may be intended.
#'
#' A trait repeated for nearly every taxon with several values, so that
#' `n_taxa_repeated` is close to `n_taxa_several` in `summary`, has most likely
#' been copied down a column, or mapped from two columns that agree. A few
#' repeated taxa among many that vary is what a trait that varies little looks
#' like.
#'
#' @param database Compiled database object, e.g. `austraits`
#' @param dataset_ids `dataset_id`'s to check. The default, `NULL`, checks every
#' dataset in the database.
#' @param trait_types Types of trait to check. The default checks categorical
#' traits only: numeric values repeat by coincidence, especially for traits
#' reported to few significant figures, but `c("categorical", "numeric")`
#' checks both.
#'
#' @return A list of two tibbles:
#' \describe{
#'   \item{`summary`}{One row per dataset and trait with at least one taxon
#'   whose values are all identical. `n_taxa_several` is the number of taxa
#'   with more than one value for the trait, and `n_taxa_repeated` the number
#'   of those whose values are all identical, counting a taxon once for each
#'   name and location. `entity_type` lists the entity types of the trait's
#'   values. Sorted by dataset, then with the traits repeated for the largest
#'   share of taxa first.}
#'   \item{`taxa`}{One row per taxon, name and location whose values are all
#'   identical, with the `value` and how many records hold it
#'   (`n_records`).}
#' }
#' @examples
#' \dontrun{
#' dataset_check_duplicates_within_dataset(austraits, "Tsakalos_2020")
#' }
#' @export
dataset_check_duplicates_within_dataset <- function(database, dataset_ids = NULL, trait_types = "categorical") {

  util_check_dataset_ids(database, dataset_ids)

  type <- purrr::map_chr(database$definitions, ~ if (is.null(.x[["type"]])) NA_character_ else .x[["type"]])

  records <-
    database$traits %>%
    dplyr::filter(
      .data$trait_name %in% names(type)[type %in% trait_types],
      is.null(dataset_ids) | .data$dataset_id %in% dataset_ids
    )

  ids <- util_group_ids(records, c("dataset_id", "trait_name", "taxon_name", "original_name", "location_id"))
  taxa <- ids$keys
  first <- match(seq_len(nrow(taxa)), ids$id)
  taxa$value <- records$value[first]
  taxa$n_records <- util_group_stat(records$value, ids$id, nrow(taxa), "n")
  taxa$n_values <- util_group_stat(records$value, ids$id, nrow(taxa), "n_distinct")
  taxa$entity_type <- records$entity_type[first]
  taxa$value_type <- records$value_type[first]
  taxa <- taxa %>% dplyr::filter(.data$n_records > 1)

  summary <-
    taxa %>%
    dplyr::group_by(.data$dataset_id, .data$trait_name) %>%
    dplyr::summarise(
      n_taxa_several = dplyr::n(),
      n_taxa_repeated = sum(.data$n_values == 1),
      entity_type = paste(sort(unique(.data$entity_type)), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_taxa_repeated > 0) %>%
    dplyr::arrange(.data$dataset_id, dplyr::desc(.data$n_taxa_repeated / .data$n_taxa_several), .data$trait_name)

  list(
    summary = summary,
    taxa = taxa %>%
      dplyr::filter(.data$n_values == 1) %>%
      dplyr::left_join(
        database$locations %>%
          dplyr::distinct(dplyr::across(dplyr::all_of(c("dataset_id", "location_id", "location_name")))),
        by = c("dataset_id", "location_id")
      ) %>%
      dplyr::select(dplyr::all_of(c(
        "dataset_id", "trait_name", "taxon_name", "original_name", "location_name", "value", "n_records",
        "entity_type", "value_type"
      ))) %>%
      dplyr::arrange(.data$dataset_id, .data$trait_name, .data$taxon_name, .data$original_name, .data$location_name)
  )
}


#' List the categorical values a dataset needs substitutions for
#'
#' Lists each categorical value a dataset has that is not an allowed value of
#' its trait in `traits.yml`, and was therefore moved to `excluded_data` with
#' the error "Unsupported trait value". The result is laid out as a table of
#' substitutions with `replace` empty: fill in `replace` for each value, in R
#' or a spreadsheet, and add them to the dataset's `metadata.yml` with
#' `metadata_add_substitutions_table(x, "dataset_id", "trait_name", "find",
#' "replace")`.
#'
#' The values are as the build left them, after the dataset's existing
#' `substitutions` and the synonyms declared in `traits.yml` were applied, and
#' lower-cased, as `find` is matched. A value holding several space-separated
#' values is excluded whole when any one of them is not allowed; `unsupported`
#' lists which, since a single `match: word` substitution can fix that word
#' wherever it occurs (see [metadata_add_substitution()]).
#'
#' @param database Compiled database object, e.g. `austraits`
#' @param dataset_ids `dataset_id`'s to check. The default, `NULL`, lists the
#' values for every dataset in the database.
#'
#' @return Tibble with one row per dataset, trait and unsupported value, with
#' columns `dataset_id`, `trait_name`, `find`, `replace` (empty), `n_records`,
#' the number of records excluded with that value, and `unsupported`, the
#' words in `find` that are not allowed values of the trait.
#' @examples
#' \dontrun{
#' substitutions <- dataset_check_categorical_substitutions(austraits, "Test_2025")
#' # ... fill in `replace`, then
#' metadata_add_substitutions_table(substitutions, "dataset_id", "trait_name", "find", "replace")
#' }
#' @export
dataset_check_categorical_substitutions <- function(database, dataset_ids = NULL) {

  if (!is.null(dataset_ids)) {
    missing_ids <- setdiff(dataset_ids, c(database$traits$dataset_id, database$excluded_data$dataset_id))
    if (length(missing_ids) > 0) {
      stop("`dataset_ids` not in the database: ", paste(missing_ids, collapse = ", "), call. = FALSE)
    }
  }

  substitutions <-
    database$excluded_data %>%
    dplyr::filter(
      .data$error == "Unsupported trait value",
      is.null(dataset_ids) | .data$dataset_id %in% dataset_ids
    ) %>%
    dplyr::count(.data$dataset_id, .data$trait_name, .data$value, name = "n_records") %>%
    dplyr::rename(find = "value") %>%
    dplyr::mutate(replace = NA_character_, .after = "find")

  substitutions$unsupported <- purrr::map2_chr(
    substitutions$find, substitutions$trait_name,
    function(find, trait_name) {
      allowed <- names(database$definitions[[trait_name]][["allowed_values_levels"]])
      paste(setdiff(stringr::str_split_1(find, " "), allowed), collapse = " ")
    }
  )

  substitutions %>% dplyr::arrange(.data$dataset_id, .data$trait_name, .data$find)
}


# Integer ids numbering the distinct combinations of `cols` in `df`, and those
# combinations, `keys`, in id order
util_group_ids <- function(df, cols) {
  grouped <- dplyr::group_by(df, dplyr::across(dplyr::all_of(cols)))
  list(id = dplyr::group_indices(grouped), keys = dplyr::ungroup(dplyr::group_keys(grouped)))
}

# A statistic of `x` within each group, for groups numbered 1 to `n_groups` by
# `id`, all of them present: "n", "sum", "min", "max", "median" or
# "n_distinct", which counts `NA` as a value, as `dplyr::n_distinct()` does.
# The others expect no `NA`. Computed from a single sort of `x`, not a function
# call per group.
util_group_stat <- function(x, id, n_groups, stat) {
  n <- tabulate(id, n_groups)
  if (stat == "n") {
    return(n)
  }
  o <- order(id, x, method = "radix")
  x <- x[o]
  id <- id[o]
  end <- cumsum(n)
  start <- end - n + 1
  switch(stat,
    sum = {
      total <- c(0, cumsum(x))
      total <- total[end + 1] - total[start]
      if (is.integer(x)) as.integer(total) else total
    },
    min = x[start],
    max = x[end],
    median = (x[start + (n - 1) %/% 2] + x[start + n %/% 2]) / 2,
    n_distinct = {
      same <- x[-1] == x[-length(x)]
      same[is.na(same)] <- is.na(x[-1])[is.na(same)] & is.na(x[-length(x)])[is.na(same)]
      first <- c(TRUE, id[-1] != id[-length(id)] | !same)
      tabulate(id[first], n_groups)
    }
  )
}


# Numeric traits on a ratio scale, whose values can be compared as fold
# differences: those whose `allowed_values_min` is zero or more, or, lacking
# one, with no negative value in the database.
util_ratio_scale_traits <- function(database) {

  type <- purrr::map_chr(database$definitions, ~ if (is.null(.x[["type"]])) NA_character_ else .x[["type"]])
  allowed_min <- purrr::map_dbl(
    database$definitions,
    ~ if (is.null(.x[["allowed_values_min"]])) NA_real_ else suppressWarnings(as.numeric(.x[["allowed_values_min"]]))
  )

  numeric <- names(type)[type %in% "numeric"]
  no_min <- intersect(numeric, names(allowed_min)[is.na(allowed_min)])
  negative <-
    database$traits %>%
    dplyr::filter(.data$trait_name %in% no_min) %>%
    dplyr::filter(suppressWarnings(as.numeric(.data$value)) < 0) %>%
    dplyr::pull("trait_name")

  setdiff(intersect(numeric, names(allowed_min)[allowed_min >= 0 | is.na(allowed_min)]), negative)
}


# Stops with the `dataset_ids` not in the database, for the checks that take
# `dataset_ids = NULL` to mean every dataset
util_check_dataset_ids <- function(database, dataset_ids) {
  if (!is.null(dataset_ids)) {
    missing_ids <- setdiff(dataset_ids, database$traits$dataset_id)
    if (length(missing_ids) > 0) {
      stop("`dataset_ids` not in the database: ", paste(missing_ids, collapse = ", "), call. = FALSE)
    }
  }
}
