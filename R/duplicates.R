#' Find values that may duplicate data already in another dataset
#'
#' Compares trait values across datasets in a compiled database and lists
#' matches that may be the same data entered twice, such as a dataset that
#' reuses values from an earlier study or a flora. This is a curation aid, not
#' a test: many matches are coincidences, and the output is for deciding which
#' are not. Only pairs of *different* datasets are compared.
#'
#' **Numeric traits.** Two records match when they are for the same
#' `taxon_name` and `trait_name` and their values agree once both are rounded to
#' the precision of the less precise one -- so `1.23` matches `1.2`, but `1.26`
#' does not. Precision is the number of significant figures in the value as
#' stored, capped at 6. When either value has fewer than `min_sig_figs`
#' significant figures, the values must be exactly equal, since `2` would
#' otherwise match anything from `1.5` to `2.49`. Bins (`value_type` `bin`, e.g.
#' `1--5`) must be identical. Records that both have coordinates must also be
#' within `max_distance_km` of each other; records without coordinates are
#' matched on value alone, as most species-level data (floras, compilations)
#' have none.
#'
#' **Categorical traits.** A set value per species is the norm, so identical
#' values are only compared when both records have coordinates within
#' `max_distance_km`, and a pair of datasets is only reported when such matches
#' span at least `min_categorical_taxa` taxa. Traits of type `month_presence`
#' and `text` are compared in the same way.
#'
#' **Telling duplicates from coincidences.** Genuinely duplicated data usually
#' shows up as a high `prop_matched_combos` -- nearly every taxon x trait the two
#' datasets share has a matching value -- across several taxa, often with
#' `n_obs_pairs_multi_trait` > 0 (the same pair of observations matching on
#' several traits) and a low `p_chance`. Beware traits with a narrow range that
#' are commonly reported to two significant figures, such as leaf nitrogen
#' concentration: `p_chance` is high for these, and a large pair of datasets
#' will share some values by chance.
#'
#' @param database Compiled database object, e.g. `austraits`
#' @param dataset_ids `dataset_id`'s to check. Pairs involving any of them are
#' reported, oriented so the dataset being checked is `dataset_id_1`. The
#' default, `NULL`, checks every pair of datasets in the database.
#' @param max_distance_km Records that both have coordinates only match if they
#' are within this distance of each other. The default, 10 km, allows for
#' coordinates rounded to 0.1 degrees.
#' @param min_sig_figs Numeric values with fewer significant figures than this
#' must be exactly equal to match
#' @param min_matches Minimum number of taxon x trait combinations with a
#' matching numeric value for a pair of datasets to be reported
#' @param min_categorical_taxa Minimum number of taxa with matching categorical
#' values for a pair of datasets to be reported
#'
#' @return A list of four tibbles:
#' \describe{
#'   \item{`numeric_pairs`}{One row per pair of datasets with numeric matches,
#'   sorted by `prop_matched_combos`. `n_shared_combos` is the number of taxon x
#'   trait combinations with numeric values in both datasets, and
#'   `n_matched_combos` how many of those have a matching value.
#'   `prop_records_1` and `prop_records_2` are the proportions of each
#'   dataset's records in those shared combinations that match a record in the
#'   other. `n_identical` counts matches with exactly equal values,
#'   `n_georef_close` matches where both records have coordinates, and
#'   `n_obs_pairs_multi_trait` pairs of observations that match on two or more
#'   traits. `median_p_chance` summarises `p_chance` (see `numeric_records`).}
#'   \item{`numeric_records`}{The matching records for the pairs in
#'   `numeric_pairs`, two rows per match sharing a `match_id`, with the
#'   dataset's location and context columns. `match_precision` is the number of
#'   significant figures the values were compared at (`NA` for bins),
#'   `p_chance` is the proportion of the trait's values across the whole
#'   database that share the matched value at that precision -- the chance a
#'   value picked at random would match -- and `n_traits_observation_pair` is
#'   the number of traits on which the two records' observations match.}
#'   \item{`categorical_pairs`}{As `numeric_pairs`, for georeferenced
#'   categorical values. `prop_records_1` and `prop_records_2` are the
#'   proportions of each dataset's georeferenced categorical records that
#'   match, and `n_same_coordinates` counts matches less than 10 m apart.}
#'   \item{`categorical_records`}{The matching records for the pairs in
#'   `categorical_pairs`, laid out as `numeric_records`.}
#' }
#' @export
dataset_check_duplicate_values <- function(
  database,
  dataset_ids = NULL,
  max_distance_km = 10,
  min_sig_figs = 2,
  min_matches = 3,
  min_categorical_taxa = 10
) {

  if (!is.null(dataset_ids)) {
    missing_ids <- setdiff(dataset_ids, database$traits$dataset_id)
    if (length(missing_ids) > 0) {
      stop("`dataset_ids` not in the database: ", paste(missing_ids, collapse = ", "), call. = FALSE)
    }
  }

  trait_types <- purrr::map_chr(database$definitions, ~ if (is.null(.x[["type"]])) NA_character_ else .x[["type"]])

  # `.row` identifies a record throughout, so matches can be traced back to the
  # full traits table once candidates have been narrowed down
  database$traits$.row <- seq_len(nrow(database$traits))

  records <-
    austraits::join_location_coordinates(database)$traits %>%
    dplyr::select(dplyr::all_of(c(
      ".row", "dataset_id", "taxon_name", "trait_name", "value", "value_type", "observation_id",
      lat = "latitude (deg)", lon = "longitude (deg)"
    ))) %>%
    dplyr::mutate(
      lat = suppressWarnings(as.numeric(.data$lat)),
      lon = suppressWarnings(as.numeric(.data$lon))
    )

  numeric_records <- records %>% dplyr::filter(.data$trait_name %in% names(trait_types)[trait_types == "numeric"])
  # `month_presence` and `text` values are compared as categorical ones are, on
  # identical values
  categorical_records <- records %>%
    dplyr::filter(
      .data$trait_name %in% names(trait_types)[trait_types %in% c("categorical", "month_presence", "text")],
      !is.na(.data$lat), !is.na(.data$lon)
    )

  ## Numeric traits
  numeric_matches <-
    util_numeric_value_matches(numeric_records, min_sig_figs) %>%
    util_add_match_records(numeric_records, dataset_ids) %>%
    dplyr::filter(is.na(.data$distance_km) | .data$distance_km <= max_distance_km)

  numeric_pairs <-
    numeric_matches %>%
    dplyr::group_by(.data$dataset_id_1, .data$dataset_id_2) %>%
    dplyr::summarise(
      n_matched_combos = dplyr::n_distinct(.data$taxon_name, .data$trait_name),
      n_taxa_matched = dplyr::n_distinct(.data$taxon_name),
      n_traits_matched = dplyr::n_distinct(.data$trait_name),
      n_record_pairs = dplyr::n(),
      n_records_1 = dplyr::n_distinct(.data$.row_1),
      n_records_2 = dplyr::n_distinct(.data$.row_2),
      n_identical = sum(.data$identical_value),
      n_georef_close = sum(!is.na(.data$distance_km)),
      n_obs_pairs_multi_trait = dplyr::n_distinct(
        .data$observation_id_1[.data$n_traits_observation_pair > 1],
        .data$observation_id_2[.data$n_traits_observation_pair > 1]
      ),
      median_p_chance = stats::median(.data$p_chance),
      traits = paste(sort(unique(.data$trait_name)), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_matched_combos >= min_matches)

  # Denominators: what the two datasets could have matched on
  numeric_combos <- numeric_records %>% dplyr::count(.data$dataset_id, .data$taxon_name, .data$trait_name)

  numeric_pairs <-
    numeric_pairs %>%
    dplyr::select(dplyr::all_of(c("dataset_id_1", "dataset_id_2"))) %>%
    dplyr::inner_join(numeric_combos, by = c("dataset_id_1" = "dataset_id"), relationship = "many-to-many") %>%
    dplyr::inner_join(
      numeric_combos, by = c("dataset_id_2" = "dataset_id", "taxon_name", "trait_name"), suffix = c("_1", "_2")
    ) %>%
    dplyr::group_by(.data$dataset_id_1, .data$dataset_id_2) %>%
    dplyr::summarise(
      n_shared_combos = dplyr::n(),
      n_shared_records_1 = sum(.data$n_1),
      n_shared_records_2 = sum(.data$n_2),
      .groups = "drop"
    ) %>%
    dplyr::right_join(numeric_pairs, by = c("dataset_id_1", "dataset_id_2")) %>%
    dplyr::mutate(
      prop_matched_combos = .data$n_matched_combos / .data$n_shared_combos,
      prop_records_1 = .data$n_records_1 / .data$n_shared_records_1,
      prop_records_2 = .data$n_records_2 / .data$n_shared_records_2
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "dataset_id_1", "dataset_id_2", "n_shared_combos", "n_matched_combos", "prop_matched_combos",
      "n_taxa_matched", "n_traits_matched", "n_record_pairs", "n_records_1", "n_records_2",
      "prop_records_1", "prop_records_2", "n_identical", "n_georef_close", "n_obs_pairs_multi_trait",
      "median_p_chance", "traits"
    ))) %>%
    dplyr::arrange(dplyr::desc(.data$prop_matched_combos), dplyr::desc(.data$n_matched_combos))

  ## Categorical traits
  categorical_matches <-
    util_categorical_value_matches(categorical_records, max_distance_km) %>%
    util_add_match_records(categorical_records, dataset_ids)

  categorical_totals <- categorical_records %>% dplyr::count(.data$dataset_id)

  categorical_pairs <-
    categorical_matches %>%
    dplyr::group_by(.data$dataset_id_1, .data$dataset_id_2) %>%
    dplyr::summarise(
      n_taxa_matched = dplyr::n_distinct(.data$taxon_name),
      n_traits_matched = dplyr::n_distinct(.data$trait_name),
      n_record_pairs = dplyr::n(),
      n_records_1 = dplyr::n_distinct(.data$.row_1),
      n_records_2 = dplyr::n_distinct(.data$.row_2),
      n_same_coordinates = sum(.data$distance_km < 0.01),
      median_distance_km = stats::median(.data$distance_km),
      traits = paste(sort(unique(.data$trait_name)), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_taxa_matched >= min_categorical_taxa) %>%
    dplyr::left_join(categorical_totals %>% dplyr::rename(n_georef_1 = "n"), by = c("dataset_id_1" = "dataset_id")) %>%
    dplyr::left_join(categorical_totals %>% dplyr::rename(n_georef_2 = "n"), by = c("dataset_id_2" = "dataset_id")) %>%
    dplyr::mutate(
      prop_records_1 = .data$n_records_1 / .data$n_georef_1,
      prop_records_2 = .data$n_records_2 / .data$n_georef_2
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "dataset_id_1", "dataset_id_2", "n_taxa_matched", "n_traits_matched", "n_record_pairs",
      "n_records_1", "n_records_2", "prop_records_1", "prop_records_2", "n_same_coordinates",
      "median_distance_km", "traits"
    ))) %>%
    dplyr::arrange(dplyr::desc(.data$n_taxa_matched))

  ## Records behind the reported pairs
  list(
    numeric_pairs = numeric_pairs,
    numeric_records = util_duplicate_records(
      database,
      numeric_matches %>% dplyr::semi_join(numeric_pairs, by = c("dataset_id_1", "dataset_id_2")),
      c("match_precision", "identical_value", "distance_km", "p_chance", "n_traits_observation_pair")
    ),
    categorical_pairs = categorical_pairs,
    categorical_records = util_duplicate_records(
      database,
      categorical_matches %>% dplyr::semi_join(categorical_pairs, by = c("dataset_id_1", "dataset_id_2")),
      c("distance_km")
    )
  )
}


# Number of significant figures in numeric values stored as text. Trailing
# zeros count after a decimal point ("2.50" has 3) but not in an integer
# ("250" has 2), where they cannot be told apart from padding. Zero has none.
util_sig_figs <- function(value) {
  digits <- value %>%
    stringr::str_remove("^[-+]") %>%
    stringr::str_remove("[eE].*$")
  has_point <- stringr::str_detect(digits, "\\.")
  digits <- digits %>%
    stringr::str_remove("\\.") %>%
    stringr::str_remove("^0+")
  digits <- ifelse(has_point, digits, stringr::str_remove(digits, "0+$"))
  n <- nchar(digits)
  ifelse(n == 0, NA_integer_, n)
}

# Key identifying nonzero `x` rounded to `k` significant figures, e.g. 1.23
# at k = 2 is "12e0" and 1230 is "12e3". A value exactly halfway rounds up, as
# a spreadsheet does, or with `half_up = FALSE` down, which is what R's
# `signif()` usually does (`signif(145, 2)` is 140). Rounding is done on an
# integer mantissa with a small tolerance, so a halfway value is recognised
# despite being stored as 1.2499999...
util_sig_fig_key <- function(x, k, half_up = TRUE) {
  exponent <- floor(log10(abs(x)))
  scaled <- abs(x) / 10^(exponent - k + 1)
  mantissa <- if (half_up) floor(scaled + 0.5 + 1e-9 * scaled) else ceiling(scaled - 0.5 - 1e-9 * scaled)
  # Rounding up can add a digit, e.g. 9.96 at k = 2
  carry <- which(mantissa >= 10^k)
  mantissa[carry] <- mantissa[carry] / 10
  exponent[carry] <- exponent[carry] + 1
  paste0(ifelse(x < 0, "-", ""), mantissa, "e", exponent)
}

# Pairs of numeric records from different datasets whose values match (see
# `dataset_check_duplicate_values()`). Each pair is found once, from the less precise
# record: a record with `k` significant figures is compared with every record
# of the same taxon and trait with at least `k`, both rounded to `k`. A value
# exactly halfway at `k` matches whichever way it was rounded. `p_chance` is the share of the trait's values that could have been compared
# at that precision and have the matched value.
util_numeric_value_matches <- function(records, min_sig_figs, max_sig_figs = 6) {

  records <-
    records %>%
    dplyr::mutate(
      is_bin = .data$value_type %in% "bin",
      x = ifelse(.data$is_bin, NA_real_, suppressWarnings(as.numeric(.data$value))),
      sig = pmin(util_sig_figs(.data$value), max_sig_figs)
    )

  numbers <- records %>% dplyr::filter(!.data$is_bin, !is.na(.data$x))

  match_on_key <- function(A, B, match_precision) {
    B <- B %>%
      # A halfway value is in `B` twice, once per key
      dplyr::group_by(.data$trait_name) %>%
      dplyr::mutate(n_trait = dplyr::n_distinct(.data$.row)) %>%
      dplyr::ungroup() %>%
      dplyr::add_count(.data$trait_name, .data$key, name = "n_key") %>%
      dplyr::mutate(p_chance = .data$n_key / .data$n_trait)
    A %>%
      dplyr::select(dplyr::all_of(c(.row_1 = ".row", dataset_id_1 = "dataset_id", "taxon_name", "trait_name", "key"))) %>%
      dplyr::inner_join(
        B %>% dplyr::select(dplyr::all_of(c(.row_2 = ".row", dataset_id_2 = "dataset_id", "taxon_name", "trait_name", "key", "p_chance"))),
        by = c("taxon_name", "trait_name", "key"), relationship = "many-to-many"
      ) %>%
      dplyr::filter(.data$dataset_id_1 != .data$dataset_id_2) %>%
      # Both in one step: a second `mutate()` argument would see the new `.row_1`
      dplyr::transmute(
        row_a = pmin(.data$.row_1, .data$.row_2), row_b = pmax(.data$.row_1, .data$.row_2),
        match_precision = match_precision, p_chance = .data$p_chance
      ) %>%
      dplyr::rename(dplyr::all_of(c(.row_1 = "row_a", .row_2 = "row_b")))
  }

  bins <- records %>% dplyr::filter(.data$is_bin) %>% dplyr::mutate(key = .data$value)

  exact <- numbers %>% dplyr::mutate(key = sprintf("%.6g", .data$x))
  low_precision <- exact %>% dplyr::filter(is.na(.data$sig) | .data$sig < min_sig_figs)

  rounded <- purrr::map(seq(min_sig_figs, max_sig_figs), function(k) {
    B <- numbers %>% dplyr::filter(.data$x != 0, .data$sig >= k)
    B <- dplyr::bind_rows(
      B %>% dplyr::mutate(key = util_sig_fig_key(.data$x, k)),
      B %>% dplyr::mutate(key = util_sig_fig_key(.data$x, k, half_up = FALSE))
    ) %>%
      dplyr::distinct(.data$.row, .data$key, .keep_all = TRUE)
    match_on_key(B %>% dplyr::filter(.data$sig == k), B, k)
  })

  dplyr::bind_rows(
    match_on_key(bins, bins, NA_integer_),
    match_on_key(low_precision, exact, NA_integer_) %>%
      # Report the precision of the less precise value
      dplyr::mutate(match_precision = pmin(
        records$sig[match(.data$.row_1, records$.row)], records$sig[match(.data$.row_2, records$.row)]
      )),
    rounded
  ) %>%
    # Records with equal precision are found from both sides
    dplyr::distinct(.data$.row_1, .data$.row_2, .keep_all = TRUE)
}

# Pairs of georeferenced categorical records from different datasets with the
# same taxon, trait and value, within `max_distance_km` of each other. Records
# are binned into grid cells at least `max_distance_km` wide and compared with
# the 3 x 3 block of cells around them, so only nearby records are paired.
# Longitude cells are sized for 60 degrees of latitude, beyond which they
# would be too narrow.
util_categorical_value_matches <- function(records, max_distance_km) {

  cell_lat <- max_distance_km / 111
  cell_lon <- max_distance_km / (111 * cos(60 * pi / 180))

  records <- records %>%
    dplyr::mutate(
      cell_y = floor(.data$lat / cell_lat),
      cell_x = floor(.data$lon / cell_lon)
    )

  neighbours <-
    records %>%
    dplyr::select(dplyr::all_of(c(
      .row_2 = ".row", dataset_id_2 = "dataset_id", "taxon_name", "trait_name", "value",
      "cell_y", "cell_x", lat_2 = "lat", lon_2 = "lon"
    ))) %>%
    tidyr::crossing(dy = -1:1, dx = -1:1) %>%
    dplyr::mutate(cell_y = .data$cell_y + .data$dy, cell_x = .data$cell_x + .data$dx)

  records %>%
    dplyr::select(dplyr::all_of(c(
      .row_1 = ".row", dataset_id_1 = "dataset_id", "taxon_name", "trait_name", "value",
      "cell_y", "cell_x", lat_1 = "lat", lon_1 = "lon"
    ))) %>%
    dplyr::inner_join(
      neighbours, by = c("taxon_name", "trait_name", "value", "cell_y", "cell_x"), relationship = "many-to-many"
    ) %>%
    dplyr::filter(.data$dataset_id_1 < .data$dataset_id_2) %>%
    dplyr::filter(util_distance_km(.data$lat_1, .data$lon_1, .data$lat_2, .data$lon_2) <= max_distance_km) %>%
    dplyr::select(dplyr::all_of(c(".row_1", ".row_2")))
}

# Great-circle distance in km
util_distance_km <- function(lat_1, lon_1, lat_2, lon_2) {
  to_rad <- pi / 180
  h <- sin((lat_2 - lat_1) * to_rad / 2)^2 +
    cos(lat_1 * to_rad) * cos(lat_2 * to_rad) * sin((lon_2 - lon_1) * to_rad / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(h)))
}

# Adds each record's details to a table of matched pairs, keeps pairs involving
# `dataset_ids` (all pairs when `NULL`), and orients each pair so the dataset
# being checked -- or, failing that, the first alphabetically -- is dataset 1.
util_add_match_records <- function(matches, records, dataset_ids) {

  details <- records %>% dplyr::select(dplyr::all_of(c(
    ".row", "dataset_id", "taxon_name", "trait_name", "value", "observation_id", "lat", "lon"
  )))

  matches <-
    matches %>%
    dplyr::left_join(details %>% dplyr::rename_with(~ paste0(.x, "_1")), by = ".row_1") %>%
    dplyr::left_join(
      details %>% dplyr::select(-dplyr::all_of(c("taxon_name", "trait_name"))) %>% dplyr::rename_with(~ paste0(.x, "_2")),
      by = ".row_2"
    ) %>%
    dplyr::rename(dplyr::all_of(c(taxon_name = "taxon_name_1", trait_name = "trait_name_1")))

  checked_1 <- is.null(dataset_ids) | matches$dataset_id_1 %in% dataset_ids
  checked_2 <- is.null(dataset_ids) | matches$dataset_id_2 %in% dataset_ids

  swap <- !checked_1 & checked_2 | checked_1 == checked_2 & matches$dataset_id_1 > matches$dataset_id_2

  keep <- checked_1 | checked_2
  matches <- matches[keep, ]
  swap <- swap[keep]

  for (v in c(".row", "dataset_id", "value", "observation_id", "lat", "lon")) {
    v_1 <- matches[[paste0(v, "_1")]]
    matches[[paste0(v, "_1")]] <- dplyr::if_else(swap, matches[[paste0(v, "_2")]], v_1)
    matches[[paste0(v, "_2")]] <- dplyr::if_else(swap, v_1, matches[[paste0(v, "_2")]])
  }

  matches %>%
    dplyr::mutate(
      distance_km = util_distance_km(.data$lat_1, .data$lon_1, .data$lat_2, .data$lon_2),
      identical_value = suppressWarnings(as.numeric(.data$value_1) == as.numeric(.data$value_2)) %in% TRUE |
        .data$value_1 == .data$value_2
    ) %>%
    dplyr::add_count(
      .data$dataset_id_1, .data$dataset_id_2, .data$observation_id_1, .data$observation_id_2,
      name = "n_traits_observation_pair"
    ) %>%
    dplyr::arrange(.data$dataset_id_1, .data$dataset_id_2, .data$taxon_name, .data$trait_name)
}

# Two rows per match, one for each record, with the columns needed to judge it:
# the match's own measures, then the record's details, locations and contexts.
util_duplicate_records <- function(database, matches, match_cols) {

  matches <- matches %>% dplyr::mutate(match_id = dplyr::row_number())

  long <-
    dplyr::bind_rows(
      matches %>% dplyr::select(dplyr::all_of(c("match_id", .row = ".row_1", match_cols))),
      matches %>% dplyr::select(dplyr::all_of(c("match_id", .row = ".row_2", match_cols)))
    )

  database$traits <- database$traits %>% dplyr::filter(.data$.row %in% long$.row)

  details <-
    database %>%
    austraits::join_location_coordinates() %>%
    austraits::join_context_properties(format = "many_columns", include_description = FALSE) %>%
    purrr::pluck("traits") %>%
    dplyr::select(
      dplyr::all_of(c(
        ".row", "dataset_id", "taxon_name", "original_name", "trait_name", "value", "unit", "entity_type",
        "value_type", "basis_of_value", "replicates", "basis_of_record", "life_stage", "collection_date",
        "location_name", latitude = "latitude (deg)", longitude = "longitude (deg)", "source_id",
        "measurement_remarks", "observation_id"
      )),
      dplyr::contains("_context:")
    ) %>%
    # Contexts are added for every dataset in the database; keep those in use here
    dplyr::select(!(dplyr::contains("_context:") & dplyr::where(~ all(is.na(.x)))))

  long %>%
    dplyr::left_join(details, by = ".row") %>%
    dplyr::arrange(.data$match_id, .data$dataset_id != matches$dataset_id_1[.data$match_id]) %>%
    dplyr::select(-dplyr::all_of(".row"))
}
