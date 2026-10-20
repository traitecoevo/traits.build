# Fixtures are built here rather than read from `test_austraits.rds`, which
# `test-setup.R` writes only after this file has run (see test-duplicates.R).

# A minimal database: the checks only read `traits`, `definitions`, `taxa`,
# `locations` and `excluded_data`
check_fixture <- function(traits, excluded_data = NULL) {
  traits <- traits %>%
    dplyr::mutate(
      observation_id = sprintf("%03d", dplyr::row_number()),
      original_name = dplyr::coalesce(.data$original_name, .data$taxon_name),
      unit = "mg", entity_type = "individual", life_stage = "adult", basis_of_record = "field"
    )
  list(
    traits = traits,
    definitions = list(
      seed_dry_mass = list(type = "numeric", allowed_values_min = 0.00001, allowed_values_max = 1e6),
      leaf_delta13C = list(type = "numeric", allowed_values_min = -40, allowed_values_max = 0),
      plant_growth_form = list(
        type = "categorical", allowed_values_levels = list(herb = "Herb.", shrub = "Shrub.", tree = "Tree.")
      )
    ),
    taxa = tibble::tibble(
      taxon_name = unique(traits$taxon_name),
      genus = stringr::word(unique(traits$taxon_name), 1)
    ),
    locations = tibble::tibble(
      dataset_id = "A", location_id = c("01", "02"), location_name = c("Site one", "Site two"),
      location_property = "description", value = "A site."
    ),
    excluded_data = excluded_data
  )
}

trait_rows <- function(dataset_id, taxon_name, value, trait_name = "seed_dry_mass", value_type = "mean",
                       location_id = NA_character_, original_name = NA_character_) {
  tibble::tibble(
    dataset_id, taxon_name, trait_name, value = as.character(value), value_type, location_id, original_name
  )
}

# Seed mass for "Genus one" near 1 mg in datasets A, B and C, and 1000 times
# smaller -- a g/mg slip -- in 30 individual values from dataset D
outlier_fixture <- function() {
  check_fixture(dplyr::bind_rows(
    trait_rows("A", "Genus one", 1.0),
    trait_rows("B", "Genus one", 1.2),
    trait_rows("C", "Genus one", 0.9),
    trait_rows("D", "Genus one", 0.001 * (1 + seq_len(30) / 100), value_type = "raw"),
    # Measured by A and D only
    trait_rows("A", "Genus two", 2),
    trait_rows("D", "Genus two", 0.002),
    # A maximum, and zero, which have nothing like them to compare with
    trait_rows("E", "Genus one", 50, value_type = "maximum"),
    trait_rows("E", "Genus one", 0),
    # A trait that can be negative
    trait_rows(c("A", "B", "C", "D"), "Genus one", c(-28, -27, -29, -2.8), trait_name = "leaf_delta13C")
  ))
}


test_that("`dataset_check_outlier_by_species` flags values far from other datasets", {
  database <- outlier_fixture()

  out <- dataset_check_outlier_by_species(database, "D")
  expect_named(out, c("summary", "records"))

  # All 30 of D's values for "Genus one" are about 1000-fold below the median of
  # A, B and C. "Genus two" has one other dataset, fewer than the default 2
  expect_equal(nrow(out$records), 30)
  expect_true(all(out$records$taxon_name == "Genus one"))
  expect_equal(unique(out$records$other_median), 1.0)
  expect_equal(unique(out$records$n_other_datasets), 3L)
  expect_true(all(out$records$ratio < 0.0014 & out$records$fold_difference > 700))

  expect_equal(out$summary$n_taxa, 1L)
  expect_equal(out$summary$n_values, 30L)
  expect_equal(out$summary$n_flagged, 30L)
  expect_lt(out$summary$median_ratio, 0.0014)

  # With one other dataset enough, "Genus two" is compared too
  out_1 <- dataset_check_outlier_by_species(database, "D", min_other_datasets = 1)
  expect_setequal(unique(out_1$records$taxon_name), c("Genus one", "Genus two"))
})

test_that("`dataset_check_outlier_by_species` gives each other dataset one vote", {
  database <- outlier_fixture()

  # D's 30 values count once, so A is compared with the median of B, C and D,
  # 0.9, not with D's values, which would outnumber the rest
  out <- dataset_check_outlier_by_species(database, "A")
  expect_equal(nrow(out$records), 0)
  expect_equal(out$summary$median_ratio, 1 / 0.9)

  # Checking every dataset finds D's values and nothing else
  out_all <- dataset_check_outlier_by_species(database)
  expect_equal(unique(out_all$records$dataset_id), "D")
  expect_setequal(out_all$summary$dataset_id, c("A", "B", "C", "D"))
})

test_that("`dataset_check_outlier_by_species` compares only like value types and ratio-scale traits", {
  database <- outlier_fixture()

  # E's maximum has no other maxima to compare with, and its zero has no ratio
  expect_equal(nrow(dataset_check_outlier_by_species(database, "E", min_other_datasets = 1)$summary), 0)

  # Pooling all value types compares the maximum with the means
  pooled <- dataset_check_outlier_by_species(
    database, "E", value_types = list(all = c("mean", "raw", "maximum"))
  )
  expect_equal(pooled$records$value, "50")
  expect_equal(pooled$summary$value_type_group, "all")

  # delta13C can be negative, so a fold-difference means nothing
  expect_false("leaf_delta13C" %in% dataset_check_outlier_by_species(database)$summary$trait_name)
  expect_error(dataset_check_outlier_by_species(database, trait_names = "leaf_delta13C"), "cannot be negative")

  expect_error(dataset_check_outlier_by_species(database, "Not_a_dataset"), "not in the database")
  expect_error(dataset_check_outlier_by_species(database, min_fold = 1), "greater than 1")
})

test_that("`dataset_check_outlier_by_genus` compares with the rest of the genus", {
  database <- outlier_fixture()
  database$traits <- dplyr::bind_rows(
    database$traits,
    # A third species of the genus, measured only by F, at 1000 times the rest
    database$traits[1, ] %>% dplyr::mutate(dataset_id = "F", taxon_name = "Genus three", value = "1500")
  )
  database$taxa <- dplyr::bind_rows(database$taxa, tibble::tibble(taxon_name = "Genus three", genus = "Genus"))

  # Nothing to compare at species level...
  expect_equal(nrow(dataset_check_outlier_by_species(database, "F")$summary), 0)

  # ...but against the genus
  out <- dataset_check_outlier_by_genus(database, "F")
  expect_equal(out$records$taxon_name, "Genus three")
  expect_equal(out$records$genus, "Genus")
  # Other taxa in the genus, one vote per taxon per dataset: A's two, B's
  # and C's one, and D's two
  expect_equal(out$records$n_other_taxa, 2L)
  expect_equal(out$records$n_other_datasets, 4L)
})

test_that("`dataset_check_outlier_by_species` finds a unit slip in a built dataset", {
  database <- suppressMessages(dataset_build(
    "data/Test_2022/test-metadata.yml", "data/Test_2022/data.csv",
    get_schema("config/traits.yml", "traits"), get_unit_conversions("config/unit_conversions.csv"),
    get_schema(), get_schema("config/metadata.yml", "metadata"), read_csv_char("config/taxon_list.csv")
  ))

  # Two more datasets with the same values, one with leaf area entered in the
  # wrong unit, 1000 times too large
  copy <- database$traits %>% dplyr::mutate(dataset_id = "Copy_2024")
  slip <- database$traits %>%
    dplyr::mutate(
      dataset_id = "Slip_2024",
      value = ifelse(.data$trait_name == "leaf_area", as.character(as.numeric(.data$value) * 1000), .data$value)
    )
  database$traits <- dplyr::bind_rows(database$traits, copy, slip)

  out <- dataset_check_outlier_by_species(database, "Slip_2024")

  expect_equal(unique(out$records$trait_name), "leaf_area")
  slipped <- database$traits$dataset_id == "Slip_2024" & database$traits$trait_name == "leaf_area"
  expect_equal(nrow(out$records), sum(slipped))
  expect_equal(out$records$ratio, rep(1000, nrow(out$records)))
  # Every trait was compared, and only leaf area is shifted
  expect_setequal(out$summary$trait_name, unique(database$traits$trait_name))
  expect_equal(out$summary$trait_name[1], "leaf_area")
  expect_equal(out$summary$median_ratio[-1], rep(1, nrow(out$summary) - 1))
})


test_that("`dataset_check_duplicates_within_dataset` finds values repeated for a taxon", {
  pgf <- "plant_growth_form"
  database <- check_fixture(dplyr::bind_rows(
    # Copied down to three individuals at one site
    trait_rows("A", "Sp one", "shrub", pgf, "mode", "01"),
    trait_rows("A", "Sp one", "shrub", pgf, "mode", "01"),
    trait_rows("A", "Sp one", "shrub", pgf, "mode", "01"),
    # Varies
    trait_rows("A", "Sp two", c("shrub", "tree"), pgf, "mode", "01"),
    # Once at each of two sites
    trait_rows("A", "Sp three", "herb", pgf, "mode", c("01", "02")),
    # Two names the taxonomy merged
    trait_rows("A", "Sp four", "tree", pgf, "mode", original_name = c("Sp four", "Sp quattuor")),
    # Numeric, repeated
    trait_rows("A", "Sp one", c(2, 2), location_id = "01"),
    # Another dataset, repeated
    trait_rows("B", "Sp one", c("herb", "herb"), pgf, "mode")
  ))

  out <- dataset_check_duplicates_within_dataset(database, "A")
  expect_named(out, c("summary", "taxa"))
  expect_equal(out$taxa$taxon_name, "Sp one")
  expect_equal(out$taxa$location_name, "Site one")
  expect_equal(out$taxa$value, "shrub")
  expect_equal(out$taxa$n_records, 3L)
  expect_equal(out$summary$trait_name, pgf)
  expect_equal(out$summary$n_taxa_several, 2L)
  expect_equal(out$summary$n_taxa_repeated, 1L)

  numeric <- dataset_check_duplicates_within_dataset(database, "A", trait_types = c("categorical", "numeric"))
  expect_setequal(numeric$summary$trait_name, c(pgf, "seed_dry_mass"))

  everything <- dataset_check_duplicates_within_dataset(database)
  expect_equal(everything$summary$dataset_id, c("A", "B"))
  expect_true(is.na(everything$taxa$location_name[everything$taxa$dataset_id == "B"]))

  expect_error(dataset_check_duplicates_within_dataset(database, "Not_a_dataset"), "not in the database")
})


test_that("`dataset_check_categorical_substitutions` lists unsupported values as substitutions", {
  database <- check_fixture(
    trait_rows("A", "Sp one", "shrub", "plant_growth_form", "mode"),
    excluded_data = tibble::tibble(
      dataset_id = c("A", "A", "A", "A", "B"),
      trait_name = c(rep("plant_growth_form", 3), "seed_dry_mass", "plant_growth_form"),
      value = c("shrub small_tree", "shrub small_tree", "bushy", "-1", "vine"),
      error = c(rep("Unsupported trait value", 3), "Value out of allowable range", "Unsupported trait value")
    )
  )

  out <- dataset_check_categorical_substitutions(database, "A")
  expect_named(out, c("dataset_id", "trait_name", "find", "replace", "n_records", "unsupported"))
  expect_equal(out$find, c("bushy", "shrub small_tree"))
  expect_true(all(is.na(out$replace)))
  expect_equal(out$n_records, c(1L, 2L))
  # Only the word that is not allowed
  expect_equal(out$unsupported, c("bushy", "small_tree"))

  expect_equal(nrow(dataset_check_categorical_substitutions(database)), 3)
  expect_error(dataset_check_categorical_substitutions(database, "Not_a_dataset"), "not in the database")
})


test_that("`util_group_stat` agrees with dplyr", {
  set.seed(1)
  df <- tibble::tibble(
    g = sample(c("a", "b", "c", "d"), 200, replace = TRUE),
    x = stats::rnorm(200),
    w = sample(1:5, 200, replace = TRUE),
    y = sample(c("p", "q", NA), 200, replace = TRUE)
  )
  df <- dplyr::bind_rows(df, tibble::tibble(g = "e", x = 1, w = 1L, y = NA))
  ids <- util_group_ids(df, "g")
  n_groups <- nrow(ids$keys)

  expected <- df %>%
    dplyr::group_by(.data$g) %>%
    dplyr::summarise(
      n = dplyr::n(), sum = sum(.data$w), min = min(.data$x), max = max(.data$x),
      median = stats::median(.data$x), n_distinct = dplyr::n_distinct(.data$y)
    )

  expect_equal(ids$keys$g, expected$g)
  expect_equal(util_group_stat(df$x, ids$id, n_groups, "n"), expected$n)
  expect_equal(util_group_stat(df$w, ids$id, n_groups, "sum"), expected$sum)
  expect_equal(util_group_stat(df$x, ids$id, n_groups, "min"), expected$min)
  expect_equal(util_group_stat(df$x, ids$id, n_groups, "max"), expected$max)
  expect_equal(util_group_stat(df$x, ids$id, n_groups, "median"), expected$median)
  expect_equal(util_group_stat(df$y, ids$id, n_groups, "n_distinct"), expected$n_distinct)
})
