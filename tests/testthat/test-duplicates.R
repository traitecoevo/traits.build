test_that("`util_sig_figs` counts significant figures as written", {
  expect_equal(
    util_sig_figs(c("1.23", "0.050", "12.30", "250", "2", "-4.5", "1.00e+05", "0", "0.0")),
    c(3, 2, 4, 2, 1, 2, 3, NA, NA)
  )
})

test_that("`util_sig_fig_key` rounds half-up to significant figures", {
  expect_equal(util_sig_fig_key(c(1.23, 1.2, 1.25, 1.26, 1250, 0.0123), 2), c("12e0", "12e0", "13e0", "13e0", "13e3", "12e-2"))
  expect_equal(util_sig_fig_key(c(1.25, 145, 1.26), 2, half_up = FALSE), c("12e0", "14e2", "13e0"))
  # Rounding up that adds a digit
  expect_equal(util_sig_fig_key(9.96, 2), util_sig_fig_key(10, 2))
  expect_equal(util_sig_fig_key(-1.23, 2), "-12e0")
})

test_that("`util_numeric_value_matches` compares values at the coarser precision", {
  records <- tibble::tibble(
    .row = 1:9,
    dataset_id = c("A", "B", "B", "B", "A", "B", "A", "B", "B"),
    taxon_name = c(rep("Sp one", 7), "Sp two", "Sp one"),
    trait_name = "leaf_length",
    value = c("1.23", "1.2", "1.26", "2.0", "2", "2.4", "10--50", "1.2", "10--50"),
    value_type = c(rep("mean", 6), "bin", "mean", "bin")
  )
  pairs <- util_numeric_value_matches(records, min_sig_figs = 2) %>%
    dplyr::arrange(.data$.row_1, .data$.row_2)

  # 1.23 ~ 1.2; 2 == 2.0 exactly; bins identical. Not: 1.23 vs 1.26, 2 vs 2.4
  # (too few significant figures to round), 1.2 for another taxon, 1.2 vs 1.26
  # within dataset B.
  expect_equal(pairs$.row_1, c(1L, 4L, 7L))
  expect_equal(pairs$.row_2, c(2L, 5L, 9L))
  expect_equal(pairs$match_precision, c(2L, 1L, NA))
})

test_that("`util_categorical_value_matches` pairs identical values within the distance", {
  records <- tibble::tibble(
    .row = 1:4,
    dataset_id = c("A", "B", "B", "B"),
    taxon_name = "Sp one",
    trait_name = "plant_growth_form",
    value = c("shrub", "shrub", "shrub", "tree"),
    # ~5 km and ~50 km north of record 1, and one with a different value
    lat = c(-33, -33 + 5 / 111, -33 + 50 / 111, -33),
    lon = 151
  )
  expect_equal(util_categorical_value_matches(records, 10), tibble::tibble(.row_1 = 1L, .row_2 = 2L))
  expect_equal(nrow(util_categorical_value_matches(records, 100)), 2)
})

test_that("`dataset_check_duplicate_values` finds a dataset copied with rounded values", {
  # Built here rather than read from `test_austraits.rds`, which `test-setup.R`
  # writes only after this file has run, so reading it passed only where an
  # earlier run had left a copy behind -- never on a fresh checkout or on CI.
  # For the same reason it reads the tracked `test-metadata.yml`, not the
  # gitignored `metadata.yml` that `test-setup.R` copies from it
  database <- suppressMessages(dataset_build(
    "data/Test_2022/test-metadata.yml", "data/Test_2022/data.csv",
    get_schema("config/traits.yml", "traits"), get_unit_conversions("config/unit_conversions.csv"),
    get_schema(), get_schema("config/metadata.yml", "metadata"), read_csv_char("config/taxon_list.csv")
  ))

  copy <- database$traits %>%
    dplyr::mutate(
      dataset_id = "Copy_2024",
      # Values with decimals rounded to 2 significant figures by `signif()`,
      # which usually rounds a halfway value down, written as a contributor
      # would: "0.50" rather than "0.5", and "580" rather than "580."
      x = as.numeric(.data$value),
      value = dplyr::case_when(
        !stringr::str_detect(.data$value, "\\.") ~ .data$value,
        abs(.data$x) >= 100 ~ formatC(signif(.data$x, 2), format = "f", digits = 0),
        TRUE ~ formatC(signif(.data$x, 2), digits = 2, format = "fg", flag = "#")
      )
    )
  database$traits <- dplyr::bind_rows(database$traits, copy %>% dplyr::select(-dplyr::all_of("x")))
  database$locations <- dplyr::bind_rows(
    database$locations, database$locations %>% dplyr::mutate(dataset_id = "Copy_2024")
  )

  out <- dataset_check_duplicate_values(database, "Copy_2024")

  expect_named(out, c("numeric_pairs", "numeric_records", "categorical_pairs", "categorical_records"))
  expect_equal(nrow(out$numeric_pairs), 1)
  expect_equal(out$numeric_pairs$dataset_id_1, "Copy_2024")
  expect_equal(out$numeric_pairs$dataset_id_2, "Test_2022")
  expect_equal(out$numeric_pairs$prop_matched_combos, 1)
  # Every copied record is paired with its original
  expect_equal(out$numeric_pairs$prop_records_1, 1)

  # Two rows per match, the checked dataset first
  expect_equal(nrow(out$numeric_records), 2 * out$numeric_pairs$n_record_pairs)
  expect_equal(out$numeric_records$dataset_id[1:2], c("Copy_2024", "Test_2022"))

  # Checking the original instead orients the same pair the other way round
  expect_equal(dataset_check_duplicate_values(database, "Test_2022")$numeric_pairs$dataset_id_1, "Test_2022")

  expect_error(dataset_check_duplicate_values(database, "Not_a_dataset"), "not in the database")
})

test_that("`util_numeric_value_matches` matches a halfway value rounded either way", {
  records <- tibble::tibble(
    .row = 1:3,
    dataset_id = c("A", "B", "C"),
    taxon_name = "Sp one",
    trait_name = "leaf_length",
    value = c("145", "140", "150"),
    value_type = "mean"
  )
  # "140" and "150" have 2 significant figures, so both are compared with 145 at 2
  pairs <- util_numeric_value_matches(records, min_sig_figs = 2) %>% dplyr::arrange(.data$.row_2)
  expect_equal(pairs$.row_1, c(1L, 1L))
  expect_equal(pairs$.row_2, c(2L, 3L))
})
