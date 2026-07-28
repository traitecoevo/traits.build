
test_data <- "data/Test_2022/data.csv"

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
traits_definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
test_config <- dataset_configure("data/Test_2022/test-metadata.yml",
                                  traits_definitions)


test_that("`dataset_configure` is working", {
  expect_no_error(
    test_config <- dataset_configure("data/Test_2022/test-metadata.yml",
                                      traits_definitions))
  expect_type(test_config, "list")
  expect_length(test_config, 3)
  expect_named(test_config,
               c("dataset_id", "metadata", "definitions"))
})


test_that("`dataset_process` is working", {
  expect_no_error(austraits_names <- schema$austraits$elements %>% names())
  expect_no_error(x <- dataset_process(test_data, test_config, schema, resource_metadata, unit_conversions))
  expect_type(x, "list")
  expect_equal(class(x), c("list", "traits.build"))
  expect_length(x, 14)
  expect_named(x, austraits_names)
  expect_equal(nrow(x$excluded_data), 0)
  # Test to see if `filter_missing_values` argument works
  expect_equal(
    nrow(
      dataset_process(test_data, test_config, schema, resource_metadata, unit_conversions,
                      filter_missing_values = TRUE)$excluded_data
    ),
  0)
  expect_equal(
    nrow(
      dataset_process(test_data, test_config, schema, resource_metadata, unit_conversions,
      filter_missing_values = FALSE)$excluded_data
    ),
  44)
})


test_that("`process_custom_code` is working", {
  expect_no_error(metadata <- test_config$metadata)
  expect_no_error(data <- readr::read_csv(test_data, col_types = cols(), guess_max = 100000, progress = FALSE))
  expect_equal(ncol(data), 13)
  expect_equal(ncol(process_custom_code(metadata[["dataset"]][["custom_R_code"]])(data)), 16)
  expect_silent(process_custom_code(NA))
})


test_that("`process_format_identifiers` is working", {
  # The third argument is the schema, not the trait data, and `my_list` is the
  # identifiers list itself rather than something wrapping it. Getting either
  # wrong made every call fail with "object 'schema' not found", or silently
  # return no rows.
  identifiers <- read_metadata("examples/Test_2023_1/metadata.yml")$identifiers
  expected_cols <-
    names(schema[["austraits"]][["elements"]][["identifiers"]][["elements"]])

  out <- process_format_identifiers(identifiers, "Test_2023_1", schema)

  expect_named(out, expected_cols)
  expect_equal(nrow(out), length(identifiers))
  expect_equal(out$dataset_id, rep("Test_2023_1", length(identifiers)))
  expect_equal(out$identifier_type, purrr::map_chr(identifiers, "identifier_type"))

  # A dataset that declares no identifiers still gets the full set of columns
  empty <- process_format_identifiers(list(), "Test_2023_1", schema)
  expect_named(empty, expected_cols)
  expect_equal(nrow(empty), 0)
})


test_that("a `var_in` naming a column that does not exist is rejected", {
  # This used to pass silently: `process_add_all_columns()` creates the missing
  # column as all-NA and the `!is.na(identifier_value)` filter then drops every
  # row, so the dataset lost its identifiers with no error and an empty table
  # (#232).
  metadata_path <- file.path(withr::local_tempdir(), "metadata.yml")
  metadata <- readLines("examples/Test_2023_1/metadata.yml")
  metadata[grepl("^- var_in: herbarium_voucher$", metadata)] <-
    "- var_in: herbarium_vouchers"
  writeLines(metadata, metadata_path)

  expect_error(
    dataset_process(
      "examples/Test_2023_1/data.csv",
      dataset_configure(metadata_path, traits_definitions),
      schema, resource_metadata, unit_conversions
    ),
    "herbarium_vouchers"
  )
})


test_that("a `var_in` created by `custom_R_code` is still accepted", {
  # The check has to run against the data as it stands after `custom_R_code`,
  # not against the raw csv header. Every dataset in the three downstream
  # repositories that uses identifiers creates its `var_in` column this way, so
  # validating the raw header would reject all of them (#232).
  metadata_path <- file.path(withr::local_tempdir(), "metadata.yml")
  metadata <- readLines("examples/Test_2023_1/metadata.yml")

  # Point `var_in` at a column absent from data.csv, then add it to the
  # existing `custom_R_code` mutate so it exists by the time identifiers load
  metadata[grepl("^- var_in: herbarium_voucher$", metadata)] <-
    "- var_in: voucher_made_by_custom_code"
  metadata[grepl("^\\s+LASA1000_dupe = LASA1000$", metadata)] <-
    "        LASA1000_dupe = LASA1000,\n        voucher_made_by_custom_code = herbarium_voucher"
  writeLines(metadata, metadata_path)

  # Guard the premise: the column really is absent from the raw csv
  expect_false(
    "voucher_made_by_custom_code" %in% names(read_csv_char("examples/Test_2023_1/data.csv"))
  )

  expect_no_error(
    built <- dataset_process(
      "examples/Test_2023_1/data.csv",
      dataset_configure(metadata_path, traits_definitions),
      schema, resource_metadata, unit_conversions
    )
  )
  expect_gt(nrow(built$identifiers), 0)
})


test_that("`write_plaintext` exports every table in the database", {
  # The table list used to be hardcoded and had `identifiers` missing from it
  # for the whole of 2.1.0, so exports silently dropped the release's headline
  # table. Nothing caught that, because nothing tested this function at all.
  #
  # Test_2023_1 is used because it is the example with a populated identifiers
  # table, so the round-trip below actually carries rows.
  built <-
    dataset_process(
      "examples/Test_2023_1/data.csv",
      dataset_configure("examples/Test_2023_1/metadata.yml",
                        traits_definitions),
      schema, resource_metadata, unit_conversions
    ) %>%
    # Read the committed fixture rather than the `config/taxon_list.csv` build
    # artefact, so this assertion is pinned to a known taxon list
    dataset_update_taxonomy(read_csv_char("config/taxon_list-orig.csv"))

  path <- withr::local_tempdir()
  suppressMessages(write_plaintext(built, path))

  tables <- names(built)[purrr::map_lgl(built, is.data.frame)]
  expect_true("identifiers" %in% tables)
  expect_gt(nrow(built$identifiers), 0)

  expect_true(all(paste0(tables, ".csv") %in% list.files(path)))

  # Round-trip, not just presence: an exported table has to read back as what
  # the database held
  for (v in tables) {
    expect_equal(
      read_csv_char(file.path(path, paste0(v, ".csv"))),
      built[[v]],
      info = v
    )
  }

  # The non-tabular artefacts users also receive
  expect_true(all(
    c("definitions.yml", "metadata.yml", "schema.yml", "sources.bib",
      "build_info.md") %in% list.files(path)
  ))
})


test_that("the pipeline published in Wenk et al. 2024 Fig. 1 runs as written", {
  # Three of the seven functions the paper's figure names were not usable:
  # `data_update_taxonomy()` did not exist, `database_create_combined_table`
  # was never exported, and `build_combine()`'s shim called the wrong function.
  # A reader following the published figure could not run the documented
  # workflow. This walks it exactly as drawn, so that stays true.
  taxon_list <- read_csv_char("config/taxon_list-orig.csv")

  build_config <- dataset_configure("examples/Test_2023_1/metadata.yml",
                                    traits_definitions)

  built <-
    dataset_process("examples/Test_2023_1/data.csv", build_config,
                    schema, resource_metadata, unit_conversions) %>%
    data_update_taxonomy(taxon_list)

  # The published name and the current one are the same function
  expect_equal(
    built,
    dataset_process("examples/Test_2023_1/data.csv", build_config,
                    schema, resource_metadata, unit_conversions) %>%
      dataset_update_taxonomy(taxon_list)
  )

  database <- austraits::bind_databases(databases = list(Test_2023_1 = built))

  combined <- database_create_combined_table(database)
  expect_s3_class(combined, "data.frame")
  expect_equal(nrow(combined), nrow(built$traits))
  # Wider than the traits table, since it is the relational tables joined in
  expect_gt(ncol(combined), ncol(built$traits))
})


# The below functions are not working
#test_that("process_flag_unsupported_traits is working", {
#  process_flag_unsupported_traits(data, definitions)
#})

#test_that("process_flag_excluded_observations is working", {
#  process_flag_excluded_observations
#})
