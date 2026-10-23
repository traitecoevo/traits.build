## `dataset_test` is the validation layer, and the most-taught function in the
## book. Before Stage 0 of #225 nothing verified that a failing check actually
## fails: `R/test_functions.R` had 2 of its 30 helpers referenced by any test,
## and the one near-miss asserted `expect_output(dataset_test(...))`, which
## passes on any output at all.


# `dataset_test` reads `config/taxon_list.csv`, which is not in the repository.
# `helper.R` seeds it before any test file runs, so this file no longer needs to
# guard for itself.


# The messages `dataset_test` reports, with everything environment-dependent
# stripped: the running progress counter, the dashed rules (whose width follows
# the console), and the backtraces (whose line numbers move whenever anything
# in `R/` is edited, which would make these snapshots churn on unrelated
# changes).
dataset_test_failures <- function(dataset_id, path_data = "examples") {

  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )

  out <- capture.output(
    suppressMessages(
      dataset_test(dataset_ids = dataset_id, path_data = path_data)
    )
  )

  drop <-
    grepl("^\\[ FAIL", out) |
    grepl("^-- (Failure|Error)", out) |
    grepl("^Backtrace:", out) |
    grepl("^\\s*[0-9]+\\. ", out) |
    grepl("^\\s*x\\s*$", out) |
    !nzchar(trimws(out))

  out[!drop] %>%
    # `colour_characters()` emits raw ANSI escapes rather than going through
    # crayon, so disabling colour above does not reach them
    gsub("\033\\[[0-9]+m", "", .) %>%
    trimws()
}


test_that("`dataset_test` handles several identifiers", {
  # A dataset may declare several identifiers, so `metadata$identifiers` is a
  # list and `is.na()` on it returns one value per element. Guarding with
  # `if (!is.na(metadata$identifiers))` therefore aborted the whole run with
  # "the condition has length > 1" for any such dataset.
  metadata <- read_metadata("examples/Test_2023_1/metadata.yml")
  expect_gt(length(metadata$identifiers), 1)

  out <- capture.output(
    suppressMessages(
      dataset_test(dataset_ids = "Test_2023_1", path_data = "examples")
    )
  )

  expect_false(any(grepl("the condition has length > 1", out, fixed = TRUE)))
})


test_that("`dataset_test` reports the error Test_2023_1 was given", {
  # Test_2023_1's metadata deliberately names a trait that does not exist. The
  # point of asserting the message rather than just "some output happened" is
  # that a validator which stopped checking trait names would still produce
  # output, and would still have passed the old test.
  failures <- dataset_test_failures("Test_2023_1")

  expect_match(
    failures,
    "`trait_name`'s should not contain: 'wrong_trait_name'",
    all = FALSE
  )
})


test_that("`dataset_test` reports the errors Test_2023_9 was built to provoke", {
  # Test_2023_9 was added and commented out in the same commit (9e4ceb9) as a
  # fixture for testing `dataset_test`, and left parked for three years. Its
  # README lists the faults it carries deliberately: an NA `trait_name`,
  # duplicate and NA context values, a numeric column coerced to character.
  #
  # Snapshotting the whole report, rather than asserting a handful of strings,
  # is what gives the validation layer real coverage: any check that stops
  # firing, starts firing, or changes its wording shows up here. Review the
  # diff on failure -- `snapshot_accept()` is only correct once you know which
  # check changed and why.
  expect_snapshot(writeLines(dataset_test_failures("Test_2023_9")))
})


test_that("`dataset_test` rejects invalid `match: word` substitutions (#271)", {
  # See Test_2023_9/README.md for what these three added substitutions pin:
  # an invalid `match` value, and two `match: word` rules that chain into each
  # other. Both are covered by the snapshot above too, but pinned individually
  # here since they're new checks, not a pre-existing one being extended.
  failures <- dataset_test_failures("Test_2023_9")

  expect_match(
    failures, "`match`'s should not contain: 'sometimes'",
    all = FALSE
  )
  expect_match(
    failures, "`plant_growth_form` has chaining `match: word` rules",
    all = FALSE
  )
})


# Run `dataset_test` from a directory with no `config/` folder, so any config
# file it reads from a hardcoded `config/` rather than `path_config` fails.
dataset_test_elsewhere <- function(dataset_id, path_config) {
  # Both paths must resolve before leaving the tests directory
  path_config <- normalizePath(path_config)
  path_data <- normalizePath("examples")

  withr::local_dir(withr::local_tempdir())
  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )

  capture.output(
    suppressMessages(
      dataset_test(dataset_id, path_config = path_config, path_data = path_data)
    )
  )
}


test_that("`dataset_test` reads every config file from `path_config` (#129)", {
  # The build step read `config/traits.yml`, `config/metadata.yml`,
  # `config/unit_conversions.csv` and `config/taxon_list.csv` regardless of
  # `path_config`, so a run from anywhere else failed "building dataset".
  # Test_2023_5 passes every check, so any failure here is the config.
  out <- dataset_test_elsewhere("Test_2023_5", "config")

  expect_false(any(grepl("building dataset", out, fixed = TRUE)))
  # The last progress line is the final tally
  expect_match(tail(grep("^\\[ FAIL", out, value = TRUE), 1), "[ FAIL 0 |", fixed = TRUE)
})


test_that("a missing config file fails each dataset rather than the run (#129)", {
  # The config files are read once per run, not once per dataset. Reading one
  # that is missing must still be a failure reported against the dataset, as it
  # was when each dataset read the files itself, not an error that aborts the
  # run before any dataset is checked.
  config <- withr::local_tempdir()
  file.copy(list.files("config", full.names = TRUE), config)
  unlink(file.path(config, "taxon_list.csv"))

  out <- dataset_test_elsewhere(c("Test_2023_2", "Test_2023_3"), config)

  expect_equal(sum(grepl("building dataset", out, fixed = TRUE)), 2)
})


# Test_2023_2, with `edit` applied to its metadata, in a fresh data folder
dataset_test_edited <- function(edit) {
  path_data <- withr::local_tempdir()
  dir.create(file.path(path_data, "Test_2023_2"))
  file.copy(
    list.files(file.path("examples", "Test_2023_2"), full.names = TRUE),
    file.path(path_data, "Test_2023_2"), recursive = TRUE
  )
  path_metadata <- file.path(path_data, "Test_2023_2", "metadata.yml")
  write_metadata(edit(read_metadata(path_metadata)), path_metadata)

  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )
  out <- capture.output(
    suppressMessages(
      dataset_test("Test_2023_2", path_data = path_data)
    )
  )
  gsub("\033\\[[0-9]+m", "", out)
}


test_that("`dataset_test` reports each location lacking coordinates, and only those (#129)", {
  # Locations are tested only once found to be missing coordinates, rather
  # than one expectation per location, so check that selection is right
  out <- dataset_test_edited(function(metadata) {
    # No coordinates at all: reported
    metadata$locations$Atherton[c("latitude (deg)", "longitude (deg)")] <- NULL
    # Ranges in place of a point: allowed
    cape <- metadata$locations$`Cape Tribulation`
    cape[c("latitude (deg)", "longitude (deg)")] <- NULL
    cape[["latitude range (deg)"]] <- "-16.2--16.0"
    cape[["longitude range (deg)"]] <- "145.4--145.5"
    metadata$locations$`Cape Tribulation` <- cape
    metadata
  })

  expect_equal(sum(grepl("location '", out, fixed = TRUE)), 1)
  expect_match(
    out, "location 'Atherton'.*does not contain: 'latitude \\(deg\\)', 'longitude \\(deg\\)'",
    all = FALSE
  )
})


test_that("`dataset_test` reports an inline location that is not a list (#129)", {
  out <- dataset_test_edited(function(metadata) {
    metadata$locations$Atherton <- "not a list"
    metadata
  })

  expect_equal(sum(grepl("is not a list", out, fixed = TRUE)), 1)
  expect_match(out, "location Atherton - is not a list", fixed = TRUE, all = FALSE)
})


test_that("`dataset_test` runs on the package's testthat edition (#234)", {
  # It used to switch to edition 2 for `context()`, which edition 3
  # deprecates, and relied on `context("end")` to make the reporter print the
  # failures at all; the tests above cover that they are still printed.
  # Running outside a test file, it now opens a context of its own, so a
  # reporter that collects results rather than printing them works too.
  # `ListReporter` used to abort with "attempt to use zero-length variable name".
  reporter <- testthat::ListReporter$new()
  testthat::with_reporter(
    reporter,
    dataset_test_worker("Test_2023_1", path_data = "examples")
  )
  results <- as.data.frame(reporter$get_results())

  # Test_2023_1's 3 failures include blank `collection_date` values, which the
  # NA-date check reports (its ambiguous `1/2/2004` is read through the
  # declared `collection_date_format: dmy`, so is not one of them)
  expect_equal(results$test, "Test_2023_1")
  expect_equal(results$failed, 3)
})


test_that("`dataset_test` rejects a `collection_date` that doesn't parse", {
  # See Test_2023_9/README.md: the `huber_value` trait's `collection_date` was
  # changed to `2002-02-30`, a calendar date that doesn't exist. Also covered
  # by the snapshot above, pinned individually since it is written as a
  # standard date and only the calendar rules it out.
  failures <- dataset_test_failures("Test_2023_9")

  expect_match(
    failures, "not written as `yyyy-mm-dd`, `yyyy-mm`, `yyyy` or a `/` range of these: '2002-02-30'",
    fixed = TRUE, all = FALSE
  )
})


test_that("`dataset_test` rejects missing `collection_date` values", {
  # Test_2023_7 reads `collection_date` from its `Date` column. Blank a few of
  # those cells in a temporary copy, so the shared example (and its `output/`)
  # stay untouched. An unknown date should be recorded explicitly (e.g.
  # `.na/2009`), not left as NA.
  path_data <- withr::local_tempdir()
  file.copy("examples/Test_2023_7", path_data, recursive = TRUE)

  data_file <- file.path(path_data, "Test_2023_7", "data.csv")
  data <- readr::read_csv(data_file, col_types = readr::cols(.default = "c"))
  data$Date[1:3] <- NA
  readr::write_csv(data, data_file, na = "")

  failures <- dataset_test_failures("Test_2023_7", path_data = path_data)

  expect_match(
    failures, "Some date values are NA \\([0-9]+ of [0-9]+ records\\)",
    all = FALSE
  )
  # NA is not a non-standard date, so it isn't reported twice
  expect_false(any(grepl("not written as `yyyy-mm-dd`", failures, fixed = TRUE)))
})


test_that("`dataset_test` accepts a context scoped to specific traits by a column (#276)", {
  # Test_2024_1's `wood_density` trait entry sets `leaf_temp: LfT_Rdark`, naming a
  # column of `data.csv` rather than a literal -- scoping the `leaf temperature`
  # context (declared once, at the dataset level) to just that trait. Its values
  # are never listed in `metadata.yml`, so they can only be read off the column.
  #
  # The context-consistency check used to compare the column's per-record values
  # against the literal string "LfT_Rdark" (the field's own declared value, never
  # resolved against the data), which could never match and always failed (#276).
  # One record leaves `LfT_Rdark` blank, as a column scoped to some traits
  # usually will: a blank is no value for the context, not a value of `NA`.
  # Nothing beyond the "== Testing dataset_test ==" header
  failures <- dataset_test_failures("Test_2024_1")
  expect_equal(grep("^== Testing", failures, invert = TRUE, value = TRUE), character(0))
})


test_that("`dataset_test` accepts long-format traits with no `traits` entry (#308)", {
  # Test_2023_3 is long format. Add rows for a trait its `metadata.yml` does not
  # map, in a temporary copy so the shared example stays untouched. The build
  # leaves them out, as it does a column a wide dataset does not map, so they
  # should cause no failure.
  path_data <- withr::local_tempdir()
  file.copy("examples/Test_2023_3", path_data, recursive = TRUE)

  data_file <- file.path(path_data, "Test_2023_3", "data.csv")
  data <- readr::read_csv(data_file, col_types = readr::cols(.default = "c"))
  extra <- data[1:3, ]
  extra$trait_name <- "unmapped_trait"
  readr::write_csv(dplyr::bind_rows(data, extra), data_file, na = "")

  expect_identical(
    dataset_test_failures("Test_2023_3", path_data = path_data),
    dataset_test_failures("Test_2023_3")
  )

  built <- dataset_process(
    data_file,
    dataset_configure(file.path(path_data, "Test_2023_3", "metadata.yml"), get_schema("config/traits.yml", "traits")),
    get_schema(),
    get_schema("config/metadata.yml", "metadata"),
    get_unit_conversions("config/unit_conversions.csv")
  )
  expect_false("unmapped_trait" %in% c(built$traits$trait_name, built$excluded_data$trait_name))
})
