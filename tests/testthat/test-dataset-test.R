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
dataset_test_failures <- function(dataset_id) {

  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )

  out <- capture.output(
    suppressMessages(
      dataset_test(dataset_ids = dataset_id, path_data = "examples")
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
  # Test_2023_2 passes every check, so any failure here is the config.
  out <- dataset_test_elsewhere("Test_2023_2", "config")

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

  expect_equal(results$test, "Test_2023_1")
  expect_equal(results$failed, 2)
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


# `source_id` links each record to the reference it was taken from (#296).
# Test_2023_2 maps a `source_id` column holding `Test_2022` and `Test_2002`, and
# lists both as `original_source` references, so it passes; each edit below
# breaks one thing and must be reported for that thing.

test_that("`dataset_test` reports a `source` block in the old shape, and the build refuses it (#296)", {
  out <- dataset_test_edited(function(metadata) {
    # One named entry per reference, as traits.build wrote before 2.3.0
    originals <- metadata$source$original_source
    metadata$source$original_source <- NULL
    metadata$source$original_01 <- originals[[1]]
    metadata$source$original_02 <- originals[[2]]
    metadata
  })

  expect_match(
    paste(out, collapse = "\n"),
    "`original_01`, `original_02`: not in the expected shape, so the dataset will not build",
    fixed = TRUE
  )
  # The lenient read lets the other source checks run: both keys still link
  expect_false(any(grepl("should not contain", out, fixed = TRUE)))
  # ...and the build itself stops, naming the entries
  expect_true(any(grepl("building dataset", out, fixed = TRUE)))
  expect_true(any(grepl("entries in a shape traits.build no longer reads: `original_01`, `original_02`", out, fixed = TRUE)))
})


test_that("`dataset_test` reports `source_id` values that match no reference (#296)", {
  out <- dataset_test_edited(function(metadata) {
    metadata$source$original_source <- NULL
    metadata
  })

  hits <- grep("`source_id` (each value must be the `key` of the primary or an `original_source` reference)", out, fixed = TRUE)
  expect_length(hits, 1)
  expect_match(out[hits], "should not contain: 'Test_2022', 'Test_2002'|should not contain: 'Test_2002', 'Test_2022'")
  # Not an old-shape problem, and the dataset still builds
  expect_false(any(grepl("not in the expected shape", out, fixed = TRUE)))
  expect_false(any(grepl("building dataset", out, fixed = TRUE)))
})


test_that("`dataset_test` says to move a `secondary` reference that records are taken from (#296)", {
  out <- dataset_test_edited(function(metadata) {
    moved <- metadata$source$original_source[[2]]
    metadata$source$original_source <- metadata$source$original_source[1]
    metadata$source$secondary <- list(moved)
    metadata
  })

  expect_match(
    paste(out, collapse = "\n"),
    "move these entries to `original_source`: 'Test_2002'",
    fixed = TRUE
  )
  # The moved key is reported once, as misplaced, not again as unmatched
  expect_false(any(grepl("should not contain: 'Test_2002'", out, fixed = TRUE)))
})


test_that("`dataset_test` reports an `original_source` reference no record names (#296)", {
  out <- dataset_test_edited(function(metadata) {
    unused <- metadata$source$original_source[[1]]
    unused$key <- "Test_1999"
    metadata$source$original_source <- c(metadata$source$original_source, list(unused))
    metadata
  })

  expect_match(
    paste(out, collapse = "\n"),
    "no record's `source_id` names these `original_source` references (a typo in the data, or the reference is really `secondary`): 'Test_1999'",
    fixed = TRUE
  )
})


test_that("`dataset_test` hints when `source_id` holds an entry's name rather than its key (#296)", {
  out <- dataset_test_edited(function(metadata) {
    # A constant, as `process_parse_data()` would read it: no such column
    metadata$dataset$source_id <- "original_source"
    metadata
  })

  hits <- grep("a value such as `secondary_01` is the name of an entry, not its `key`", out, fixed = TRUE)
  expect_length(hits, 1)
  expect_match(out[hits], "should not contain: 'original_source'", fixed = TRUE)
  # Nothing links to the two originals now, so they are reported as unused
  expect_true(any(grepl("no record's `source_id` names these `original_source` references", out, fixed = TRUE)))
})
