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
