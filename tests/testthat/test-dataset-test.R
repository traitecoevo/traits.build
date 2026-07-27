test_that("`dataset_test` handles several identifiers", {
  # A dataset may declare several identifiers, so `metadata$identifiers` is a
  # list and `is.na()` on it returns one value per element. Guarding with
  # `if (!is.na(metadata$identifiers))` therefore aborted the whole run with
  # "the condition has length > 1" for any such dataset.
  metadata <- read_metadata("examples/Test_2023_1/metadata.yml")
  expect_gt(length(metadata$identifiers), 1)

  # `dataset_test` reads `config/taxon_list.csv`, which is not in the repository
  # — it is created as a side effect of `test-setup.R`. Test files run
  # alphabetically, so this one runs first on a clean checkout. Seed it from the
  # committed fixture that `test-setup.R` copies over it anyway.
  if (!file.exists("config/taxon_list.csv")) {
    file.copy("config/taxon_list-orig.csv", "config/taxon_list.csv")
    withr::defer(unlink("config/taxon_list.csv"))
  }

  out <- capture.output(
    suppressMessages(
      dataset_test(dataset_ids = "Test_2023_1", path_data = "examples")
    )
  )

  expect_false(any(grepl("the condition has length > 1", out, fixed = TRUE)))
})
