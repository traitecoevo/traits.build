## `dataset_configure()` reads some files besides `metadata.yml` for itself: a
## csv its `locations:` names, and a .yml or .bib file its `source:` or
## `secondary:` reference list names. The `targets` and `remake` pipelines
## must declare each as a dependency of the dataset, or editing it rebuilds
## nothing and the built database silently keeps the old content.
## (`test-locations-csv.R` covers the locations file on its own.)


# Copy `Test_2023_2` and `Test_2023_1` into `dir/data`, and move
# `Test_2023_2`'s reference lists into a .yml and a .bib file
setup_reference_files <- function(dir) {
  dir.create(file.path(dir, "data"))
  dir.create(file.path(dir, "config"))
  file.copy(file.path("examples", c("Test_2023_1", "Test_2023_2")),
            file.path(dir, "data"), recursive = TRUE)

  path <- file.path(dir, "data", "Test_2023_2", "metadata.yml")
  metadata <- read_metadata(path)
  metadata$source$source <- "sources.yml"
  metadata$source$secondary <- "secondary.bib"
  write_metadata(metadata, path)
  writeLines(c(
    "- key: Jones_1999",
    "  bibtype: Thesis",
    "  year: '1999'",
    "  author: Bob Jones",
    "  title: A thesis"
  ), file.path(dir, "data", "Test_2023_2", "sources.yml"))
  writeLines(c(
    "@Article{Smith_2021,",
    "  author = {Ann Smith},",
    "  title = {A re-use},",
    "  journal = {A Journal},",
    "  year = {2021},",
    "}"
  ), file.path(dir, "data", "Test_2023_2", "secondary.bib"))
}


test_that("`pipeline_dataset_files` finds the files a dataset names", {
  dir <- withr::local_tempdir()
  setup_reference_files(dir)

  expect_equal(
    pipeline_dataset_files(file.path(dir, "data", "Test_2023_2", "metadata.yml")),
    c(sources = "sources.yml", secondary_sources = "secondary.bib")
  )
  # Inline locations and references name no files
  none <- pipeline_dataset_files(file.path(dir, "data", "Test_2023_1", "metadata.yml"))
  expect_type(none, "character")
  expect_length(none, 0)
})


test_that("the targets pipeline depends on a dataset's reference files", {

  skip_if_not_installed("targets")

  dir <- withr::local_tempdir()
  setup_reference_files(dir)

  withr::local_dir(dir)
  suppressMessages(build_setup_pipeline(method = "targets"))

  expect_silent(parse("_targets.R"))
  expect_false(any(grepl("FALSE", readLines("_targets.R"), fixed = TRUE)))

  manifest <- targets::tar_manifest(callr_function = NULL)
  expect_true(all(
    c("file_Test_2023_2_sources", "file_Test_2023_2_secondary_sources") %in% manifest$name
  ))
  expect_false(any(grepl("^file_Test_2023_1_(sources|secondary)", manifest$name)))
  expect_equal(
    manifest$command[manifest$name == "file_Test_2023_2_sources"],
    "\"data/Test_2023_2/sources.yml\""
  )

  # Reachable from the dataset's config, which is what makes an edit rebuild
  # the dataset
  config <- manifest$command[manifest$name == "Test_2023_2_config"]
  expect_match(config, "file_Test_2023_2_sources")
  expect_match(config, "file_Test_2023_2_secondary_sources")
  expect_equal(
    manifest$command[manifest$name == "Test_2023_1_config"],
    "dataset_configure(file_Test_2023_1_metadata, definitions)"
  )
})


test_that("the remake pipeline depends on a dataset's reference files", {
  dir <- withr::local_tempdir()
  setup_reference_files(dir)

  withr::local_dir(dir)
  suppressMessages(build_setup_pipeline(method = "remake"))
  targets <- yaml::read_yaml("remake.yml")$targets

  expect_equal(
    targets$Test_2023_2_config$depends,
    c("data/Test_2023_2/sources.yml", "data/Test_2023_2/secondary.bib")
  )
  expect_null(targets$Test_2023_1_config$depends)
})
