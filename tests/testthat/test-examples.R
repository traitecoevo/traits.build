## Golden-file regression tests for the nine example datasets.
##
## Each `examples/Test_2023_*/` holds an input `data.csv` and `metadata.yml`, a
## README describing the behaviours it was built to pin, and an `output/`
## directory of expected artefacts. Regenerate `output/` with
## `Rscript regenerate-examples.R` and read the diff -- never hand-edit an
## expected file towards the observed output.
##
## Every artefact in `output/` is compared. That was not true until Stage 0 of
## #225: only eight of the twelve were checked, and the unchecked
## `identifiers.csv` files had drifted so far that one of them held values no
## build had produced since the column it read from was dropped from `data.csv`.
## If you add an artefact to `write_plaintext()`, add it here too, or it will
## rot the same way.

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml",  "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- traits.build:::get_unit_conversions("config/unit_conversions.csv")
taxon_list <- read_csv_char("config/taxon_list.csv")
examples_dir <- "examples"

dataset_ids <- sprintf("Test_2023_%d", 1:9)

# Every table in the database structure, not a hand-maintained subset
tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
            "taxonomic_updates", "taxa", "contributors", "identifiers")


# The output `metadata.yml` records the version of traits.build that built it,
# so it changes on every version bump and cannot be compared verbatim. Drop it
# from both sides rather than skipping the file: everything else in there --
# the resource metadata, the related identifiers, the contributor block -- is
# worth pinning.
drop_build_version <- function(metadata) {
  metadata[["related_identifiers"]] <-
    metadata[["related_identifiers"]] %>%
    lapply(function(entry) {
      if (identical(entry[["identifier"]],
                    "https://github.com/traitecoevo/traits.build")) {
        entry[["version"]] <- NULL
      }
      entry
    })
  metadata
}


expect_dataset_matches_output <- function(dataset_id) {

  output_dir <- file.path(examples_dir, dataset_id, "output")

  expect_no_error(
    built <- test_build_dataset(
      file.path(examples_dir, dataset_id, "metadata.yml"),
      file.path(examples_dir, dataset_id, "data.csv"),
      dataset_id, definitions, unit_conversions, schema,
      resource_metadata, taxon_list
    )
  )

  for (v in tables) {
    expected <- read_csv(
      file.path(output_dir, sprintf("%s.csv", v)),
      col_types = cols(.default = "c")
    )
    expect_equal(built[[v]], expected, info = paste(dataset_id, v))
  }

  # `definitions` is the subset of `config/traits.yml` covering the traits this
  # dataset actually reports, so it pins the trait-selection logic
  expect_equal(
    built$definitions,
    read_yaml(file.path(output_dir, "definitions.yml")),
    info = paste(dataset_id, "definitions")
  )

  expect_equal(
    drop_build_version(built$metadata),
    drop_build_version(read_yaml(file.path(output_dir, "metadata.yml"))),
    info = paste(dataset_id, "metadata")
  )

  # `sources` is a BibEntry object; compare the .bib file the build writes,
  # since that is the artefact users receive and it is what would silently
  # change if RefManageR altered its output
  bib_written <- withr::local_tempfile(fileext = ".bib")
  suppressMessages(
    RefManageR::WriteBib(built$sources, bib_written, verbose = FALSE)
  )
  expect_equal(
    readLines(bib_written),
    readLines(file.path(output_dir, "sources.bib")),
    info = paste(dataset_id, "sources")
  )

  invisible(built)
}


for (dataset_id in dataset_ids) {
  # `local()` so each iteration captures its own `dataset_id`
  local({
    id <- dataset_id
    testthat::test_that(paste(id, "builds correctly"), {
      # See README.md in examples/<id> for what this dataset pins
      expect_dataset_matches_output(id)
    })
  })
}


# `dataset_test` runs the validation layer rather than the build. Test_2023_1
# deliberately carries an incorrect trait_name in its metadata, so it is
# expected to report a failure; the rest are expected to pass cleanly.
testthat::test_that("`dataset_test` passes for the valid example datasets", {
  expect_no_error(
    dataset_test(
      dataset_ids = c("Test_2023_2", "Test_2023_3", "Test_2023_4",
                      "Test_2023_5", "Test_2023_6", "Test_2023_7"),
      path_data = "examples")
  )
})
