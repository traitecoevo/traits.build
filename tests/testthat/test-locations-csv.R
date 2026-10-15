## A dataset compiled from per-record georeferenced occurrences generates one
## location per record, which makes `metadata.yml` unreviewable -- `AVH_2026` in
## austraits.build reaches 162,469 locations and 487,518 lines (#263). Such a
## dataset can instead name a csv file holding them.
##
## The test that matters is the last one: a dataset converted to the csv form
## must build to the same database it built before.

schema <- get_schema()


# Copy an example dataset to a temporary directory and rewrite it to hold its
# locations in a csv file. Returns the path to the copied `metadata.yml`.
as_locations_csv <- function(dataset_id, dir = withr::local_tempdir(.local_envir = envir),
                             file = "locations.csv", envir = parent.frame()) {

  dir.create(file.path(dir, "data"), showWarnings = FALSE)
  file.copy(file.path("examples", dataset_id), file.path(dir, "data"),
            recursive = TRUE)
  path <- file.path(dir, "data", dataset_id, "metadata.yml")

  metadata <- read_metadata(path)

  metadata$locations <-
    metadata$locations %>%
    process_format_locations(dataset_id, schema) %>%
    dplyr::select(dplyr::all_of(c("location_name", "location_property", "value"))) %>%
    # A property recorded but unknown is `.na` in the yaml and `.na` in the csv
    dplyr::mutate(value = tidyr::replace_na(value, ".na")) %>%
    tidyr::pivot_wider(names_from = "location_property", values_from = "value")

  attr(metadata$locations, "locations_file") <- file
  write_metadata(metadata, path)

  path
}


test_that("a `locations:` naming a csv file is read from that file", {
  path <- as_locations_csv("Test_2023_1")

  expect_equal(grep("^locations:", readLines(path), value = TRUE),
               "locations: locations.csv")
  expect_true(file.exists(file.path(dirname(path), "locations.csv")))

  locations <- read_metadata(path)$locations
  expect_s3_class(locations, "data.frame")
  expect_true("location_name" %in% names(locations))
  expect_equal(nrow(locations), 3)
})


test_that("both forms of `locations:` format to the same table", {
  for (dataset_id in sprintf("Test_2023_%d", 1:9)) {

    inline <- read_metadata(file.path("examples", dataset_id, "metadata.yml"))
    if (length(unlist(inline$locations)) <= 1) next

    from_yaml <- process_format_locations(inline$locations, dataset_id, schema)
    from_csv <- process_format_locations(
      read_metadata(as_locations_csv(dataset_id))$locations, dataset_id, schema
    )

    expect_equal(from_csv, from_yaml, info = dataset_id)
  }
})


test_that("an empty cell drops the property, `.na` keeps it as missing", {
  path <- as_locations_csv("Test_2023_1")

  # `Test_2023_1` has a location recording neither elevation nor rainfall, and
  # locations recording `.na` coordinates
  written <- readLines(file.path(dirname(path), "locations.csv"))
  expect_match(written[length(written)], ",,$")

  locations <- process_format_locations(
    read_metadata(path)$locations, "Test_2023_1", schema
  )
  absent <- locations[locations$location_name == "test_excluded_site", ]

  expect_false(
    any(c("elevation (m)", "rainfall (mm)") %in% absent$location_property)
  )
  expect_true("latitude (deg)" %in% absent$location_property)
  expect_true(is.na(absent$value[absent$location_property == "latitude (deg)"]))
})


test_that("a location genuinely named NA survives the round trip", {
  path <- as_locations_csv("Test_2023_1")
  csv <- file.path(dirname(path), "locations.csv")

  locations <- read_csv_char(csv)
  locations$location_name[1] <- "NA"
  readr::write_csv(locations, csv, na = "")

  expect_true("NA" %in% read_metadata(path)$locations$location_name)
})


test_that("an unusable locations file is reported against the file", {
  path <- as_locations_csv("Test_2023_1")
  csv <- file.path(dirname(path), "locations.csv")

  unlink(csv)
  expect_error(read_metadata(path), "does not exist", fixed = TRUE)

  readr::write_csv(tibble::tibble(site = "a", `latitude (deg)` = "1"), csv)
  expect_error(read_metadata(path), "no `location_name` column", fixed = TRUE)

  readr::write_csv(
    tibble::tibble(location_name = c("a", "a"), `latitude (deg)` = c("1", "2")),
    csv
  )
  expect_error(read_metadata(path), "duplicated `location_name`", fixed = TRUE)
})


test_that("`metadata_add_locations(file =)` writes the csv form", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "data"))
  file.copy("examples/Test_2023_1", file.path(dir, "data"), recursive = TRUE)
  withr::local_dir(dir)

  location_data <- tibble::tibble(
    site = c("Atherton", "Cape Tribulation", "test_excluded_site"),
    `latitude (deg)` = c("-17.1166667", ".na", ".na"),
    `longitude (deg)` = c("145.65", ".na", ".na")
  )
  responses <- list(location_name = "site",
                    keep = c("latitude (deg)", "longitude (deg)"))

  suppressMessages(
    metadata_add_locations("Test_2023_1", location_data,
                           user_responses = responses, file = "locations.csv")
  )

  path <- file.path("data", "Test_2023_1", "metadata.yml")
  expect_equal(grep("^locations:", readLines(path), value = TRUE),
               "locations: locations.csv")
  expect_equal(nrow(read_metadata(path)$locations), 3)

  # Without `file`, the locations are still written into the yaml itself
  suppressMessages(
    metadata_add_locations("Test_2023_1", location_data,
                           user_responses = responses)
  )
  expect_type(read_metadata(path)$locations, "list")
  expect_length(read_metadata(path)$locations, 3)
})


test_that("`build_setup_pipeline` makes remake depend on the locations file", {
  dir <- withr::local_tempdir()
  path <- as_locations_csv("Test_2023_1", dir = dir)
  file.copy("examples/Test_2023_2", file.path(dir, "data"), recursive = TRUE)
  # `build_setup_pipeline()` writes `config/taxon_list.csv` as a side effect
  dir.create(file.path(dir, "config"))

  withr::local_dir(dir)
  suppressMessages(build_setup_pipeline(method = "remake"))
  remake <- readLines("remake.yml")

  expect_true(any(grepl("data/Test_2023_1/locations.csv", remake, fixed = TRUE)))
  expect_false(any(grepl("Test_2023_2/locations.csv", remake, fixed = TRUE)))
  # A dataset with no locations file must not render the section at all
  expect_false(any(grepl("FALSE", remake, fixed = TRUE)))
})


test_that("the targets pipeline depends on the locations file too", {

  skip_if_not_installed("targets")

  dir <- withr::local_tempdir()
  as_locations_csv("Test_2023_1", dir = dir)
  file.copy("examples/Test_2023_2", file.path(dir, "data"), recursive = TRUE)
  dir.create(file.path(dir, "config"))

  withr::local_dir(dir)
  suppressMessages(build_setup_pipeline(method = "targets"))
  generated <- readLines("_targets.R")

  expect_silent(parse("_targets.R"))
  expect_true(any(grepl("data/Test_2023_1/locations.csv", generated, fixed = TRUE)))
  expect_false(any(grepl("Test_2023_2/locations.csv", generated, fixed = TRUE)))
  expect_false(any(grepl("FALSE", generated, fixed = TRUE)))

  # The file target has to exist and be reachable from the dataset's config,
  # or editing the locations file would not rebuild anything
  manifest <- targets::tar_manifest(callr_function = NULL)
  expect_true("file_Test_2023_1_locations" %in% manifest$name)
  expect_false("file_Test_2023_2_locations" %in% manifest$name)
  expect_match(
    manifest$command[manifest$name == "Test_2023_1_config"],
    "file_Test_2023_1_locations"
  )
})


test_that("`dataset_test` passes on a dataset holding locations in a csv", {
  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )

  dir <- withr::local_tempdir()
  as_locations_csv("Test_2023_1", dir = dir)
  config <- normalizePath("config")

  # `Test_2023_1` is a fixture that deliberately fails some checks, so what is
  # asserted is that converting it changes nothing: the same checks pass and
  # the same ones fail. In particular `locations.csv` is neither a disallowed
  # file nor a dataset that has stopped declaring any locations.
  report <- function(path_data) {
    out <- capture.output(
      suppressMessages(
        dataset_test("Test_2023_1", path_config = config, path_data = path_data)
      )
    )
    out <- out[!grepl("^\\[ FAIL|^Backtrace:|^\\s*[0-9]+\\. |^\\s*x\\s*$", out)]
    out <- gsub("\033\\[[0-9]+m", "", out)
    # The two runs read the dataset from different directories
    trimws(gsub(file.path(path_data, "Test_2023_1"), "<dataset>", out, fixed = TRUE))
  }

  expect_equal(report(file.path(dir, "data")), report("examples"))
})


test_that("a dataset converted to the csv form builds the same database", {
  resource_metadata <- get_schema("config/metadata.yml", "metadata")
  definitions <- get_schema("config/traits.yml", "traits")
  unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
  taxon_list <- read_csv_char("config/taxon_list.csv")

  for (dataset_id in sprintf("Test_2023_%d", 1:9)) {

    inline_path <- file.path("examples", dataset_id, "metadata.yml")
    if (length(unlist(read_metadata(inline_path)$locations)) <= 1) next

    csv_path <- as_locations_csv(dataset_id)

    build <- function(metadata_path, data_path) {
      dataset_update_taxonomy(
        dataset_process(
          data_path, dataset_configure(metadata_path, definitions),
          schema, resource_metadata, unit_conversions
        ),
        taxon_list
      )
    }

    from_yaml <- build(inline_path, file.path("examples", dataset_id, "data.csv"))
    from_csv <- build(csv_path, file.path(dirname(csv_path), "data.csv"))

    for (table in c("traits", "locations", "contexts", "methods",
                    "excluded_data", "taxonomic_updates", "taxa",
                    "contributors", "identifiers")) {
      expect_equal(from_csv[[table]], from_yaml[[table]],
                   info = paste(dataset_id, table))
    }
  }
})
