
test_that("`metadata_create_template` is working", {
  # Remove the metadata file if it exists before testing `metadata_create_template`
  unlink("data/Test_2022/metadata.yml")
  expect_silent(schema <- get_schema())
  expect_silent(
    expect_invisible(
      test_metadata <- metadata_create_template(
        dataset_id = "Test_2022",
        path = file.path("data", "Test_2022"),
        skip_manual = TRUE)
    ))

  metadata_names <- c("source", "contributors", "dataset", "identifiers", "locations", "contexts", "traits",
                      "substitutions", "taxonomic_updates", "exclude_observations",
                      "questions")
  collectors_names <- c("last_name", "given_name", "ORCID", "affiliation")
  # Test names exist
  expect_named(test_metadata)
  # Test metadata exists with correct names
  expect_named(test_metadata, metadata_names)
  expect_length(test_metadata$source$primary, 10)
  expect_in(names(test_metadata$dataset), names(schema$metadata$elements$dataset$values))
  expect_type(test_metadata$contributors$data_collectors, "list")
  expect_length(test_metadata$contributors$data_collectors, 1L)
  expect_named(test_metadata$contributors$data_collectors[[1]], collectors_names)
  # Test that metadata_create_template puts all data collector fields as "unknown" by default
  expect_equal((test_metadata$contributors$data_collectors[[1]] %>% unique)[[1]], "unknown")
 })


test_that("`metadata_create_template` is working with simulated user input", {
  # Test that a message is shown if metadata already exists
  expect_silent(schema <- get_schema())

  # Check long format
  user_responses <- list(
    data_is_long_format = TRUE, repeat_measurements_id = TRUE,
    taxon_name = "Species", trait_name = "trait_name", value = "value",
    location_name = NA, individual_id = "id", collection_date = "2008/2009"
  )
  expect_message(
    test_metadata <- metadata_create_template("Test_2022", user_responses = user_responses),
    ".*(?=already exists and will be overwritten).*", perl = TRUE
  )

  metadata_names <- c("source", "contributors", "dataset", "identifiers", "locations", "contexts", "traits",
                      "substitutions", "taxonomic_updates", "exclude_observations",
                      "questions")
  # Test metadata exists with correct names
  expect_named(test_metadata, metadata_names)
  expect_in(names(test_metadata$dataset), names(schema$metadata$elements$dataset$values))

  expect_true(test_metadata$dataset$data_is_long_format)
  # Test metadata fields are equal to inputs in `user_responses`
  fields <- c(
    "taxon_name", "trait_name", "value", "location_name", "individual_id",
    "collection_date", "repeat_measurements_id")
  for (f in fields) {
    if (f == "location_name") {
      expect_equal(test_metadata[["dataset"]][[f]], "unknown")
    } else {
      expect_equal(test_metadata[["dataset"]][[f]], user_responses[[f]])
    }
  }

  # Check wide format
  user_responses <- list(
    data_is_long_format = FALSE,
    taxon_name = "Species",
    location_name = NA, individual_id = "id", collection_date = "2008/2009"
  )
  unlink("data/Test_2022/metadata.yml")
  expect_no_error(
    test_metadata <- metadata_create_template(
    "Test_2022",
    user_responses = user_responses)
  )

  # Test metadata exists with correct names
  expect_named(test_metadata, metadata_names)
  expect_in(names(test_metadata$dataset), names(schema$metadata$elements$dataset$values))

  expect_false(test_metadata$dataset$data_is_long_format)
  # Test metadata fields are equal to inputs in `user_responses`
  fields <- c("taxon_name", "location_name", "individual_id", "collection_date")
  for (f in fields) {
    if (f == "location_name") {
      expect_equal(test_metadata[["dataset"]][[f]], "unknown")
    } else {
      expect_equal(test_metadata[["dataset"]][[f]], user_responses[[f]])
    }
  }
  # Expect `repeat_measurements_id` is null if not specified
  expect_null(test_metadata[["dataset"]][["repeat_measurements_id"]])

 })


test_that("`metadata_path_dataset_id` is working", {
  expect_silent(metadata_path_dataset_id("Test_2022"))
  expect_equal(metadata_path_dataset_id("Test_2022"), "data/Test_2022/metadata.yml")
  expect_type(metadata_path_dataset_id("Test_2022"), "character")
})


test_that("`read_metadata_dataset` is working", {
  expect_silent(read_metadata_dataset("Test_2022"))
  expect_type(read_metadata_dataset("Test_2022"), "list")
})


test_that("`write_metadata_dataset` is working", {
  metadata <- read_metadata_dataset("Test_2022")

  unlink("data/Test_2022/metadata.yml")
  expect_false(file.exists("data/Test_2022/metadata.yml"))

  expect_silent(write_metadata_dataset(metadata, "Test_2022"))
  expect_true(file.exists("data/Test_2022/metadata.yml"))

  expect_silent(read_metadata_dataset("Test_2022"))
  expect_type(read_metadata_dataset("Test_2022"), "list")
})


test_that("`metadata_add_source_bibtex` is working", {
  expect_silent(metadata_add_source_bibtex(dataset_id = "Test_2022", file = "data/test2.bib"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$primary$journal, "Journal of Ecology")
})


test_that("`metadata_add_source_doi` is working", {
  expect_no_error(suppressMessages(test_metadata <- metadata_create_template("Test_2022", skip_manual = TRUE)))
  test_doi <- "https://doi.org/10.3389/fmars.2021.671145"
  test_doi2 <- "https://doi.org/10.1111/j.0022-0477.2005.00992.x"
  doi <- "10.3389/fmars.2021.671145"
  doi2 <- "10.1111/j.0022-0477.2005.00992.x"

  # Test `util_standardise_doi`
  expect_equal(util_standardise_doi(doi), doi)
  expect_equal(util_standardise_doi(test_doi), doi)
  expect_equal(util_standardise_doi("doi.org/10.3389/fmars.2021.671145"), doi)
  expect_equal(util_standardise_doi("http://doi.org/10.3389/fmars.2021.671145"), doi) # http not https

  # We won't actually test querying of rcrossref, to avoid unnecessary fails
  # Passing in .bib files avoids calling crossref

  # Create and load test data
  # rcrossref::cr_cn prefers doi to be in this format "10.3389/fmars.2021.671145"
  #bib <- rcrossref::cr_cn(doi)
  #writeLines(bib, "data/test.bib")
  #bib2 <- rcrossref::cr_cn(doi2)
  #writeLines(bib2, "data/test2.bib")
  bib <- readLines("data/test.bib", encoding = "UTF-8") %>% paste(collapse = "\n")
  bib2 <- readLines("data/test2.bib", encoding = "UTF-8") %>% paste(collapse = "\n")
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = doi, bib = bib))
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = doi2, bib = bib2, type = "secondary"))

  test_metadata <- read_metadata("data/Test_2022/metadata.yml")
  expect_equal(test_metadata$source$primary$journal, "Frontiers in Marine Science")
  expect_equal(test_metadata$source$primary$year, "2021")
  expect_equal(test_metadata$source$primary$doi, doi)
  # Test standardised capitalisation of authors
  expect_equal(test_metadata$source$primary$author, "Gary Truong and Tracey L. Rogers")
  expect_equal(test_metadata$source$secondary$journal, "Journal of Ecology")
  expect_equal(test_metadata$source$secondary$year, "2005")
  expect_equal(test_metadata$source$secondary$doi, doi2)
  # Test standardised capitalisation of authors and key
  expect_equal(test_metadata$source$secondary$author, "Daniel S. Falster and Mark Westoby")
  expect_equal(test_metadata$source$secondary$key, "Falster_2005")

  # Test that adding a primary source overwrites the existing source and sends a message
  expect_message(
    metadata_add_source_doi(dataset_id = "Test_2022", doi = doi2, bib = bib2),
    ".*(?=already exists and is being overwritten)", perl = TRUE
  )
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$primary$journal, "Journal of Ecology")
  # Test if adding a secondary source overwrites existing source and sends a message
  expect_message(
    metadata_add_source_doi(dataset_id = "Test_2022", doi = doi, bib = bib, type = "secondary"),
    ".*(?=already exists and is being overwritten)", perl = TRUE
  )
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$secondary$journal, "Frontiers in Marine Science")
  # Test if adding secondary_02 works
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = doi, bib = bib, type = "secondary_02"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$secondary_02$journal, "Frontiers in Marine Science")
})


test_that("`metadata_add_source_doi` reports informatively when `rcrossref` is missing", {
  # `rcrossref` is an optional (Suggests) dependency, needed only when `bib` is
  # not supplied. Pretend it is absent to check the error names the package and
  # says what it is for, rather than reporting a bare `loadNamespace` failure.
  local_mocked_bindings(
    util_require_package = function(pkg, reason) {
      rlang::abort(sprintf("The package `%s` is required %s", pkg, reason))
    }
  )

  expect_error(
    metadata_add_source_doi(dataset_id = "Test_2022", doi = "10.3389/fmars.2021.671145"),
    "`rcrossref` is required to look up citation details"
  )

  # Supplying `bib` bypasses crossref, so it must not consult the package at all
  bib <- readLines("data/test.bib", encoding = "UTF-8") %>% paste(collapse = "\n")
  expect_invisible(
    suppressMessages(
      metadata_add_source_doi(dataset_id = "Test_2022", doi = "10.3389/fmars.2021.671145", bib = bib)
    )
  )
})


test_that("`metadata_check_custom_R_code` is working", {
  # Check that the `custom_R_code` produces a tibble class object
  expect_equal(class(metadata_check_custom_R_code("Test_2022")), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(metadata_check_custom_R_code("Test_2022")), 13)
  expect_equal(nrow(metadata_check_custom_R_code("Test_2022")), 45)
  expect_visible(metadata_check_custom_R_code("Test_2022"))
})


test_that("`metadata_add_locations` is working", {
  locations <- tibble(
    site_name = c("site 1", "site 2"),
    latitude = c("-16", "-17"),
    longitude = c("145", "146"),
    elevation = c("100", "150"))

  suppressMessages(
    expect_message(
      x <- metadata_add_locations("Test_2022", locations,
        # Gives responses for user input, for testing
        user_responses = list(location_name = "site_name", keep = c("latitude", "longitude", "elevation"))),
      "Following locations added to metadata for", perl = TRUE
    ))
  expect_equal(names(x$locations), locations$site_name)
  expect_equal(lapply(x$locations, "[[", "latitude") %>% unlist() %>% as.character(), locations$latitude)

  # Check message if locations already exist
  suppressMessages(
    expect_message(
      x <- metadata_add_locations("Test_2022", locations,
      # Gives responses for user input, for testing
      user_responses = list(location_name = "site_name", keep = c("latitude", "longitude", "elevation"))),
      ".*(?=already exists).*", perl = TRUE
    )
  )
})


test_that("`metadata_add_identifiers` is working", {
  # This was the only `metadata_add_*` function with no test, precisely because
  # it was the only one with no `user_responses` hook to test through -- and
  # with nothing exercising it, `institution_code = .na` shipped. `.na` is
  # YAML's null, not an R object, so the function errored with
  # "object '.na' not found" the moment a column was selected. It never worked.
  expect_true(file.copy("data/Test_2022/test-metadata.yml",
                        "data/Test_2022/metadata.yml", overwrite = TRUE))

  expect_no_error(
    x <- metadata_add_identifiers(
      "Test_2022",
      user_responses = list(
        var_in = c("species_abb", "site"),
        identifier_type = c("catalogNumber", "institutionCode")))
  )

  expect_length(x$identifiers, 2)
  expect_equal(purrr::map_chr(x$identifiers, "var_in"), c("species_abb", "site"))
  expect_equal(purrr::map_chr(x$identifiers, "identifier_type"),
               c("catalogNumber", "institutionCode"))

  # Not asked for, since it cannot be inferred from the data -- but it has to be
  # a real NA the user can fill in, not an error
  expect_true(all(is.na(purrr::map_chr(x$identifiers, "institution_code"))))

  # Round-trips through `metadata.yml` rather than only existing in memory
  written <- read_metadata_dataset("Test_2022")
  expect_equal(written$identifiers, x$identifiers)

  # Appends rather than overwrites, matching its siblings
  expect_message(
    y <- metadata_add_identifiers(
      "Test_2022",
      user_responses = list(var_in = "Species", identifier_type = "occurrenceID"))
  )
  expect_length(y$identifiers, 3)

  expect_error(
    suppressMessages(
      metadata_add_identifiers(
        "Test_2022",
        user_responses = list(var_in = c("Species", "site"),
                              identifier_type = "occurrenceID"))),
    "one `identifier_type` per `var_in`"
  )
})


test_that("`metadata_add_contexts` is working", {
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))
  var_in <- c("test_context_1", "test_context_2")
  categories <- c("treatment", "entity_context")

  expect_no_error(
    x <- metadata_add_contexts(
      "Test_2022",
      user_responses = list(
        var_in = var_in,
        categories = categories,
        replace_needed = c("y", "n")))
  )

  expect_equal(lapply(x$contexts, "[[", "context_property") %>% unlist() %>% unique, "unknown")
  expect_equal(lapply(x$contexts, "[[", "category") %>% unlist(), categories)
  expect_equal(lapply(x$contexts, "[[", "var_in") %>% unlist(), var_in)
  # Expect `find` column for context variable where replacements are needed
  expect_no_error(x$contexts[[1]][["values"]] %>% pull("find"))
  # Expect no `find` column for context variable where replacements are not needed
  expect_error(x$contexts[[2]][["values"]] %>% pull("find"))

  # Check if contexts get appended, not overwritten
  expect_error(x$contexts[[3]])
  expect_message(
    x <- metadata_add_contexts(
        "Test_2022",
        user_responses = list(
          var_in = "test_context_1",
          categories = "treatment",
          replace_needed = c("y"))),
    ".*(?=Existing context information detected, from the following columns)", perl = TRUE
  )
  expect_no_error(x$contexts[[3]])
})


test_that("`metadata_add_contexts` records time values as the build reads them", {
  # `read_csv` types time-like columns as `hms`, which reached `yaml::as.yaml`
  # as the underlying number of seconds, so "9:00:00" was written to the
  # metadata as `32400.0` and never matched the data (#49).

  # Build the dataset on the fly and remove it again, so that it is not picked
  # up as a dataset by the functions that scan the whole `data` directory
  dir.create("data/Test_time")
  withr::defer(unlink("data/Test_time", recursive = TRUE))
  writeLines(
    c("Species,site,sampling_time,LMA (mg mm-2)",
      "Acacia celsa,Atherton,9:00:00,0.145",
      "Acmena graveolens,Atherton,11:30,0.180",
      "Neolitsea dealbata,Cape Tribulation,,0.200",
      "Brombya platynema,Cape Tribulation,9:00:00,0.210"),
    "data/Test_time/data.csv"
  )

  expect_no_error(suppressMessages(metadata_create_template("Test_time", skip_manual = TRUE)))

  expect_message(
    x <- metadata_add_contexts(
      "Test_time",
      user_responses = list(
        var_in = "sampling_time",
        categories = "temporal_context",
        replace_needed = "y")),
    ".*(?=Time data detected in column)", perl = TRUE
  )

  # "9:00:00" is zero-padded and bare "11:30" gains seconds, matching the
  # reformatting `read_csv` applies during the build
  expect_equal(sort(x$contexts[[1]][["values"]][["find"]]), c("09:00:00", "11:30:00"))

  # The seconds-since-midnight representation must not reach the metadata file
  expect_false(any(grepl("32400", readLines("data/Test_time/metadata.yml"), fixed = TRUE)))
})


test_that("`metadata_add_traits` is working", {
  metadata <- read_metadata_dataset("Test_2022")
  metadata$traits <- NA
  write_metadata_dataset(metadata, "Test_2022")
  var_in <- c("LMA (mg mm-2)", "Leaf nitrogen (mg mg-1)")
  expect_message(
    x <- metadata_add_traits("Test_2022",
      # Gives responses for user input, for testing
      user_responses = list(var_in = var_in)),
    ".*(?=Following traits added to metadata)", perl = TRUE
  )
  expect_equal(lapply(x$traits, "[[", "var_in") %>% unlist(), var_in)
  expect_equal(lapply(x$traits, "[[", "unit_in") %>% unlist() %>% unique(), "unknown")

  # Check if traits get appended, not overwritten and that duplicate traits are not added
  suppressMessages(
    expect_message(
      x <- metadata_add_traits("Test_2022",
        # Gives responses for user input, for testing
        user_responses = list(var_in = c("LMA (mg mm-2)", "Leaf nitrogen (mg mg-1)", "LASA1000"))),
      ".*(?=Following traits already exist in the metadata).*", perl = TRUE
    ))
  expect_equal(x$traits[[3]]$var_in, "LASA1000")
  expect_error(x$traits[[4]])
})


test_that("`metadata_add_substitution` is working", {
  expect_message(
    metadata_add_substitution("Test_2022", "leaf_mass_per_area", "leaf_area", "leaf_mass_per_area"),
    ".*(?=Adding substitution in)", perl = TRUE
  )
  x <- read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]
  expect_length(x, 3)
  expect_equal(x$trait_name, "leaf_mass_per_area")
  expect_equal(x$find, "leaf_area")
  expect_equal(x$replace, "leaf_mass_per_area")

  # Test if substitution already exists
  expect_message(
    metadata_add_substitution("Test_2022", "leaf_mass_per_area", "leaf_area", "leaf_mass_per_area"),
    ".*(?=Substitution already exists for)", perl = TRUE
  )
  # Expect that second substitution should not exist
  expect_error(read_metadata("data/Test_2022/metadata.yml")$substitutions[[2]])

  # Test that a new substitution is appended
  expect_message(
    metadata_add_substitution("Test_2022", "leaf_length", "small", "large"),
    ".*(?=Adding substitution in)", perl = TRUE
  )
  # Second substitution should now exist
  expect_no_error(x <- read_metadata("data/Test_2022/metadata.yml")$substitutions[[2]])
  expect_length(x, 3)
  expect_equal(x$trait_name, "leaf_length")

  # Third substitution should not exist yet
  expect_error(x <- read_metadata("data/Test_2022/metadata.yml")$substitutions[[3]])
  # Test if substitution is appended if there is the same `find` value as before but
  # for a different `trait_name` that already exists in substitutions
  expect_message(
    metadata_add_substitution("Test_2022", "leaf_mass_per_area", "small", "large"),
    ".*(?=Adding substitution in)", perl = TRUE
  )
  # Third substitution should now exist
  expect_no_error(x <- read_metadata("data/Test_2022/metadata.yml")$substitutions[[3]])
  expect_length(x, 3)
  expect_equal(x$trait_name, "leaf_mass_per_area")
})


testthat::test_that("`metadata_add_substitutions_table` is working", {

  # Test if error is thrown if column doesn't exist in substitutions table
  substitutions_df <- tibble::tibble(
    dataset_id = c("Test_2022", "Test_2022", "Test_2022"),
    trait_name = c("fruit_colour", "plant_growth_form", "plant_growth_form"),
    find = c("red", "shrubby", "palm"),
    replace = c("black", "shrub", "tree")
  )
  expect_error(
    metadata_add_substitutions_table(substitutions_df, "dataset", "trait_name", "find", "replace"),
    ".*(?=is not a column in the substitutions table)", perl = TRUE
  )

  # Overwrite `substitutions` section to NA
  path_metadata <- "data/Test_2022/metadata.yml"
  metadata <- read_metadata(path_metadata)
  metadata$substitutions <- NA
  write_metadata(metadata, path_metadata)

  # Check adding substitutions from table
  expect_message(
    metadata_add_substitutions_table(substitutions_df, "dataset_id", "trait_name", "find", "replace"),
    ".*(?=Substitutions have been added for)", perl = TRUE
  )

  # Test if any of the substitutions contain "shrubby" in the `find` fields
  expect_true(
    read_metadata(path_metadata)$substitutions %>% lapply("[[", "find") %>% lapply("==", "shrubby") %>% unlist %>% any
  )

  # Test if alternate naming of columns in substitutions table works
  # Also test if it works if substitutions already exist
  substitutions_df <- tibble::tibble(
    dataset = c("Test_2022", "Test_2022", "Test_2022"),
    trait = c("fruit_colour", "plant_growth_form", "plant_growth_form"),
    find = c("red", "shrubby", "palm"),
    replace = c("black", "shrub", "tree")
  )

  # Test that a message about duplicate find values was outputted
  suppressMessages(expect_message(
    metadata_add_substitutions_table(substitutions_df, "dataset", "trait", "find", "replace"),
    "d*(?=already exists, but new substitution has been added)", perl = TRUE
  ))
  # Expect that two substitutions contain "shrubby" in the `find` fields
  expect_equal(
    read_metadata(path_metadata)$substitutions %>%
    lapply("[[", "find") %>% lapply("==", "shrubby") %>% unlist %>% sum, 2
  )

  # Also test if substitutions can be added a third time
  expect_silent(
    suppressMessages(
      metadata_add_substitutions_table(substitutions_df, "dataset", "trait", "find", "replace"))
  )
  # Expect that three substitutions contain "shrubby" in the `find` fields
  expect_equal(
    read_metadata(path_metadata)$substitutions %>%
    lapply("[[", "find") %>% lapply("==", "shrubby") %>% unlist %>% sum, 3
  )
})


testthat::test_that("`metadata_add_substitutions_list` is working", {
  substitutions_df <- tibble::tibble(
    trait_name = c("fruit_colour", "plant_growth_form", "plant_growth_form"),
    find = c("red", "shrubby", "palm"),
    replace = c("black", "shrub", "tree")
  )
  expect_message(
    metadata_add_substitutions_list("Test_2022", substitutions_df),
    "Existing substitutions have been overwritten."
  )
  # Expect that this function overwrites existing substitutions, so fourth substitutions should not exist
  expect_error(read_metadata("data/Test_2022/metadata.yml")$substitutions[[4]])
})


test_that("`metadata_exclude_observations` is working", {
  expect_message(
    metadata_exclude_observations("Test_2022", "test", "stem", "reason"),
    ".*(?=excluding)", perl = TRUE
  )

  x <- read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]
  expect_equal(x$variable, "test")
  expect_equal(x$find, "stem")
  expect_equal(x$reason, "reason")

  # Test if observation is already excluded
  expect_message(
    metadata_exclude_observations("Test_2022", "test", "stem", "reason2"),
    ".*(?=Exclusion already exists for)", perl = TRUE
  )
  # Expect that second excluded observation should not exist
  expect_error(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[2]])

  # Test if substitution is appended
  expect_message(
    metadata_exclude_observations("Test_2022", "test", "branch", "reason"),
    ".*(?=excluding)", perl = TRUE
  )
  # Expect that second excluded observation exists
  expect_no_error(read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[2]])
})


test_that("`metadata_add_taxonomic_change` is working", {
  metadata <- read_metadata_dataset("Test_2022")
  metadata$taxonomic_updates <- NA
  write_metadata_dataset(metadata, "Test_2022")
  expect_message(
    metadata_add_taxonomic_change("Test_2022", "flower", "tree", "leaves", "test resolution"),
    ".*(?=Adding taxonomic change in )", perl = TRUE
  )

  x <- read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]
  expect_equal(x$find, "flower")
  expect_equal(x$replace, "tree")
  expect_equal(x$reason, "leaves")
  expect_equal(x$taxonomic_resolution, "test resolution")

  # Test if taxonomic substitution already exists
  expect_message(
    metadata_add_taxonomic_change("Test_2022", "flower", "tree", "leaves", "test resolution",overwrite=FALSE),
    ".*(?=Substitution already exists for )", perl = TRUE
  )
  # Expect that second substitution should not exist
  expect_error(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[2]])

  # Test that a new taxonomic substitution is appended
  expect_message(
    metadata_add_taxonomic_change("Test_2022", "test", "replacement", "test reason", "test resolution"),
    ".*(?=Adding taxonomic change in)", perl = TRUE
  )
  # Second substitution should now exist
  expect_no_error(x <- read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[2]])
  expect_length(x, 4)
  expect_equal(x$find, "test")
})


test_that("`metadata_update_taxonomic_change` is working", {
  # Test that `metadata_update_taxonomic_change` gives a message if the substitution does not exist
  expect_message(
    metadata_update_taxonomic_change("Test_2022", "test species", "new species", "new name", "genus"),
    ".*(?=does not exist)", perl = TRUE
  )

  expect_message(
    metadata_update_taxonomic_change("Test_2022", "test", "new name", "update", "subspecies"),
    ".*(?=Updating taxonomic change in )", perl = TRUE
  )

  x <- read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates
  # Find existing taxonomic change where `find` == test
  y <- x[x %>% lapply("[[", "find") %>% lapply("==", "test") %>% unlist()][[1]]
  expect_equal(y$find, "test")
  expect_equal(y$replace, "new name")
  expect_equal(y$reason, "update")
  expect_equal(y$taxonomic_resolution, "subspecies")

  # Test if `taxonomic_updates` is empty
  metadata <- read_metadata_dataset("Test_2022")
  metadata$taxonomic_updates <- NA
  write_metadata_dataset(metadata, "Test_2022")
  expect_message(
    metadata_update_taxonomic_change("Test_2022", "test species", "new species", "new name", "genus"),
    " does not exist"
  )
})


test_that("`metadata_remove_taxonomic_change` is working", {

  # Test if `taxonomic_updates` is empty
  expect_message(
    metadata_remove_taxonomic_change("Test_2022", "test species"),
    ".*(?=to remove)", perl = TRUE
  )

  expect_no_error(
    suppressMessages(
      metadata_add_taxonomic_change("Test_2022", "flower", "tree", "leaves", "test resolution")
  ))

  # Test if taxonomic change does not exist
  expect_message(
    metadata_remove_taxonomic_change("Test_2022", "species to remove"),
    ".*(?=does not exist)", perl = TRUE
  )
  # Test that existing taxonomic change is not removed
  expect_no_error(x <- read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]])
  expect_equal(x$find, "flower")

  expect_message(metadata_remove_taxonomic_change("Test_2022", "flower"))
  # Test that `taxonomic_updates` is now NA
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates, NA)
})


testthat::test_that("`metadata_add_taxonomic_changes_list` is working", {
  taxonomic_changes <- tibble::tibble(
    find = c("species 1", "species 2", "species 3"),
    replace = c("new 1", "new 2", "new 3"),
    reason = c("test reason 1", "test reason 2", "test reason 3"),
    taxonomic_resolution = c("species", "variety", "subspecies")
  )
  expect_silent(metadata_add_taxonomic_changes_list("Test_2022", taxonomic_changes))
  extra_taxonomic_changes <- tibble::tibble(
    find = c("species 1", "species 2", "species 4"),
    replace = c("replaced species 1", "replaced species 2", "new 4"),
    reason = c("taxonomy change", "another taxonomy change", "test reason 4"),
    taxonomic_resolution = c("form", "species", "variety")
  )
  expect_message(
    metadata_add_taxonomic_changes_list("Test_2022", extra_taxonomic_changes),
    ".*(?=already exist)", perl = TRUE
  )
  # Expect that this function appends to existing substitutions, so fourth substitutions should exist
  expect_no_error(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[4]])
  # but fifth substitution should not, because two of the substitutions already exist
  expect_error(read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[5]])
})

testthat::test_that("`metadata_find_taxonomic_change` is working", {
  expect_message(
    metadata_find_taxonomic_change(find = "species 1", studies = c("Test_2022", "Test_2022_2")),
    ".*(?=The following studies contain )", perl = TRUE
  )
  expect_message(
    metadata_find_taxonomic_change(find = "species 2", replace = "new 2", studies = c("Test_2022", "Test_2022_2")),
    ".*(?=The following studies contain )", perl = TRUE
  )
  expect_message(
    metadata_find_taxonomic_change(find = "species 10", studies = c("Test_2022", "Test_2022_2")),
    ".*(?=No studies contain)", perl = TRUE
  )
    expect_message(
    metadata_find_taxonomic_change(find = "species 10", replace = "new 10", studies = c("Test_2022", "Test_2022_2")),
    ".*(?=No studies contain)", perl = TRUE
  )
})


test_that("`build_setup_pipeline` is working", {

  unlink("remake.yml")
  unlink("build.R")
  unlink("R/custom_R_code.R")
  unlink("config/taxon_list.csv")
  unlink(".git", recursive = TRUE)
  expect_false(file.exists("remake.yml"))
  expect_false(file.exists("config/taxon_list.csv"))
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))

  expect_no_error(zip::unzip("config/testgit.zip"))
  # Expect no error if not within a git repo
  expect_no_error(sha <- util_get_SHA("../../.."))
  expect_no_error(sha <- util_get_SHA())
  # Expect error if path or method is wrong
  expect_error(build_setup_pipeline(path = "Datas"))
  expect_error(build_setup_pipeline(method = "grrrr"))

  # Base workflow
  expect_silent(suppressMessages(build_setup_pipeline(method = "base")))
  expect_true(file.exists("build.R"))
  expect_true(file.exists("config/taxon_list.csv"))
  expect_true(file.exists("R/custom_R_code.R"))

  ## Check details on taxon list
  expect_silent(taxa1 <- read_csv_char("config/taxon_list.csv"))

  vars <- c("taxon_name", "aligned_name", "taxon_rank")
  expect_contains(names(taxa1), vars)
  expect_true(length(names(taxa1)) > 2)
  expect_true(nrow(taxa1) == 0)
  expect_true(file.copy("config/taxon_list-orig.csv", "config/taxon_list.csv", TRUE))
  expect_silent(taxa2 <- read_csv_char("config/taxon_list.csv"))
  expect_contains(names(taxa2), vars)
  expect_true(length(names(taxa2)) > 2)
  expect_true(nrow(taxa2) == 261)

  ## Now try building in a controlled env, using base method
  base_tmp_env <- new.env()
  expect_silent(suppressMessages(source("build.R", local = base_tmp_env)))

  targets <- c(
    "database", "database_raw", "definitions", "git_SHA", "resource_metadata", "schema", "taxon_list",
    "Test_2022", "Test_2022_config", "Test_2022_raw", "unit_conversions", "version_number"
  )
  expect_equal(sort(names(base_tmp_env)), sort(targets))

  # `furrr` workflow
  furrr_tmp_env <- new.env()
  expect_silent(suppressMessages(build_setup_pipeline(method = "furrr")))

  expect_true(file.exists("build.R"))
  expect_true(file.exists("config/taxon_list.csv"))
  expect_true(file.exists("R/custom_R_code.R"))

  expect_silent(suppressMessages(source("build.R", local = furrr_tmp_env)))

  targets <- c(
    "database", "database_raw", "dataset_ids", "f", "definitions", "git_SHA", "resource_metadata",
    "schema", "sources", "taxon_list", "unit_conversions", "version_number"
  )
  expect_equal(sort(names(furrr_tmp_env)), sort(targets))

  out1 <- get("Test_2022", envir = base_tmp_env)
  out2 <- get("sources", envir = furrr_tmp_env)[["Test_2022"]]

  # don't compare build_info, as these differ through packages used.
  expect_equal(out1[names(out1) != "build_info"], out2[names(out2) != "build_info"])

  # Remake workflow
  expect_silent(suppressMessages(build_setup_pipeline(method = "remake")))
  expect_true(file.exists("remake.yml"))
  expect_silent(yaml::read_yaml("remake.yml"))
  expect_true(file.exists("config/taxon_list.csv"))

  unlink(".remake", recursive = TRUE)
  expect_silent(suppressMessages(austraits_raw <- remake::make("database_raw")))
  expect_silent(suppressMessages(austraits <- remake::make("database")))

  # Save output for future tests on database
  saveRDS(austraits, "test_austraits.rds")

  # Test that austraits_raw has no version number or git_SHA
  expect_null(austraits_raw$build_info$version)
  expect_null(austraits_raw$build_info$git_SHA)
  # Test that austraits has version and git_SHA from testgit folder
  expect_equal(austraits$build_info$version, "5.0.0")
  expect_type(austraits$build_info$git_SHA, "character")
  expect_equal(austraits$build_info$git_SHA, sha)
  expect_equal(austraits$build_info$git_SHA, "6c73238d8d048781d9a4f5239a03813be313f0dd")

  # Check output lists have required parts
  ## Todo add mode here

  ## sources

  ## metadata

  ## schema
  expect_equal(austraits$schema, get_schema())

  ## Compiled by traits.build
  traits.build_tag <- last(austraits$metadata$related_identifiers)
  expected_output <- list(
    related_identifier_type = "url",
    identifier = "https://github.com/traitecoevo/traits.build",
    relation_type = "isCompiledBy",
    resource_type = "software",
    version = as.character(packageVersion("traits.build"))
  )
  expect_equal(traits.build_tag, expected_output)

  expect_equal(nrow(austraits$taxa), nrow(austraits_raw$taxa))

  # Compare products from three methods, except `build_info`
  v <- setdiff(names(austraits), "build_info")
  expect_equal(base_tmp_env$database[v], austraits[v])
  expect_equal(furrr_tmp_env$database[v], austraits[v])

  # Try building database with a different name with base method
  expect_silent(suppressMessages(build_setup_pipeline(method = "base", database_name = "test_name")))

  base_tmp_env <- new.env()
  expect_silent(suppressMessages(source("build.R", local = base_tmp_env)))

  targets <- c(
    "test_name", "test_name_raw", "definitions", "git_SHA", "resource_metadata", "schema", "taxon_list",
    "Test_2022", "Test_2022_config", "Test_2022_raw", "unit_conversions", "version_number"
  )
  expect_equal(sort(names(base_tmp_env)), sort(targets))

  # Try building database with a different name with `remake` method
  expect_silent(suppressMessages(build_setup_pipeline(method = "remake", database_name = "test_name")))
  expect_silent(suppressMessages(test_name_raw <- remake::make("test_name_raw")))
  expect_silent(suppressMessages(test_name <- remake::make("test_name")))
})


testthat::test_that("`dataset_find_taxon` is working", {
  expect_silent(suppressMessages(austraits <- remake::make("test_name")))
  taxon <- c("Acacia celsa", "Acronychia acidula", "Aleurites rockinghamensis", "Syzygium sayeri")
  expect_no_error(x <- dataset_find_taxon(taxon, austraits))
  expect_equal(unname(x[[4]]), "Test_2022")
  expect_equal(names(x[[4]]), "Syzygium sayeri")
})


test_that("reports and plots are produced", {
  # This used to assert only `expect_silent()`. `dataset_report()` renders in a
  # child process, so a failure there writes to that process's stderr and
  # `expect_silent()` cannot see it -- and `dataset_report()` swallowed the
  # failure anyway. The test passed on a render that had errored, and asserted
  # nothing about the report existing or containing anything (#244).
  expect_silent(suppressMessages(austraits <- remake::make("test_name")))

  # The report calls `plot_trait_distribution_jitter()`, which uses `forcats`
  # -- declared in this package's Suggests and used unguarded, so without it
  # the whole render fails and no file is written at all. Skipping rather than
  # failing keeps a missing optional dependency from reading as a regression
  # here; `forcats` is in this package's Suggests so CI does run it.
  skip_if_not_installed("forcats")

  # The report template is a `.qmd`, rendered by shelling out to the `quarto`
  # CLI (`quarto::quarto_render()`), which is not installed everywhere `forcats`
  # is. Beyond that, the CLI always loads traits.build via its own fresh
  # `library(traits.build)`, in a separate process `devtools::load_all()` has
  # no reach into -- so this test exercises whatever is *installed*, not this
  # session's edits. `R CMD check` installs the package fresh before running
  # tests, so CI genuinely covers this; running it via plain `devtools::test()`
  # after editing the plotting functions or the template needs
  # `devtools::install()` first, or this test silently checks stale code.
  skip_if_not_installed("quarto")
  skip_if(is.null(quarto::quarto_path()), "Quarto CLI not found")

  output_path <- withr::local_tempdir()
  output_html <- file.path(output_path, "Test_2022.html")

  expect_no_warning(
    suppressMessages(
      built <- dataset_report(
        dataset_id = "Test_2022", austraits = austraits,
        output_path = output_path, overwrite = TRUE
      )
    ))

  # The report was actually written, not merely attempted
  expect_true(file.exists(output_html))
  expect_gt(file.size(output_html), 10000)

  # ...and carries the sections it is built to carry, rather than being a
  # rendered shell with the failing chunks silently dropped
  report <- paste(readLines(output_html, warn = FALSE), collapse = "\n")
  expect_match(report, "Test_2022", fixed = TRUE)
  expect_no_match(report, "there is no package called", fixed = TRUE)
})


test_that("`dataset_report` reports a failed render instead of swallowing it", {
  # A render failure used to be discarded by a bare `try()` whose result was
  # never inspected, so the success line printed regardless and no file was
  # written. In CI that is not hypothetical: `forcats` is absent there, the
  # render failed on every run, and this test suite reported success (#244).
  expect_silent(suppressMessages(austraits <- remake::make("test_name")))

  output_path <- withr::local_tempdir()

  # A template that parses but stops during knitting, so the failure happens
  # where a real one does rather than in argument handling
  template <- file.path(withr::local_tempdir(), "boom.Rmd")
  writeLines(
    c("---", "title: t", "output: html_document",
      "params:", "    dataset_id: provide", "    austraits: provide", "---",
      "", "```{r}", "stop('deliberate render failure')", "```"),
    template
  )

  expect_warning(
    suppressMessages(
      built <- dataset_report(
        dataset_id = "Test_2022", austraits = austraits,
        output_path = output_path, overwrite = TRUE, input_file = template
      )
    ),
    "failed to build"
  )
  expect_false(built)
  expect_false(file.exists(file.path(output_path, "Test_2022.html")))
})


test_that("`dataset_report` rejects a missing template clearly", {
  # `readLines()` on a nonexistent template gave "cannot open the connection",
  # which names neither the file nor the argument
  expect_silent(suppressMessages(austraits <- remake::make("test_name")))
  expect_error(
    dataset_report(
      dataset_id = "Test_2022", austraits = austraits,
      output_path = withr::local_tempdir(), input_file = "no-such-template.Rmd"
    ),
    "Report template not found"
  )
})

testthat::test_that("`dataset_test` is working", {
  # Expect error if no `dataset_ids` argument is input
  expect_error(dataset_test())
  expect_silent(
    out <- dataset_test("Test_2022", reporter = testthat::SilentReporter))
  expect_in(
    class(out), c("SilentReporter", "Reporter", "R6"))
})


# `targets` is the caching pipeline that replaces `remake` (#17): remake is
# unmaintained and off CRAN, and a curator's normal loop is editing one dataset,
# where rebuilding all of them is the whole cost.
test_that("`build_setup_pipeline(method = 'targets')` builds the same database", {

  skip_if_not_installed("targets")

  # Same fixture state the base/remake pipelines are tested against
  file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml",
            overwrite = TRUE)
  if (!file.exists("config/taxon_list.csv")) {
    file.copy("config/taxon_list-orig.csv", "config/taxon_list.csv")
  }

  withr::defer({
    unlink("_targets", recursive = TRUE)
    unlink("_targets.R")
  })

  expect_silent(suppressMessages(build_setup_pipeline(method = "targets")))
  expect_true(file.exists("_targets.R"))

  # The generated pipeline has to be valid R, and declare the dataset's targets
  expect_silent(parse("_targets.R"))
  expect_contains(
    targets::tar_manifest(callr_function = NULL)$name,
    c("Test_2022_config", "Test_2022_raw", "Test_2022",
      "file_Test_2022_metadata", "file_Test_2022_data",
      "version_number", "git_SHA", "database")
  )

  # The export is a publishing step, not a target: `tar_make()` rebuilds the
  # compilation and leaves `export/data/curr` alone
  expect_false("file_database" %in% targets::tar_manifest(callr_function = NULL)$name)

  # `build_add_version()` is folded into the `database` target rather than given
  # one of its own, so that the database is written to the store once per
  # rebuild instead of twice
  expect_false("database_raw" %in% targets::tar_manifest(callr_function = NULL)$name)

  # `callr_function = NULL` so the pipeline runs in this session, where the
  # package under test is the one loaded by pkgload rather than an installed one
  expect_no_error(
    suppressMessages(targets::tar_make(reporter = "silent", callr_function = NULL))
  )

  from_targets <- targets::tar_read(database)

  # Same database the base pipeline produces. `build_info` records the packages
  # used, which differ between the two, so it is compared separately.
  base_env <- new.env()
  suppressMessages(build_setup_pipeline(method = "base"))
  suppressMessages(source("build.R", local = base_env))
  from_base <- get("database", envir = base_env)

  expect_equal(
    from_targets[names(from_targets) != "build_info"],
    from_base[names(from_base) != "build_info"]
  )

  # `build_export()` writes the file the pipeline deliberately does not
  exported <- build_export(path = file.path(withr::local_tempdir(), "curr"))
  expect_true(file.exists(exported))
  expect_equal(
    readRDS(exported)[names(from_targets) != "build_info"],
    from_targets[names(from_targets) != "build_info"]
  )

  # Nothing changed, so nothing rebuilds
  expect_no_error(
    suppressMessages(targets::tar_make(reporter = "silent", callr_function = NULL))
  )
  rebuilt <- targets::tar_progress()
  rebuilt <- rebuilt[rebuilt$progress == "completed", ]
  # `git_SHA` is deliberately re-read every run; nothing else should move
  expect_equal(setdiff(rebuilt$name, "git_SHA"), character(0))
})


test_that("`build_setup_pipeline` declares a controller only when parallel", {

  withr::defer({
    unlink("_targets.R")
    suppressMessages(build_setup_pipeline(method = "base"))
  })

  suppressMessages(build_setup_pipeline(method = "targets"))
  expect_false(any(grepl("crew", readLines("_targets.R"), fixed = TRUE)))

  suppressMessages(build_setup_pipeline(method = "targets", workers = 4))
  generated <- readLines("_targets.R")
  expect_true(any(grepl("crew::crew_controller_local(workers = 4)", generated,
                        fixed = TRUE)))
  expect_silent(parse("_targets.R"))
})


# Building from `_targets.R` is three steps, not one, and a curator coming from
# `remake` gets both of the other two wrong: `tar_make()` returns nothing rather
# than the database, and it does not write the export. Both the setup message
# and the generated file have to say so, with the name actually in use -- a bare
# `build_export()` errors against a pipeline named anything but `database`.
test_that("the `targets` setup names all three steps, with the database's name", {

  withr::defer({
    unlink("_targets.R")
    suppressMessages(build_setup_pipeline(method = "base"))
  })

  instructions <- testthat::capture_messages(
    build_setup_pipeline(method = "targets", database_name = "austraits")
  )
  instructions <- paste(instructions, collapse = "")

  expect_match(instructions, "targets::tar_make()", fixed = TRUE)
  expect_match(instructions, "targets::tar_read(austraits)", fixed = TRUE)
  expect_match(instructions, "build_export(\"austraits\")", fixed = TRUE)

  # The same three steps in the file itself, since that is where a curator
  # returning to the repository looks
  generated <- paste(readLines("_targets.R"), collapse = "\n")
  expect_match(generated, "targets::tar_read(austraits)", fixed = TRUE)
  expect_match(generated, "build_export(\"austraits\")", fixed = TRUE)

  # The header used to credit `remake.yml.whisker`, which is a different template
  expect_match(generated, "build_targets.whisker", fixed = TRUE)
  expect_false(grepl("remake.yml.whisker", generated, fixed = TRUE))
})


test_that("`build_export` says what to fix when the database name is wrong", {

  skip_if_not_installed("targets")

  expect_error(
    build_export("not_a_target_in_this_pipeline"),
    "must match the one given to `build_setup_pipeline\\(\\)`"
  )
})
