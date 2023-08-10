
test_that("metadata_create_template is working", {
  # Remove the metadata file if it exists before testing `metadata_create_template`
  unlink("data/Test_2022/metadata.yml")
  expect_silent(schema <- get_schema())
  # Whether or not skip_manual is TRUE or FALSE, this test passes -- is that intended?
  expect_invisible(metadata_create_template(dataset_id = "Test_2022",
                                            path = file.path("data", "Test_2022"),
                                            skip_manual = TRUE))
  expect_no_error(test_metadata <- read_metadata("data/Test_2022/metadata.yml"))
  metadata_names <- c("source", "contributors", "dataset", "locations", "contexts", "traits",
                      "substitutions", "taxonomic_updates", "exclude_observations",
                      "questions")
  collectors_names <- c("last_name", "given_name", "ORCID", "affiliation")
  # Test names exist
  expect_named(test_metadata)
  # Test metadata exists with correct names
  expect_named(test_metadata, metadata_names)
  expect_length(test_metadata$source$primary, 10)
  expect_isin(names(test_metadata$dataset), names(schema$metadata$elements$dataset$values))
  expect_type(test_metadata$contributors$data_collectors, "list")
  expect_length(test_metadata$contributors$data_collectors, 1L)
  expect_named(test_metadata$contributors$data_collectors[[1]], collectors_names)
  # Test that metadata_create_template puts all data collector fields as "unknown" by default
  expect_equal((test_metadata$contributors$data_collectors[[1]] %>% unique)[[1]], "unknown")
 })

test_that("metadata_create_template is working with simulated user input", {
  # Remove the metadata file if it exists before testing `metadata_create_template`
  unlink("data/Test_2022/metadata.yml")
  # Check long format
  expect_no_error(
    x <- metadata_create_template(
      "Test_2022",
      user_responses = list(
        data_is_long_format = TRUE,
        taxon_name = "Species", trait_name = "trait_name", value = "value",
        location_name = NA, individual_id = "id", collection_date = "2008/2009"
    ))
  )
  # Check wide format
  expect_no_error(
    x <- metadata_create_template(
      "Test_2022",
      user_responses = list(
        data_is_long_format = FALSE,
        taxon_name = "Species",
        location_name = NA, individual_id = "id", collection_date = "2008/2009"
    ))
  )
  # Run more tests (see ideas from above)
 })


test_that("metadata_path_dataset_id is working", {
  expect_silent(metadata_path_dataset_id("Test_2022"))
  expect_equal(metadata_path_dataset_id("Test_2022"), "data/Test_2022/metadata.yml")
  expect_type(metadata_path_dataset_id("Test_2022"), "character")
})


test_that("read_metadata_dataset is working", {
  expect_silent(read_metadata_dataset("Test_2022"))
  expect_type(read_metadata_dataset("Test_2022"), "list")
})


test_that("write_metadata_dataset is working", {
  metadata <- read_metadata_dataset("Test_2022")

  unlink("data/Test_2022/metadata.yml")
  expect_false(file.exists("data/Test_2022/metadata.yml"))

  expect_silent(write_metadata_dataset(metadata, "Test_2022"))
  expect_true(file.exists("data/Test_2022/metadata.yml"))

  expect_silent(read_metadata_dataset("Test_2022"))
  expect_type(read_metadata_dataset("Test_2022"), "list")
})


test_that("metadata_add_source_doi is working", {

  doi <- "https://doi.org/10.3389/fmars.2021.671145"
  doi2 <- "https://doi.org/10.1111/j.0022-0477.2005.00992.x"

  expect_equal(doi, util_standardise_doi(doi))
  expect_equal(doi, util_standardise_doi("http://doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, util_standardise_doi("doi.org/10.3389/fmars.2021.671145"))
  expect_equal(doi, util_standardise_doi("10.3389/fmars.2021.671145"))

  # We won't actually test querying of rcrossref, to avoid unnecessary fails
  # Passing in .bib files avoids calling crossref

  # Create and load test data
  # I believe rcrossref::cr_cn prefers doi to be in this format "10.3389/fmars.2021.671145" otherwise
  # it throws out a warning (see this issue: https://github.com/ropensci/rcrossref/issues/226)
  # bib <- rcrossref::cr_cn(doi)
  # The working directory of the other tests is in "tests/testthat" so should that also be the same
  # for here, i.e. the commented out lines?
  # writeLines(bib, "tests/testthat/data/test.bib")
  # bib2 <- rcrossref::cr_cn(doi2)
  # writeLines(bib2, "tests/testthat/data/test2.bib")
  bib <- readLines("data/test.bib") %>% paste(collapse = "\n")
  bib2 <- readLines("data/test2.bib") %>% paste(collapse = "\n")
  # I noticed that because the bib information has authors in all caps, it's entered into AusTraits that way
  # Should we add some code to standardise capitalisation?
  # Also the key is "Test_2022" for both primary and secondary sources
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = doi, bib = bib))
  expect_invisible(metadata_add_source_doi(dataset_id = "Test_2022", doi = doi2, bib = bib2, type = "secondary"))

  test_metadata <- read_metadata("data/Test_2022/metadata.yml")
  expect_equal(test_metadata$source$primary$journal, "Frontiers in Marine Science")
  expect_equal(test_metadata$source$primary$year, "2021")
  expect_equal(paste0("https://doi.org/", test_metadata$source$primary$doi), doi)

  expect_equal(test_metadata$source$secondary$journal, "Journal of Ecology")
  expect_equal(test_metadata$source$secondary$year, "2005")
  expect_equal(paste0("https://doi.org/", test_metadata$source$secondary$doi), doi2)
})

test_that("metadata_check_custom_R_code is working", {
  # Check that the `custom_R_code` produces a tibble class object
  expect_equal(class(metadata_check_custom_R_code("Test_2022")), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(metadata_check_custom_R_code("Test_2022")), 13)
  expect_equal(nrow(metadata_check_custom_R_code("Test_2022")), 45)
  expect_visible(metadata_check_custom_R_code("Test_2022"))
})

test_that("metadata_add_source_bibtex is working", {
  expect_silent(metadata_add_source_bibtex(dataset_id = "Test_2022", file = "data/test2.bib"))
  expect_equal(read_metadata("data/Test_2022/metadata.yml")$source$primary$journal, "Journal of Ecology")
})

test_that("metadata_add_locations is working", {
  locations <- tibble(
    site_name = c("site 1", "site 2"),
    latitude = c("-16", "-17"),
    longitude = c("145", "146"),
    elevation = c("100", "150"))

  expect_no_error(
    suppressMessages(
      x <- metadata_add_locations("Test_2022", locations,
        # Gives responses for user input, for testing
        user_responses = list(location_name = "site_name", keep = c("latitude", "longitude", "elevation"))
      )
    )
  )
  expect_equal(names(x$locations), locations$site_name)
  # Add more tests
  expect_equal(lapply(x$locations, "[[", "latitude") %>% unlist() %>% as.character(), locations$latitude)
})

test_that("metadata_add_contexts is working", {
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))
  var_in <- c("test_context_1", "test_context_2")
  categories <- c("treatment", "entity_context")

  expect_no_error(
    suppressMessages(
      x <- metadata_add_contexts(
        "Test_2022",
        user_responses = list(
          var_in = var_in,
          categories = categories,
          replace_needed = c("y", "n")
      ))
    ))

  expect_equal(lapply(x$contexts, "[[", "context_property") %>% unlist() %>% unique, "unknown")
  expect_equal(lapply(x$contexts, "[[", "category") %>% unlist(), categories)
  expect_equal(lapply(x$contexts, "[[", "var_in") %>% unlist(), var_in)
  # Expect `find` column for context variable where replacements are needed
  expect_no_error(x$contexts[[1]][["values"]] %>% pull("find"))
  # Expect no `find` column for context variable where replacements are not needed
  expect_error(x$contexts[[2]][["values"]] %>% pull("find"))
})


test_that("metadata_add_traits is working", {
  metadata <- read_metadata_dataset("Test_2022")
  metadata$traits <- NA
  write_metadata_dataset(metadata, "Test_2022")
  var_in <- c("LMA (mg mm-2)", "Leaf nitrogen (mg mg-1)")
  expect_no_error(
    suppressMessages(
    x <- metadata_add_traits("Test_2022",
      # Gives responses for user input, for testing
      user_responses = list(var_in = var_in)
    ))
  )
  expect_equal(lapply(x$traits, "[[", "var_in") %>% unlist(), var_in)
  expect_equal(lapply(x$traits, "[[", "unit_in") %>% unlist() %>% unique(), "unknown")
})


test_that("metadata_add_substitution is working", {
  expect_silent(
    suppressMessages(
      metadata_add_substitution("Test_2022", "leaf_mass_per_area", "leaf_area", "leaf_mass_per_area")
    ))

  x <- read_metadata("data/Test_2022/metadata.yml")$substitutions[[1]]
  expect_length(x, 3)
  expect_equal(x$trait_name, "leaf_mass_per_area")
  expect_equal(x$find, "leaf_area")
  expect_equal(x$replace, "leaf_mass_per_area")
})


test_that("metadata_add_taxonomic_change is working", {
  # If the taxonomic substitution already exists, this throws an uninformative error
  # Also if ANY taxonomic substitution already exists, this throws an error I think
  # Do we want to add in a similar message like with `metadata_add_substitution`?
  expect_output(metadata_add_taxonomic_change("Test_2022", "flower", "tree", "leaves", "Tissue"))

  x <- read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]
  expect_equal(x$find, "flower")
  expect_equal(x$replace, "tree")
  expect_equal(x$reason, "leaves")
  expect_equal(x$taxonomic_resolution, "Tissue")
})


test_that("metadata_exclude_observations is working", {
  # If the observation is already excluded, this throws an uninformative error
  # Do we want to add in a similar message like with `metadata_add_substitution`?
  expect_output(metadata_exclude_observations("Test_2022", "stem", "branch", "test"))

  x <- read_metadata("data/Test_2022/metadata.yml")$exclude_observations[[1]]
  expect_equal(x$variable, "stem")
  expect_equal(x$find, "branch")
  expect_equal(x$reason, "test")
})


test_that("metadata_update_taxonomic_change is working", {
  # Test that `metadata_update_taxonomic_change` throws an error if the substitution does not exist
  # Can we add a more informative error message?
  expect_error(metadata_update_taxonomic_change("Test_2022", "grass", "bark", "soil", "Substrate"))
  expect_invisible(
    suppressMessages(
      metadata_update_taxonomic_change("Test_2022", "flower", "bark", "soil", "Substrate")
    ))

  x <- read_metadata("data/Test_2022/metadata.yml")$taxonomic_updates[[1]]
  expect_equal(x$find, "flower")
  expect_equal(x$replace, "bark")
  expect_equal(x$reason, "soil")
  expect_equal(x$taxonomic_resolution, "Substrate")
})


test_that("metadata_remove_taxonomic_change is working", {
  # Can we add an informative error message here too if there's no substitution to remove?
  # Also this replaces the taxonomic_updates section with an empty list, preventing you from using
  # metadata_add_taxonomic_change() again
  expect_invisible(metadata_remove_taxonomic_change("Test_2022", "flower"))
})


test_that("dataset_test is working", {
  # Expect error if no dataset_ids argument is input
  expect_error(dataset_test())
})


test_that("build_setup_pipeline is working", {

  unlink("remake.yml")
  unlink("config/taxon_list.csv")
  unlink(".git", recursive = TRUE)
  expect_false(file.exists("remake.yml"))
  expect_false(file.exists("config/taxon_list.csv"))
  expect_true(file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE))

  expect_no_error(zip::unzip("config/testgit.zip"))
  expect_no_error(sha <- git2r::sha(git2r::last_commit()))
  # Expect error if path name is wrong
  expect_error(build_setup_pipeline(path = "Datas"))
  # Should we add to build_setup_pipeline documentation that it also makes taxon_list.csv?
  expect_silent(build_setup_pipeline())
  expect_true(file.exists("remake.yml"))
  expect_silent(yaml::read_yaml("remake.yml"))
  expect_true(file.exists("config/taxon_list.csv"))
  expect_silent(taxa1 <- read_csv_char("config/taxon_list.csv"))

  vars <-
    c("cleaned_name", "taxonomic_reference", "cleaned_scientific_name_id",
    "cleaned_name_taxonomic_status", "cleaned_name_alternative_taxonomic_status",
    "taxon_name", "taxon_id", "scientific_name_authorship", "taxon_rank",
    "taxonomic_status", "family", "taxon_distribution", "establishment_means",
    "scientific_name", "scientific_name_id")
  expect_named(taxa1, vars)
  expect_length(taxa1, 15)
  expect_true(nrow(taxa1) == 0)
  expect_true(file.copy("config/taxon_list-orig.csv", "config/taxon_list.csv", TRUE))
  expect_silent(taxa2 <- read_csv_char("config/taxon_list.csv"))
  expect_named(taxa2, vars)
  expect_length(taxa2, 15)
  expect_true(nrow(taxa2) == 7)

  unlink(".remake", recursive = TRUE)
  expect_no_error(austraits_raw <- remake::make("austraits_raw"))
  expect_no_error(austraits <- remake::make("austraits"))

  # Note: austraits_raw has no version number or git_SHA, austraits does
  expect_null(austraits_raw$build_info$version)
  expect_null(austraits_raw$build_info$git_SHA)
  # Test that austraits has version and git_SHA from testgit folder
  expect_equal(austraits$build_info$version, "4.0.0")
  expect_type(austraits$build_info$git_SHA, "character")
  expect_equal(austraits$build_info$git_SHA, sha)
  expect_equal(austraits$build_info$git_SHA, "6c73238d8d048781d9a4f5239a03813be313f0dd")

  expect_length(austraits_raw$taxa, 14)
  expect_length(austraits$taxa, 14)
  expect_equal(nrow(austraits$taxa), nrow(austraits_raw$taxa))
})


test_that("reports and plots are produced", {
  expect_no_error(austraits <- remake::make("austraits"))
  expect_no_error(
    # What's this p for? I'm guessing it was for the plot
    p <- 1
    #austraits::plot_trait_distribution_beeswarm(
      #austraits, "huber_value", "dataset_id", highlight = "Test_2022", hide_ids = TRUE)
  )
  expect_no_error(
    dataset_report(dataset_id = "Test_2022", austraits = austraits, overwrite = TRUE)
  )
})


testthat::test_that("dataset_test is working", {
  expect_silent(
    out <- dataset_test("Test_2022", reporter = testthat::SilentReporter))
  expect_in(
    c("SilentReporter", "Reporter", "R6"), class(out))
})


testthat::test_that("metadata_add_substitutions_table is working", {
  substitutions_df <- tibble::tibble(
    dataset_id = "Test_2022",
    trait_name = "Tree",
    find = "Root",
    replace = "Branch"
  )

  path_metadata <- "data/Test_2022/metadata.yml"
  # I think this creates it in the wrong directory? It creates a metadata file in "data/", not "data/Test_2022/"
  # Maybe that's intended.
  metadata_create_template(
    dataset_id = "Test_2022",
    path = "data",
    skip_manual = TRUE
  )

  metadata <- read_metadata(path_metadata)
  metadata$substitutions <- NA
  write_metadata(metadata, path_metadata)
  # I think the arguments after the first one are unnecessary, it works without putting them in
  # Also you can add substitutions twice with no errors, but the third time it throws an uninformative error
  expect_invisible(metadata_add_substitutions_table(substitutions_df, "Test_2022", "trait_name", "find", "replace"))
  # Test if any of the substitutions contain "Tree" in any fields
  expect_equal(read_metadata(path_metadata)$substitutions %>% sapply(`%in%`, x = "Tree") %>% any(), TRUE)
})
