# testthat runs each test file with the working directory set to
# ontology/tests/testthat/. setup.R finds the repository root from there and
# every function in ontology/R/ takes absolute paths built from it, so nothing
# here needs to change directory.

source(file.path("..", "..", "scripts", "setup.R"))
ontology_require("withr")

.ontology_cache <- new.env(parent = emptyenv())

#' Build the data model into a throwaway directory
#'
#' Cached across test files in one run. Building into a temp directory rather
#' than ontology/output/ is what lets the committed artefacts serve as golden
#' fixtures: a test can compare against them without overwriting them first.
ontology_test_build <- function() {
  if (is.null(.ontology_cache$build_dir)) {
    dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())
    suppressMessages(ontology_build_data(out_dir = dir))
    .ontology_cache$build_dir <- dir
  }
  .ontology_cache$build_dir
}

#' Read a file as raw bytes, for byte-for-byte comparison
ontology_bytes <- function(path) readBin(path, "raw", file.size(path))
