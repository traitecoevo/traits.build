test_that("the version comes from the newest change-log heading", {
  news <- withr::local_tempfile(fileext = ".md")
  writeLines(c(
    "# change log",
    "## traits.build data model 1.2.0",
    "- things",
    "## traits.build data model 1.1.0",
    "## traits.build data model 1.0.0"
  ), news)

  expect_identical(ontology_released_versions(news), c("1.2.0", "1.1.0", "1.0.0"))
  expect_identical(ontology_version(news), "1.2.0")
  expect_identical(ontology_previous_version("1.2.0", news), "1.1.0")
  expect_identical(ontology_previous_version("1.0.0", news), "1.2.0")
})

test_that("a change log with no version heading is an error", {
  news <- withr::local_tempfile(fileext = ".md")
  writeLines(c("# change log", "## Unreleased"), news)
  expect_error(ontology_version(news), "no `## traits.build data model")
})

test_that("the previous version has a release snapshot", {
  previous <- ontology_previous_version()
  skip_if(is.na(previous), "first release")
  expect_true(dir.exists(file.path(ontology_output_dir(), "release", previous)))
})

test_that("the package writes the data model version this change log builds", {
  # R CMD check ignores ontology/, so the package cannot read the version from
  # ontology/NEWS.md at run time; R/data_model_version.R repeats it, and every
  # database the package builds records it in `metadata$data_model_version`
  # for readers to check (traits.use#40). Adding a heading here without
  # updating that file would stamp databases with the old version.
  env <- new.env()
  sys.source(repo_path("R", "data_model_version.R"), envir = env)
  expect_identical(env$data_model_version(), ontology_version())
})
