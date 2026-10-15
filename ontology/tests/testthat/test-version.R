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
