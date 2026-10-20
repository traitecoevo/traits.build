## The post-build step (#197): a database repository's `post_build()` in
## `R/post_build.R`, run by `build_post_process()` after `build_add_version()`.
##
## The unit tests use a small stand-in database and a file in a temporary
## directory. The pipeline tests write `R/post_build.R` into this directory,
## which is the database repository every pipeline test builds, so each one
## removes it again: left behind, it would run in every later build.

# A stand-in with the shape `build_post_process()` checks: tables, lists and a
# `build_info` as `build_add_version()` leaves it
post_build_test_database <- function() {
  database <- list(
    traits = tibble::tibble(dataset_id = "Test_2022", observation_id = "01", trait_name = "leaf_area", value = "1"),
    locations = tibble::tibble(dataset_id = "Test_2022", location_id = "01"),
    metadata = list(title = "A database"),
    build_info = list(version = "5.0.0", git_SHA = "abc", session_info = "session")
  )
  class(database) <- "traits.build"
  database
}

# Writes `code` to a `post_build.R` that lasts as long as the calling test
local_post_build <- function(code, envir = parent.frame()) {
  file <- file.path(withr::local_tempdir(.local_envir = envir), "post_build.R")
  writeLines(code, file)
  file
}


test_that("`build_post_process` returns the database unchanged without a file", {

  database <- post_build_test_database()

  expect_identical(build_post_process(database, file.path(withr::local_tempdir(), "post_build.R")), database)
  # What the `targets` pipeline passes when there is no `R/post_build.R`
  expect_identical(build_post_process(database, character(0)), database)

  expect_error(build_post_process(database, c("a.R", "b.R")), "single path")
})


test_that("`build_post_process` replaces the database with what `post_build` returns", {

  file <- local_post_build(c(
    "post_build <- function(database) {",
    "  database$traits <- mutate(database$traits, curve_id = paste(dataset_id, observation_id, sep = '-'))",
    "  database",
    "}"
  ))
  database <- post_build_test_database()

  expect_message(processed <- build_post_process(database, file), "Running `post_build()`", fixed = TRUE)

  expect_equal(processed$traits$curve_id, "Test_2022-01")
  expect_equal(processed$locations, database$locations)
  expect_s3_class(processed, "traits.build")

  # `build_info` keeps what `build_add_version()` wrote, and records the step
  expect_equal(processed$build_info[names(database$build_info)], database$build_info)
  expect_equal(
    processed$build_info$post_build,
    list(file = file, md5 = unname(tools::md5sum(file)))
  )
})


test_that("`post_build` may write files", {

  written <- file.path(withr::local_tempdir(), "traits.csv")
  file <- local_post_build(c(
    "post_build <- function(database) {",
    sprintf("  readr::write_csv(database$traits, '%s')", written),
    "  database",
    "}"
  ))

  processed <- suppressMessages(build_post_process(post_build_test_database(), file))

  expect_true(file.exists(written))
  expect_equal(read_csv_char(written)$trait_name, "leaf_area")
  expect_equal(processed[names(processed) != "build_info"],
               post_build_test_database()[names(processed) != "build_info"])
})


test_that("`build_post_process` fails clearly when `post_build` does not return the database", {

  database <- post_build_test_database()

  # The body of `post_build()`, and the problem the error should name
  cases <- list(
    # The usual slip: a function's value is its last expression, here the table
    # just assigned
    list("database$traits <- mutate(database$traits, x = 1)", "returned an object of class `tbl_df/tbl/data.frame`"),
    # ... or what a function writing a file returns
    list("saveRDS(database, tempfile())", "returned an object of class `NULL`"),
    list("database$locations <- NULL; database", "returned a database without `locations`"),
    list("database$traits <- as.list(database$traits); database", "`traits` is no longer a table")
  )

  for (case in cases) {
    file <- local_post_build(c("post_build <- function(database) {", case[[1]], "}"))
    expect_error(suppressMessages(build_post_process(database, file)), case[[2]], fixed = TRUE)
    expect_error(suppressMessages(build_post_process(database, file)), "must return the database", fixed = TRUE)
  }

  file <- local_post_build("postbuild <- function(database) database")
  expect_error(build_post_process(database, file), "must define a function `post_build(database)`", fixed = TRUE)
})


test_that("`post_build` sees attached packages and its own file, not the global environment", {

  assign("post_build_test_global", "from the global environment", envir = globalenv())
  withr::defer(rm("post_build_test_global", envir = globalenv()))

  file <- local_post_build(c(
    "helper <- function(x) toupper(x)",
    "post_build <- function(database) {",
    "  database$metadata$sees_global <- exists('post_build_test_global')",
    # `mutate()` and `str_detect()` unqualified: packages traits.build attaches
    "  database$traits <- mutate(database$traits, leafy = str_detect(trait_name, 'leaf'), name = helper(trait_name))",
    "  database",
    "}"
  ))

  processed <- suppressMessages(build_post_process(post_build_test_database(), file))

  expect_false(processed$metadata$sees_global)
  expect_true(processed$traits$leafy)
  expect_equal(processed$traits$name, "LEAF_AREA")
})


test_that("`build_post_process` keeps the database's class when `post_build` drops it", {

  file <- local_post_build("post_build <- function(database) database[names(database)]")

  processed <- suppressMessages(build_post_process(post_build_test_database(), file))

  expect_s3_class(processed, "traits.build")
})


test_that("every pipeline calls the post-build step, whether or not the file exists", {

  withr::defer({
    unlink(c("_targets.R", "remake.yml", "R/post_build.R"))
    suppressMessages(build_setup_pipeline(method = "base"))
  })
  unlink("R/post_build.R")

  # `build.R`: after `build_add_version()`, before the database is saved
  for (method in c("base", "furrr")) {
    suppressMessages(build_setup_pipeline(method = method))
    generated <- readLines("build.R")
    hook <- which(generated == "database <- build_post_process(database)")
    expect_length(hook, 1)
    expect_lt(grep("build_add_version(", generated, fixed = TRUE), hook)
    expect_gt(grep("saveRDS(", generated, fixed = TRUE), hook)
  }

  # `targets`: folded into the database target, with the file tracked. The
  # targets test below checks the manifest and runs it.
  suppressMessages(build_setup_pipeline(method = "targets"))
  expect_silent(parse("_targets.R"))
  generated <- paste(readLines("_targets.R"), collapse = "\n")
  expect_match(generated, "tar_target(\n    file_post_build,", fixed = TRUE)
  expect_match(generated, "    build_post_process(\n      build_add_version(", fixed = TRUE)

  # `remake`: its own target, depending on the file only if it exists, since
  # remake fails on a dependency that is missing
  suppressMessages(build_setup_pipeline(method = "remake"))
  remake_targets <- yaml::read_yaml("remake.yml")$targets
  expect_equal(remake_targets$database$command, 'build_post_process(database_versioned, I("R/post_build.R"))')
  expect_null(remake_targets$database$depends)
  expect_match(remake_targets$database_versioned$command, "build_add_version(", fixed = TRUE)

  writeLines("post_build <- function(database) database", "R/post_build.R")
  suppressMessages(build_setup_pipeline(method = "remake"))
  expect_equal(yaml::read_yaml("remake.yml")$targets$database$depends, "R/post_build.R")
})


test_that("the base pipeline runs `post_build()` and saves what it returns", {

  file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE)

  withr::defer({
    unlink("R/post_build.R")
    suppressMessages(build_setup_pipeline(method = "base"))
  })
  unlink("R/post_build.R")

  # Without the file
  suppressMessages(build_setup_pipeline(method = "base"))
  plain <- new.env()
  suppressMessages(source("build.R", local = plain))
  expect_null(plain$database$build_info$post_build)

  # With it: a new column, and a file written as a side effect
  written <- file.path(withr::local_tempdir(), "traits.csv")
  writeLines(c(
    "post_build <- function(database) {",
    "  database$traits <- mutate(database$traits, curve_id = paste(dataset_id, observation_id, sep = '-'))",
    sprintf("  readr::write_csv(database$traits, '%s')", written),
    "  database",
    "}"
  ), "R/post_build.R")

  processed <- new.env()
  expect_silent(suppressMessages(source("build.R", local = processed)))
  database <- processed$database

  expect_true(all(!is.na(database$traits$curve_id)))
  expect_equal(dplyr::select(database$traits, -"curve_id"), plain$database$traits)
  expect_equal(nrow(read_csv_char(written)), nrow(database$traits))
  expect_equal(
    database$build_info$post_build,
    list(file = "R/post_build.R", md5 = unname(tools::md5sum("R/post_build.R")))
  )
  expect_equal(database$build_info$version, plain$database$build_info$version)

  # The saved database is the one `post_build()` returned
  expect_equal(readRDS("export/data/curr/database.rds"), database)
})


test_that("the `targets` pipeline notices `R/post_build.R` being added, edited and deleted", {

  skip_if_not_installed("targets")

  file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE)

  withr::defer({
    unlink("_targets", recursive = TRUE)
    unlink(c("_targets.R", "R/post_build.R"))
  })
  unlink("R/post_build.R")

  suppressMessages(build_setup_pipeline(method = "targets"))
  manifest <- targets::tar_manifest(callr_function = NULL)
  expect_contains(manifest$name, "file_post_build")
  expect_match(manifest$command[manifest$name == "database"], "build_post_process(", fixed = TRUE)
  expect_match(manifest$command[manifest$name == "database"], "file_post_build", fixed = TRUE)

  # `callr_function = NULL` so the pipeline runs in this session, where the
  # package under test is the one loaded by pkgload rather than an installed one
  make <- function() suppressMessages(targets::tar_make(reporter = "silent", callr_function = NULL))
  rebuilt <- function() {
    progress <- targets::tar_progress()
    progress$name[progress$progress == "completed"]
  }
  post_build <- function(value) {
    writeLines(c(
      "post_build <- function(database) {",
      sprintf("  database$traits$curve_id <- '%s'", value),
      "  database",
      "}"
    ), "R/post_build.R")
  }

  # No file: a file target naming no files, and nothing recorded
  make()
  expect_null(targets::tar_read(database)$build_info$post_build)

  post_build("first")
  make()
  expect_contains(rebuilt(), "database")
  expect_equal(unique(targets::tar_read(database)$traits$curve_id), "first")
  expect_equal(targets::tar_read(database)$build_info$post_build$file, "R/post_build.R")

  # Unchanged: the file is re-checked, but nothing downstream rebuilds
  make()
  expect_false("database" %in% rebuilt())

  post_build("second")
  make()
  expect_contains(rebuilt(), "database")
  expect_equal(unique(targets::tar_read(database)$traits$curve_id), "second")

  unlink("R/post_build.R")
  make()
  expect_contains(rebuilt(), "database")
  expect_false("curve_id" %in% names(targets::tar_read(database)$traits))
  expect_null(targets::tar_read(database)$build_info$post_build)
})


test_that("the `remake` pipeline runs `post_build()`", {

  skip_if_not_installed("remake")

  file.copy("data/Test_2022/test-metadata.yml", "data/Test_2022/metadata.yml", overwrite = TRUE)
  # The `remake` pipeline depends on `.git/index`, which `test-setup.R` unpacks
  if (!file.exists(".git/index")) {
    zip::unzip("config/testgit.zip")
  }

  withr::defer({
    unlink(".remake", recursive = TRUE)
    unlink(c("remake.yml", "R/post_build.R"))
  })

  writeLines(c(
    "post_build <- function(database) {",
    "  database$traits$curve_id <- 'remade'",
    "  database",
    "}"
  ), "R/post_build.R")
  suppressMessages(build_setup_pipeline(method = "remake"))
  unlink(".remake", recursive = TRUE)

  expect_silent(suppressMessages(database <- remake::make("database")))
  expect_equal(unique(database$traits$curve_id), "remade")
  expect_equal(database$build_info$post_build$file, "R/post_build.R")
})
