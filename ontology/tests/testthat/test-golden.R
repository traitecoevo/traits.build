# The artefacts committed in ontology/output/ontology/ ARE the golden fixtures.
#
# The build writes to a temp directory and these tests compare against the
# committed copies, which git already versions and `make ontology-release`
# already archives. Change anything in ontology/R/, and if these pass the
# refactor was behaviour-preserving. To change the output deliberately, run
# `make ontology` at the root, review `git diff`, and commit the artefacts in
# their own commit, separate from the code change.

test_that("the build reproduces every committed artefact byte for byte", {
  built <- ontology_test_build()

  for (file in ONTOLOGY_OUTPUTS) {
    committed <- file.path(ontology_output_dir(), file)
    skip_if_not(file.exists(committed),
                paste(file, "is not committed; nothing to compare against"))

    expect_identical(ontology_bytes(file.path(built, file)),
                     ontology_bytes(committed),
                     info = paste(file, "differs from the committed copy"))
  }
})

test_that("the build is deterministic", {
  first <- ontology_test_build()
  second <- withr::local_tempdir()
  suppressMessages(ontology_build_data(out_dir = second))

  for (file in ONTOLOGY_OUTPUTS) {
    expect_identical(ontology_bytes(file.path(second, file)),
                     ontology_bytes(file.path(first, file)),
                     info = paste(file, "differs between two builds"))
  }
})

test_that("the build does not write to ontology/data/", {
  fingerprint <- function() {
    files <- sort(list.files(ontology_path("data"), full.names = TRUE))
    vapply(files, function(f) paste(file.size(f), file.mtime(f)), character(1))
  }

  before <- fingerprint()
  suppressMessages(ontology_build_data(out_dir = withr::local_tempdir()))

  expect_identical(fingerprint(), before)
})

test_that("the committed release snapshot matches the committed build", {
  # release/<version>/ is copied from the build outputs by `make
  # ontology-release`; if the outputs were rebuilt afterwards without cutting a
  # release, the snapshot and the latest files disagree about what the
  # current version is.
  version <- ontology_version()
  snapshot <- file.path(ontology_output_dir(), "release", version)
  skip_if_not(dir.exists(snapshot), paste("no snapshot for", version, "yet"))
  # `TRAITS_BUILD_FORCE_RELEASE=1 make ontology-release` runs these tests before
  # it replaces the snapshot, so a re-cut of a version that has not shipped
  # would otherwise fail on the very difference it is about to fix.
  skip_if(Sys.getenv("TRAITS_BUILD_FORCE_RELEASE") == "1",
          paste("release/", version, "is being re-cut"))

  for (file in setdiff(ONTOLOGY_RELEASE_FILES, "index.html")) {
    expect_identical(ontology_bytes(file.path(snapshot, file)),
                     ontology_bytes(file.path(ontology_output_dir(), file)),
                     info = paste0(file, " in release/", version, " differs from the build: ",
                                   "add a new version heading to ontology/NEWS.md, or, if ",
                                   version, " has not shipped, re-cut it with ",
                                   "TRAITS_BUILD_FORCE_RELEASE=1 make ontology-release"))
  }
})
