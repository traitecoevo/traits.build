#!/usr/bin/env Rscript
# `make ontology-release` -- snapshot the current data-model version into
# ontology/output/ontology/release/<version>/ and publish it.
#
# Assumes `make ontology-check` and `make ontology-site` have run (the Makefile
# orders them). The version is the newest heading in ontology/NEWS.md; cutting
# a new version means adding a heading there, which is also where its changes
# are recorded.

source("ontology/scripts/setup.R")

version <- ontology_version()
message("Releasing data model ", version,
        " (previous: ", ontology_previous_version(version), ")")

out_dir <- ontology_output_dir()

missing <- ONTOLOGY_RELEASE_FILES[!file.exists(file.path(out_dir, ONTOLOGY_RELEASE_FILES))]
if (length(missing) > 0) {
  stop("Missing ", paste(missing, collapse = ", "),
       " -- run `make ontology-site` first.", call. = FALSE)
}

# A release directory is an archive: it is what
# https://w3id.org/traits.build/release/<version>/ resolves to. Overwriting one
# because NEWS.md was not given a new heading would silently rewrite published
# content. While a version is still being prepared, TRAITS_BUILD_FORCE_RELEASE=1
# allows it.
to_path <- file.path(out_dir, "release", version)
if (dir.exists(to_path) && length(list.files(to_path)) > 0 &&
      !nzchar(Sys.getenv("TRAITS_BUILD_FORCE_RELEASE"))) {
  stop(to_path, "/ already exists and is not empty.\n",
       "Add a `## traits.build data model <X.Y.Z>` heading to ontology/NEWS.md,\n",
       "or set TRAITS_BUILD_FORCE_RELEASE=1 to overwrite this snapshot deliberately.",
       call. = FALSE)
}

dir.create(to_path, showWarnings = FALSE, recursive = TRUE)
for (file in ONTOLOGY_RELEASE_FILES) {
  file.copy(file.path(out_dir, file), file.path(to_path, file), overwrite = TRUE)
}
message("Snapshotted ", length(ONTOLOGY_RELEASE_FILES), " files into ", to_path, "/")

n <- ontology_publish_docs()
message("Published ", n, " files into docs/ontology/")

message("\nStill to do by hand: commit, open the PR, and (at the package release)\n",
        "fast-forward master so GitHub Pages serves the new version at\n",
        "https://w3id.org/traits.build.")
