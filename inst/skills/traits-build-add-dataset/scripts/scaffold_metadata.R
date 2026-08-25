#!/usr/bin/env Rscript
# scaffold_metadata.R -- Phase 4 of the traits-build-add-dataset skill.
#
# Reads an "answers" YAML file the agent has written (see
# references/metadata-fields.md for the full shape, and the worked example at
# the bottom of this file) and drives, in order, the documented interactive
# helpers from `traits.build`'s `R/setup.R` -- every one of them accepts a
# `user_responses` list that substitutes for its `utils::menu()` prompts:
#
#   metadata_create_template()
#   metadata_add_source_doi() / metadata_add_source_bibtex() -- or a
#     hand-built `source` block for non-article sources, since no
#     interactive helper exists for those
#   metadata_add_traits()
#   metadata_add_locations()
#   metadata_add_contexts()
#   metadata_add_identifiers()
#
# This is a *non-interactive replay of the documented interactive path*, not
# a hand-authored YAML file -- the whole point is that `dataset_test()` sees
# the same `metadata.yml` shape it would see from a curator sitting at the
# menus.
#
# `trait_name` is never filled here: `metadata_add_traits()` leaves it
# "unknown", exactly as the interactive path does, and that is deliberate --
# it is the curator's decision, made at GATE 1. The same is true of
# `context_property` after `metadata_add_contexts()`, and of `replace` in any
# substitution added later.
#
# Must be run with the working directory at the repo root (`data/<id>/...`),
# matching every other traits.build helper this drives.
#
# Usage:
#   Rscript inst/skills/traits-build-add-dataset/scripts/scaffold_metadata.R <answers.yml>

suppressPackageStartupMessages(library(traits.build))

`%||%` <- function(x, y) if (is.null(x)) y else x

# Bare `NA` in YAML parses to the *string* `"NA"`, not R's logical `NA` --
# only the R-specific `.na` token does that (`yaml::yaml.load(".na")` ->
# `NA`). `metadata.yml` itself uses `.na` for this reason, so answers files
# follow the same convention. Fail loudly rather than silently writing the
# string "NA" into a `user_responses` list, which would end up as a literal
# column name and error deep inside a `dplyr::select()`.
stop_if_literal_na_string <- function(x, context) {
  bad <- vapply(x, function(v) is.character(v) && length(v) == 1 && identical(v, "NA"), logical(1))
  if (any(bad)) {
    stop(
      sprintf(
        "In %s, %s %s the literal string \"NA\" -- use `.na` in the YAML (not bare `NA`) to mean \"no column\".",
        context,
        paste(names(x)[bad], collapse = ", "),
        if (sum(bad) > 1) "are" else "is"
      ),
      call. = FALSE
    )
  }
}

scaffold_metadata <- function(answers_file) {

  answers <- yaml::read_yaml(answers_file)
  dataset_id <- answers$dataset_id
  if (is.null(dataset_id)) stop("Answers file must set `dataset_id`", call. = FALSE)

  # `read_metadata_dataset()` / `write_metadata_dataset()` are internal to
  # traits.build (no `@export`) -- only the plain `read_metadata()` /
  # `write_metadata()`, which take an explicit path, are exported. Every
  # `_dataset` wrapper is just `read_metadata(metadata_path_dataset_id(id))`,
  # so building that path here directly reproduces it without `:::`.
  dataset_path <- file.path("data", dataset_id)
  metadata_file <- file.path(dataset_path, "metadata.yml")
  if (!dir.exists(dataset_path)) {
    stop(sprintf("No `%s` directory -- create it and write `data.csv` first.", dataset_path), call. = FALSE)
  }
  if (!file.exists(file.path(dataset_path, "data.csv"))) {
    stop(sprintf("No `%s` -- `scaffold_metadata.R` reads columns from it and cannot run without it.",
                 file.path(dataset_path, "data.csv")), call. = FALSE)
  }

  message(sprintf("== %s: metadata_create_template() ==", dataset_id))
  template_responses <- answers$template
  if (is.null(template_responses)) {
    stop("Answers file must set `template:` (see references/metadata-fields.md)", call. = FALSE)
  }
  stop_if_literal_na_string(template_responses, "`template:`")
  metadata_create_template(dataset_id, path = dataset_path, user_responses = template_responses)

  # -- source --
  if (!is.null(answers$source)) {
    src <- answers$source
    message(sprintf("== %s: source (%s) ==", dataset_id, src$type %||% "manual"))

    if (identical(src$type, "doi")) {
      metadata_add_source_doi(dataset_id = dataset_id, doi = src$doi)

    } else if (identical(src$type, "bibtex")) {
      metadata_add_source_bibtex(dataset_id = dataset_id, file = src$file, type = src$source_type %||% "primary")

    } else {
      # Non-article source (Book/Online/Thesis/Unpublished/...), or a doi
      # that didn't resolve in Crossref -- write the block directly.
      # `src$fields` is a named list keyed by source type ("primary",
      # "secondary", "original_01", ...), each a named list of the fields
      # transcribed from the source; nothing here invents a field that
      # wasn't given.
      if (is.null(src$fields)) {
        stop("`source:` with type other than `doi`/`bibtex` must set `fields:` ",
             "(see references/metadata-fields.md)", call. = FALSE)
      }
      metadata <- read_metadata(metadata_file)
      for (source_type in names(src$fields)) {
        metadata$source[[source_type]] <- src$fields[[source_type]]
      }
      write_metadata(metadata, metadata_file)
    }
  }

  # -- traits --
  if (!is.null(answers$traits)) {
    message(sprintf("== %s: metadata_add_traits() ==", dataset_id))
    var_in <- answers$traits$var_in
    data <- readr::read_csv(file.path(dataset_path, "data.csv"), col_types = readr::cols(), guess_max = 100000)
    missing_cols <- setdiff(var_in, names(data))
    if (length(missing_cols) > 0) {
      stop(sprintf("`traits: var_in` names not found in data.csv: %s",
                   paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    metadata_add_traits(dataset_id, user_responses = list(var_in = var_in))
  }

  # -- locations --
  if (!is.null(answers$locations)) {
    message(sprintf("== %s: metadata_add_locations() ==", dataset_id))
    loc <- answers$locations
    stop_if_literal_na_string(loc, "`locations:`")

    data <- readr::read_csv(file.path(dataset_path, "data.csv"), col_types = readr::cols(), guess_max = 100000)
    keep <- if (length(loc$keep) == 1 && is.na(loc$keep)) character() else loc$keep
    location_cols <- unique(c(loc$location_name, keep))
    missing_cols <- setdiff(location_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(sprintf("`locations:` columns not found in data.csv: %s",
                   paste(missing_cols, collapse = ", ")), call. = FALSE)
    }

    location_data <- dplyr::distinct(data[, location_cols, drop = FALSE])
    metadata_add_locations(
      dataset_id, location_data,
      user_responses = list(location_name = loc$location_name, keep = loc$keep)
    )
  }

  # -- contexts --
  if (!is.null(answers$contexts)) {
    message(sprintf("== %s: metadata_add_contexts() ==", dataset_id))
    ctx <- answers$contexts
    metadata_add_contexts(
      dataset_id,
      user_responses = list(var_in = ctx$var_in, categories = ctx$categories, replace_needed = ctx$replace_needed)
    )
  }

  # -- identifiers --
  if (!is.null(answers$identifiers)) {
    message(sprintf("== %s: metadata_add_identifiers() ==", dataset_id))
    ids <- answers$identifiers
    metadata_add_identifiers(
      dataset_id,
      user_responses = list(var_in = ids$var_in, identifier_type = ids$identifier_type)
    )
  }

  message(sprintf("Scaffolded %s -> %s", dataset_id, metadata_file))
  invisible(read_metadata(metadata_file))
}

# See `trait_index.R` for why the guard is `sys.nframe() == 0`, not just
# `!interactive()` -- otherwise sourcing this file (e.g. from a test) to
# reach `scaffold_metadata()` tries to auto-run with no args.
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    stop("Usage: Rscript scaffold_metadata.R <answers.yml>", call. = FALSE)
  }
  scaffold_metadata(args[1])
}

# --- Worked example answers file (tutorial_dataset_1, traits.build-template) ---
#
# dataset_id: tutorial_dataset_1
# template:
#   data_is_long_format: false
#   taxon_name: Species
#   location_name: site
#   individual_id: .na
#   collection_date: "2002-11/2002-11"
#   repeat_measurements_id: false
# source:
#   type: doi
#   doi: "10.1111/j.0022-0477.2005.00992.x"
# traits:
#   var_in: ["LMA (mg mm-2)", "Leaf nitrogen (mg mg-1)", "leaf size (mm2)"]
# locations:
#   location_name: site
#   keep: ["description", "latitude (deg)", "longitude (deg)"]
