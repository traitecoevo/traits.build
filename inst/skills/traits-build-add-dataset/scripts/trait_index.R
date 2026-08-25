#!/usr/bin/env Rscript
# trait_index.R -- writes compact, greppable indices of a database repo's
# controlled vocabulary, so an agent (or a curator) can be *shown* plausible
# concepts without reading `config/traits.yml` into context -- it is 700+ KB
# and ~576 concepts in `austraits.build` alone.
#
# Usage:
#   Rscript inst/skills/traits-build-add-dataset/scripts/trait_index.R [path] [output_dir]
#
# `path` defaults to the current directory (a traits.build database repo);
# `output_dir` defaults to a scratch subdirectory, `traits_index/`, under
# `tempdir()`. Writes three files there and prints their paths and row
# counts:
#   traits_index.tsv  -- one row per concept in config/traits.yml
#   units_index.tsv    -- config/unit_conversions.csv, unit_from/unit_to pairs
#   vocab_index.tsv    -- context_property x category, and location_property,
#                         already in use across data/*/metadata.yml
#
# Requires `yaml`; uses `readr` if available (falls back to `write.csv`).

#' Build the trait concept index from `config/traits.yml`
#'
#' @param path Path to a traits.build database repo.
#' @return A data frame, one row per trait concept.
build_traits_index <- function(path = ".") {

  traits_file <- file.path(path, "config", "traits.yml")
  if (!file.exists(traits_file)) {
    stop("No `config/traits.yml` found at: ", traits_file, call. = FALSE)
  }

  concepts <- yaml::read_yaml(traits_file)$traits$elements

  collapse_levels <- function(levels) {
    if (is.null(levels)) return(NA_character_)
    paste(sprintf("%s: %s", names(levels), unlist(levels, use.names = FALSE)), collapse = " | ")
  }

  rows <- lapply(names(concepts), function(trait_name) {
    concept <- concepts[[trait_name]]
    data.frame(
      trait_name = trait_name,
      label = concept$label %||% NA_character_,
      type = concept$type %||% NA_character_,
      units = concept$units %||% NA_character_,
      allowed_values_min = concept$allowed_values_min %||% NA,
      allowed_values_max = concept$allowed_values_max %||% NA,
      allowed_values_levels = collapse_levels(concept$allowed_values_levels),
      structure_measured = concept$structure_measured %||% NA_character_,
      keywords = concept$keywords %||% NA_character_,
      trait_group = concept$trait_group %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Build the unit conversion index from `config/unit_conversions.csv`
#'
#' @inheritParams build_traits_index
#' @return A data frame, or `NULL` if the repo has no unit conversions file.
build_units_index <- function(path = ".") {
  units_file <- file.path(path, "config", "unit_conversions.csv")
  if (!file.exists(units_file)) {
    message("No `config/unit_conversions.csv` found at: ", units_file, " -- skipping units_index.tsv")
    return(NULL)
  }
  utils::read.csv(units_file, stringsAsFactors = FALSE)
}

#' Build the in-use context/location property index from `data/*/metadata.yml`
#'
#' @inheritParams build_traits_index
#' @return A data frame with columns `kind` (`context` or `location`),
#'   `property`, `category` (context only, `NA` for locations), and
#'   `n_datasets` -- the number of datasets already using that property.
build_vocab_index <- function(path = ".") {

  metadata_files <- Sys.glob(file.path(path, "data", "*", "metadata.yml"))
  if (length(metadata_files) == 0) {
    message("No `data/*/metadata.yml` files found under: ", path)
    return(data.frame(kind = character(), property = character(),
                       category = character(), n_datasets = integer()))
  }

  context_rows <- list()
  location_rows <- list()

  for (f in metadata_files) {
    dataset_id <- basename(dirname(f))
    metadata <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.null(metadata)) {
      message("Skipping unparsable metadata file: ", f)
      next
    }

    contexts <- metadata$contexts
    if (!is.null(contexts) && !identical(contexts, "NA") && !(length(contexts) == 1 && is.na(contexts[[1]][1]))) {
      for (ctx in contexts) {
        if (is.list(ctx) && !is.null(ctx$context_property)) {
          context_rows[[length(context_rows) + 1]] <- data.frame(
            kind = "context",
            property = ctx$context_property,
            category = ctx$category %||% NA_character_,
            dataset_id = dataset_id,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    locations <- metadata$locations
    if (!is.null(locations) && !identical(locations, "NA") && !(length(locations) == 1 && is.na(locations[[1]][1]))) {
      for (loc in locations) {
        if (is.list(loc)) {
          for (prop in names(loc)) {
            location_rows[[length(location_rows) + 1]] <- data.frame(
              kind = "location",
              property = prop,
              category = NA_character_,
              dataset_id = dataset_id,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  all_rows <- do.call(rbind, c(context_rows, location_rows))
  if (is.null(all_rows) || nrow(all_rows) == 0) {
    return(data.frame(kind = character(), property = character(),
                       category = character(), n_datasets = integer()))
  }

  # One row per (kind, property, category); n_datasets counts distinct
  # datasets already using it, so a curator can judge how established a
  # property name already is before minting a new one.
  #
  # `category` is `NA` for every location row (locations have no category).
  # `split()` silently drops elements whose grouping key is `NA` -- via
  # `interaction()` or otherwise -- so building the split key straight from
  # the raw columns discarded every location row here. Substituting a
  # sentinel string for `NA` before pasting keeps them.
  key <- paste(all_rows$kind, all_rows$property, ifelse(is.na(all_rows$category), "", all_rows$category), sep = "")
  agg <- lapply(split(all_rows, key), function(rows) {
    data.frame(
      kind = rows$kind[1],
      property = rows$property[1],
      category = rows$category[1],
      n_datasets = length(unique(rows$dataset_id)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, agg)
  out[order(out$kind, out$property), , drop = FALSE]
}

`%||%` <- function(x, y) if (is.null(x)) y else x

write_index <- function(df, file) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_tsv(df, file, na = "")
  } else {
    utils::write.table(df, file, sep = "\t", row.names = FALSE, na = "", quote = FALSE)
  }
}

trait_index <- function(path = ".", output_dir = file.path(tempdir(), "traits_index")) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  traits <- build_traits_index(path)
  traits_file <- file.path(output_dir, "traits_index.tsv")
  write_index(traits, traits_file)

  units <- build_units_index(path)
  units_file <- NULL
  if (!is.null(units)) {
    units_file <- file.path(output_dir, "units_index.tsv")
    write_index(units, units_file)
  }

  vocab <- build_vocab_index(path)
  vocab_file <- file.path(output_dir, "vocab_index.tsv")
  write_index(vocab, vocab_file)

  result <- list(
    traits_index = traits_file, n_traits = nrow(traits),
    units_index = units_file, n_units = if (is.null(units)) 0L else nrow(units),
    vocab_index = vocab_file, n_vocab = nrow(vocab)
  )
  class(result) <- "trait_index_result"
  result
}

#' @export
print.trait_index_result <- function(x, ...) {
  cat("traits_index.tsv:", x$traits_index, sprintf("(%d concepts)\n", x$n_traits))
  if (!is.null(x$units_index)) {
    cat("units_index.tsv: ", x$units_index, sprintf("(%d conversions)\n", x$n_units))
  }
  cat("vocab_index.tsv: ", x$vocab_index, sprintf("(%d context/location properties in use)\n", x$n_vocab))
  invisible(x)
}

# `sys.nframe() == 0` is what actually distinguishes `Rscript trait_index.R`
# (runs at the top level, frame 0) from `source("trait_index.R")` from inside
# another R session -- e.g. a testthat run under `Rscript` (`!interactive()`
# is true in both cases, so that check alone would auto-run this with no args
# every time the file is merely sourced for its function).
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  path <- if (length(args) >= 1) args[1] else "."
  output_dir <- if (length(args) >= 2) args[2] else file.path(tempdir(), "traits_index")
  print(trait_index(path, output_dir))
}
