#!/usr/bin/env Rscript
# probe_repo.R -- Phase 0 of the traits-build-add-dataset skill.
#
# Detects which host-repo-specific helpers and build tooling *this* database
# repo provides, so the rest of the skill uses what is actually there instead
# of assuming `austraits.build`'s helper set. `ausinvertraits.build` and
# `AusFizz` do not carry the same `R/custom_R_code.R` helpers or the
# `build_align_taxon_names.R` / `build_update_taxon_list.R` files, and this
# must be detected rather than assumed -- everything here is written to fail
# soft (report `FALSE`/absent) rather than error when something is missing.
#
# Usage:
#   Rscript inst/skills/traits-build-add-dataset/scripts/probe_repo.R [path]
#
# `path` defaults to the current directory and should be the root of a
# traits.build database repo (i.e. it contains `data/` and
# `config/traits.yml`). Prints a human-readable report to stdout. Can also be
# `source()`d, in which case only the `probe_repo()` function is defined and
# nothing is run.

#' Probe a traits.build database repo for host-specific capabilities
#'
#' @param path Path to a traits.build database repo. Defaults to the current
#'   directory.
#' @return A list (class `probe_repo_result`) describing what was found.
probe_repo <- function(path = ".") {

  has_file <- function(...) file.exists(file.path(path, ...))

  # Functions that may be defined in `R/custom_R_code.R`. `austraits.build`
  # defines all of these; `ausinvertraits.build` and `AusFizz` ship the file
  # as an empty placeholder (`build_setup_pipeline()`'s default), so the
  # file's mere existence says nothing -- each function name is checked by
  # sourcing the file into a throwaway environment.
  custom_code_file <- file.path(path, "R", "custom_R_code.R")
  custom_code_fns <- c(
    "check_new_taxa", "check_new_taxa_accepted",
    "replace_duplicates_with_NA", "separate_range",
    "move_values_to_new_trait", "format_min_max_as_range",
    "format_flowering_months", "convert_month_range_string_to_binary",
    "convert_month_range_vec_to_binary", "convert_01_ny", "get_month"
  )

  custom_code_available <- stats::setNames(rep(FALSE, length(custom_code_fns)), custom_code_fns)
  custom_code_error <- NA_character_

  if (file.exists(custom_code_file)) {
    env <- new.env()
    result <- tryCatch(
      {
        sys.source(custom_code_file, envir = env)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (!is.null(result)) {
      custom_code_error <- result
    } else {
      custom_code_available[custom_code_fns] <- custom_code_fns %in% ls(env)
    }
  }

  pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)

  # `new_taxa_trait_combinations()` (R/reports.R) is a package-level
  # equivalent of `austraits.build`'s `check_new_taxa_accepted()` that reads
  # `database$taxa` instead of taking APCalign `resources`, so it works
  # without APCalign in all three database repos. It is currently unexported;
  # look it up in the namespace directly so the probe doesn't depend on that
  # being resolved.
  ntc_available <- pkg_available("traits.build") &&
    exists("new_taxa_trait_combinations", where = asNamespace("traits.build"), inherits = FALSE)

  build_method <- character()
  if (has_file("remake.yml")) build_method <- c(build_method, "remake")
  if (has_file("build.R")) build_method <- c(build_method, "base_or_furrr")
  if (length(build_method) == 0) build_method <- "none"

  result <- list(
    path = normalizePath(path, mustWork = FALSE),
    is_database_repo = has_file("data") && has_file("config", "traits.yml"),
    custom_r_code_file_exists = file.exists(custom_code_file),
    custom_r_code_source_error = custom_code_error,
    custom_r_code_functions = as.list(custom_code_available),
    build_align_taxon_names = has_file("R", "build_align_taxon_names.R"),
    build_update_taxon_list = has_file("R", "build_update_taxon_list.R"),
    build_method = build_method,
    austraits_installed = pkg_available("austraits"),
    apcalign_installed = pkg_available("APCalign"),
    new_taxa_trait_combinations_available = ntc_available
  )

  class(result) <- "probe_repo_result"
  result
}

#' @export
print.probe_repo_result <- function(x, ...) {
  yn <- function(b) if (isTRUE(b)) "yes" else "no"

  cat("traits.build repo probe:", x$path, "\n")
  cat("  looks like a database repo (data/, config/traits.yml):", yn(x$is_database_repo), "\n")
  cat("  build method available:                 ", paste(x$build_method, collapse = ", "), "\n")
  cat("  R/custom_R_code.R present:               ", yn(x$custom_r_code_file_exists), "\n")
  if (!is.na(x$custom_r_code_source_error)) {
    cat("    !! did not source cleanly:", x$custom_r_code_source_error, "\n")
  } else if (x$custom_r_code_file_exists) {
    for (fn in names(x$custom_r_code_functions)) {
      cat(sprintf("    %-38s %s\n", fn, yn(x$custom_r_code_functions[[fn]])))
    }
  }
  cat("  R/build_align_taxon_names.R present:     ", yn(x$build_align_taxon_names), "\n")
  cat("  R/build_update_taxon_list.R present:      ", yn(x$build_update_taxon_list), "\n")
  cat("  `austraits` package installed:           ", yn(x$austraits_installed), "\n")
  cat("  `APCalign` package installed:            ", yn(x$apcalign_installed), "\n")
  cat("  `new_taxa_trait_combinations()` reachable:", yn(x$new_taxa_trait_combinations_available), "\n")
  invisible(x)
}

# See `trait_index.R` for why the guard is `sys.nframe() == 0`, not just
# `!interactive()` -- otherwise sourcing this file (e.g. from a test) to
# reach `probe_repo()` tries to auto-run with no args.
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  path <- if (length(args) >= 1) args[1] else "."
  print(probe_repo(path))
}
