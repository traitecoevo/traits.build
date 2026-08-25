#' Install traits.build Claude Code skills into a database repo
#'
#' `traits.build` ships one or more Claude Code skills under `inst/skills/`
#' (visible while developing the package itself via the symlinked
#' `.claude/skills/`). `use_traits_build_skills()` copies them into a
#' *database* repo -- `austraits.build`, `ausinvertraits.build`, `AusFizz`, or
#' any other repo built on `traits.build` -- so an agent working there can use
#' them.
#'
#' @param path Path to a traits.build database repo. Defaults to the current
#'   directory.
#' @param overwrite Overwrite a skill already installed at `path`. Default
#'   `FALSE`: an already-installed skill is left untouched and reported as
#'   skipped, rather than silently clobbered.
#'
#' @return Invisibly, a character vector of the skill directories written to
#'   `<path>/.claude/skills/`. Skipped skills are not included.
#' @importFrom crayon %+%
#' @export
use_traits_build_skills <- function(path = ".", overwrite = FALSE) {

  if (!dir.exists(file.path(path, "data")) || !file.exists(file.path(path, "config", "traits.yml"))) {
    stop(
      sprintf(
        red("`%s`") %+% red(" does not look like a traits.build database repo: expected a ") %+%
          blue("data/") %+% red(" directory and a ") %+% blue("config/traits.yml") %+% red(" file."),
        path
      ),
      call. = FALSE
    )
  }

  src_root <- system.file("skills", package = "traits.build")

  if (!nzchar(src_root) || length(list.dirs(src_root, recursive = FALSE)) == 0) {
    stop("No skills found in the installed `traits.build` package (`inst/skills/` is missing or empty).", call. = FALSE)
  }

  skill_names <- list.dirs(src_root, full.names = FALSE, recursive = FALSE)
  dest_root <- file.path(path, ".claude", "skills")
  dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)

  installed <- character()
  skipped <- character()

  for (skill in skill_names) {

    src <- file.path(src_root, skill)
    dest <- file.path(dest_root, skill)

    if (dir.exists(dest) && !overwrite) {
      skipped <- c(skipped, skill)
      next
    }

    if (dir.exists(dest)) {
      unlink(dest, recursive = TRUE)
    }

    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    files <- list.files(src, recursive = TRUE, full.names = FALSE)

    for (f in files) {
      dir.create(dirname(file.path(dest, f)), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(src, f), file.path(dest, f), overwrite = TRUE)
    }

    installed <- c(installed, skill)
  }

  if (length(installed) > 0) {
    message(
      sprintf(
        red("Installed skill(s) ") %+% green("'%s'") %+% red(" into ") %+% blue("%s"),
        paste(installed, collapse = "', '"),
        dest_root
      )
    )
  }

  if (length(skipped) > 0) {
    message(
      sprintf(
        red("Skipped skill(s) already installed at ") %+% blue("%s") %+% red(": ") %+% green("'%s'") %+%
          red(" (use ") %+% blue("overwrite = TRUE") %+% red(" to replace)"),
        dest_root,
        paste(skipped, collapse = "', '")
      )
    )
  }

  invisible(file.path(dest_root, installed))
}
