#' Known replacements for disallowed characters
#'
#' A curated map covering only characters that are *wrong*, not merely unusual:
#' a look-alike standing in for the character actually meant, encoding damage,
#' or an invisible character with no content. Anything that carries meaning is
#' left alone and reported, so it can be retained via the `exceptions` argument
#' of `check_disallowed_chars()` rather than flattened.
#'
#' Correct typography is deliberately absent. U+2033 DOUBLE PRIME is the right
#' character for arcseconds, and U+2030 PER MILLE has a technical meaning, so
#' rewriting them to ASCII would lose information rather than fix a mistake.
#'
#' Letters are never in this map. An unexpected letter is either a genuine name
#' (`Briceno`, `Klimesova`, `Osvaldsson` with their accents), which belongs in
#' the `exceptions` argument of `check_disallowed_chars()`, or a mangling of a
#' symbol (`OPSII` for `Phi_PSII`), which only the curator can resolve.
#'
#' Written as `\uXXXX` escapes because several of these characters are invisible
#' or indistinguishable from their replacement in a source file.
#'
#' @return A named character vector; names are the text to find, values the
#'   replacement.
#' @keywords internal
util_disallowed_char_replacements <- function() {
  c(
    # NOT SIGN followed by DAGGER: a UTF-8 no-break space (bytes C2 A0) written
    # back out as Mac OS Roman, which renders as two characters. Longer keys
    # are applied first, so this matches before the two characters it is
    # built from.
    "\u00ac\u2020" = " ",

    # Characters that occupy space but are not a space
    "\u00a0" = " ",              # NO-BREAK SPACE
    "\u2007" = " ",              # FIGURE SPACE
    "\u2009" = " ",              # THIN SPACE
    "\u202f" = " ",              # NARROW NO-BREAK SPACE

    # Characters that are invisible entirely
    "\u200b" = "",               # ZERO WIDTH SPACE
    "\u200d" = "",               # ZERO WIDTH JOINER

    # Look-alikes for the degree sign, which the exception list already allows
    "\u00ba" = "\u00b0",         # MASCULINE ORDINAL INDICATOR
    "\u25e6" = "\u00b0",         # WHITE BULLET

    # A mis-set MICRO SIGN in a unit. The two are visually identical and only
    # the MICRO SIGN is allowed, so permitting both would let the same unit be
    # written two ways.
    "\u03bc" = "\u00b5"          # GREEK SMALL LETTER MU -> MICRO SIGN
  )
}


#' Replace disallowed characters with their allowed equivalents
#'
#' Applies [util_disallowed_char_replacements()] to a character vector. Keys are
#' applied longest-first, so a multi-character sequence is matched before the
#' individual characters that make it up.
#'
#' @param x A character vector
#' @param replacements Named character vector of replacements to apply
#' @return `x`, with known disallowed characters replaced
#' @keywords internal
util_replace_disallowed_chars <- function(x,
                                          replacements = util_disallowed_char_replacements()) {

  replacements <- replacements[order(-nchar(names(replacements)))]

  for (i in seq_along(replacements)) {
    x <- gsub(names(replacements)[i], replacements[[i]], x, fixed = TRUE)
  }

  x
}


#' Report the disallowed characters in each element of a character vector
#'
#' @param x A character vector
#' @param ascii_only Allow nothing outside ASCII, as `dataset_test()` does for
#'   data. When `FALSE`, `check_disallowed_chars()` applies its own default
#'   exceptions, which permit accented letters.
#' @return A data frame with one row per disallowed character, with columns
#'   `line`, `char` and `code`. Zero rows if there are none.
#' @keywords internal
util_locate_disallowed_chars <- function(x, ascii_only = FALSE) {

  out <- lapply(seq_along(x), function(i) {

    # Skip the ASCII-only majority without splitting it into characters
    if (!any(charToRaw(x[i]) >= 0x80)) {
      return(NULL)
    }

    disallowed <- if (ascii_only) {
      check_disallowed_chars(x[i], exceptions = "")
    } else {
      check_disallowed_chars(x[i])
    }

    if (!any(disallowed)) {
      return(NULL)
    }

    data.frame(
      line = i,
      char = util_split_chars(x[i])[disallowed],
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out)

  if (is.null(out)) {
    return(data.frame(line = integer(), char = character(), code = character(),
                      stringsAsFactors = FALSE))
  }

  out$code <- vapply(
    out$char,
    function(char) {
      # `util_split_chars()` falls back to splitting on bytes for input that is
      # not valid UTF-8, so `char` can be a byte that is not a code point.
      if (validUTF8(char)) {
        sprintf("U+%04X", utf8ToInt(char))
      } else {
        paste0("0x", toupper(as.character(charToRaw(char))), collapse = "")
      }
    },
    character(1),
    USE.NAMES = FALSE
  )

  out
}


#' Replace disallowed characters in a dataset's files
#'
#' `dataset_test()` reports characters that the database does not allow, but
#' fixing them has been a manual edit for each one. This applies the known
#' replacements in [util_disallowed_char_replacements()] and reports whatever it
#' could not resolve.
#'
#' Only the offending characters are rewritten; the rest of the file is left as
#' it was. In particular this does not round-trip `metadata.yml` through
#' [write_metadata()], which would reflow the whole file and bury the change in
#' an unreviewable diff.
#'
#' Characters with no known replacement are reported rather than guessed at.
#' Each is either a genuine character that should be added to the `exceptions`
#' argument of `check_disallowed_chars()`, or a mangling that only the curator
#' can interpret.
#'
#' A file that is not valid UTF-8 is reported and skipped, since recovering one
#' means knowing which encoding it was written in.
#'
#' @section Scope of `data.csv`:
#'
#' `data.csv` is **not** in `files` by default. It can be processed by asking for
#' it, but the two files are very different problems, and the scope for data is
#' still an open question (see #251).
#'
#' `dataset_test()` applies its ASCII-only rule for data to
#' `parsed_data$traits$value` — the values that actually reach the built
#' database. This function reads whole files, so for `data.csv` it sees every
#' column, including ones that never become a trait value. Measured over all 601
#' downstream datasets, that is the difference between **95** disallowed
#' characters in `metadata.yml` and **170,946** in `data.csv` across 147 files —
#' almost none of which `dataset_test()` reports. The bulk is invisible junk in
#' provenance columns (110,666 zero-width joiners in one `created_by` column)
#' plus legitimate typography and accented names that the ASCII-only rule can
#' only ever report, never resolve.
#'
#' So pass `files = c("metadata.yml", "data.csv")` deliberately, and read the
#' dry-run report first.
#'
#' @inheritParams metadata_path_dataset_id
#' @param dataset_id Identifier for a study, or a vector of them
#' @param files Names of files within each dataset folder to process. Defaults to
#'   `metadata.yml` only; see the section on `data.csv` below before adding it.
#' @param dry_run Report what would change without writing anything. Default
#'   `TRUE`; pass `FALSE` to apply the replacements.
#'
#' @return A data frame with one row per disallowed character found, with
#'   columns `dataset_id`, `file`, `line`, `char`, `code`, `replacement` and
#'   `status`. `status` is `"replaced"` when a replacement is known,
#'   `"no replacement known"` otherwise, or `"file not valid UTF-8"` for a file
#'   that was skipped. Returned invisibly when `dry_run = FALSE`.
#'
#' @examples
#' \dontrun{
#' # See what would change across every dataset
#' report <- dataset_replace_disallowed_chars(dir("data"))
#'
#' # Apply it
#' dataset_replace_disallowed_chars(dir("data"), dry_run = FALSE)
#'
#' # Include the data files, which is a much wider sweep -- read the report first
#' dataset_replace_disallowed_chars(dir("data"), files = c("metadata.yml", "data.csv"))
#' }
#' @export
dataset_replace_disallowed_chars <- function(dataset_id,
                                             path_data = "data",
                                             files = "metadata.yml",
                                             dry_run = TRUE) {

  report <- list()

  for (id in dataset_id) {
    for (file in files) {

      path <- file.path(path_data, id, file)

      if (!file.exists(path)) {
        next
      }

      lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

      # `dataset_test()` allows accented letters in metadata but nothing outside
      # ASCII in data, so mirror that split rather than inventing a third rule.
      found <- util_locate_disallowed_chars(lines, ascii_only = identical(file, "data.csv"))

      if (nrow(found) == 0) {
        next
      }

      # Replacing inside invalid UTF-8 needs the original encoding, which only
      # the curator knows, so report the file and leave it alone.
      if (!all(validUTF8(lines))) {
        found$replacement <- NA_character_
        found$status <- "file not valid UTF-8"
        report[[length(report) + 1]] <- cbind(dataset_id = id, file = file, found)
        next
      }

      replaced <- util_replace_disallowed_chars(lines)

      # A character counts as replaced when it is gone from its rewritten line.
      # Testing the outcome rather than consulting the map means
      # multi-character keys need no special case.
      still_present <- vapply(
        seq_len(nrow(found)),
        function(k) grepl(found$char[k], replaced[found$line[k]], fixed = TRUE),
        logical(1)
      )

      found$status <- ifelse(still_present, "no replacement known", "replaced")
      found$replacement <- ifelse(
        still_present, NA_character_, util_replace_disallowed_chars(found$char)
      )

      report[[length(report) + 1]] <- cbind(dataset_id = id, file = file, found)

      if (!dry_run && any(!still_present)) {
        util_write_lines_utf8(replaced, path)
      }
    }
  }

  report <- do.call(rbind, report)

  if (is.null(report)) {
    message(crayon::green("No disallowed characters found"))
    report <- util_char_report_cols[0, ]
  } else {
    report <- report[, names(util_char_report_cols)]
    util_report_disallowed_chars(report, dry_run)
  }

  if (dry_run) report else invisible(report)
}


# The columns of a `dataset_replace_disallowed_chars()` report, in order. Also
# the report's zero-row form, as `util_char_report_cols[0, ]`.
util_char_report_cols <- data.frame(
  dataset_id = NA_character_, file = NA_character_, line = NA_integer_,
  char = NA_character_, code = NA_character_, replacement = NA_character_,
  status = NA_character_, stringsAsFactors = FALSE
)


#' Write lines to a file as UTF-8
#'
#' @param lines Character vector to write
#' @param path File to write to
#' @return Nothing, called for its side effect
#' @keywords internal
util_write_lines_utf8 <- function(lines, path) {
  con <- file(path, "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(lines, con)
}


#' Summarise a `dataset_replace_disallowed_chars()` report on the console
#'
#' @param report Report data frame
#' @param dry_run Whether anything was written
#' @return `report`, invisibly
#' @keywords internal
util_report_disallowed_chars <- function(report, dry_run) {

  # Distinct characters in a set of rows, most frequent first
  tally <- function(rows) {
    counts <- sort(table(paste(rows$code, rows$char)), decreasing = TRUE)
    paste0("(", paste(sprintf("%s x%d", names(counts), counts), collapse = ", "), ")")
  }

  replaced <- report[report$status == "replaced", ]
  unresolved <- report[report$status == "no replacement known", ]
  invalid <- report[report$status == "file not valid UTF-8", ]

  if (nrow(replaced) > 0) {
    message(sprintf(
      "%s in %s %s",
      crayon::green(sprintf(
        "%d character(s) %s", nrow(replaced),
        if (dry_run) "can be replaced" else "replaced"
      )),
      crayon::blue(sprintf(
        "%d file(s)", nrow(unique(replaced[, c("dataset_id", "file")]))
      )),
      crayon::silver(tally(replaced))
    ))
  }

  if (nrow(unresolved) > 0) {
    message(sprintf(
      "%s %s\n\t%s",
      crayon::red(sprintf(
        "%d character(s) have no known replacement:", nrow(unresolved)
      )),
      crayon::silver(tally(unresolved)),
      crayon::silver("add each to `exceptions` if it is intended, or correct it by hand")
    ))
  }

  if (nrow(invalid) > 0) {
    skipped <- unique(invalid[, c("dataset_id", "file")])
    message(sprintf(
      "%s\n\t%s",
      crayon::red(sprintf(
        "%d file(s) are not valid UTF-8 and were skipped:", nrow(skipped)
      )),
      crayon::silver(paste(
        file.path(skipped$dataset_id, skipped$file), collapse = "\n\t"
      ))
    ))
  }

  if (dry_run && nrow(invalid) < nrow(report)) {
    message(crayon::silver("Nothing written. Call again with `dry_run = FALSE` to apply."))
  }

  invisible(report)
}

