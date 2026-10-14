#' Join a hard-wrapped word, if the break falls mid-word
#'
#' `parts` are lines from a paragraph pasted out of a PDF or Word document,
#' where the source's own line wrapping -- not paragraph structure -- put a
#' break between them. A break that lands mid-word leaves the first line
#' ending in a hyphen after a lowercase letter, with the next line resuming
#' in lowercase (`"collec-"` / `"tion"`); that hyphen is an artefact of the
#' wrap; those two `parts` become `"collection"`. Anything else -- including a
#' genuine hyphenated compound that happens to sit at a wrap point -- is
#' joined with a space instead, since a real hyphen can't be told apart from
#' a wrap artefact by this rule alone, and leaving it untouched is the safer
#' guess.
#'
#' @param parts character vector of trimmed lines to join, in order
#'
#' @return a single character string
#' @keywords internal
util_dewrap_join <- function(parts) {
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return("")
  }

  out <- parts[1]
  for (part in parts[-1]) {
    if (grepl("[a-z]-$", out) && grepl("^[a-z]", part)) {
      out <- paste0(substr(out, 1, nchar(out) - 1), part)
    } else {
      out <- paste0(out, " ", part)
    }
  }
  out
}


#' Single-quote a value if a plain YAML scalar can't hold it
#'
#' A prose paragraph joined onto one line very often contains `": "` --
#' "as follows: ...", "note: ...", a time like "9:30" is rarer but no safer
#' -- and a plain (unquoted) YAML scalar cannot contain that sequence at all;
#' `yaml::read_yaml()` reads it as a mapping starting mid-value. Wrapping the
#' value in single quotes, doubling any literal `'` it contains, sidesteps
#' this regardless of where the colon falls.
#'
#' Left alone when `x` is empty, or already begins with `'` or `"` -- that
#' means the field was quoted in the source already, so its own quote
#' characters, preserved verbatim by [util_dewrap_join()], already do the job
#' and adding another layer would nest them incorrectly.
#'
#' @param x a single joined field value
#'
#' @return `x`, single-quoted if that's needed and it isn't already
#' @keywords internal
util_quote_if_needed <- function(x) {
  if (!nzchar(x) || grepl("^['\"]", x)) {
    return(x)
  }

  needs_quoting <- grepl(": |:$|^[-?:,\\[\\]#&*!|>'\"%@`]", x)

  if (needs_quoting) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  } else {
    x
  }
}


#' Collapse hard-wrapped line breaks in a `metadata.yml` field
#'
#' `sampling_strategy` and `methods` are the two fields curators most often
#' paste straight out of a source PDF or Word document, and that pasted text
#' carries the source's own line breaks with it -- one per visual line,
#' occasionally splitting a word at a hyphen. Left as literal newlines in
#' `metadata.yml`, those continuation lines are usually indented no more than
#' the field's own key, since that's how the text looked when pasted; YAML
#' then can't tell the difference between a wrapped continuation and a new
#' key, and the file fails to parse (each wrapped line is read as an attempt
#' at a new mapping key). This finds every `fields` entry in the file --
#' `methods` occurs once per trait -- and rejoins its continuation lines into
#' one, using [util_dewrap_join()] to undo mid-word hyphen breaks along the
#' way.
#'
#' A continuation line is anything following the field's `key:` line up to
#' the next line that is itself structurally a new field (`word:`, at or
#' above the key's own indentation) or a new list item (`- `, at or above
#' that indentation) -- not indentation on its own, since pasted text is
#' often left completely unindented, shallower than the key it belongs to.
#' That means this relies on the paragraph never containing a bare line of
#' the form `word:` or `- ...` at or above the key's own indentation -- true
#' of ordinary prose, but worth checking in the diff.
#'
#' Only `fields` are rewritten; everything else in the file -- other fields,
#' comments, formatting -- is left exactly as it was. `custom_R_code` in
#' particular is never touched: its line breaks and indentation are
#' intentional, so its whole block is copied through verbatim (a line of code
#' that happens to read like `methods: ...` is never taken for a field), and
#' asking for it in `fields` is an error. This does not round-trip the file
#' through [write_metadata()], which would reflow the whole file into an
#' unreviewable diff.
#'
#' A field already spread over several properly indented lines is valid YAML,
#' but is joined all the same. That changes nothing in the value read, only
#' the layout, so run this on the dataset being fixed rather than across a
#' whole compilation.
#'
#' @inheritParams metadata_path_dataset_id
#' @param dataset_id Identifier for a study, or a vector of them
#' @param fields Names of the fields to dewrap
#' @param dry_run Print a preview without writing anything. Default `TRUE`;
#'   pass `FALSE` to apply the rewrite.
#'
#' @return A data frame with one row per field that had continuation lines to
#'   join, with columns `dataset_id`, `field`, `line` and `n_lines_joined`.
#'   Returned invisibly when `dry_run = FALSE`.
#'
#' @examples
#' \dontrun{
#' metadata_dewrap_fields("Falster_2003")
#' metadata_dewrap_fields("Falster_2003", dry_run = FALSE)
#' }
#' @export
metadata_dewrap_fields <- function(dataset_id,
                                   path_data = "data",
                                   fields = c("sampling_strategy", "methods"),
                                   dry_run = TRUE) {

  if ("custom_R_code" %in% fields) {
    stop("`custom_R_code` can't be dewrapped: its line breaks and formatting are intentional.")
  }

  field_pattern <- paste0("^(\\s*)(", paste(fields, collapse = "|"), "):\\s?(.*)$")
  # Pasted text is often left fully unindented (not even matching the key's
  # own indent), so indentation alone can't mark the end of the field -- only
  # a line that is itself structurally a new key or a new list item can.
  key_pattern <- "^\\s*[A-Za-z][A-Za-z0-9_]*:(\\s|$)"
  list_item_pattern <- "^\\s*-(\\s|$)"
  custom_R_code_pattern <- "^(\\s*)custom_R_code:"

  reports <- lapply(dataset_id, function(id) {

    path <- metadata_path_dataset_id(id, path_data)
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

    out <- character(0)
    report <- list()
    n <- length(lines)
    i <- 1L

    # A single forward pass, rather than finding every field_pattern match
    # up front: `methods` text routinely contains the word "methods:" as
    # prose (e.g. "... Authors methods: species mean wood density values
    # were ..."), which would otherwise be found as a second, spurious
    # match nested inside the field that's already being joined. Jumping
    # `i` to `j` below means a line consumed as a continuation is never
    # re-examined as a possible start.
    while (i <= n) {

      # Copy the custom_R_code block through untouched, up to the next key or
      # list item at or above its own indentation. Blank lines don't end it --
      # code often has them.
      crc <- regmatches(lines[i], regexec(custom_R_code_pattern, lines[i]))[[1]]
      if (length(crc) > 0) {
        crc_indent <- nchar(crc[2])
        j <- i + 1L
        while (j <= n) {
          line <- lines[j]
          if (nchar(sub("^(\\s*).*$", "\\1", line)) <= crc_indent &&
                (grepl(key_pattern, line) || grepl(list_item_pattern, line))) break
          j <- j + 1L
        }
        out <- c(out, lines[i:(j - 1L)])
        i <- j
        next
      }

      m <- regmatches(lines[i], regexec(field_pattern, lines[i]))[[1]]

      if (length(m) == 0) {
        out <- c(out, lines[i])
        i <- i + 1L
        next
      }

      indent <- m[2]
      field_name <- m[3]
      key_indent <- nchar(indent)
      value <- m[4]

      j <- i + 1L
      continuation <- character(0)
      while (j <= n) {
        line <- lines[j]
        this_indent <- nchar(sub("^(\\s*).*$", "\\1", line))

        if (trimws(line) == "") break
        if (this_indent <= key_indent &&
              (grepl(key_pattern, line) || grepl(list_item_pattern, line))) break

        continuation <- c(continuation, trimws(line))
        j <- j + 1L
      }

      if (length(continuation) == 0) {
        out <- c(out, lines[i])
        i <- i + 1L
        next
      }

      joined <- util_quote_if_needed(util_dewrap_join(c(value, continuation)))
      out <- c(out, paste0(indent, field_name, ": ", joined))

      report[[length(report) + 1]] <- data.frame(
        dataset_id = id, field = field_name, line = i,
        n_lines_joined = length(continuation) + 1L
      )

      i <- j
    }

    if (!dry_run && length(report) > 0) {
      util_write_lines_utf8(out, path)
    }

    do.call(rbind, report)
  })

  report <- do.call(rbind, reports)
  if (is.null(report)) {
    report <- data.frame(
      dataset_id = character(), field = character(),
      line = integer(), n_lines_joined = integer()
    )
  }

  if (dry_run) {
    print(report)
    message("Nothing written. Call again with `dry_run = FALSE` to apply.")
    return(report)
  }

  invisible(report)
}