#' Join the lines of a hard-wrapped paragraph
#'
#' `parts` are lines from a paragraph pasted out of a PDF or Word document,
#' where the source's own line wrapping -- not paragraph structure -- put a
#' break between them. Lines are joined with a space, except where a line ends
#' in a hyphen attached to a word: there the break is joined without a space,
#' and the hyphen is kept (`"oven-"` / `"dried"` becomes `"oven-dried"`).
#'
#' A hyphen at a break cannot be told apart from the source's own: a word
#' split mid-word (`"collec-"` / `"tion"`) and a genuine compound
#' (`"erosion-"` / `"resistant"`) look the same. Keeping the hyphen never
#' deletes a character; with `dehyphenate = TRUE` it is removed instead where
#' a lowercase letter precedes it and the next line resumes in lowercase,
#' giving `"collection"` but also `"erosionresistant"`. Either way, every join
#' at a hyphen is returned in `hyphen_joins` so it can be checked. A line
#' ending in a hyphen before "and" or "or" (`"pre-"` / `"and post-fire"`) is a
#' suspended hyphen and keeps its space, as does a line ending in `+/-`.
#'
#' @param parts character vector of trimmed lines to join, in order
#' @param dehyphenate remove the hyphen at a lowercase-to-lowercase break?
#'
#' @return a list: `text`, the joined string, and `hyphen_joins`, the word
#'   formed at each break that ended in a hyphen
#' @keywords internal
util_dewrap_join <- function(parts, dehyphenate = FALSE) {
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(list(text = "", hyphen_joins = character()))
  }

  out <- parts[1]
  hyphen_joins <- character()
  for (part in parts[-1]) {
    if (grepl("\\S-$", out) && !grepl("\\+/-$", out) && !grepl("^(and|or)\\b", part)) {
      if (dehyphenate && grepl("[a-z]-$", out) && grepl("^[a-z]", part)) {
        out <- substr(out, 1, nchar(out) - 1)
      }
      hyphen_joins <- c(hyphen_joins, paste0(sub(".*\\s", "", out), sub("\\s.*", "", part)))
      out <- paste0(out, part)
    } else {
      out <- paste0(out, " ", part)
    }
  }
  list(text = out, hyphen_joins = hyphen_joins)
}


#' Single-quote a value if a plain YAML scalar can't hold it
#'
#' A prose paragraph joined onto one line very often contains `": "` --
#' "as follows: ...", "note: ...", a time like "9:30" is rarer but no safer
#' -- and a plain (unquoted) YAML scalar cannot contain that sequence at all;
#' `yaml::read_yaml()` reads it as a mapping starting mid-value. Nor can it
#' contain `" #"`, which starts a comment, so `"plot #3 was"` would silently be
#' read as `"plot"`, or begin with a character YAML reserves (`{` starts a flow
#' mapping). Wrapping the value in single quotes, doubling any literal `'` it
#' contains, sidesteps all of these.
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

  # perl = TRUE: R's default engine ends a bracket expression at the first
  # `]`, escaped or not, which silently turned the rest into alternatives
  needs_quoting <- grepl(": |:$|\\s#|^[-?:,\\[\\]{}#&*!|>'\"%@`]", x, perl = TRUE)

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
#' one, using [util_dewrap_join()].
#'
#' A continuation line is anything following the field's `key:` line up to a
#' blank line, or the next line at or above the key's own indentation that is
#' itself structurally a new field or a new list item (`- `) -- not
#' indentation on its own, since pasted text is often left completely
#' unindented, shallower than the key it belongs to. A new field is a key
#' written as a single word (`word:`), or one of the file's `var_in` values:
#' a `traits` entry can carry a field named after a context's `var_in`, such
#' as `stem type: branch`, which contains a space. So this relies on the
#' paragraph never containing a line of the form `word:` or `- ...` at or
#' above the key's own indentation -- true of ordinary prose, and when it is
#' not, the joined field stops early and the file still fails to parse,
#' rather than a key being absorbed into the text.
#'
#' At a line that ends in a hyphen the hyphen is kept and the next line joined
#' to it without a space; pass `dehyphenate = TRUE` to remove it instead where
#' it splits a lowercase word (see [util_dewrap_join()]). The preview lists
#' every such join, since neither rule is right every time.
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
#' but is joined all the same. That changes nothing in the value read except
#' at a line ending in a hyphen, which YAML reads as `"oven- dried"` and this
#' writes as `"oven-dried"`. Run this on the dataset being fixed rather than
#' across a whole compilation: run on `austraits.build`, it rewrites 400 of
#' 442 files that already parse.
#'
#' @inheritParams metadata_path_dataset_id
#' @param dataset_id Identifier for a study, or a vector of them
#' @param fields Names of the fields to dewrap
#' @param dehyphenate Remove the hyphen at a line break that splits a
#'   lowercase word, rather than keep it? Default `FALSE`.
#' @param dry_run Print a preview without writing anything. Default `TRUE`;
#'   pass `FALSE` to apply the rewrite.
#'
#' @return A data frame with one row per field that had continuation lines to
#'   join, with columns `dataset_id`, `field`, `line`, `n_lines_joined`,
#'   `hyphen_joins` (the words formed at line breaks ending in a hyphen,
#'   separated by `"; "`, or `NA`) and `value` (the joined value as written).
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
                                   dehyphenate = FALSE,
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
  var_in_pattern <- "^\\s*(-\\s+)?var_in:\\s*(.*?)\\s*$"

  reports <- lapply(dataset_id, function(id) {

    path <- metadata_path_dataset_id(id, path_data)
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

    # A `traits` entry can carry a field named after a context's `var_in`
    # (`stem type: branch`), which `key_pattern` would miss for its space,
    # absorbing it into the text above. Such a key is always one of the file's
    # `var_in` values.
    var_ins <- sub(var_in_pattern, "\\2", grep(var_in_pattern, lines, value = TRUE))
    var_ins <- unique(gsub("^(['\"])(.*)\\1$", "\\2", var_ins))
    var_ins <- var_ins[nzchar(var_ins) & !grepl("^[A-Za-z][A-Za-z0-9_]*$", var_ins)]
    var_in_keys <- paste0(c(var_ins, paste0("'", var_ins, "'"), paste0("\"", var_ins, "\"")), ":")

    starts_key <- function(line) {
      if (grepl(key_pattern, line)) return(TRUE)
      line <- sub("^\\s*", "", line)
      any(vapply(var_in_keys, function(k) {
        startsWith(line, k) && (nchar(line) == nchar(k) || grepl("^\\s", substr(line, nchar(k) + 1, nchar(k) + 1)))
      }, logical(1)))
    }

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
                (starts_key(line) || grepl(list_item_pattern, line))) break
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
              (starts_key(line) || grepl(list_item_pattern, line))) break

        continuation <- c(continuation, trimws(line))
        j <- j + 1L
      }

      if (length(continuation) == 0) {
        out <- c(out, lines[i])
        i <- i + 1L
        next
      }

      joined <- util_dewrap_join(c(value, continuation), dehyphenate = dehyphenate)
      written <- util_quote_if_needed(joined$text)
      out <- c(out, paste0(indent, field_name, ": ", written))

      report[[length(report) + 1]] <- data.frame(
        dataset_id = id, field = field_name, line = i,
        n_lines_joined = length(continuation) + 1L,
        hyphen_joins = if (length(joined$hyphen_joins) > 0) paste(joined$hyphen_joins, collapse = "; ") else NA_character_,
        value = written
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
      line = integer(), n_lines_joined = integer(),
      hyphen_joins = character(), value = character()
    )
  }

  if (dry_run) {
    for (r in seq_len(nrow(report))) {
      cat(sprintf("%s, line %d: %s (%d lines joined)\n", report$dataset_id[r], report$line[r],
                  report$field[r], report$n_lines_joined[r]))
      if (!is.na(report$hyphen_joins[r])) {
        cat("  joined at a hyphen: ", report$hyphen_joins[r], "\n", sep = "")
      }
      cat("  ", report$value[r], "\n\n", sep = "")
    }
    message("Nothing written. Call again with `dry_run = FALSE` to apply.")
    return(report)
  }

  invisible(report)
}
