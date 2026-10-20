#' Pipe operator
#'
#' See `magrittr::[\%>\%][magrittr::pipe]` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
#' @usage lhs \%>\% rhs
NULL

#' Read yaml (from package yaml)
#' @importFrom yaml read_yaml
#' @name read_yaml
#' @rdname read_yaml
NULL

#' write yaml (from package yaml)
#' @importFrom yaml write_yaml
#' @name write_yaml
#' @rdname write_yaml
NULL


#' Require an optional package
#'
#' Checks that a package listed under `Suggests` is available before it is used,
#' reporting the package name and why it is needed. In an interactive session
#' the user is offered the chance to install it.
#'
#' A thin wrapper around [rlang::check_installed()], kept as its own function so
#' that the missing-package branch can be exercised in tests without having to
#' uninstall the package.
#'
#' @param pkg Name of the required package
#' @param reason Sentence completing "The package is required ..." explaining
#'   what the package is needed for
#'
#' @return Called for its side effect. Returns `NULL` invisibly if `pkg` is
#'   available, otherwise throws an error.
#' @keywords internal
util_require_package <- function(pkg, reason) {
  rlang::check_installed(pkg, reason = reason)
}

#' Read in a csv as a tibble with column types as characters
#'
#' Reads in a csv file using the `read_csv` function from readr
#' with columns as characters.
#'
#' @param ... Arguments passed to the `read_csv()`
#'
#' @return A tibble
#' @export
read_csv_char <- function(...) {
  readr::read_csv(..., col_types = cols(.default = "c"), progress = FALSE)
}

#' Convert NULL values to a different value
#'
#' `util_replace_null` converts NULL values a different value. Default is
#' converting NULL to NA.
#'
#' @param x A NULL value or a non-NULL object
#' @param val Specify what the null value should be returned as, default is NA
#'
#' @return NA or a non-NULL object
#' @examples \dontrun{
#' util_replace_null(NULL)
#' }
util_replace_null <- function(x, val = NA) {
  if (is.null(x)) return(val)
    x
}


#' Convert all columns in data frame to character

#' @param df A dataframe
#'
#' @return A dataframe
#'
#' @examples lapply(traits.build:::util_df_convert_character(dplyr::starwars), class)
util_df_convert_character <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
}

#' Extract a trait element from the definitions$traits$elements
#'
#' @param i A value within the definitions$traits$elements list which refers to types of traits
#' @param my_list The list that contains the element we're interested in (i.e. definitions$traits$elements)
#' @param var The type of variable of a trait
#'
#' @return The element/properties of a trait
#'
#' @export
#' @examples
#' \dontrun{
#' util_extract_list_element(1, definitions$traits$elements, "units")
#' }
util_extract_list_element <- function(i, my_list, var) {
  i %>% lapply(function(x) my_list[[x]][[var]]) %>% lapply(util_replace_null) %>% unlist()
}



#' Sort a vector independently of the session's locale
#'
#' `sort()`, `order()` and `as.factor()` all collate character vectors
#' according to `LC_COLLATE`, so the same input is ordered differently
#' depending on the machine. Anything derived from that order (notably the
#' ids generated during a build) then differs between machines too. Sorting
#' with `method = "radix"` always collates in the C locale, which is the order
#' the database has always been built in on CI, and gives a reproducible result.
#'
#' `NA` values are dropped.
#'
#' @param x A vector
#'
#' @return `x`, sorted, with any `NA` removed
#' @keywords internal
util_sort_locale_independent <- function(x) {
  x[order(x, method = "radix", na.last = NA)]
}

#' Number a vector's unique values independently of the session's locale
#'
#' Replacement for `as.integer(as.factor(x))`, which numbers values by their
#' locale-collated order. `NA` is returned for `NA`, matching the behaviour of
#' `as.factor()`.
#'
#' @param x A vector
#'
#' @return Integer vector giving the position of each element of `x` among its
#'   sorted unique values
#' @keywords internal
util_index_locale_independent <- function(x) {
  match(x, util_sort_locale_independent(unique(x)))
}


#' Build the error message for a context `var_in` that names no column
#'
#' Reports the dataset, each affected `context_property`, and the column name
#' that is missing -- the three things the raw tidyselect failure did not name
#' (#247). Where a close match exists among the available columns it is
#' suggested, since the usual cause is a typo.
#'
#' @param contexts Contexts tibble, as returned by `process_format_contexts()`
#' @param missing_var_in Character vector of `var_in` values not found
#' @param data_columns Character vector of columns in the data, after
#'   `custom_R_code` has run
#' @param traits_fields Character vector of fields declared on the `traits`
#'   entries of `metadata.yml`, the other place a `var_in` may be satisfied
#'
#' @return A length-1 character string
#' @keywords internal
util_context_var_in_message <- function(contexts, missing_var_in,
                                        data_columns, traits_fields = character()) {

  dataset_id <- unique(contexts[["dataset_id"]])
  if (length(dataset_id) != 1 || is.na(dataset_id)) dataset_id <- "(unknown)"

  # `var_in` -> the context properties it was declared for
  properties <- function(v) {
    p <- unique(contexts[["context_property"]][contexts[["var_in"]] %in% v])
    paste(sprintf("'%s'", p), collapse = ", ")
  }

  # The usual cause is a typo, so point at the nearest available name. Edit
  # distance rather than `agrep()`, which matches on substrings and so offers
  # wild suggestions for short names.
  candidates <- unique(c(data_columns, traits_fields))
  suggestion <- function(v) {
    if (length(candidates) == 0) return("")
    d <- utils::adist(v, candidates, ignore.case = TRUE)[1, ]
    near <- candidates[d == min(d) & d <= max(1, floor(nchar(v) / 3))]
    if (length(near) == 0) return("")
    sprintf(" (did you mean %s?)", paste(sprintf("`%s`", near), collapse = " or "))
  }

  lines <- purrr::map_chr(
    missing_var_in,
    ~sprintf("  - context_property %s declares `var_in: %s`%s",
             properties(.x), .x, suggestion(.x))
  )

  paste0(
    sprintf("Dataset %s declares contexts with `var_in` naming %s not present in the data:\n",
            dataset_id,
            ifelse(length(missing_var_in) > 1, "columns", "a column")),
    paste(lines, collapse = "\n"), "\n",
    "  A context `var_in` must name either a column of `data.csv` (after `custom_R_code` has run)\n",
    "  or a field declared on the `traits` entries of `metadata.yml` (e.g. `method_context`).\n",
    sprintf("  Columns in the data: %s\n", paste(data_columns, collapse = ", ")),
    sprintf("  Fields on the `traits` entries: %s", paste(traits_fields, collapse = ", "))
  )
}


#' Resolve declared `traits`-entry field values against the data
#'
#' A field set on the `traits` entries of `metadata.yml` for a context's
#' `var_in` (e.g. `leaf_temp: LfT_Rdark`, scoping that context to just the
#' traits declaring it) is read the way `process_parse_data()` reads every such
#' field: a declared value naming a column of `data.csv` (after `custom_R_code`
#' has run) stands for that column's contents, read per record, and anything
#' else is the literal value itself (e.g. `method_context: 250 mm branch`).
#' `process_format_contexts()` and `dataset_test()`'s context-consistency check
#' both need this same resolution, so it lives here rather than being
#' duplicated and drifting between the two (#276).
#'
#' A blank cell in such a column is a record with no value for the context,
#' not a context value, so `NA` is dropped -- as it is for a `var_in` naming a
#' column directly. Otherwise `dataset_test()` reported `NA` as a context value
#' missing from the metadata whenever the column had a gap.
#'
#' @param declared Character vector of values declared on the `traits` entries
#'   for one field (already unique and stripped of `NA`)
#' @param data Data frame to check declared values against -- `data.csv` with
#'   `custom_R_code` applied
#'
#' @return Character vector of resolved values, without `NA`
#' @keywords internal
util_resolve_trait_field_values <- function(declared, data) {
  declared %>%
    purrr::map(~if (is.null(data[[.x]])) .x else unique(stats::na.omit(data[[.x]]))) %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    unique()
}


#'  Split and sort cells with multiple values
#'
#'  `util_separate_and_sort`: For a vector x in which individual cell may have
#'  multiple values (separated by 'sep'), sort records within each cell alphabetically.
#'
#' @param x An individual cell with multiple values
#' @param sep A separator, a whitespace is the default
#'
#' @return A vector of alphabetically sorted records
#'
#' @examples \dontrun{util_separate_and_sort("z y x")}
util_separate_and_sort <- function(x, sep = " ") {

  # Find cells with multiple values, indicated by presence of sep
  i <- grep(sep, x)
  # For those cells, split, sort then combine
  x[i] <- x[i] %>%
      stringr::str_split(" ") %>%
      lapply(function(xi) xi %>% util_sort_locale_independent() %>% unique() %>% paste(collapse = " ")) %>%
      unlist()
  x

}

#' Convert BibEntry object to a list
#'
#' @param bib BibEntry object
#'
#' @return List
util_bib_to_list <- function(bib) {

  # Read in file, convert to list, set key
  bib <- bib %>% unlist()

  if (!is.null(bib$author)) {
    bib$author <- paste(bib$author, collapse = " and ")
  }
  if (!is.null(bib$editor)) {
    bib$editor <- paste(bib$editor, collapse = " and ")
  }

  bib
}

#' Add an item to the end of a list
#'
#' @param my_list A list
#' @param to_append A list
#'
#' @return A list merged with an added item at the end
#' @examples  \dontrun{
#' util_append_to_list(as.list(dplyr::starwars)[c(1,2)], as.list(dplyr::starwars)[c(3,4)])
#' }
util_append_to_list <- function(my_list, to_append) {
  my_list[[length(my_list) + 1]] <- to_append
  my_list
}

# Default name for a dataset's separate locations file, and the number of
# locations beyond which holding them in `metadata.yml` stops being reasonable
locations_file_default <- "locations.csv"
locations_file_suggest_at <- 1000


#' Is this `locations:` block a reference to a separate csv file?
#'
#' @param locations The `locations` element of a metadata list
#'
#' @return `TRUE` for a single string naming a `.csv` file
#' @noRd
util_locations_is_file <- function(locations) {
  is.character(locations) &&
    length(locations) == 1 &&
    !is.na(locations) &&
    grepl("\\.csv$", locations, ignore.case = TRUE)
}


#' Read a dataset's locations from a separate csv file
#'
#' A `locations:` block holding one entry per georeferenced record makes a
#' `metadata.yml` unreadable and unreviewable -- `AVH_2026` in `austraits.build`
#' reaches 162,469 locations and 487,518 lines (#263). Such a dataset can
#' instead name a csv file, `locations: locations.csv`, kept beside
#' `metadata.yml` and read from here.
#'
#' The file is read one column per location property, which is the shape
#' `austraits$locations %>% tidyr::spread(location_property, value)` returns and
#' the shape [metadata_add_locations()] is given. Every column is read as
#' character, matching what the yaml path is coerced to downstream and leaving
#' values exactly as they are written in the file.
#'
#' A rectangle has no way of saying that a location does not record a property
#' at all, which an inline `locations:` block says by simply not listing it. An
#' **empty cell** means exactly that -- this location does not record this
#' property -- and the property is absent for that location, as it would be in
#' the yaml. A cell holding **`.na`** is the same "recorded, but unknown" that
#' `.na` means in the yaml, and is kept as a location property with a missing
#' value. Nothing else is treated as missing, so a location genuinely named
#' `NA` survives being written out and read back.
#'
#' @param file Name of the csv file, relative to the metadata file
#' @param path Location of the metadata file
#'
#' @return A tibble, carrying the file it was read from as an attribute
#' @noRd
util_read_locations_file <- function(file, path) {

  full <- file.path(dirname(path), file)

  if (!file.exists(full)) {
    stop(
      sprintf(
        paste0(
          "%s declares `locations: %s`, but %s does not exist.\n",
          "  A `locations:` entry naming a .csv file is read from that file, ",
          "which must sit beside `metadata.yml`."
        ),
        path, file, full
      ),
      call. = FALSE
    )
  }

  # `na = character()` so that neither an empty cell nor the string "NA" is
  # read as missing -- an empty cell means the property is absent for this
  # location, and `.na` means recorded but unknown. Both are handled in
  # `process_format_locations()`, which is where the distinction matters
  locations <- readr::read_csv(
    full,
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    progress = FALSE
  )

  if (!"location_name" %in% names(locations)) {
    stop(
      sprintf(
        paste0(
          "%s has no `location_name` column, so its rows cannot be matched to ",
          "the location names in `data.csv`.\n  Columns found: %s"
        ),
        full, paste(names(locations), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  duplicated_names <- unique(
    locations$location_name[duplicated(locations$location_name)]
  )

  if (length(duplicated_names) > 0) {
    stop(
      sprintf(
        "%s has %d duplicated `location_name`: %s",
        full, length(duplicated_names),
        paste(utils::head(duplicated_names, 5), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  attr(locations, "locations_file") <- file
  locations
}


#' Standardise a messy `collection_date` (or raw date) column
#'
#' Best-effort conversion of raw date values into the format
#' [dataset_test()]'s `collection_date`-parses check requires: a year
#' (`yyyy`), a year and month (`yyyy-mm`), or a full date (`yyyy-mm-dd`),
#' singly or as a `start/end` range. Meant to be called from a dataset's
#' `custom_R_code`, before `collection_date` is otherwise read in, e.g.:
#'
#' ```
#' custom_R_code: 'data %>% mutate(Date = util_parse_collection_date(Date))'
#' ```
#'
#' **What this deliberately will not touch**: a bare numeric date like
#' `01/02/2008`, seen *in isolation*, is genuinely ambiguous -- day-first or
#' month-first? -- and guessing wrong silently corrupts the data worse than
#' not guessing at all. Two things narrow that down first, though: (1) the
#' value's own two leading numbers -- `10/23/2021` can only be month-first,
#' since no month is `23` -- and only when that alone doesn't settle it, (2)
#' the rest of the column, on the view that a single source normally writes
#' dates one way (e.g. `9/14/2020` elsewhere rules out month `14`, settling
#' an otherwise-ambiguous `8/11/2020`). Real data can still mix conventions
#' *within* one column, though -- e.g. automated timestamps in month-first
#' order alongside hand-entered dates in day-first order -- so (1) is
#' checked per value, never overridden by (2), and if the column as a whole
#' disagrees with itself, (2) is not applied at all rather than picking a
#' side. Only once neither of those applies does a value come back
#' unchanged. Run [dataset_test()] again afterwards: whatever it still flags
#' under "Some date values are not parsing" needs a human decision, not a
#' better regex.
#'
#' **What it does handle**:
#' - Values already valid are passed through untouched
#' - A *named* month removes the day/month ambiguity regardless of which
#'   order it was written in, so `2-Sep-08`, `September 2, 2008`, `2 Sep
#'   2008` and similar all resolve safely
#' - A month name with no day (`Sep 2008`) resolves to the `yyyy-mm`
#'   resolution, not a fabricated day-1 date
#' - A plain numeric `d/m/y`-shaped date, resolved per-value or from the
#'   rest of the column as described above
#' - `mm-yyyy` (month first, the reverse of the schema's `yyyy-mm`) --
#'   unambiguous on its own, since a 4-digit year can't be mistaken for
#'   anything else
#' - A dot-separated `yy.mm.dd`/`yyyy.mm.dd`, confirmed (not assumed) by
#'   checking the middle component is a plausible month and the last a
#'   plausible day. A two-digit dotted date written day-first instead
#'   (`18.3.12`) can't be told apart from that by its values, so it needs
#'   `order = "dmy"`
#' - A dot-separated `d.m.yyyy` (`21.11.2019`), resolved like the `/` and `-`
#'   numeric dates above
#' - A compact `yyyymmdd` (`20190206`)
#' - A span of years written with a dash (`2019-2020` becomes `2019/2020`)
#' - A comma- or semicolon-separated list of dates (`Jan-98, Feb-98,
#'   Mar-98`), collapsed to the `first/last` range it covers (`1998-01/1998-03`)
#'   -- tried only once the value has failed to parse whole, so a comma
#'   *inside* one date (`September 2, 2008`) is never split on
#' - Excel's numeric serial dates (e.g. `39692`), distinguished from a bare
#'   4-digit year by digit count. A number that is really R's own day count
#'   (left behind when `ifelse()` strips a `Date` column's class) looks exactly
#'   like one and would be read decades too early -- pass the raw column to
#'   this function instead of converting it with `lubridate` first
#' - A trailing time-of-day is dropped, both `"yyyy-mm-ddTHH:MM:SSZ"` (ISO
#'   8601) and `"d/m/y HH:MM"` (e.g. a Google Forms timestamp column) --
#'   `collection_date` has no way to record a time, only the date
#' - A `start/end` range is split on `/` and each side parsed independently,
#'   *unless* the whole value already looks like a plain `d/m/y`-shaped date
#'   (in which case `/` is a date separator, not a range separator, and the
#'   rules above apply to the whole thing)
#' - `Date`/`POSIXct` columns (already parsed by `read_csv()`'s own type
#'   guessing before `custom_R_code` runs) are formatted directly, dropping
#'   any time-of-day component
#'
#' @param x A character, `Date`, or `POSIXct` vector of raw date values --
#'  the whole column at once, not one value at a time, since resolving a
#'  plain numeric date's order depends on seeing the rest of `x`
#' @param order Optional, `"dmy"` or `"mdy"`: the curator's own statement of
#'  which way round an all-numeric date is written, for when nothing in the
#'  column can settle it -- e.g. a single `4/07/2022`, or two-digit dotted
#'  dates like `18.3.12` (`d.m.yy`). It replaces the column-wide inference,
#'  but a value that settles its own order (e.g. `10/23/2021`, since no month
#'  is `23`) is still read that way. Converting a `custom_R_code` that called
#'  `lubridate::dmy()` or `lubridate::mdy()` is then a like-for-like swap
#' @param excel_origin Origin date for numeric Excel serial dates. The
#'  default (`1899-12-30`) is correct for a workbook built on Windows Excel;
#'  one built on old Mac Excel needs `1904-01-01` instead
#' @return Character vector the same length as `x`. Values already valid, or
#'  safely resolved to `yyyy`, `yyyy-mm` or `yyyy-mm-dd` (singly or as a
#'  `/`-delimited range), are returned in that form; anything left ambiguous
#'  or unrecognised is returned completely unchanged
#'
#' @examples
#' util_parse_collection_date(c(
#'   "2-Sep-08", "September 2, 2008", "Sep 2008", "39692",
#'   "2008-09-02/2008-09-03", "01/02/2008", "2020", NA
#' ))
#'
#' # the whole column is used to resolve a plain numeric date's order:
#' # `9/14/2020` can only be month-first, so `8/11/2020` is read the same way
#' util_parse_collection_date(c("8/11/2020 10:09", "9/14/2020 12:06"))
#'
#' # nothing in a lone `4/07/2022` says which way round it is, so say so:
#' util_parse_collection_date("4/07/2022", order = "dmy")
#' @export
util_parse_collection_date <- function(x, order = NULL, excel_origin = "1899-12-30") {

  if (!is.null(order)) {
    order <- match.arg(order, c("dmy", "mdy"))
  }

  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d"))
  }

  x <- as.character(x)
  x <- vapply(x, util_strip_time_of_day, character(1), USE.NAMES = FALSE)

  numeric_order <- if (!is.null(order)) order else util_infer_numeric_date_order(x)

  vapply(
    x,
    util_parse_collection_date_one,
    character(1),
    excel_origin = excel_origin,
    numeric_order = numeric_order,
    order = order,
    USE.NAMES = FALSE
  )
}


# Drop a trailing time-of-day from a datetime string, leaving just the date
# part -- `collection_date` has no way to record a time. Handles ISO 8601
# (`yyyy-mm-ddTHH:MM:SS[.ffffff]Z`) and a plain numeric date followed by a
# time (`d/m/y HH:MM[:SS][ AM|PM]`, e.g. a Google Forms timestamp column).
# Anything else is returned unchanged.
util_strip_time_of_day <- function(value) {
  if (is.na(value)) {
    return(value)
  }
  value <- sub(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2})[T ][0-9]{1,2}:[0-9]{2}(:[0-9]{2})?(\\.[0-9]+)?Z?$",
    "\\1", value
  )
  value <- sub(
    "^([0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4})\\s+[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?\\s*([AaPp][Mm])?$",
    "\\1", value
  )
  value
}


# Does any value in `x` unambiguously reveal whether a plain numeric
# `_/_/_`-, `_-_-_`- or `_._.yyyy`-shaped date is day-first or month-first --
# i.e. does the first or second number exceed 12 anywhere, ruling out that
# slot being a month? All separators are pooled into one inference, on the
# view that this is a property of who/what recorded the dates, not of which
# punctuation a given row happens to use. A dotted date only counts with a
# 4-digit year: a two-digit dotted triple is read as `yy.mm.dd` unless the
# caller says otherwise, so it has no say here. Returns "dmy", "mdy", or
# `NULL` if nothing in `x` resolves it (or if values disagree, which
# shouldn't happen in a single column, and isn't trusted if it does).
util_infer_numeric_date_order <- function(x) {
  candidates <- x[!is.na(x) & grepl(
    "^[0-9]{1,2}([/-][0-9]{1,2}[/-][0-9]{2,4}|\\.[0-9]{1,2}\\.[0-9]{4})$", trimws(x)
  )]
  if (length(candidates) == 0) {
    return(NULL)
  }

  parts <- strsplit(trimws(candidates), "[/.-]")
  first <- vapply(parts, function(p) as.integer(p[1]), integer(1))
  second <- vapply(parts, function(p) as.integer(p[2]), integer(1))

  first_over_12 <- any(first > 12, na.rm = TRUE)
  second_over_12 <- any(second > 12, na.rm = TRUE)

  if (first_over_12 && !second_over_12) {
    return("dmy")
  }
  if (second_over_12 && !first_over_12) {
    return("mdy")
  }
  NULL
}


# Resolve a plain numeric date's two leading components (already parsed as
# integers, in the order they appear in the original string) plus its year
# string into `yyyy-mm-dd`. Shared by the `/`- and `-`-separated branches of
# util_parse_collection_date_one() below -- identical ambiguity, identical
# resolution, just a different separator. Returns `NA_character_` if the
# order can't be determined (self-disambiguation failed and `numeric_order`
# is `NULL`) or the resulting date doesn't exist on the calendar.
util_resolve_numeric_dmy_date <- function(first, second, year_str, numeric_order) {

  # This *specific* value can be self-disambiguating regardless of what the
  # rest of the column looks like: if exactly one of its own two leading
  # numbers exceeds 12, it can't be a month, so the order is settled without
  # needing column-wide agreement.
  order <-
    if (first > 12 && second <= 12) "dmy"
    else if (second > 12 && first <= 12) "mdy"
    else numeric_order

  if (is.null(order)) {
    return(NA_character_)
  }

  day <- if (order == "dmy") first else second
  month <- if (order == "dmy") second else first
  year <- util_expand_two_digit_year(year_str)
  if (is.na(day) || day < 1 || day > 31 || is.na(month) || month < 1 || month > 12 || is.na(year)) {
    return(NA_character_)
  }

  full_date <- sprintf("%d-%02d-%02d", year, month, day)
  if (!is.na(as.Date(full_date, format = "%Y-%m-%d"))) {
    return(full_date)
  }
  NA_character_
}


# One value's worth of `util_parse_collection_date()`'s logic. `numeric_order`
# ("dmy", "mdy", or NULL) is decided once, up front, for the *whole* column --
# the caller's `order` if given, otherwise `util_infer_numeric_date_order()`
# -- since a single value has no way to resolve its own ambiguity. `order` is
# passed through separately because one shape (a two-digit dotted triple) is
# only re-read on the caller's say-so, never on inference alone.
#
# A value that doesn't parse whole but holds a comma- or semicolon-separated
# list of dates (e.g. `Jan-98, Feb-98, Mar-98`, the months a collection
# spanned) is collapsed to the `first/last` range they cover. That is only
# tried *after* the whole value has failed, because a comma can also sit
# inside a single date -- `September 2, 2008` split on its comma would read
# `September 2` as 2002.
util_parse_collection_date_one <- function(value, excel_origin, numeric_order = NULL, order = NULL) {

  parsed <- util_parse_collection_date_single(value, excel_origin, numeric_order, order)

  if (is.na(parsed) || util_collection_date_is_valid(parsed) || !grepl("[,;]", parsed)) {
    return(parsed)
  }

  items <- trimws(strsplit(parsed, "[,;]")[[1]])
  items <- items[nzchar(items)]
  items_parsed <- vapply(
    items, util_parse_collection_date_single, character(1),
    excel_origin = excel_origin, numeric_order = numeric_order, order = order,
    USE.NAMES = FALSE
  )
  if (length(items_parsed) == 0 || anyNA(items_parsed) ||
      !all(util_collection_date_is_valid(items_parsed))) {
    return(parsed)
  }

  # Every item is a valid `yyyy[-mm[-dd]]` (or a range of two), and those sort
  # correctly as plain strings, so the span is just the smallest and largest
  # endpoint
  endpoints <- unlist(strsplit(items_parsed, "/", fixed = TRUE))
  if (".na" %in% endpoints) {
    return(parsed)
  }
  span <- unique(c(min(endpoints), max(endpoints)))
  paste(span, collapse = "/")
}


util_parse_collection_date_single <- function(value, excel_origin, numeric_order = NULL, order = NULL) {

  if (is.na(value) || !nzchar(trimws(value))) {
    return(NA_character_)
  }

  value <- trimws(value)

  if (util_collection_date_is_valid(value)) {
    return(value)
  }

  # A span of years written with a dash (`2019-2020`) rather than the schema's
  # `/`. Two 4-digit years can't be read as anything else.
  if (grepl("^[0-9]{4}\\s*-\\s*[0-9]{4}$", value)) {
    years <- as.integer(strsplit(gsub("\\s", "", value), "-", fixed = TRUE)[[1]])
    if (years[2] >= years[1]) {
      return(paste(years, collapse = "/"))
    }
    return(value)
  }

  # An unambiguous `mm-yyyy` (month first, dash, 4-digit year) -- the
  # reverse of the schema's `yyyy-mm`. A 4-digit year in the second slot
  # rules out any other reading, so this needs no column-wide context.
  if (grepl("^[0-9]{1,2}-[0-9]{4}$", value)) {
    date_parts <- strsplit(value, "-", fixed = TRUE)[[1]]
    month <- suppressWarnings(as.integer(date_parts[1]))
    if (!is.na(month) && month >= 1 && month <= 12) {
      return(sprintf("%s-%02d", date_parts[2], month))
    }
    return(value)
  }

  # A dot-separated `d.m.yyyy` (e.g. Lequerica_Tamara_2025's `21.11.2019`) is
  # the `/`- and `-`-separated numeric date again with different punctuation,
  # and is resolved the same way. A two-digit dotted triple is less settled:
  # Doyle_2023 writes `yy.mm.dd` (`19.08.20`), Bloomfield_2018 writes `d.m.yy`
  # (`18.3.12`), and nothing in the values tells the two apart -- so it is
  # only read as day/month-first when the caller passed `order`, and as
  # `yy.mm.dd` (just below) otherwise.
  if (grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$", value) ||
      (!is.null(order) && grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$", value))) {
    date_parts <- strsplit(value, ".", fixed = TRUE)[[1]]
    resolved <- util_resolve_numeric_dmy_date(
      as.integer(date_parts[1]), as.integer(date_parts[2]), date_parts[3], numeric_order
    )
    if (!is.na(resolved)) {
      return(resolved)
    }
    return(value)
  }

  # A dot-separated `yy.mm.dd` or `yyyy.mm.dd`. Confirmed (not just assumed)
  # by checking the middle component is a plausible month (<=12) and the
  # last is a plausible day (<=31) -- if the shape doesn't fit that specific
  # order, this is left alone rather than guessed at.
  if (grepl("^[0-9]{2,4}\\.[0-9]{1,2}\\.[0-9]{1,2}$", value)) {
    date_parts <- suppressWarnings(as.integer(strsplit(value, ".", fixed = TRUE)[[1]]))
    year_part <- date_parts[1]; month <- date_parts[2]; day <- date_parts[3]
    if (!anyNA(c(year_part, month, day)) && month >= 1 && month <= 12 && day >= 1 && day <= 31) {
      year <- util_expand_two_digit_year(as.character(year_part))
      if (!is.na(year)) {
        full_date <- sprintf("%d-%02d-%02d", year, month, day)
        if (!is.na(as.Date(full_date, format = "%Y-%m-%d"))) {
          return(full_date)
        }
      }
    }
    return(value)
  }

  # A plain `d-m-y`-shaped date (dash, no named month), structurally
  # identical to the `/`-separated case just below -- just a different
  # separator. `Coates_2024`'s `date_standardised` column is exclusively this
  # shape (its raw `date` column is the `/`-separated equivalent below,
  # zero-padded and re-punctuated upstream of `data.csv`), and mixes
  # self-disambiguating values (e.g. `28-11-2021`) with ones that need the
  # rest of the column to resolve (e.g. `05-01-2022`).
  if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", value)) {
    date_parts <- as.integer(strsplit(value, "-", fixed = TRUE)[[1]])
    resolved <- util_resolve_numeric_dmy_date(date_parts[1], date_parts[2], as.character(date_parts[3]), numeric_order)
    if (!is.na(resolved)) {
      return(resolved)
    }
    return(value)
  }

  # A plain `d/m/y`-shaped date (no named month) is ambiguous on its own,
  # and that ambiguity applies to the whole value, not to "is `/` a range
  # separator here?" -- so this check comes before the range-splitting
  # attempt below. Real datasets do mix conventions row to row -- Coates_2024's
  # raw `date` column has automated camera-trap timestamps in month-first
  # order alongside hand-entered dates in day-first order in the *same*
  # column -- so column-wide agreement would wrongly hold a self-disambiguating
  # value hostage to unrelated rows; see util_resolve_numeric_dmy_date().
  if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", value)) {
    date_parts <- as.integer(strsplit(value, "/", fixed = TRUE)[[1]])
    resolved <- util_resolve_numeric_dmy_date(date_parts[1], date_parts[2], as.character(date_parts[3]), numeric_order)
    if (!is.na(resolved)) {
      return(resolved)
    }
    return(value)
  }

  if (grepl("/", value, fixed = TRUE)) {
    parts <- strsplit(value, "/", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      fixed_parts <- vapply(
        parts, util_parse_collection_date_single, character(1),
        excel_origin = excel_origin, numeric_order = numeric_order, order = order
      )
      if (!anyNA(fixed_parts) && all(vapply(fixed_parts, util_collection_date_is_valid, logical(1)))) {
        return(paste(fixed_parts, collapse = "/"))
      }
    }
    return(value)
  }

  # A compact `yyyymmdd` (e.g. Arnold_2021's `20190206`), checked before the
  # Excel serial rule below would claim it: a serial for any real collection
  # date has 5 digits, so 8 digits starting with a plausible century can't be
  # one. Checked against the real calendar, not just its shape.
  if (grepl("^(18|19|20)[0-9]{6}$", value)) {
    full_date <- sprintf("%s-%s-%s", substr(value, 1, 4), substr(value, 5, 6), substr(value, 7, 8))
    if (!is.na(as.Date(full_date, format = "%Y-%m-%d"))) {
      return(full_date)
    }
    return(value)
  }

  # Excel serial date: a purely numeric string that isn't a plausible bare
  # year (exactly 4 digits) is almost certainly a spreadsheet serial number,
  # not a year -- e.g. `39692`, not `2008`
  if (grepl("^[0-9]+$", value) && nchar(value) != 4) {
    serial <- suppressWarnings(as.numeric(value))
    parsed <- if (!is.na(serial)) as.Date(serial, origin = excel_origin) else NA
    if (!is.na(parsed)) {
      return(format(parsed, "%Y-%m-%d"))
    }
    return(value)
  }

  # From here on, only attempt a guess when a *named* month makes the
  # day/month order unambiguous regardless of who wrote it or in what
  # locale -- a purely numeric date with no name to anchor it is exactly
  # the ambiguous case this function refuses to guess at.
  #
  # This is done with explicit string surgery below rather than handed to
  # `lubridate::parse_date_time(orders = c("dmy", "mdy", "ymd"))`, because
  # that genuinely gives wrong answers for exactly this kind of input: it
  # reuses digits from a *single* token to fill more than one date
  # component when the string has fewer separate tokens than the order
  # template expects, e.g. `parse_date_time("Sep 2008", orders = "mdy")`
  # returns 2008-09-**20** (day fabricated from within "2008"), and
  # `parse_date_time("2008-Sep-02", orders = "dmy")` returns **2002-08-20**
  # -- both silently wrong, not `NA`, so there is no failure mode to catch.
  month_match <- util_extract_month_name(value)
  if (is.null(month_match)) {
    return(value)
  }

  numbers <- regmatches(value, gregexpr("[0-9]+", value))[[1]]

  if (length(numbers) == 0) {
    return(value)
  }

  if (length(numbers) == 1) {
    year <- util_expand_two_digit_year(numbers[1])
    if (is.na(year)) {
      return(value)
    }
    return(sprintf("%d-%s", year, month_match$month))
  }

  if (length(numbers) == 2) {
    is_four_digit <- nchar(numbers) == 4

    if (sum(is_four_digit) == 1) {
      year_str <- numbers[is_four_digit]
      day_str <- numbers[!is_four_digit]
    } else if (sum(is_four_digit) == 0) {
      # Neither token unambiguously reads as a 4-digit year (both are the
      # short kind that could be a day or a 2-digit year). The only such
      # shape actually seen in practice is `d-Mon-yy` (e.g. `2-Sep-08`), so
      # fall back to position: whichever number is written after the month
      # name is the year, whichever is before it is the day.
      number_starts <- as.integer(gregexpr("[0-9]+", value)[[1]])
      is_after_month <- number_starts > month_match$end
      if (sum(is_after_month) != 1) {
        return(value)
      }
      year_str <- numbers[is_after_month]
      day_str <- numbers[!is_after_month]
    } else {
      return(value)
    }

    day <- suppressWarnings(as.integer(day_str))
    if (is.na(day) || day < 1 || day > 31) {
      return(value)
    }

    year <- if (nchar(year_str) == 4) as.integer(year_str) else util_expand_two_digit_year(year_str)

    full_date <- sprintf("%d-%s-%02d", year, month_match$month, day)
    if (!is.na(as.Date(full_date, format = "%Y-%m-%d"))) {
      return(full_date)
    }
    return(value)
  }

  # 3+ leftover numbers alongside a month name isn't a shape this function
  # knows how to interpret safely
  value
}


# Month names/abbreviations (English only) mapped to "01".."12"
month_number_by_name <- stats::setNames(
  sprintf("%02d", rep(1:12, 2)),
  tolower(c(month.name, month.abb))
)


# Find the first month name/abbreviation in `value` as a whole word
# (case-insensitive). Returns `NULL` if there isn't one, otherwise the
# 2-digit month number and the match's start/end character positions (used
# to tell whether a number token sits before or after the month name).
util_extract_month_name <- function(value) {
  pattern <- paste0("(?i)\\b(", paste(names(month_number_by_name), collapse = "|"), ")\\b")
  m <- regexpr(pattern, value, perl = TRUE)
  if (m == -1) {
    return(NULL)
  }
  matched_text <- tolower(regmatches(value, m))
  list(
    month = month_number_by_name[[matched_text]],
    start = as.integer(m),
    end = as.integer(m) + attr(m, "match.length") - 1L
  )
}


# Expand a 1-2 digit year to 4 digits (the POSIX/glibc convention: 00-68 ->
# 2000-2068, 69-99 -> 1969-1999), or pass a plausible 4-digit year through
# unchanged. Anything else (more or fewer digits) returns `NA`.
util_expand_two_digit_year <- function(year_str) {
  n <- nchar(year_str)
  year <- suppressWarnings(as.integer(year_str))
  if (is.na(year)) {
    return(NA_integer_)
  }
  if (n == 4) {
    return(year)
  }
  if (n %in% c(1, 2)) {
    return(if (year <= 68) 2000L + year else 1900L + year)
  }
  NA_integer_
}


# Top-level blocks of a `metadata.yml`, in the order `write_metadata()` writes
# them. Also used by `util_read_yaml_chunked()` to find where a block ends.
metadata_blocks <- c(
  "source", "contributors", "dataset", "identifiers", "locations", "contexts",
  "traits", "substitutions", "taxonomic_updates", "exclude_observations",
  "questions"
)


#' Read a `metadata.yml`, parsing a very large `locations:` block in chunks
#'
#' `yaml::read_yaml()` costs roughly O(n^2) in the number of *container* nodes
#' a document contains -- not in its size. A flat sequence of 40,000 scalars
#' parses in 0.02s; the 40,000-entry map of maps that a `locations:` block is
#' takes 20s. Datasets compiled from per-record georeferenced occurrences
#' generate one location per record, so this bites hard: `AVH_2026` in
#' `austraits.build` has 162,469 locations and took 911s to read (#263).
#'
#' The cost is per *parse call*, so this lifts the `locations:` body out of the
#' file, parses the (now small) remainder normally, and parses the body in
#' chunks of `chunk` entries. Every chunk still goes through the same YAML
#' parser, so nothing about how the file is interpreted changes -- on `AVH_2026`
#' this reads the same 162,469 locations in 1.7s.
#'
#' Anything unexpected about the file's structure, and any error from a chunk,
#' falls back to parsing the whole file with `yaml::read_yaml()`. The fast path
#' can therefore only ever be slower than the plain one, never disagree with it,
#' and a genuinely malformed file still reports the real parser's error against
#' the real line numbers.
#'
#' @param path Location of the metadata file
#' @param chunk Number of `locations:` entries to parse per call
#' @param threshold Minimum number of entries before chunking is worth its
#'  overhead. Below this the plain parser is used.
#'
#' @return A list, as `yaml::read_yaml()` returns
#' @noRd
util_read_yaml_chunked <- function(path, chunk = 500L, threshold = 1000L) {

  plain <- function() yaml::read_yaml(path)

  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

  # A `locations:` block with its entries on following lines. Anything else --
  # absent, `locations: .na`, more than one -- is not the case we optimise.
  opens <- which(lines == "locations:")
  if (length(opens) != 1L) return(plain())

  # The block runs to the next top-level block. Anchoring on the known block
  # names rather than on "any unindented line" matters: `custom_R_code` holds
  # arbitrary R, whose lines are frequently unindented.
  starts_block <- grep(
    paste0("^(", paste(metadata_blocks, collapse = "|"), "):"), lines
  )
  following <- starts_block[starts_block > opens]
  last <- if (length(following) > 0) following[1] - 1L else length(lines)
  if (last <= opens) return(plain())

  body <- lines[(opens + 1L):last]
  entries <- grep("^  [^ ]", body)

  # Only chunk a block that is unambiguously a map of locations: enough entries
  # to be worth it, every line indented into the block, and every entry a
  # mapping key. The last test is what keeps sequence-valued blocks out --
  # `taxonomic_updates:` writes `- find:` at indent 0 with its remaining keys at
  # indent 2, so its continuation lines would otherwise read as entry starts and
  # chunk boundaries would fall inside an entry.
  worth_it <- length(entries) >= threshold &&
    all(grepl("^(\\s*$|  )", body)) &&
    all(grepl("^  [^ #-].*:", body[entries]))
  if (!worth_it) return(plain())

  breaks <- seq(1L, length(entries), by = chunk)

  out <- try(
    {
      parsed <- yaml::yaml.load(
        paste(lines[-((opens + 1L):last)], collapse = "\n")
      )

      chunks <- vector("list", length(breaks))
      for (i in seq_along(breaks)) {
        from <- entries[breaks[i]]
        to <- if (breaks[i] + chunk <= length(entries)) {
          entries[breaks[i] + chunk] - 1L
        } else {
          length(body)
        }
        # Dedent by one level so each chunk is a document in its own right
        chunks[[i]] <- yaml::yaml.load(
          paste(sub("^  ", "", body[from:to]), collapse = "\n")
        )
      }
      parsed$locations <- unlist(chunks, recursive = FALSE)
      parsed
    },
    silent = TRUE
  )

  # Every entry accounted for, or we did not understand the file after all
  if (inherits(out, "try-error") || length(out$locations) != length(entries)) {
    return(plain())
  }

  out
}


#' Read in a `metadata.yml` file for a study
#'
#' A dataset's `locations:` may either list the locations inline, or name a csv
#' file beside `metadata.yml` -- `locations: locations.csv` -- which is read in
#' its place and returned as a tibble.
#'
#' @param path Location of the metadata file
#' @importFrom rlang .data
#'
#' @export
read_metadata <- function(path) {

  data <- util_read_yaml_chunked(path)

  if (util_locations_is_file(data[["locations"]])) {
    data[["locations"]] <- util_read_locations_file(data[["locations"]], path)
  }

  # We want to preserve formatting in custom R code
  # but `read_yaml` loses it
  # So read in as text, if not empty
  if (!is.na(data$dataset$custom_R_code)) {
    # Read in again, extracting custom R code

    data2 <- readLines(path, encoding = "UTF-8")

    code_start <- grep("  custom_R_code:", data2, fixed = TRUE)
    code_end <- grep("  collection_date:", data2, fixed = TRUE)[1] - 1

    data$dataset$custom_R_code <-
      data2[code_start:code_end] %>%
      gsub("custom_R_code:", "", ., fixed = TRUE) %>%
      paste(collapse = "\n")
  }

  data
}


#' Read the `metadata.yml` file for specified `dataset_id`
#'
#' @inheritParams metadata_path_dataset_id
#'
#' @return A list with contents of metadata for specified `dataset_id`
read_metadata_dataset <- function(dataset_id, path_data = "data") {
  dataset_id %>%
    metadata_path_dataset_id(path_data = path_data) %>%
    read_metadata()
}


#' Write `metadata.yml` for a study
#'
#' Write `metadata.yml` file with custom R code formatted to allow line breaks.
#'
#' @param data `austraits` metadata object (a list)
#' @param path Location where the metadata file is to be written to
#' @param style_code Should the R code be styled?
#'
#' @rdname write_metadata
#' @importFrom rlang .data
#' @importFrom styler style_text
#' @export
#' @examples
#' \dontrun{
#' f <- "data/Falster_2003/metadata.yml"
#' data <- read_metadata(f)
#' write_metadata(data, f)
#' }
write_metadata <- function(data, path, style_code = FALSE) {

  y <- data
  y$dataset$custom_R_code <- NA

  # For metadata files that don't yet include "identifiers", add before writing file
  if (!"identifiers" %in% names(y)) {
    y["identifiers"] <- NA
  }

  # Locations held in a separate csv are written back to it, and referred to
  # from the yaml by name rather than inlined
  if (is.data.frame(y[["locations"]])) {
    file <- attr(y[["locations"]], "locations_file")
    if (is.null(file)) file <- locations_file_default

    # `na = ""`, matching how the file is read: an empty cell is a property
    # this location does not record
    readr::write_csv(
      y[["locations"]], file.path(dirname(path), file), na = ""
    )
    y[["locations"]] <- file
  }

  y <- y[metadata_blocks]


  txt <- yaml::as.yaml(y, column.major = FALSE, indent = 2) %>%
    gsub(": ~", ":", ., fixed = TRUE)


  # Reinsert custom R code
  if (!is.na(data$dataset$custom_R_code)) {

    code <- data$dataset$custom_R_code
    code <- stringr::str_trim(code, side = "left")

    if (style_code)
      code <- code %>% suppressWarnings(styler::style_text(transformers = .data$tidyverse_style(strict = TRUE)))

    txt <- gsub("custom_R_code: .na", code %>% paste(collapse = "\n") %>%
                  paste0("custom_R_code: ", .), txt, fixed = TRUE)
  }

  if (!stringr::str_sub(txt, nchar(txt)) == "\n")
    txt <- c(txt, "\n")

  file <- file(path, "w", encoding = "UTF-8")
  on.exit(close(file))
  cat(txt, file = file)
}


#' Write the YAML representation of `metadata.yml` for specified `dataset_id` to
#' file `data/dataset_id/metadata.yml`
#'
#' @inheritParams metadata_path_dataset_id
#' @param metadata Metadata file
#'
#' @return A yaml file
write_metadata_dataset <- function(metadata, dataset_id) {
  write_metadata(metadata, dataset_id %>% metadata_path_dataset_id())
}


#' Format a tree structure from a vector
#'
#' `create_tree_branch()` is used to create a tree structure to show how things
#' are related. In AusTraits, this is used in the vignettes to show the file
#' structure of the repository and also to show the different components of the
#' AusTraits database.
#'
#' @param x Vector of terms
#' @param title Name of branch
#' @param prefix Specifies the amount of indentation
#'
#' @return Vector of character strings for the tree structure
create_tree_branch <- function(x, title, prefix = "") {
  c(
    sprintf("%s%s", prefix, title),
    sprintf(
      "%s%s %s", prefix,
      c(rep("\u251c\u2500\u2500", length(x) - 1), "\u2514\u2500\u2500"),
      x
    )
  )
}

# Renaming and re-exporting austraits functions to ensure old scripts still work

#' Convert a list with single entries to dataframe
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param my_list A list with single entries
#' @return A tibble with two columns
#' @export
#' @examples \dontrun{
#' util_list_to_df1(as.list(dplyr::starwars)[2])
#' }
util_list_to_df1 <- function(my_list) {
  lifecycle::deprecate_warn("1.0.0", "util_list_to_df1()", "austraits::convert_list_to_df1()")
  austraits::convert_list_to_df1(my_list)
}

#' @importFrom austraits convert_list_to_df1
#' @export
austraits::convert_list_to_df1

#' Convert a list of lists to dataframe
#'
#' @description
#' Convert a list of lists to dataframe; requires that every list have same named elements.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param my_list A list of lists to dataframe
#' @param as_character A logical value, indicating whether the values are read as character
#' @param on_empty Value to return if my_list is NULL, NA or is length == 0, default = NA
#'
#' @export
#' @examples util_list_to_df2(util_df_to_list(dplyr::starwars))
util_list_to_df2 <- function(my_list, as_character = TRUE, on_empty = NA) {
  lifecycle::deprecate_warn("1.0.0", "util_list_to_df2()", "austraits::convert_list_to_df2()")
  austraits::convert_list_to_df2(my_list, as_character, on_empty)
}

#' @importFrom austraits convert_list_to_df2
#' @export
austraits::convert_list_to_df2

#' Convert dataframe to list
#'
#' @description
#' Convert a dataframe to a named list, useful when converting to yaml.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param df A dataframe
#' @return A (yaml) list
#' @export
#' @examples util_df_to_list(dplyr::starwars)
util_df_to_list <- function(df) {
  lifecycle::deprecate_warn("1.0.0", "util_df_to_list()", "austraits::convert_df_to_list()")
  austraits::convert_df_to_list(df)
}

#' @importFrom austraits convert_df_to_list
#' @export
austraits::convert_df_to_list

#' Combine all the AusTraits studies into the compiled AusTraits database
#'
#' @description
#' `build_combine` compiles all the loaded studies into a single AusTraits
#' database object as a large list.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param ... Arguments passed to other functions
#' @param d List of all the AusTraits studies
#'
#' @return AusTraits compilation database as a large list
#' @importFrom rlang .data
#' @export
build_combine <- function(..., d = list(...)) {
  lifecycle::deprecate_warn("1.0.0", "build_combine()", "austraits::bind_databases()")
  # The shim pointed users at `bind_databases()` but called
  # `convert_df_to_list()`, and passed the studies twice -- once through `...`
  # and again as `d`, which `d = list(...)` had already collected. So it
  # returned nonsense for anyone who followed the deprecation notice.
  austraits::bind_databases(databases = d)
}

#' @importFrom austraits bind_databases
#' @export
austraits::bind_databases

#' @importFrom austraits flatten_database
#' @export
austraits::flatten_database

#' Create a single combined table from a database
#'
#' Joins the relational tables of a built database into one wide table, by
#' calling [austraits::flatten_database()].
#'
#' Wenk et al. 2024 (*Ecological Informatics* 83:102773) presents
#' `database_create_combined_table` as the route to the combined table, but it
#' was only ever assigned here and never exported, so the published workflow
#' could not be followed. The paper is the public specification of this
#' workflow, so the name it documents resolves.
#'
#' This is a thin pass-through rather than a reimplementation on purpose. The
#' joins it relies on are eight functions totalling ~264 lines in `austraits`,
#' and querying a built compilation is that package's job -- this one builds
#' the database. Arguments are passed straight through so the defaults have a
#' single definition.
#'
#' Note that `austraits` is currently in `Depends`. When it moves to `Suggests`
#' this needs a `util_require_package("austraits", ...)` guard, as does
#' [build_combine()] -- see #225.
#'
#' @param database A built database object
#' @param ... Further arguments passed to [austraits::flatten_database()],
#' such as `format`, `vars` and `include_description`
#'
#' @return A single wide table combining the database's relational tables
#' @export
database_create_combined_table <- function(database, ...) {
  austraits::flatten_database(database, ...)
}

#' Check `collection_date` values against the standard formats
#'
#' A value is valid when it is one ISO 8601 date -- `yyyy`, `yyyy-mm` or
#' `yyyy-mm-dd` -- or a range of two such dates separated by a `/` whose start
#' is not after its end. Each date must exist in the calendar and fall between
#' 1700 and today. Nothing else is: no surrounding whitespace, no other
#' separators and no time of day. In particular not `unknown/yyyy` or
#' `.na/yyyy`, which the build writes as the year alone, `yyyy`
#' (see [util_drop_unknown_collection_date_start()]).
#'
#' Each distinct value is checked once, so the cost follows the number of
#' distinct dates rather than the number of records.
#'
#' @param x Vector of `collection_date` values
#'
#' @return Logical vector the length of `x`, `NA` where `x` is `NA`
#' @noRd
util_collection_date_is_valid <- function(x) {
  x <- as.character(x)
  values <- unique(x[!is.na(x)])
  valid <- vapply(values, util_collection_date_is_valid_one, logical(1), USE.NAMES = FALSE)
  valid[match(x, values)]
}

#' Write `unknown/yyyy` and `.na/yyyy` `collection_date` values as `yyyy`
#'
#' A range with an unknown start says no more than its end does, so the end
#' alone is kept. `.na` here is the literal text: only a whole field of `.na`
#' is read as missing. The end may be any one `yyyy`, `yyyy-mm` or
#' `yyyy-mm-dd` date; anything else is left as it is, for `dataset_test()`
#' to report.
#'
#' @param x Vector of `collection_date` values
#'
#' @return `x`, with those values replaced by their end date
#' @noRd
util_drop_unknown_collection_date_start <- function(x) {
  sub("^(unknown|\\.na)/([0-9]{4}(-[0-9]{2}){0,2})$", "\\2", x)
}

#' @noRd
util_collection_date_is_valid_one <- function(value) {
  # `strsplit()` drops an empty trailing piece, so `2020/` would pass as a
  # single date if the separators were not counted separately
  n_separators <- nchar(gsub("[^/]", "", value))
  parts <- strsplit(value, "/", fixed = TRUE)[[1]]
  if (n_separators > 1 || length(parts) != n_separators + 1) {
    return(FALSE)
  }

  bounds <- lapply(parts, util_collection_date_bounds)
  if (any(vapply(bounds, is.null, logical(1)))) {
    return(FALSE)
  }

  length(bounds) == 1 || bounds[[1]][1] <= bounds[[2]][2]
}

#' First and last day covered by one `yyyy`, `yyyy-mm` or `yyyy-mm-dd` date
#'
#' @return Date vector of length two, or `NULL` when `value` is not one of those
#' forms, is not a real date, or falls outside 1700 to today
#' @noRd
util_collection_date_bounds <- function(value) {
  if (!grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", value)) {
    return(NULL)
  }

  precision <- nchar(value)
  first <- as.Date(
    switch(as.character(precision), "4" = paste0(value, "-01-01"), "7" = paste0(value, "-01"), value),
    format = "%Y-%m-%d"
  )
  if (is.na(first) || first < as.Date("1700-01-01") || first > Sys.Date()) {
    return(NULL)
  }

  last <- switch(
    as.character(precision),
    "4" = as.Date(paste0(value, "-12-31")),
    "7" = seq(first, by = "month", length.out = 2)[2] - 1,
    first
  )
  c(first, last)
}

#' Convert `collection_date` values from a declared format to the standard one
#'
#' Rewrites dates written in the format a dataset declares as `yyyy-mm-dd`,
#' `yyyy-mm` or `yyyy`. A value is never guessed at: one that does not match
#' the declared format is returned unchanged, so `dataset_test()` reports it
#' rather than the build storing a guess. Values that are already standard pass
#' through untouched, and a range of years written with a hyphen, `2019-2020`,
#' becomes `2019/2020`.
#'
#' The build calls this on the `collection_date` column(s) of `data.csv` for a
#' dataset that sets `collection_date_format` or `collection_date_timezone` in
#' its `metadata.yml`, after `custom_R_code` has run. It can also be called from
#' `custom_R_code`, for a dataset that needs to convert dates before combining
#' them -- for example, to paste a start and an end column into a range.
#'
#' @param x Vector of dates as read from `data.csv`: character, numeric, `Date`
#'   or `POSIXct`
#' @param format Character vector of the formats the values are written in,
#'   tried in turn for each value: any of `"ymd"`, `"dmy"`, `"mdy"`, `"ym"`,
#'   `"my"` and `"y"`. The order sets the precision kept, so a column holding
#'   full dates, month-year values and bare years is declared
#'   `c("dmy", "my", "y")` -- or `dmy my y` in `metadata.yml`. Formats that
#'   could read the same value two ways -- `"dmy"` with `"mdy"`, say -- cannot
#'   be combined. `"ym"` needs a four-digit year, since `15-Mar` is how Excel
#'   writes the 15th of March with no year.
#'
#'   `"auto"` works the format out from the whole column: it tries
#'   `dmy my y`, `mdy my y` and `ymd ym y`, and uses the one that reads every
#'   value any of them can. If more than one does and they give different
#'   dates -- a column whose values could all be day-first or month-first, such
#'   as `4/07/2022` -- it converts nothing, so the curator declares which. It
#'   decides for the column, never value by value, so it cannot read one value
#'   day-first and the next month-first.
#'
#'   Within a value, day, month and year may be separated by `-`, `/`, `.`, `,`
#'   or spaces, or not at all (`20190206`). Months may be numbers or English
#'   names, full or abbreviated, and a leading weekday name is ignored
#'   (`Thr Nov 18 2010`, as LI-6400 files write it). A two-digit year is taken as the most recent
#'   year ending in those digits that is not in the future, so `08` is 2008 and
#'   `78` is 1978. A time of day following the date is dropped.
#' @param timezone Time zone to take the date in, for values that are a point in
#'   time -- a `POSIXct` column, or text ending in `Z` or a UTC offset -- as an
#'   Olson name such as `"Australia/Sydney"`. Without it those values are
#'   returned unchanged: `2022-06-03T14:00:00Z` is 3 June in UTC but 4 June in
#'   Sydney, and only the dataset's curator knows which is meant.
#'
#' @return Character vector the length of `x`
#' @export
#'
#' @examples
#' util_format_collection_date(c("2-Sep-08", "15/10/2008", "Oct 2008", "2008"), c("dmy", "my", "y"))
#' util_format_collection_date("2022-06-03T14:00:00Z", timezone = "Australia/Sydney")
util_format_collection_date <- function(x, format = NULL, timezone = NULL) {
  format <- util_check_collection_date_format(format)
  timezone <- util_check_collection_date_timezone(timezone)

  if (inherits(x, "POSIXt")) {
    if (is.null(timezone)) {
      return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M:%S %Z")))
    }
    return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d", tz = timezone)))
  }

  if (inherits(x, "Date")) {
    return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d")))
  }

  if (is.numeric(x)) {
    x <- ifelse(is.na(x), NA_character_, format(x, scientific = FALSE, trim = TRUE))
  }

  x <- as.character(x)
  values <- unique(x[!is.na(x)])

  if (identical(format, "auto")) {
    format <- util_collection_date_auto(values)$format
  }

  converted <- vapply(
    values, util_format_collection_date_one, character(1),
    format = format, timezone = timezone, USE.NAMES = FALSE
  )
  converted[match(x, values)]
}

# The formats `auto` chooses between: a full date written day-first,
# month-first or year-first, each followed by the coarser precisions a column
# of such dates also holds
util_collection_date_families <- list(
  dmy = c("dmy", "my", "y"),
  mdy = c("mdy", "my", "y"),
  ymd = c("ymd", "ym", "y")
)

#' Work out the format of a column of dates, or decline to
#'
#' Reads every value in each of the formats in `util_collection_date_families`,
#' and chooses the one that reads all the values any of them can. If more than
#' one does, they must give the same dates -- as for a column of `Jan-96`
#' values, which `dmy my y` and `mdy my y` both read as `1996-01`. Otherwise
#' nothing is chosen, so a column whose values could all be day-first or
#' month-first (`4/07/2022`) is left for the curator to declare, as is one
#' mixing values only day-first can read with values only month-first can.
#' Values no format reads are left out, so one malformed value does not block
#' the rest.
#'
#' @param values Character vector of dates
#'
#' @return List with `format`, the chosen formats or `NULL`, and `fits`, the
#'   names of the formats that read every readable value
#' @noRd
util_collection_date_auto <- function(values) {
  values <- unique(values[!is.na(values)])

  readings <- lapply(util_collection_date_families, function(format) {
    out <- vapply(
      values, util_format_collection_date_one, character(1),
      format = format, timezone = NULL, USE.NAMES = FALSE
    )
    ifelse(util_collection_date_is_valid(out) %in% TRUE, out, NA_character_)
  })

  readable <- Reduce(`|`, lapply(readings, function(r) !is.na(r)))
  fits <- names(readings)[vapply(readings, function(r) !anyNA(r[readable]), logical(1))]
  agree <- length(fits) > 0 &&
    all(vapply(fits, function(f) identical(readings[[f]][readable], readings[[fits[1]]][readable]), logical(1)))

  list(
    format = if (agree) util_collection_date_families[[fits[1]]] else NULL,
    fits = fits,
    readings = readings,
    values = values
  )
}

#' Explain why `auto` could not choose a format, for `dataset_test()`
#'
#' @return A sentence, or `""` if `auto` can choose one for these values
#' @noRd
util_collection_date_auto_message <- function(values) {
  auto <- util_collection_date_auto(values)
  if (!is.null(auto$format)) {
    return("")
  }

  if (length(auto$fits) == 0) {
    return("`auto` found no one format that reads them all, so a column mixing formats needs `custom_R_code`.")
  }

  # A value the candidate formats read differently, to show why
  first <- auto$readings[[auto$fits[1]]]
  differs <- which(Reduce(`|`, lapply(auto$fits[-1], function(f) auto$readings[[f]] != first)) %in% TRUE)[1]
  sprintf(
    "`auto` cannot tell whether they are written %s: '%s' reads as %s. Declare the one they are written in.",
    paste0("`", auto$fits, "`", collapse = " or "),
    auto$values[differs],
    paste(vapply(auto$fits, function(f) auto$readings[[f]][differs], character(1)), collapse = " or ")
  )
}

#' @noRd
util_format_collection_date_one <- function(value, format, timezone) {
  value <- trimws(value)

  if (isTRUE(util_collection_date_is_valid(value))) {
    return(value)
  }

  # A point in time, which falls on different dates in different time zones
  if (grepl("[0-9]:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)? ?(Z|[+-][0-9]{2}:?[0-9]{2})$", value)) {
    if (is.null(timezone)) {
      return(value)
    }
    instant <- lubridate::ymd_hms(value, quiet = TRUE)
    return(if (is.na(instant)) value else format(instant, "%Y-%m-%d", tz = timezone))
  }

  # A range of years written with a hyphen, `2019-2020`, can be read only one way
  if (grepl("^[0-9]{4} ?[-\u2013] ?[0-9]{4}$", value)) {
    years <- gsub(" ?[-\u2013] ?", "/", value)
    return(if (isTRUE(util_collection_date_is_valid(years))) years else value)
  }

  # A local time of day does not move the date, so it is dropped
  date <- sub("[ T][0-9]{1,2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?( ?[AaPp][Mm])?$", "", value)

  for (f in format) {
    converted <- util_collection_date_match(date, f)
    if (!is.na(converted) && isTRUE(util_collection_date_is_valid(converted))) {
      return(converted)
    }
  }

  value
}

#' Read one value as one declared format
#'
#' @return The value as `yyyy-mm-dd`, `yyyy-mm` or `yyyy`, to the precision
#' `format` gives, or `NA` if it does not have that shape. Whether the date
#' exists is left to the caller.
#' @noRd
util_collection_date_match <- function(value, format) {
  roles <- strsplit(format, "")[[1]]
  tokens <- strsplit(value, "[-/., ]+")[[1]]
  tokens <- tokens[tokens != ""]

  # A leading weekday adds nothing to the date. LI-6400 files write them, with
  # Thursday as `Thr`: `Thr Nov 18 2010`
  weekdays <- c(
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
    "mon", "tue", "tues", "wed", "thu", "thur", "thurs", "thr", "fri", "sat", "sun"
  )
  if (length(tokens) > length(roles) && tolower(tokens[1]) %in% weekdays) {
    tokens <- tokens[-1]
  }

  # Written without separators: yyyymmdd, ddmmyyyy, mmddyyyy, yyyymm or mmyyyy
  if (length(tokens) == 1 && length(roles) > 1 && grepl("^[0-9]+$", tokens)) {
    widths <- list(ymd = c(4, 2, 2), dmy = c(2, 2, 4), mdy = c(2, 2, 4), ym = c(4, 2), my = c(2, 4))[[format]]
    if (nchar(tokens) != sum(widths)) {
      return(NA_character_)
    }
    tokens <- substring(tokens, cumsum(widths) - widths + 1, cumsum(widths))
  }

  if (length(tokens) != length(roles)) {
    return(NA_character_)
  }

  # A two-digit year before a month is how Excel writes day-month with no year
  # (`15-Mar`), not a year-month, so `ym` needs all four digits
  if (format == "ym" && !grepl("^[0-9]{4}$", tokens[1])) {
    return(NA_character_)
  }

  year <- util_collection_date_year(tokens[roles == "y"])
  month <- if ("m" %in% roles) util_collection_date_month(tokens[roles == "m"]) else NULL
  day <- NULL
  if ("d" %in% roles) {
    day <- if (grepl("^[0-9]{1,2}$", tokens[roles == "d"])) as.integer(tokens[roles == "d"]) else NA_integer_
  }

  if (is.na(year) || isTRUE(is.na(month)) || isTRUE(is.na(day))) {
    return(NA_character_)
  }

  paste0(
    sprintf("%04d", year),
    if (!is.null(month)) sprintf("-%02d", month),
    if (!is.null(day)) sprintf("-%02d", day)
  )
}

#' @noRd
util_collection_date_year <- function(token) {
  if (grepl("^[0-9]{4}$", token)) {
    return(as.integer(token))
  }
  if (grepl("^[0-9]{2}$", token)) {
    # The most recent year ending in these digits that is not in the future
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    year <- this_year - (this_year - as.integer(token)) %% 100
    return(year)
  }
  NA_integer_
}

#' @noRd
util_collection_date_month <- function(token) {
  if (grepl("^[0-9]{1,2}$", token)) {
    month <- as.integer(token)
    return(if (month >= 1 && month <= 12) month else NA_integer_)
  }
  token <- tolower(token)
  month <- match(token, tolower(month.name))
  if (is.na(month)) month <- match(token, tolower(month.abb))
  if (is.na(month) && token == "sept") month <- 9L
  month
}

#' Validate a dataset's `collection_date_format`
#'
#' In `metadata.yml` several formats are written as one space-separated value,
#' `dmy my y`, the convention for multiple values elsewhere in traits.build.
#'
#' @return The formats as a character vector, empty when none is declared
#' @noRd
util_check_collection_date_format <- function(format) {
  format <- unlist(strsplit(as.character(unlist(format)), "\\s+"))
  format <- format[!is.na(format) & format != "" & format != ".na"]
  if (length(format) == 0) {
    return(character(0))
  }

  allowed <- c("ymd", "dmy", "mdy", "ym", "my", "y", "auto")
  unknown <- setdiff(format, allowed)
  if (length(unknown) > 0) {
    stop(
      sprintf(
        "`collection_date_format` must be one or more of %s, not '%s'",
        paste0("`", allowed, "`", collapse = ", "), paste(unknown, collapse = "', '")
      ),
      call. = FALSE
    )
  }

  if ("auto" %in% format && length(format) > 1) {
    stop("`collection_date_format: auto` works out the formats itself, so cannot be combined with others", call. = FALSE)
  }

  if (sum(format %in% c("ymd", "dmy", "mdy")) > 1 || sum(format %in% c("ym", "my")) > 1) {
    stop(
      sprintf(
        paste0(
          "`collection_date_format` combines formats that read the same value two ways ('%s'). ",
          "Convert a column that mixes them with `custom_R_code` instead."
        ),
        paste(format, collapse = "', '")
      ),
      call. = FALSE
    )
  }

  format
}

#' Validate a dataset's `collection_date_timezone`
#'
#' @return The time zone, or `NULL` when none is declared
#' @noRd
util_check_collection_date_timezone <- function(timezone) {
  timezone <- unlist(timezone)
  if (length(timezone) == 0 || (length(timezone) == 1 && (is.na(timezone) || timezone == ".na"))) {
    return(NULL)
  }
  if (length(timezone) != 1 || !timezone %in% OlsonNames()) {
    stop(
      sprintf(
        "`collection_date_timezone` must be one Olson time zone name, such as 'Australia/Sydney', not '%s'",
        paste(timezone, collapse = "', '")
      ),
      call. = FALSE
    )
  }
  timezone
}
