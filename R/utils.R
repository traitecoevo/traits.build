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


#' Does a `collection_date` value parse under the allowed formats?
#'
#' `collection_date` may record a year (`yyyy`), a year and month (`yyyy-mm`),
#' or a full date (`yyyy-mm-dd`), depending on the resolution available, and
#' may give either a single such value or a `start/end` range of two (e.g.
#' `2010-10/2011-03`). `NA` is always valid -- it means the date was not
#' recorded, not that it failed to parse.
#'
#' A range may have one side unknown, written as `.na` (e.g. `.na/2022` for
#' "before some point in 2022, exact start date unknown"). A *bare* `.na`
#' (the whole field, not one side of a range) is read by `yaml::read_yaml()`
#' as a real `NA` before this function ever sees it -- `.na` as one part of a
#' `/`-delimited range is not that same case, since the field as a whole is
#' not exactly `.na`, so it survives as the literal 3-character string and
#' has to be recognised here instead.
#'
#' A full `yyyy-mm-dd` date is checked against the real calendar (via
#' `as.Date()`), not just its shape, so e.g. `2021-02-29` (not a leap year)
#' is correctly rejected.
#'
#' @param x Character vector of `collection_date` values
#' @return Logical vector the same length as `x`, `TRUE` where the value is
#'  `NA` or parses under one of the allowed formats
#' @noRd
util_collection_date_is_valid <- function(x) {

  is_valid_single_date <- function(value) {
    if (identical(value, ".na")) {
      return(TRUE)
    }
    if (grepl("^[0-9]{4}$", value)) {
      return(TRUE)
    }
    if (grepl("^[0-9]{4}-[0-9]{2}$", value)) {
      month <- as.integer(substr(value, 6, 7))
      return(!is.na(month) && month >= 1 && month <= 12)
    }
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
      return(!is.na(as.Date(value, format = "%Y-%m-%d")))
    }
    FALSE
  }

  vapply(x, function(value) {
    if (is.na(value)) {
      return(TRUE)
    }
    parts <- strsplit(trimws(value), "/", fixed = TRUE)[[1]]
    if (!length(parts) %in% c(1, 2)) {
      return(FALSE)
    }
    all(vapply(parts, is_valid_single_date, logical(1)))
  }, logical(1), USE.NAMES = FALSE)
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
#'   plausible day
#' - Excel's numeric serial dates (e.g. `39692`), distinguished from a bare
#'   4-digit year by digit count
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
#' @export
util_parse_collection_date <- function(x, excel_origin = "1899-12-30") {

  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d"))
  }

  x <- as.character(x)
  x <- vapply(x, util_strip_time_of_day, character(1), USE.NAMES = FALSE)

  numeric_order <- util_infer_numeric_date_order(x)

  vapply(
    x,
    util_parse_collection_date_one,
    character(1),
    excel_origin = excel_origin,
    numeric_order = numeric_order,
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
# `_/_/_`- or `_-_-_`-shaped date is day-first or month-first -- i.e. does the
# first or second number exceed 12 anywhere, ruling out that slot being a
# month? Both separators are pooled into one inference, on the view that this
# is a property of who/what recorded the dates, not of which punctuation a
# given row happens to use. Returns "dmy", "mdy", or `NULL` if nothing in `x`
# resolves it (or if values disagree, which shouldn't happen in a single
# column, and isn't trusted if it does).
util_infer_numeric_date_order <- function(x) {
  candidates <- x[!is.na(x) & grepl("^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$", trimws(x))]
  if (length(candidates) == 0) {
    return(NULL)
  }

  parts <- strsplit(trimws(candidates), "[/-]")
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


# One value's worth of `util_parse_collection_date()`'s logic, split out so
# the `start/end` range case can recurse on each side. `numeric_order`
# ("dmy", "mdy", or NULL) is decided once, up front, by
# `util_infer_numeric_date_order()` looking at the *whole* column -- a
# single value has no way to resolve its own ambiguity.
util_parse_collection_date_one <- function(value, excel_origin, numeric_order = NULL) {

  if (is.na(value) || !nzchar(trimws(value))) {
    return(NA_character_)
  }

  value <- trimws(value)

  if (util_collection_date_is_valid(value)) {
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
        parts, util_parse_collection_date_one, character(1),
        excel_origin = excel_origin, numeric_order = numeric_order
      )
      if (!anyNA(fixed_parts) && all(vapply(fixed_parts, util_collection_date_is_valid, logical(1)))) {
        return(paste(fixed_parts, collapse = "/"))
      }
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
