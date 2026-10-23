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


#' Trait types a database may declare
#'
#' The values `type` may take in a trait's definition in `config/traits.yml`:
#' `numeric`, `categorical`, `monthly_presence` and `text`, as listed under
#' `trait_type` in the traits.build schema, which describes each. The type
#' decides which checks the build applies to the trait's values, so a trait
#' whose type is none of these would have its values pass through unchecked.
#'
#' @param schema The traits.build schema, as returned by [get_schema()]
#'
#' @return Character vector of trait types
#' @keywords internal
util_trait_types <- function(schema = get_schema()) {
  names(schema[["trait_type"]][["values"]])
}

#' Names of the traits of a given type
#'
#' @param definitions Trait definitions keyed by `trait_name` -- the `elements`
#'   of `traits.yml`, or the subset of them returned by [dataset_configure()]
#' @param types Character vector of trait types
#'
#' @return Character vector of the names of traits whose `type` is in `types`
#' @keywords internal
util_traits_of_type <- function(definitions, types) {
  if (length(definitions) == 0) return(character(0))
  is_type <- purrr::map_lgl(definitions, ~ length(.x[["type"]]) == 1 && .x[["type"]] %in% types)
  names(definitions)[is_type]
}

#' Find trait definitions whose `type` is missing or not supported
#'
#' @inheritParams util_traits_of_type
#' @inheritParams util_trait_types
#'
#' @return Character vector with one entry per offending trait, naming it and
#'   its type (e.g. "flowering_time (`categorial`)"); empty if every trait
#'   declares a supported type
#' @keywords internal
util_unsupported_trait_types <- function(definitions, schema = get_schema()) {
  if (length(definitions) == 0) return(character(0))
  types <- purrr::map_chr(
    definitions,
    ~ if (length(.x[["type"]]) == 1) as.character(.x[["type"]]) else NA_character_
  )
  bad <- is.na(types) | !types %in% util_trait_types(schema)
  sprintf(
    "%s (%s)",
    names(types)[bad],
    ifelse(is.na(types[bad]), "no `type`", sprintf("`%s`", types[bad]))
  )
}

#' Check values have the `monthly_presence` format
#'
#' A `monthly_presence` value is twelve characters, one per month starting with
#' January, each `y` or `n` (either case).
#'
#' @param x Character vector
#'
#' @return Logical vector, `TRUE` where `x` has the format
#' @keywords internal
util_is_monthly_presence <- function(x) {
  grepl("^[YyNn]{12}$", x)
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
