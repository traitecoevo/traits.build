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

  data <- yaml::read_yaml(path)

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

  y <- y[c("source", "contributors", "dataset", "identifiers", "locations", "contexts", "traits",
               "substitutions", "taxonomic_updates", "exclude_observations", "questions")]


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
