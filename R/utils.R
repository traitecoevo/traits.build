#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
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
      lapply(function(xi) xi %>% sort() %>% unique() %>% paste(collapse = " ")) %>%
      unlist()
  x

}

#'  Convert dataframe to list
#'
#'  Convert a dataframe to a named list, useful when converting to yaml.
#'
#' @param df A dataframe
#' @return A (yaml) list
#' @export
#' @examples util_df_to_list(dplyr::starwars)
util_df_to_list <- function(df) {
  attr(df, "out.attrs") <- NULL
  unname(lapply(split(df, seq_len(nrow(df))), as.list))
}

#' Convert a list of lists to dataframe
#'
#' Convert a list of lists to dataframe; requires that every list have same named elements.
#'
#' @param my_list A list of lists to dataframe
#' @param as_character A logical value, indicating whether the values are read as character
#' @param on_empty Value to return if my_list is NULL, NA or is length == 0, default = NA
#'
#' @export
#' @examples util_list_to_df2(util_df_to_list(dplyr::starwars))
util_list_to_df2 <- function(my_list, as_character = TRUE, on_empty = NA) {

  if (is.null(my_list) || any(is.na(my_list)) || length(my_list) == 0)
    return(on_empty)

  if (as_character)
    my_list <- lapply(my_list, lapply, as.character)

  dplyr::bind_rows(lapply(my_list, tibble::as_tibble))
}

#' Convert a list with single entries to dataframe
#'
#' @param my_list A list with single entries
#' @return A tibble with two columns
#' @export
#' @examples \dontrun{
#' util_list_to_df1(as.list(dplyr::starwars)[2])
#' }
util_list_to_df1 <- function(my_list) {

  for (f in names(my_list)) {
    if (is.null(my_list[[f]]))
      my_list[[f]] <- NA
  }

  tibble::tibble(key = names(my_list), value = unname(unlist(my_list)))
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

#' Read in a `metadata.yml` file for a study
#'
#' @param path Location of the metadata file
#' @importFrom rlang .data
#'
#' @export
read_metadata <- function(path) {

  data <- yaml::read_yaml(path)

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
#' file \code{data/dataset_id/metadata.yml}
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
