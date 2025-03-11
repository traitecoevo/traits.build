#' Path to the `metadata.yml` file for specified `dataset_id`
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param path_data Path to folder with data
#'
#' @return A string
metadata_path_dataset_id <- function(dataset_id, path_data = "data") {
  file.path(path_data, dataset_id, "metadata.yml")
}

#' Create a template of file `metadata.yml` for specified `dataset_id`
#'
#' Includes place-holders for major sections of the metadata.
#'
#' @inheritParams metadata_path_dataset_id
#' @param path Location of file where output is saved
#' @param skip_manual Allows skipping of manual selection of variables, default = FALSE
#' @param user_responses Named list containing simulated user input for manual selection
#' of variables, mainly for testing purposes
#'
#' @importFrom readr read_csv
#' @importFrom utils menu
#' @return A yaml file template for metadata
#' @export
metadata_create_template <- function(dataset_id,
                                     path = file.path("data", dataset_id),
                                     skip_manual = FALSE,
                                     user_responses = NULL
                                     ) {

  if (file.exists(paste0(path, "/metadata.yml"))) {
    message(
      sprintf(
        red("Metadata for ") %+%
          blue("%s") %+% red(" already exists and will be overwritten"),
        blue(dataset_id)
      ))
  }

  `%notin%` <- Negate(`%in%`)
  fields <- c("source", "contributors", "dataset")
  articles <- c("key", "bibtype", "year", "author", "title", "journal", "volume", "number", "pages", "doi")

  # Get schema and build metadata template
  out <- get_schema()$metadata$elements

  out[names(out) %notin% fields] <- NA
  out$source <- out$source$values["primary"]
  out$source$primary <- out$source$primary$values[articles]
  out$source$primary[] <- "unknown"
  out$source$primary["key"] <- dataset_id
  out$source$primary["bibtype"] <- "Article"

  collectors <- c("last_name", "given_name", "ORCID", "affiliation")

  out$contributors <- out$contributors$elements
  out$contributors$data_collectors <- list(out$contributors$data_collectors$elements[collectors])
  out$contributors$data_collectors[[1]][] <- "unknown"
  out$contributors[c("assistants", "dataset_curators")] <- "unknown"

  out$dataset <-
    out$dataset$values[c("data_is_long_format", "custom_R_code", "collection_date", "taxon_name", "location_name",
                         "description", "basis_of_record", "life_stage", "sampling_strategy", "original_file", "notes")]
  out$dataset[] <- "unknown"
  out$dataset$custom_R_code <- NA

  if (skip_manual == FALSE) {

    # Fill metadata fields with manual selection from user
    if (is.null(user_responses)) {

      # Check format of data
      tmp <- menu(c("Long", "Wide"), title = "Is the data long or wide format?")
      data_is_long_format <- ifelse(tmp == 1, TRUE, FALSE)

      out$dataset$data_is_long_format <- data_is_long_format

      data <- readr::read_csv(paste0(path, "/data.csv"), col_types = cols())

      # Setup config and select columns as appropriate
      if (data_is_long_format) {
        v1 <- c("taxon_name", "trait_name", "value")
      } else {
        out$dataset[c("trait_name", "value")] <- NULL
        v1 <- c("taxon_name")
      }

      for (v in v1) {
        out[["dataset"]][[v]] <- metadata_user_select_column(v, names(data))
      }

      v2 <- c("location_name", "individual_id", "collection_date")

      for (v in v2) {
        tmp <- metadata_user_select_column(v, c(NA, names(data)))
        if (!is.na(tmp)) {
          out[["dataset"]][[v]] <- tmp
        }
        if (v == "collection_date" && is.na(tmp)) {
          collection_date <- readline(prompt = "\nEnter `collection_date` range in format '2007/2009': ")
          out[["dataset"]][[v]] <- collection_date
        }
      }

      # `repeat_measurements_id`
      tmp <- menu(c("Yes", "No"), title = "\nDo all traits need `repeat_measurements_id`'s?")

      if (tmp == 1) {
        repeat_measurements_id <- TRUE
        out[["dataset"]][["repeat_measurements_id"]] <- repeat_measurements_id
      }

    # Use `user_responses` to fill metadata fields
    } else {

      data_is_long_format <- user_responses$data_is_long_format
      out$dataset$data_is_long_format <- data_is_long_format

      data <- readr::read_csv(paste0(path, "/data.csv"), col_types = cols())

      # Setup config and select columns as appropriate
      if (data_is_long_format) {
        v1 <- c("taxon_name", "trait_name", "value")
      } else {
        out$dataset[c("trait_name", "value")] <- NULL
        v1 <- c("taxon_name")
      }

      for (v in v1) {
        out[["dataset"]][[v]] <- user_responses[[v]]
      }

      v2 <- c("location_name", "individual_id", "collection_date")

      for (v in v2) {
        tmp <- user_responses[[v]]
        if (!is.na(tmp)) {
          out[["dataset"]][[v]] <- tmp
        }
      }

      # `repeat_measurements_id`
      if (!is.null(user_responses[["repeat_measurements_id"]]) && user_responses[["repeat_measurements_id"]] == TRUE) {
        out[["dataset"]][["repeat_measurements_id"]] <- TRUE
      }

    }
  }

  # Reorder elements in dataset
  order <- c("data_is_long_format", "custom_R_code", "collection_date", "taxon_name", "trait_name",
             "value", "location_name", "individual_id", "repeat_measurements_id", "description",
             "basis_of_record", "life_stage", "sampling_strategy", "original_file", "notes")

  order <- order[which(order %in% names(out[["dataset"]]))]
  out[["dataset"]] <- out[["dataset"]][order]

  write_metadata(out, paste0(path, "/metadata.yml"))
  return(invisible(out))

}


#' Select column by user
#'
#' `metadata_user_select_column` is used to select which columns in a dataframe/tibble
#' corresponds to the variable of interest. It is used to compile the metadata yaml
#' file by prompting the user to choose the relevant columns. It is used in
#' `metadata_add_locations` and `metadata_create_template`.
#'
#' @param column Name of the variable of interest
#' @param choices The options that can be selected from
#'
metadata_user_select_column <- function(column, choices) {

  tmp <- utils::menu(choices, title = sprintf("\nSelect column for `%s`", column))

  choices[tmp]
}


#' Select variable names by user
#'
#' `metadata_user_select_names` is used to prompt the user to select the variables that
#' are relevant for compiling the metadata yaml file. It is currently used for
#' `metadata_add_traits`, `metadata_add_locations` and `metadata_add_contexts`.
#'
#' @param title Character string providing the instruction for the user
#' @param vars Variable names
#'
metadata_user_select_names <- function(title, vars) {

  txt <- sprintf(
    "%s (by number separated by space; e.g. '1 2 4'):\n%s\n",
    title, paste(sprintf("%d: %s", seq_len(length(vars)), vars), collapse = "\n")
  )

  success <- FALSE
  while (!success) {
    cat(txt)
    i <- readline("\nSelection: ")
    i <- strsplit(i, " ")[[1]] %>% as.integer()

    if (all(i %in% seq_len(length(vars)))) {
      success <- TRUE
    } else {
      message("Invalid selection, please try again\n")
    }
  }

  vars[i]

}


#' Check the output of running `custom_R_code` specified in
#' the metadata for specified `dataset_id`
#'
#' Function to check the output of running `custom_R_code` specified in
#' the `metadata.yml` file for specified `dataset_id`.
#' For the specified `dataset_id`, reads in the file `data.csv` and
#' applies manipulations as described in the file `metadata.yml`
#'
#' @inheritParams metadata_path_dataset_id
#'
#' @export
metadata_check_custom_R_code <- function(dataset_id, path_data = "data") {

  # Read metadata
  metadata <- read_metadata_dataset(dataset_id, path_data)

  # Load trait data and run `custom_R_code`
  readr::read_csv(file.path(path_data, dataset_id, "data.csv"), col_types = cols(), guess_max = 100000) %>%
    process_custom_code(metadata[["dataset"]][["custom_R_code"]])()

}


#' For specified `dataset_id`, populate columns for traits into metadata
#'
#' This function asks users which traits they would like to keep, and adds a template
#' for those traits in the metadata. This template must then be finished manually.
#'
#' Can also be used to add a trait to an existing metadata file.
#'
#' @inheritParams metadata_path_dataset_id
#' @param user_responses Named list containing simulated user input for manual selection
#' of variables, mainly for testing purposes
#'
#' @importFrom rlang .data
#' @importFrom crayon %+%
#' @export
metadata_add_traits <- function(dataset_id, user_responses = NULL) {

  # Read metadata
  metadata <- read_metadata_dataset(dataset_id)

  # Load and clean trait data
  data <- readr::read_csv(file.path("data", dataset_id, "data.csv"), col_types = cols()) %>%
    process_custom_code(metadata[["dataset"]][["custom_R_code"]])()

  # Get list of potential traits
  if (!metadata$dataset$data_is_long_format) {
    v <- names(data)
  } else {
    v <- unique(data[[metadata$dataset$trait_name]])
  }

  # Check if `user_responses` have been inputted
  if (is.null(user_responses)) {
    var_in <-
      metadata_user_select_names(
        paste("Indicate all columns you wish to keep as distinct traits in", dataset_id), v
      )
  } else {
    var_in <- user_responses$var_in
  }

  traits <- tibble::tibble(
    var_in = var_in,
    unit_in = "unknown",
    trait_name = "unknown",
    entity_type = "unknown",
    value_type = "unknown",
    basis_of_value = "unknown",
    replicates = "unknown",
    methods = "unknown")

  # Check if existing content, if so append
  if (!all(is.na(metadata$traits))) {

    existing_var_in <- metadata$traits %>% austraits::convert_list_to_df2() %>% dplyr::pull("var_in")
    if (any(var_in %in% existing_var_in)) {
      message(
        sprintf(red("Following traits already exist in the metadata and will be skipped: ") %+%
          green("'%s'"),
        paste(var_in[var_in %in% existing_var_in], collapse = "', '"))
      )
    }

    # Append new traits if not already in metadata
    traits <- dplyr::bind_rows(metadata$traits %>% austraits::convert_list_to_df2(), traits) %>%
      dplyr::filter(!duplicated(var_in))

    if (length(var_in[!var_in %in% existing_var_in]) > 0) {
      message(
        sprintf(
          red("Following traits added to metadata for %s") %+% red(": ") %+% green("'%s'") %+% red("\n\tPlease complete information in %s"),
          blue(dataset_id),
          paste(var_in[!var_in %in% existing_var_in], collapse = "', '"),
          blue(dataset_id %>% metadata_path_dataset_id())
        ))
    }
  } else {
    message(
      sprintf(
        red("Following traits added to metadata for %s") %+% red(": ") %+% green("'%s'") %+% red("\n\tPlease complete information in %s"),
        blue(dataset_id),
        paste(var_in, collapse = "', '"),
        blue(dataset_id %>% metadata_path_dataset_id())
      ))
  }

  metadata$traits <- traits %>% austraits::convert_df_to_list()
  write_metadata_dataset(metadata, dataset_id)
  return(invisible(metadata))

}


#' For specified `dataset_id` import location data from a dataframe
#'
#' This functions asks users which columns in the dataframe they would like to keep
#' and records this appropriately in the metadata. The input data is assumed to be
#' in wide format.
#' The output may require additional manual editing.
#'
#' @inheritParams metadata_path_dataset_id
#' @param location_data A dataframe of site variables
#' @param user_responses Named list containing simulated user input for manual selection
#' of variables, mainly for testing purposes
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' austraits$locations %>% dplyr::filter(dataset_id == "Falster_2005_1") %>%
#' select(-dataset_id) %>% spread(location_property, value) %>% type_convert() -> location_data
#' metadata_add_locations("Falster_2005_1", location_data)
#' }
metadata_add_locations <- function(dataset_id, location_data, user_responses = NULL) {

  vars <- names(location_data)

  # Read metadata
  metadata <- read_metadata_dataset(dataset_id)

  if (!all(is.na(metadata[["locations"]]))) {
    message(sprintf(red("Location metadata for %s") %+% red(" already exists and will be overwritten"), blue(dataset_id)))
  }

  # Check if `user_responses` have been inputted
  if (is.null(user_responses)) {
    # Choose column for location_name
    location_name <- metadata_user_select_column("location_name", vars)

    # From remaining variables, choose those to keep
    keep <- metadata_user_select_names(
      paste("Indicate all columns you wish to keep as distinct location_properties in",
      dataset_id), vars[vars != location_name]
    )
  } else {
    location_name <- user_responses$location_name
    keep <- user_responses$keep
  }

  # Save and notify
  location_data <-  location_data %>%
    dplyr::select(dplyr::all_of(c(location_name, keep))) %>%
    dplyr::distinct()

  # If user didn't select any variables to keep, so add defaults
  if (is.na(keep[1])) {
    location_data <-  location_data %>%
    dplyr::mutate(
      `latitude (deg)` = NA_character_,
      `longitude (deg)` = NA_character_,
      `description` = NA_character_,
      )
  }

  metadata$locations <- location_data %>%
    dplyr::select(-dplyr::any_of(location_name)) %>%
    split(location_data[[location_name]]) %>%
    lapply(as.list)

  message(
    sprintf(
      red("Following locations added to metadata for %s") %+% red(": ") %+% green("'%s'\n\t") %+%
        red("with variables ") %+% green("'%s'\n\t") %+% red("Please complete information in %s"),
      blue(dataset_id),
      paste(names(metadata$locations), collapse = "', '"),
      ifelse(is.na(keep[1]), "latitude (deg)', 'longitude (deg)', 'description", paste(keep, collapse = "', '")),
      blue(dataset_id %>% metadata_path_dataset_id())
    )
  )

  if (nrow(location_data) != length(unique(location_data[[location_name]]))) {
  message(
    sprintf(
      red("WARNING: The number of unique location names (%s), is less than the number rows of location data to add (%s). ") %+%
        red("Manual editing is REQUIRED in %s to ensure each location has a single value for each location property."),
      blue(length(unique(location_data[[location_name]]))),
      blue(nrow(location_data)),
      blue(dataset_id %>% metadata_path_dataset_id())
    )
  )
  }

  write_metadata_dataset(metadata, dataset_id)
  return(invisible(metadata))

}


#' For specified `dataset_id` import context data from a dataframe
#'
#' This functions asks users which columns in the dataframe they would like to keep
#' and records this appropriately in the metadata. The input data is assumed to be
#' in wide format.
#' The output may require additional manual editing.
#'
#' @inheritParams metadata_path_dataset_id
#' @param overwrite Overwrite existing information
#' @param user_responses Named list containing simulated user input for manual selection
#' of variables, mainly for testing purposes
#'
#' @importFrom rlang .data
#' @export
metadata_add_contexts <- function(dataset_id, overwrite = FALSE, user_responses = NULL) {

  # Read metadata
  metadata <- read_metadata_dataset(dataset_id)

  # Load and clean trait data
  data <-
    readr::read_csv(file.path("data", dataset_id, "data.csv"), col_types = cols()) %>%
    process_custom_code(metadata[["dataset"]][["custom_R_code"]])()

  # Get list of potential columns
  v <- names(data)

  contexts <- list()
  n_existing <- 0

  # Check for existing info
  if (!overwrite && !is.na(metadata$contexts[1])) {
    contexts <- metadata$contexts
    n_existing <- length(metadata$contexts)

    message(
      sprintf(
        red("Existing context information detected, from the following columns in the dataset: ") %+%
        green("'%s'\n\t") %+% red("Metadata is being appended; please review duplicates manually"),
      contexts %>% purrr::map_chr(~.x[["var_in"]]) %>% paste(collapse = "', '"))
    )
  }

  # Populate context metadata with manual selection from user
  if (is.null(user_responses)) {

    var_in <- metadata_user_select_names(
      paste("Indicate all columns that contain additional contextual data for ", dataset_id), v)
    categories <-
      c("treatment_context", "plot_context", "temporal_context",
        "method_context", "entity_context")

    for (i in seq_along(var_in)) {

      ii <- n_existing + i
      category <- metadata_user_select_names(
        paste("What category does context", var_in[i], "fit in?"), categories)
      context_values <- data[[var_in[i]]] %>% unique() %>% na.omit() %>% as.character()

      message(sprintf("\tThe following values exist for this context: %s", context_values %>% paste(collapse = ", ")))

      replace_needed <- readline(prompt = "Are replacement values required? (y/n) ")

      description_needed <- readline(prompt = "Are descriptions required? (y/n) ")

      contexts[[ii]] <-
        list(
          context_property = "unknown",
          category = category,
          var_in = var_in[i],
          values = tibble::tibble(
            find = context_values,
            value = context_values,
            description = "unknown"
          )
        )

      if (tolower(replace_needed) == "y") {
        contexts[[ii]][["values"]][["value"]] <- "unknown"
      } else {
        contexts[[ii]][["values"]][["find"]] <- NULL
      }

      # Don't list the description field if no descriptions are required
      if (tolower(description_needed) == "y") {
        contexts[[ii]][["values"]][["description"]] <- "unknown"
      } else {
        contexts[[ii]][["values"]][["description"]] <- NULL
      }
    # If neither replacement values nor descriptions are required,
    # there is no reason to list the values; these will be automatically read in later
      if (tolower(replace_needed) == "n" && tolower(description_needed) == "n") {
        contexts[[ii]][["values"]] <- NULL
      }
    }

  # Populate context metadata with `user_responses`
  } else {

    var_in <- user_responses$var_in
    categories <- user_responses$categories
    replace_needed <- user_responses$replace_needed

    for (i in seq_along(var_in)) {

      ii <- n_existing + i
      category <- categories[i]
      context_values <- data[[var_in[i]]] %>% unique() %>% na.omit()

      contexts[[ii]] <-
        list(
          context_property = "unknown",
          category = category,
          var_in = var_in[i],
          values = tibble::tibble(
            find = context_values,
            value = context_values,
            description = "unknown"
          )
        )

      if (tolower(replace_needed[i]) == "y") {
        contexts[[ii]][["values"]][["value"]] <- "unknown"
      } else {
        contexts[[ii]][["values"]][["find"]] <- NULL
      }
    }
  }

  metadata$contexts <- contexts
  write_metadata_dataset(metadata, dataset_id)
  return(invisible(metadata))

}


#' Adds citation details to a metadata file for given study
#'
#' @inheritParams metadata_path_dataset_id
#' @param file Name of file where reference is saved
#' @param type Type of reference: `primary`, `secondary` or `original`
#' (or `original_01`, `original_02`, etc., for multiple sources)
#' @param drop Variables in bibtex to ignore
#'
#' @return `metadata.yml` file with citation details added
#' @export
#'
metadata_add_source_bibtex <- function(dataset_id, file,
                                       type = "primary",
                                       drop = c("dateobj", "month")) {

  # Read in file, convert to list, set key
  bib <- RefManageR::ReadBib(file) %>% util_bib_to_list()
  if (type == "primary") {
    bib$key <- dataset_id
  }

  for (v in drop)
    bib[[v]] <- NULL

  if (!is.null(bib$url) && !is.null(bib$doi))
    bib[["url"]] <- NULL

  if (tolower(bib$bibtype) == "article")
    bib[["publisher"]] <- NULL

  # Standardise author names capitalisation
  bib[["author"]] <-
    strsplit(bib[["author"]], split = " ") %>%
    purrr::map(
      ~ifelse(
        # If the word is 'and', make lowercase, otherwise make every word title case
        .x != "and",
        stringr::str_to_title(.x),
        stringr::str_to_lower(.x)
    )) %>% unlist %>% paste(collapse = " ")

  # Standardise key capitalisation
  bib[["key"]] <- stringr::word(bib[["key"]], 1L, sep = "_") %>%
    stringr::str_to_title() %>%
    paste(stringr::word(bib[["key"]], 2, sep = "_"), sep = "_")

  # Somewhat sensible ordering of elements
  order <- c("key", "bibtype", "year", "author", "journal", "title", "volume", "number",
              "pages", "doi", "publisher", "place")
  v <-  c(order, names(bib)) %>% unique()
  v <- v[v %in% names(bib)]

  # Output message if source metadata already exists
  metadata <- read_metadata_dataset(dataset_id)
  if (type == "primary" && any(metadata[["source"]][["primary"]][c("year", "author", "journal", "title", "volume", "doi")] != "unknown")) {
    message(sprintf(red("Primary source metadata for %s ") %+% red("already exists and is being overwritten"), blue(dataset_id)))
  }
  if (type != "primary" && length(metadata[["source"]][[type]]) > 0)
    message(sprintf(red("Source metadata of type ") %+% green("'%s'") %+% red(" for %s") %+% red(" already exists and is being overwritten"), type, blue(bib$key)))

  # Save to metadata
  metadata$source[[type]] <- bib[v]
  write_metadata_dataset(metadata, dataset_id)

}


#' Standardise doi
#'
#' @param doi doi of reference to add
util_standardise_doi <- function(doi) {

  if (stringr::str_starts(doi, "https://doi.org/"))
    return(gsub("https://doi.org/", "", doi, fixed = TRUE))

  if (stringr::str_starts(doi, "http://doi.org/"))
    return(gsub("http://doi.org/", "", doi, fixed = TRUE))

  if (stringr::str_starts(doi, "doi.org/"))
    return(gsub("doi.org/", "", doi, fixed = TRUE))

  return(doi)

}


#' Adds citation details from a doi to a metadata file for a `dataset_id`
#'
#' Uses rcrossref package to access publication details from the crossref
#' database
#'
#' @param bib (Only use for testing purposes) Result of calling `bib rcrossref::cr_cn(doi)`
#' @inheritParams metadata_path_dataset_id
#' @inheritParams util_standardise_doi
#' @param ... Arguments passed from metadata_add_source_bibtex()
#'
#' @return `metadata.yml` file with citation details added
#' @export
#'
metadata_add_source_doi <- function(..., doi, bib = NULL) {

  doi <- util_standardise_doi(doi)

  if (is.null(bib))
    bib <- rcrossref::cr_cn(doi)

  if (is.null(bib)) {
    message(red("DOI not available in Crossref database, please fill record manually"))
    return(invisible())
  }

  file <- tempfile()
  writeLines(bib, file)

  metadata_add_source_bibtex(file = file, ...)

}


#' Add a categorical trait value substitution into a metadata file for a `dataset_id`
#'
#' `metadata_add_substitution` is used to align the categorical trait values used
#' by a contributor to the categorical values supported by the database. These values
#' are defined in the `traits.yml` file.
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param trait_name The database defined name for a particular trait
#' @param find Trait value in the original data.csv file
#' @param replace Trait value supported by database
#'
#' @return `metadata.yml` file with a substitution added
#' @export
metadata_add_substitution <- function(dataset_id, trait_name, find, replace) {

  set_name <- "substitutions"
  metadata <- read_metadata_dataset(dataset_id)
  to_add <- list(trait_name = trait_name, find = find, replace = replace)

  # Add `set_name` category if it doesn't yet exist
  if (all(is.na(metadata[[set_name]]))) {
    metadata[[set_name]] <- list()
  } else {
    # Check if find record already exists for that trait
    data <- austraits::convert_list_to_df2(metadata[[set_name]])
    # Has to be rowwise (both conditions have to be true for a given row)
    if (nrow(data[data$trait_name == trait_name & data$find == find, ]) > 0) {
      message(
        sprintf(
          red("Substitution already exists for ") %+% green("'%s'") %+% red(" from trait ") %+% blue("`%s`") %+%
            red(" -> please review manually in %s"),
          find, trait_name, blue(metadata_path_dataset_id(dataset_id))
        ))
      return(invisible())
    }
  }

  metadata[[set_name]] <- util_append_to_list(metadata[[set_name]], to_add)
  message(
    sprintf(red("Adding substitution in %s") %+% red(" for trait ") %+% blue("`%s`") %+% red(": ") %+%
      green("'%s'") %+% red(" -> ") %+% green("'%s'"),
    blue(dataset_id), trait_name, find, replace)
  )
  write_metadata_dataset(metadata, dataset_id)

}


#' Add a dataframe of trait value substitutions into a metadata file for a dataset_id
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param substitutions Dataframe of trait value substitutions
#'
#'
#' @return `metadata.yml` file with multiple trait value substitutions added
#' @importFrom rlang .data
#' @export
metadata_add_substitutions_list <- function(dataset_id, substitutions) {

  # Read metadata
  metadata <- read_metadata_dataset(dataset_id)

  if (!all(is.na(metadata[["substitutions"]]))) {
    message(red("Existing substitutions have been overwritten"))
  }
  # Read in dataframe of substitutions, split into single-row lists, and add to metadata file
  metadata$substitutions <- substitutions %>% dplyr::group_split(.data$trait_name, .data$find) %>% lapply(as.list)

  # Write metadata
  write_metadata_dataset(metadata, dataset_id)

}


#' Substitutions from a dataframe
#' @description Function that simultaneously adds many trait value replacements, potentially
#' across many `trait_name`'s and `dataset_id`'s, to the respective `metadata.yml` files.
#' This function will be used to quickly re-align/re-assign trait values across all studies.
#'
#' @param dataframe_of_substitutions Dataframe with columns indicating `dataset_id`, `trait_name`,
#' original trait values (`find`), and database aligned trait value (`replace`)
#' @param dataset_id Name of column containing study `dataset_id`(s) in database
#' @param trait_name Name of column containing trait name(s) for which a trait value replacement needs to be made
#' @param find Name of column containing trait values submitted by the contributor for a data observation
#' @param replace Name of column containing database aligned trait values
#'
#' @importFrom rlang .data
#'
#' @return Modified metadata files with trait value replacements
#' @export
#'
#' @examples \dontrun{
#' read_csv("export/dispersal_syndrome_substitutions.csv") %>%
#'   select(-extra) %>%
#'   filter(dataset_id == "Angevin_2011") -> dataframe_of_substitutions
#' metadata_add_substitutions_table(dataframe_of_substitutions, dataset_id, trait_name, find, replace)
#' }
metadata_add_substitutions_table <- function(dataframe_of_substitutions, dataset_id, trait_name, find, replace) {

  # Throw error if the column doesn't exist in the dataframe
  for (col in c(dataset_id, trait_name, find, replace)) {
    if (!col %in% names(dataframe_of_substitutions)) {
      stop(sprintf(green("'%s'") %+% red(" is not a column in the substitutions table"), col))
    }
  }

  set_name <- "substitutions"

  # Detect studies with no substitutions versus existing substitutions
  empty_substitutions <- list()
  existing_substitutions <- list()

  for (dataset in unique(dataframe_of_substitutions[[dataset_id]])) {

    metadata <- read_metadata_dataset(dataset)
    if (all(is.na(metadata[[set_name]]))) {
      empty_substitutions <- append(empty_substitutions, dataset)
    } else {
      existing_substitutions <- append(existing_substitutions, dataset)
    }

  }

  # Split dataframe of substitutions by row
  dataframe_of_substitutions <-
    dataframe_of_substitutions %>%
    dplyr::mutate(rows = dplyr::row_number()) %>%
    dplyr::group_split(.data$rows)

  # Add substitutions to metadata files
  for (i in 1:max(dataframe_of_substitutions)$rows) {

    metadata <- read_metadata_dataset(dataframe_of_substitutions[[i]][[dataset_id]])

    to_add <- list(
      trait_name = dataframe_of_substitutions[[i]][[trait_name]],
      find = dataframe_of_substitutions[[i]][[find]],
      replace = dataframe_of_substitutions[[i]][[replace]]
    )
    # If `substitutions` is empty, make new list
    if (all(is.na(metadata[[set_name]]))) {

      metadata[[set_name]] <- list()

    } else {

      data <- austraits::convert_list_to_df2(metadata[[set_name]])
      # Check whether the same `find` value for a given `trait_name` already exists
      if (nrow(data[data$trait_name == to_add$trait_name & data$find == to_add$find, ]) > 0) {
        message(
          sprintf(
            red("Substitution in %s") %+% red(" for trait ") %+% blue("`%s`") %+% red(": ") %+% green("'%s'") %+%
              red(" already exists, but new substitution has been added\n\tPlease review manually"),
            blue(dataframe_of_substitutions[[i]][[dataset_id]]), to_add$trait_name, to_add$find
          ))
      }

    }

    metadata[[set_name]] <- util_append_to_list(metadata[[set_name]], to_add)
    write_metadata_dataset(metadata, dataframe_of_substitutions[[i]][[dataset_id]])

  }

  if (length(empty_substitutions) > 0) {
    message(
      sprintf(
        red("Substitutions have been added for %s"),
        blue(paste(empty_substitutions %>% unique, collapse = ", ")))
    )
  }

  if (length(existing_substitutions) > 0) {
    message(sprintf(
      red("Substitutions were appended to existing substitutions in %s"),
      blue(paste(existing_substitutions %>% unique, collapse = ", "))
    ))
  }

}


#' Add a taxonomic change into the `metadata.yml` file for a `dataset_id`
#'
#' Add a single taxonomic change into the `metadata.yml` file for a specific study.
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param find Original name used by the contributor
#' @param replace Taxonomic name accepted by APC or APNI
#' @param reason Reason for taxonomic change
#' @param taxonomic_resolution The rank of the most specific taxon name (or scientific name)
#' to which a submitted orignal name resolves
#' @param overwrite Parameter indicating whether preexisting find-replace entries should be overwritten. Defaults to `true`
#'
#' @return `metadata.yml` file with taxonomic change added
#' @export
metadata_add_taxonomic_change <- function(dataset_id, find, replace, reason, taxonomic_resolution, overwrite = TRUE) {

  if (length(replace) > 1) {
    stop(sprintf(red("Cannot replace with two names! (for ") %+% green("'%s' ") %+% red("-> ") %+%
      green("'%s'") %+% red(")\n"), find, replace))
  }
  set_name <- "taxonomic_updates"
  metadata <- read_metadata_dataset(dataset_id)

  to_add <- dplyr::tibble(find = find, replace = replace, reason = reason, taxonomic_resolution = taxonomic_resolution)

  # Add `set_name` category if it doesn't yet exist
  if (all(is.na(metadata[[set_name]]))) {
    data <- to_add
  } else {
    data <- austraits::convert_list_to_df2(metadata[[set_name]])
    # Check if find record already exists for that trait
    if (find %in% data$find) {
      # If overwrite set to false, don't add a new substitution
      if (overwrite == FALSE) {
        message(sprintf(red("Substitution already exists for ") %+% green("'%s'"), find))
        return(invisible())
      # Default is to overwrite existing substitution
      } else {
        message(sprintf(red("Existing substitution will be overwritten for ") %+% green("'%s'"), find))
        data <- data %>%
          dplyr::filter(.data$find != to_add$find) %>%
          dplyr::bind_rows(to_add) %>%
          dplyr::filter(!.data$find == replace) %>%
          dplyr::arrange(.data$find)
      }
    } else {
      data <- dplyr::bind_rows(data, to_add) %>%
        dplyr::filter(!.data$find == replace) %>%
        dplyr::arrange(.data$find)
    }
  }

  metadata[[set_name]] <- data

  message(
    sprintf(red("\tAdding taxonomic change in %s") %+% red(": ") %+% green("'%s'") %+% red(" -> ") %+%
      green("'%s'") %+% red("(%s)"),
    blue(dataset_id), find, replace, reason)
  )

  # Write metadata
  write_metadata_dataset(metadata, dataset_id)

}


#' Add a list of taxonomic updates into a metadata file for a `dataset_id`
#'
#' Add multiple taxonomic changes to the `metadata.yml` file using a dataframe
#' containing the taxonomic changes to be made.
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param taxonomic_updates Dataframe of taxonomic updates
#'
#' @importFrom rlang .data
#' @return `metadata.yml` file with multiple taxonomic updates added
#' @export
metadata_add_taxonomic_changes_list <- function(dataset_id, taxonomic_updates) {

  # Read metadata
  metadata <- read_metadata_dataset(dataset_id)

  if (!all(is.na(metadata[["taxonomic_updates"]]))) {

    existing_updates <- metadata[["taxonomic_updates"]] %>% austraits::convert_list_to_df2()
    already_exist <- c()

    for (i in seq_len(nrow(taxonomic_updates))) {
      # Check if the taxonomic update already exists
      if (taxonomic_updates[i, ]$find %in% existing_updates$find) {
        # Overwrite existing taxonomic update if TRUE
        existing_updates[which(existing_updates$find == taxonomic_updates[i, ]$find), ] <- taxonomic_updates[i, ]
        already_exist <- c(already_exist, taxonomic_updates[i, ]$find)
      } else {
        # Otherwise, bind to end of existing taxonomic updates
        existing_updates <- existing_updates %>% dplyr::bind_rows(taxonomic_updates[i, ])
      }
    }

    if (length(already_exist) > 0) {
      message(
        sprintf(
          green("%s") %+% red(" already exist(s) in `taxonomic_updates` and is being overwritten"),
          paste(already_exist, collapse = ", ")
      ))
    }
    # Write new taxonomic updates to metadata
    metadata$taxonomic_updates <- existing_updates %>% dplyr::arrange(.data$find) %>% dplyr::filter(!.data$find == .data$replace)
  } else {

    # Read in dataframe of taxonomic changes, split into single-row lists, and add to metadata file
    metadata$taxonomic_updates <- taxonomic_updates %>% dplyr::filter(!.data$find == .data$replace)
  }

  # Write metadata
  write_metadata_dataset(metadata, dataset_id)

}


#' Exclude observations in a yaml file for a `dataset_id`
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param variable Variable name
#' @param find Term to find by
#' @param reason Reason for exclusion
#'
#' @return `metadata.yml` file with excluded observations
#' @export
metadata_exclude_observations <- function(dataset_id, variable, find, reason) {

  set_name <- "exclude_observations"
  metadata <- read_metadata_dataset(dataset_id)

  to_add <- list(variable = variable, find = find, reason = reason)

  # Add `set_name` category if it doesn't yet exist
  if (all(is.na(metadata[[set_name]]))) {
    metadata[[set_name]] <- list()
  } else {
    # Check if find record already exists for that trait
    data <- austraits::convert_list_to_df2(metadata[[set_name]])

    if (nrow(data[data$find == find & data$variable == variable, ]) > 0) {
      message(sprintf(red("Exclusion already exists for ") %+% green("'%s'"), find))
      return(invisible())
    }
  }

  metadata[[set_name]] <- util_append_to_list(metadata[[set_name]], to_add)

  message(
    sprintf(blue("%s") %+% red(" - excluding ") %+% blue("`%s`") %+% red(": ") %+% green("'%s'") %+% red(" (%s)"),
    dataset_id, variable, find, reason)
  )
  write_metadata_dataset(metadata, dataset_id)

}


#' Update a taxonomic change into a yaml file for a `dataset_id`
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param find Original taxonomic name
#' @param replace Updated taxonomic name to replace original taxonomic name
#' @param reason Reason for change
#' @param taxonomic_resolution The rank of the most specific taxon name (or scientific name)
#' to which a submitted orignal name resolves
#'
#' @return `metadata.yml` file with added substitution
#' @export
metadata_update_taxonomic_change <- function(dataset_id, find, replace, reason, taxonomic_resolution) {

  set_name <- "taxonomic_updates"
  metadata <- read_metadata_dataset(dataset_id)

  if (!all(is.na(metadata[[set_name]]))) {
    data <- austraits::convert_list_to_df2(metadata[[set_name]])
  }

  # Check if `taxonomic_updates` doesn't exist or if substitution does not exist
  if (all(is.na(metadata[[set_name]])) || !find %in% data$find) {
    message(
      sprintf(red("Substitution for ") %+% green("'%s'") %+% red(" in %s") %+% red(" does not exist"),
      find, blue(dataset_id)))
    return(invisible())
  }

  i <- match(find, data$find)

  metadata[[set_name]][[i]][["replace"]] <- replace
  metadata[[set_name]][[i]][["reason"]] <- reason
  metadata[[set_name]][[i]][["taxonomic_resolution"]] <- taxonomic_resolution
  message(
    sprintf(red("Updating taxonomic change in %s") %+% red(": ") %+% green("'%s'") %+% red(" -> ") %+%
      green("'%s'") %+% red(" (%s)"),
    blue(dataset_id), find, replace, reason)
  )

  write_metadata_dataset(metadata, dataset_id)

}


#' Remove a taxonomic change from a yaml file for a `dataset_id`
#'
#' @param dataset_id Identifier for a particular study in the database
#' @param find Taxonomic name to find
#'
#' @return `metadata.yml` file with a taxonomic change removed
#' @export
metadata_remove_taxonomic_change <- function(dataset_id, find) {

  set_name <- "taxonomic_updates"
  metadata <- read_metadata_dataset(dataset_id)

  # If `taxonomic_updates` section is empty
  if (all(is.na(metadata[[set_name]]))) {
    message(sprintf(red("No taxonomic changes in %s") %+% red(" to remove"), blue(dataset_id)))
    return(invisible())
  } else {
    # Check if taxonomic change does not exist
    data <- austraits::convert_list_to_df2(metadata[[set_name]])
    if (!find %in% data$find) {
      message(
        sprintf(
          red("Taxonomic change in %s") %+% red(" for ") %+% green("'%s'") %+% red(" does not exist"),
          blue(dataset_id), find
        ))
      return(invisible())
    }
  }

  i <- data$find == find
  metadata[[set_name]][which(i)] <- NULL

  # If no more taxonomic changes exist, change `taxonomic_updates` section to NA
  if (length(metadata[[set_name]]) == 0) {
    metadata[[set_name]] <- NA
  }

  message(sprintf(red("Taxonomic change in %s") %+% red(" for ") %+% green("'%s'") %+% red(" removed"), blue(dataset_id), find))
  write_metadata_dataset(metadata, dataset_id)

}


#' Find `dataset_id`'s with a given taxonomic change
#'
#' @param find Name of original species
#' @param replace Name of replacement species, default = NULL
#' @param studies Name of studies to look through, default = NULL
#'
#' @importFrom stringr str_remove_all str_replace_all
#' @export
metadata_find_taxonomic_change <- function(find, replace = NULL, studies = NULL) {

  if (is.null(studies))
    studies <- dirname(dir("data", pattern = "metadata.yml", recursive = TRUE))

  f <- file.path("data", studies, "metadata.yml")

  contents <- lapply(f, function(x) paste0(readLines(x, encoding = "UTF-8"), collapse = "\n"))

  if (!is.null(replace))
    txt <- sprintf("- find: %s\n  replace: %s", find, replace)
  else
    txt <- sprintf("- find: %s\n", find)
  i <- sapply(contents, function(s) any(grepl(txt, s, fixed = TRUE)))

  if (length(studies[i]) > 0) {
    if (!is.null(replace))
      message(
        sprintf(red("The following studies contain ") %+% green("'find: %s'") %+% red(" and ") %+%
          green("'replace: %s'") %+% red(": %s"),
        find, replace, blue(paste(studies[i], collapse = ", ")))
      )
    else
      message(
        sprintf(red("The following studies contain ") %+% green("'find: %s'") %+% red(": %s"),
        find, blue(paste(studies[i], collapse = ", ")))
      )
  } else {
    if (!is.null(replace))
      message(
        sprintf(red("No studies contain ") %+% green("'find: %s'") %+% red(" and ") %+%
          green("'replace: %s'"), find, replace)
      )
    else
      message(
        sprintf(red("No studies contain ") %+% green("'find: %s'"), find)
      )
  }

}


#' Update the `remake.yml` file with new studies
#'
#' `build_setup_pipeline` rewrites the `remake.yml` file to include new
#' studies.
#'
#' @param dataset_ids `dataset_id`'s to include; by default includes all
#' @param method Approach to use in build
#' @param database_name Name of database to be built
#' @param template Template used to build
#' @param workers Number of workers/parallel processes to use when using
#' method = "furrr"
#'
#' @return Updated `remake.yml` file
#' @export
build_setup_pipeline <- function(dataset_ids = dir("data"),
                                 method = "base",
                                 database_name = "database",
                                 template = select_pipeline_template(method),
                                 workers = 1
                                 ) {

  if (!method %in% c("base", "remake", "furrr")) {
    stop(sprintf("Invalid method selected in `build_setup_pipeline`: %s", method))
  }

  path <- "data"

  if (!file.exists(path)) {
    stop("cannot find data directory: ", path)
  }

  # Check directories have both files
  has_both_files <-
    sapply(
      dataset_ids,
      function(id) sprintf("%s/%s/%s", path, id, c("data.csv", "metadata.yml")) %>% file.exists() %>% all()
    )

  dataset_ids <- dataset_ids[has_both_files]

  message(green(sprintf("Setting up build pipeline for %s studies, using `%s` method", length(dataset_ids), method)))

  vals <- list(
    dataset_ids = whisker::iteratelist(dataset_ids, value = "dataset_id"),
    dataset_ids_vector =
      sprintf("c(%s)", sprintf("'%s'", dataset_ids) %>% paste(collapse = ", ")),
    path = path,
    database_name = database_name,
    workers = workers
    )

  # Setup pipeline based on selected method for building
  pipeline <- whisker::whisker.render(template, data = vals)

  if (method == "base") {
    writeLines(pipeline, "build.R")
    message(green("\t-> build compilation using file `build.R`"))
  }

  if (method == "furrr") {
    writeLines(pipeline, "build.R")
    message(green("\t-> build compilation using file `build.R`"))

    if (workers == 1) {
      message(green("\nSpecify number of workers to use with the `workers` argument (default = 1)"))
    }

  }

  if (method == "remake") {
    writeLines(pipeline, "remake.yml")
    message(green("\t-> build compilation using file `remake.yml`"))
  }

  # Check file R/custom_R_code.R exists
  filename <- "R/custom_R_code.R"
  if (!file.exists(filename)) {
    dir.create("R", FALSE)
    writeLines("\n# Put any functions you use in custom R code here\n\n", "R/custom_R_code.R")
  }

  # Check taxon list exists
  filename <- "config/taxon_list.csv"

  if (!file.exists(filename)) {
    dplyr::tibble(
      aligned_name = character(),
      taxonomic_dataset = character(),
      aligned_scientific_name_id = character(),
      aligned_name_taxonomic_status = character(),
      aligned_name_alternative_taxonomic_status = character(),
      taxon_name = character(),
      taxon_id = character(),
      scientific_name_authorship = character(),
      taxon_rank = character(),
      taxonomic_status = character(),
      family = character(),
      taxon_distribution = character(),
      establishment_means = character(),
      scientific_name = character(),
      scientific_name_id = character()
    ) %>% readr::write_csv(filename)
  }
}


select_pipeline_template <- function(method) {

  file <-
  switch(method,
    base = "build_base.whisker",
    remake = "build_remake.whisker",
    furrr = "build_furrr.whisker",
    default = "build_base.whisker"
  )

  readLines(system.file("support", file, package = "traits.build"))
}

#' Find list of unique datasets within compilation containing specified taxa
#'
#' @param taxa A vector which contains species names
#' @param austraits AusTraits compilation
#' @param original_name Logical; if TRUE use column in compilation which contains original
#' species names, default = FALSE
#'
#' @importFrom rlang .data
#' @return List of unique datasets within compilation containing each taxon
#' @export
dataset_find_taxon <- function(taxa, austraits, original_name = FALSE) {

  data <- austraits$traits

  if (!original_name) {
    data <- data %>%
      dplyr::select(dplyr::all_of(c("taxon_name", "dataset_id"))) %>%
      dplyr::rename("name" = "taxon_name") %>%
      dplyr::distinct()
  } else {
    data <- data %>%
      dplyr::select(dplyr::all_of(c("original_name", "dataset_id"))) %>%
      dplyr::rename("name" = "original_name") %>%
      dplyr::distinct()
  }

  f <- function(sp) {
    datasets <-
      dplyr::filter(data, .data$name == sp) %>%
      dplyr::pull(.data$dataset_id) %>%
      unique()
    names(datasets) <- sp
    return(datasets)
  }

  if (length(taxa) == 1) {
    f(taxa)
  } else {
    lapply(taxa, f)
  }
}
