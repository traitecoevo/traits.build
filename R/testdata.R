#' Test whether specified `dataset_id` has the correct setup
#'
#' Run tests to ensure that specified `dataset_id` has the correct setup.
#'
#' @param dataset_ids Vector of `dataset_id` for sources to be tested
#' @param path_config Path to folder containing configuration files
#' @param path_data Path to folder containing data files
#' @param reporter `testthat` reporter to use to summarise output
#'
#' @importFrom rlang .data .env
#' @export

dataset_test <-
  function(dataset_ids,
           path_config = "config",
           path_data = "data",
           reporter = testthat::CompactProgressReporter) {

    requireNamespace("testthat", quietly = TRUE)

    # Clean up when done
    Sys.setenv("TESTTHAT_MAX_FAILS" = Inf)

    testthat::with_reporter(
      reporter,
      dataset_test_worker(
        test_dataset_ids = dataset_ids,
        path_config = path_config,
        path_data = path_data
      ),
      start_end_reporter = TRUE
    )
  }


#' Test whether specified `dataset_id` has the correct setup
#'
#' Run tests to ensure that specified `dataset_id` has the correct setup.
#'
#' @param test_dataset_ids Vector of `dataset_id` for sources to be tested
#' @inheritParams dataset_test
#' @param schema Data schema
#' @param definitions Trait defininitons
#' @importFrom testthat local_edition compare expect test_that context expect_silent
#' @importFrom rlang .data
#' @importFrom stats na.omit
dataset_test_worker <-
  function(test_dataset_ids,
           path_config = "config",
           path_data = "data",
           schema = get_schema(),
           definitions =
             get_schema(file.path(path_config, "traits.yml"), I("traits"))
           ) {

    # We're using 2nd edition of test that, which has "context" field
    # https://cran.r-project.org/web/packages/testthat/vignettes/third-edition.html
    # Is there a reason to be using the 2nd edition, while context has been superseded in later versions?
    local_edition(2)

    expect_is_in <- function(object, expected, ..., info, label, na.rm = TRUE) {

        if (na.rm)
          object <- object[!is.na(object)]
        i <- object %in% expected

        comp <- compare(all(i), TRUE, ...)
        expect(
          comp$equal,
          sprintf(
            "%s - %s should not contain: '%s'",
            info, label,
            paste(object[!i], collapse = "', '")
          ))

        invisible(object)
      }

    expect_contains <- function(object, expected, ..., info) {

      i <- expected %in% object

      comp <- compare(all(i), TRUE, ...)
      expect(
        comp$equal,
        sprintf("%s - does not contain: '%s'", info, paste(expected[!i], collapse = "', '"))
      )

      invisible(object)
    }

    expect_allowed <- function(object, allowed, ..., info, label) {

      i <- object %in% allowed

      comp <- compare(all(i), TRUE, ...)
      expect(
        comp$equal,
        sprintf(
          "%s - %s include(s) invalid terms: '%s'",
          info, label,
          paste(object[!i], collapse = "', '")
        ))

      invisible(object)
    }

    expect_equal <- function(object, expected, info) {
      i <- object == expected
      comp <- compare(all(i), TRUE)
      expect(comp$equal, info)
    }

    expect_true <- function(object, info) {
      i <- object == TRUE
      comp <- compare(all(i), TRUE)
      expect(comp$equal, info)
    }

    expect_false <- function(object, info) {
      i <- object == FALSE
      comp <- compare(all(i), TRUE)
      expect(comp$equal, info)
    }

    expect_named <- function(object, expected_names, info, label) {

      if (missing(expected_names)) {
        expect(
          !identical(names(object), NULL),
          sprintf("%s - %s do not exist", info, label))
      } else {
        expect(
          identical(names(object), expected_names),
          sprintf(
            "%s\tnames of %s (%s) don't match %s",
            info,
            label, paste0("'", names(object), "'", collapse = ", "),
            paste0("'", expected_names, "'", collapse = ", ")
          )
        )
      }
    }

    expect_type <- function(object, type, info, label) {
      stopifnot(is.character(type), length(type) == 1)
      expect(
        identical(typeof(object), type),
        sprintf("%s - %s has type %s, not %s", info, label, typeof(object), type)
      )
    }

    expect_not_NA <- function(object, info, label) {
      i <- !is.na(object)
      comp <- compare(all(i), TRUE)
      expect(comp$equal, sprintf("%s - %s contain(s) NAs", info, label))
      invisible(object)
    }

    expect_length_zero <- function(object, info, label) {
      comp <- compare(length(object), 0)
      expect(comp$equal, sprintf("%s: %s", info, label))
      invisible(object)
    }

    expect_unique <- function(object, info, label) {
      x <- table(unlist(object))
      i <- x == 1
      comp <- compare(all(i), TRUE)
      expect(comp$equal, sprintf("%s - %s not unique: '%s'", info, label, paste(names(x)[!i], collapse = "', '")))
      invisible(object)
    }

    expect_allowed_text <- function(object, is_data = FALSE, info, label) {

      if (length(object) > 0) {

        if (is_data) {
          disallowed <-
            object %>% lapply(check_disallowed_chars, exceptions = c("")) %>% simplify2array()
        } else {
          disallowed <-
            object %>% lapply(check_disallowed_chars) %>% simplify2array()
        }

        check <- disallowed %>% lapply(any) %>% unlist()

        txt <- "\n"
        for (i in which(check)) {
          txt <- sprintf(
            "%s\t- ln %s: %s\n",
            txt, i, colour_characters(object[[i]], which(disallowed[[i]])))
        }

        if (is_data) {
          expect(
            identical(as.vector(all(!check)), TRUE),
            sprintf(
              "%s\tdisallowed characters in data detected: %s\n\tPlease replace using `custom_R_code`",
              info, txt
            )
          )
        } else {
          expect(
            identical(as.vector(all(!check)), TRUE),
            sprintf("%s - disallowed characters in %s detected: \n%s", info, label, txt)
          )
        }

      }

      invisible(object)

    }

    colour_characters <- function(x, i = NULL) {

      chars <- x %>% charToRaw() %>% lapply(rawToChar) %>% unlist()

      # Wrapper around characters to print as colour
      # obtained from crayon::red(x)
      if (!is.null(i))
        chars[i] <- sprintf("\033[31m%s\033[39m", chars[i])

      paste0(chars, collapse = "")
    }

    check_disallowed_chars <- function(x, exceptions = c("ÁÅÀÂÄÆÃĀâíåæäãàáíÇčóöøéèłńl°êÜüùúû±µµ“”‘’-–—≈˜×≥≤")) {

      i <- charToRaw(x)
      # Allow all ascii text
      is_ascii <- i < 0x7F

      # Allow some utf8 characters, those with accents over letters for foreign names
      # List of codes is here: http://www.utf8-chartable.de/
      # Note c3 is needed because this is prefix for allowed UTF8 chars
      # Warning: Portable packages must use only ASCII characters in their R code
      # Sophie - could replace these with unicode like Lizzy did before?
      exceptions <- exceptions

      is_allowed <- i %in% charToRaw(exceptions)
      !(is_ascii | is_allowed)
    }

    # Better than expect_silent as contains `info` and allows for complete failures
    expect_no_error <- function(object, ..., info) {
      error <- tryCatch({
        object
        NULL
      }, error = function(e) {
        e
      })
      expect(
        is.null(error),
        sprintf("%s threw an error:\n\n" %+% red("%s"), info, paste(error, collapse = ",")))
      invisible(object)
    }

    expect_no_warning <- function(object, ..., info) {
      warning <- tryCatch({
        object
        NULL
      }, warning = function(w) {
        w
      })
      expect(is.null(warning), info)
    }

    expect_list_elements_contains_names <- function(object, expected, info) {
      for (i in seq_along(object))
        expect_contains(names(object[[i]]), expected, info = paste(info, i))
      invisible(object)
    }

    expect_list_elements_allowed_names <- function(object, allowed, info, label) {
      for (i in seq_along(object))
        expect_allowed(names(object[[i]]), allowed, info = paste(info, i), label = "field names")
      invisible(object)
    }

    expect_list_elements_exact_names <- function(object, expected, info) {
      for (i in seq_along(object)) {
        expect_contains(names(object[[i]]), expected, info = paste(info, i))
        expect_allowed(names(object[[i]]), expected, info = paste(info, i), label = "field names")
      }
      invisible(object)
    }

    expect_dataframe_valid <- function(data, info, label) {
      expect_not_NA(colnames(data), info, label)
      expect_allowed_text(colnames(data), info = info, label = label)
      expect_unique(colnames(data), info, label)
      expect_true(is.data.frame(data), info = sprintf("%s - is not a dataframe", info))
    }

    # Function is assigned but not used
    expect_dataframe_named <- function(data, expected_colnames, info, label) {
      expect_dataframe_valid(data, info, label)
      expect_named(data, expected_colnames, info = info, label = label)
    }

    expect_dataframe_names_contain <- function(data, expected_colnames, info, label) {
      expect_dataframe_valid(data, info, label)
      expect_contains(names(data), expected_colnames, info = info)
    }

    expect_list <- function(data, info) {
      expect_true("list" %in% class(data), info = sprintf("%s - is not a list", info))
    }

    expect_list_names_valid <- function(data, info, label) {
      expect_list(data, info)
      expect_not_NA(names(data), info = info, label = label)
      expect_unique(names(data), info = info, label = label)
    }

    expect_list_names_exact <- function(data, expected_names, info, label) {
      expect_list_names_valid(data, info, label = label)
      expect_named(data, expected_names, info = info, label = label)
    }

    expect_list_names_allowed <- function(data, allowed_names, info, label) {
      expect_list_names_valid(data, info, label = label)
      expect_named(data, info = info, label = label)
      expect_allowed(names(data), allowed_names, info = info, label = label)
    }

    # Function is assigned but not used
    expect_list_names_contain <- function(data, expected_names, info, label) {
      expect_list_names_valid(data, info, label = label)
      expect_named(data, info = info, label = label)
      expect_contains(names(data), expected_names, info = info)
    }

    test_build_dataset <- function(
      path_metadata, path_data, info, definitions, unit_conversions, schema, resource_metadata, taxon_list) {

      # Test it builds with no errors
      expect_no_error({
        build_config <- dataset_configure(path_metadata, definitions)
      }, info = "`dataset_configure`")

      expect_no_error({
        build_dataset_raw <- dataset_process(path_data, build_config, schema, resource_metadata, unit_conversions)
      }, info = "`dataset_process`")

      expect_no_error({
        build_dataset <- dataset_update_taxonomy(build_dataset_raw, taxon_list)
      }, info = "`dataset_update_taxonomy`")

      build_dataset
    }

    # Now run tests for each dataset

    for (dataset_id in test_dataset_ids) {

      s <- file.path(path_data, dataset_id)

      test_that(dataset_id, {

        context(sprintf("%s", dataset_id))

        ## Files exist
        files <- file.path(s, c("data.csv", "metadata.yml"))
        for (f in files) {
          expect_true(file.exists(f), info = sprintf("%s" %+% "\tfile does not exist", red(f)))
        }

        ## Check for other files
        vals <- c("data.csv", "metadata.yml", "raw")
        expect_is_in(
          dir(s), vals,
          info = paste0(red(file.path(path_data, dataset_id)), "\tdisallowed files"),
          label = "folder"
        )

        ## `data.csv`
        f <- files[1]
        expect_silent(
          data <- read_csv(f, col_types = cols(), guess_max = 1e5, progress = FALSE) # Time columns get reformatted
        )

        ## Check no issues flagged when parsing file
        expect_no_error(
          readr::stop_for_problems(data),
          info = sprintf(red("`read_csv(%s)`"), f)
        )

        expect_dataframe_valid(data, info = paste0(red(f), "\tdata"), label = "column names")

        ## Metadata
        f <- files[2]
        expect_allowed_text(readLines(f, encoding = "UTF-8"), info = paste0(red(f), "\tmetadata"), label = "metadata")
        expect_silent(metadata <- yaml::read_yaml(f))
        expect_list_names_exact(
          metadata, schema$metadata$elements %>% names(),
          info = red(f), label = "metadata sections"
        )

        ## Custom R code
        txt <- metadata[["dataset"]][["custom_R_code"]]
        # Check that `custom_R_code` is immediately followed by `collection_date`
        expect_equal(
          metadata[["dataset"]][which(names(metadata[["dataset"]]) == "custom_R_code") + 1] %>% names(),
          "collection_date",
          info = sprintf("%s\tdataset - the `custom_R_code` field must be followed by `collection_date`", red(f))
        )
        # Apply custom manipulations
        expect_no_error(data <- process_custom_code(txt)(data), info = paste0(red(f), "\t`custom_R_code`"))

        ## Source
        expect_list_names_valid(metadata[["source"]], info = sprintf("%s\tsource", red(f)), label = "field names")

        v <- names(metadata[["source"]])
        i <- grepl("primary", v) | grepl("secondary", v) | grepl("original", v)

        expect_contains(v, "primary", info = paste0(red(f), "\tsource"))

        expect_true(
          sum(grepl("primary", v)) <= 1,
          info = paste0(red(f), "\tsources can have max 1 type labelled 'primary': ", paste(v, collapse = ", "))
        )

        expect_true(
          all(i),
          info = paste0(red(f), "\tsources must be primary, secondary or original: ", paste(v[!i], collapse = ", "))
        )

        vals <- c("key", "bibtype", "author", "title", "year")

        for (bib in names(metadata[["source"]])) {
          expect_contains(
            names(metadata[["source"]][[bib]]), vals,
            info = sprintf("%s\tsource '%s'", red(f), bib)
          )
        }

        keys <- unlist(lapply(metadata[["source"]], "[[", "key"))

        expect_unique(
          keys,
          info = paste0(red(f), "\tsources"),
          label = "keys"
        )

        ## People
        expect_list(metadata[["contributors"]], info = paste0(red(f), "\tcontributors"))

        expect_list_names_allowed(
          metadata[["contributors"]],
          schema$metadata$elements$contributors$elements %>% names(),
          info = paste0(red(f), "\tcontributors"), label = "contributor type fields"
        )

        ## Data collectors
        if (!is.na(metadata[["contributors"]][["data_collectors"]][1])) {

          expect_list(metadata[["contributors"]][["data_collectors"]], info = paste0(red(f), "\tdata_collectors"))
          vars <- schema$metadata$elements$contributors$elements$data_collectors$elements %>% names()

          for (i in seq_along(metadata[["contributors"]][["data_collectors"]])) {
            expect_list_names_allowed(
              metadata[["contributors"]][["data_collectors"]][[i]],
              vars,
              info = paste0(red(f), "\tdata_collector ", i), label = "`data_collector` field names"
            )

            expect_contains(
              metadata[["contributors"]][["data_collectors"]][[i]] %>% names(), vars[1:4],
              info = sprintf("%s\tdata_collector %s", red(f), i)
            )
          }
        }

        ## Dataset curators
        expect_true(
          !is.null(metadata[["contributors"]][["dataset_curators"]]),
          info = sprintf("%s\tcontributors - `dataset_curators` is missing", red(f))
        )
        expect_type(
          metadata[["contributors"]][["dataset_curators"]], "character",
          info = paste0(red(f), "\tcontributors"), label = "`dataset_curators`"
        )

        ## Assistants
        if (!is.null(metadata[["contributors"]][["assistants"]][1]))
          expect_type(
            metadata[["contributors"]][["assistants"]], "character",
            info = paste0(red(f), "\tcontributors"), label = "`assistants`"
          )

        ## Dataset
        expect_list_names_allowed(
          metadata[["dataset"]],
          schema$metadata$elements$dataset$values %>% names(),
          info = paste0(red(f), "\tdataset"), label = "`dataset` field names"
        )

        expect_type(
          metadata[["dataset"]][["data_is_long_format"]], "logical",
          info = paste0(red(f), "\tdataset"), label = "`data_is_long_format`"
        )

        expect_type(
          metadata[["dataset"]], "list",
          info = paste0(red(f), "\tdataset"), label = "metadata"
        )

        ## Locations

        expect_silent(
          locations <-
            metadata$locations %>%
            process_format_locations(dataset_id, schema) %>%
            process_add_all_columns(names(schema[["austraits"]][["elements"]][["locations"]][["elements"]]))
          )

        if (length(unlist(metadata[["locations"]])) > 1) {

          expect_list(metadata[["locations"]], info = paste0(red(f), "\tlocations"))

          expect_dataframe_names_contain(
            locations,
            c("dataset_id", "location_name", "location_property", "value"),
            info = paste0(red(f), "\tlocations"), label = "field names"
          )

          for (v in names(metadata$locations)) {

            expect_list(metadata[["locations"]][[v]], info = paste0(red(f), "\tlocation ", v))

            # If fields do not contain both 'latitude range (deg)' and 'longitude range (deg)'
            if (!(all(c("latitude range (deg)", "longitude range (deg)") %in% names(metadata[["locations"]][[v]])))) {

              # Check that it contains 'latitude (deg)' and 'longitude (deg)'
              expect_contains(
                names(metadata[["locations"]][[v]]),
                c("latitude (deg)", "longitude (deg)"),
                info = paste0(red(f), "\tlocation '", v, "'")
              )
            }
          }

          # Check `location_name`'s from metadata are in dataset and vice versa
          expect_true(
            !is.null(metadata[["dataset"]][["location_name"]]),
            info = paste0(red(files[2]), "\tdataset - `location_name` is missing")
          )

          expect_contains(
            names(data),
            metadata[["dataset"]][["location_name"]],
            info = paste0(
              red(files[2]),
              "\tdataset - `location_name` column not found in data")
          )

          v <- data[[metadata[["dataset"]][["location_name"]]]] %>% unique %>% na.omit
          i <- v %in% names(metadata$locations)
          expect_true(
            all(i),
            info = paste0(
              red(f),
              "\tlocations - location names from data file not present in metadata: ",
              v[!i])
          )

          i <- names(metadata$locations) %in% v
          expect_true(
            all(i),
            info = paste0(
              red(f),
              "\tlocations - location names from metadata not present in data file: ",
              names(metadata$locations)[!i])
          )
        }

        ## Contexts
        expect_silent(
          contexts <-
            metadata$contexts %>%
            process_format_contexts(dataset_id, data)
        )

        # Check context details load
        if (nrow(contexts) > 0) {

          expect_dataframe_names_contain(
            contexts,
            c("context_property", "category", "var_in"),
            info = paste0(red(f), "\tcontexts"), label = "field names"
          )

          # Check that unique context `value`'s only have one unique description
          expect_equal(
            contexts %>% dplyr::group_by(.data$context_property, .data$value) %>% dplyr::summarise(n = dplyr::n_distinct(.data$description)) %>%
              filter(.data$n > 1) %>% nrow(),
            0, info = sprintf(
              "%s\tcontexts - `value`'s should only have one unique description each: '%s'",
              red(f),
              paste(
                contexts %>% dplyr::group_by(.data$context_property, .data$value) %>% dplyr::summarise(n = dplyr::n_distinct(.data$description)) %>%
                  filter(.data$n > 1) %>% dplyr::pull(.data$value) %>% unique(),
                collapse = "', '")
            )
          )

          # Check that there are no duplicate `find` fields
          expect_equal(
            contexts %>% dplyr::group_by(.data$context_property, .data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>%
              nrow(),
            0, info = sprintf(
              "%s\tcontexts - duplicate `find` values detected: '%s'",
              red(f),
              paste(
                contexts %>% dplyr::group_by(.data$context_property, .data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>%
                  dplyr::pull(.data$find) %>% unique(),
                collapse = "', '")
            )
          )

          for (i in seq_along(metadata$contexts)) {

            vals <- metadata$contexts[[i]][["values"]]

            if (!is.null(vals)) {

              for (s in seq_along(vals)) {

                # Check that no `find` values are NA
                if (!is.null(vals[[s]][["find"]])) {
                  expect_false(
                    is.na(vals[[s]][["find"]]),
                    info = paste0(red(f),
                      sprintf(
                        "\tcontexts - `find` value in the `context_property` '%s' should not be NA\n\tPlease replace NAs with desired value using `custom_R_code`",
                        metadata$contexts[[i]][["context_property"]]))
                  )
                }

                # Check that there are no `find` fields without accompanying `value` fields
                expect_false(
                  !is.null(vals[[s]][["find"]]) && is.null(vals[[s]][["value"]]),
                  info = paste0(red(f),
                    sprintf(
                      "\tcontexts - `find: %s` in the `context_property` '%s' is not accompanied by a `value` field",
                      vals[[s]][["find"]], metadata$contexts[[i]][["context_property"]]))
                )

                # Check that there are no `description` fields with NA `value` fields
                if (!is.null(vals[[s]][["value"]]) && !is.null(vals[[s]][["description"]])) {
                  expect_false(
                    !is.na(vals[[s]][["description"]]) && is.na(vals[[s]][["value"]]),
                    info = paste0(red(f),
                      sprintf(
                        "\tcontexts - `description: %s` in the `context_property` '%s' should not be accompanied by an NA `value` field\n\tPlease use `custom_R_code` to assign a proper value to NA fields",
                        vals[[s]][["description"]], metadata$contexts[[i]][["context_property"]]))
                  )
                }

                # Check that there are no `description` fields without accompanying `find` and `value` fields
                expect_false(
                  !is.null(vals[[s]][["description"]]) && is.null(vals[[s]][["find"]]) && is.null(vals[[s]][["value"]]),
                  info = paste0(red(f),
                    sprintf(
                      "\tcontexts - `description: %s` in the `context_property` '%s' is not accompanied by `find` and `value` fields",
                      vals[[s]][["description"]], metadata$contexts[[i]][["context_property"]]
                    )
                  )
                )
              }
            }
          }
        }

        ## Traits
        expect_list_elements_contains_names(
          metadata[["traits"]],
          c("var_in", "unit_in", "trait_name", "value_type", "basis_of_value"),
          info = paste0(red(f), "\ttrait")
        )

        expect_list_elements_allowed_names(
          metadata[["traits"]],
          c(schema$metadata$elements$traits$elements %>% names(), unique(contexts$var_in)),
          info = paste0(red(f), "\ttrait")
        )

        expect_silent(traits <- traits.build::util_list_to_df2(metadata[["traits"]]))

        expect_true(
          is.data.frame(traits),
          info = paste0(red(f), "\ttraits - metadata cannot be converted to a dataframe")
        )

        expect_is_in(
          traits$trait_name, definitions$elements %>% names(),
          info = paste0(red(f), "\ttraits"),
          label = "`trait_name`'s"
        )

        # Check no duplicate `var_in`'s

        # Now that traits loaded, check details of contexts match
        if (nrow(contexts > 0)) {

          # Check they are in context dataset
          expect_contains(
            c(names(data), names(traits)),
            unique(contexts$var_in),
            info = paste0(red(files[1]), red(", "), red(files[2]), "\tdata and/or traits metadata")
          )

          unique2 <- function(x) unique(x[!is.na(x)])

          for (j in unique(contexts[["var_in"]])) {

            contextsub <- contexts %>% filter(var_in == j)

            # Context values align either with a column of data or a column of traits table
            if (is.null(data[[j]])) {
              v <- traits[[j]] %>% unique2()
            } else {
              v <- data[[j]] %>% unique2()
            }

            # `find` will always be non-NA unless both `find` and `value` fields are missing
            # since `process_format_contexts` replaces NA `find` with `value`
            # Look for context values in `find` column
            i <- v %in% contextsub[["find"]]

            expect_true(
              all(i),
              info = ifelse(
                "hms" %in% class(v),
                sprintf(
                  "%s\tcontexts - context values of '%s' from data file not present in metadata contexts: '%s'\n\n'%s' has been detected as a time data type and reformatted\n\t-> Please make sure context metadata matches reformatting",
                  red(f), j, paste(as.character(v)[!i], collapse = "', '"), j),
                sprintf(
                  "%s\tcontexts - context values of '%s' from data file not present in metadata: '%s'",
                  red(f), j, paste(v[!i], collapse = "', '")))
            )

            i <- contextsub[["find"]] %in% v

            expect_true(
              all(i),
              info = ifelse(
                !is.null(data[[j]]),
                sprintf(
                  "%s\tcontexts - values of '%s' in metadata not detected in context values from data file: '%s'",
                  red(f), j, paste(contextsub[["find"]][!i], collapse = "', '")),
                sprintf(
                  "%s\tcontexts - values of '%s' in metadata not detected in context values from traits metadata: '%s'",
                  red(f), j, paste(contextsub[["find"]][!i], collapse = "', '"))
              )
            )
          }
        }

        ## Check metadata values are allowed for fields specified in traits metadata
        metadata_fields <- c("entity_type", "value_type", "basis_of_value", "basis_of_record")

        for (field in metadata_fields) {

          if (field %in% names(traits)) {

            not_allowed <- schema[[field]][["values"]] %>% names()
            i <- traits[[field]] %in% names(data) & !(traits[[field]] %in% not_allowed)
            fixed <- traits[[field]][!i] %>% unique()
            cols <- traits[[field]][i] %>% unique()

            # Check fixed values in metadata are allowed
            expect_is_in(
              fixed, c("unknown", schema[[field]][["values"]] %>% names),
              info = paste0(red(f), "\ttraits"), label = sprintf("`%s`", field)
            )

            # Check column values are allowed
            if (length(cols) > 0) {
              for (c in cols) {
                expect_is_in(
                  stringr::str_split(data[[c]], " ") %>% unlist() %>% unique(), c("unknown", schema[[field]][["values"]] %>% names),
                  info = sprintf("%s\t'%s'", red(files[1]), c),
                  label = sprintf("`%s` column", field)
                )
              }
            }
          }

          ## Check metadata values are allowed for fields specified in dataset metadata
          if (field %in% names(metadata[["dataset"]])) {

            not_allowed <- schema[[field]][["values"]] %>% names()

            # If the metadata field is a column in the data (and not an accepted value of the field)
            if (metadata[["dataset"]][[field]] %in% names(data) & !(metadata[["dataset"]][[field]] %in% not_allowed)) {
             
              expect_is_in(
                stringr::str_split(data[[metadata[["dataset"]][[field]]]], " ") %>% unlist() %>% unique(), c("unknown", schema[[field]][["values"]] %>% names),
                info = sprintf("%s\t'%s'", red(files[1]), metadata[["dataset"]][[field]]),
                label = sprintf("`%s` column", field)
              )

            # Otherwise check fixed value
            } else {

              fields_by_word <- stringr::str_split(metadata[["dataset"]][[field]], " ") %>% unlist()
              expect_is_in(
                fields_by_word, c("unknown", schema[[field]][["values"]] %>% names),
                info = paste0(red(f), "\tdataset"), label = sprintf("`%s`", field)
              )

            }
          }
        }

        ## Substitutions

        if (!is.na(metadata[["substitutions"]][1])) {

          expect_list_elements_exact_names(
            metadata[["substitutions"]],
            schema$metadata$elements$substitutions$values %>% names(),
            info = paste0(red(f), "\tsubstitution")
          )
          trait_names <- sapply(metadata[["substitutions"]], "[[", "trait_name")
          expect_is_in(
            unique(trait_names), definitions$elements %>% names(),
            info = paste0(red(f), "\tsubstitutions"), label = "`trait_name`'s"
          )
          expect_is_in(
            unique(trait_names), unique(traits$trait_name),
            info = paste0(red(f), "\tsubstitutions"), label = "`trait_name`'s"
          )

          expect_no_error(
            x <- metadata[["substitutions"]] %>% util_list_to_df2() %>% split(.$trait_name),
            info = paste0(red(f), "\tconverting substitutions to a dataframe and splitting by `trait_name`")
          )

          # Check for allowable values of categorical variables
          for (trait in names(x)) {

            # First check no duplicate combinations of `find`
            expect_equal(
              x[[trait]] %>% dplyr::group_by(.data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>% nrow(),
              0, info = sprintf(
                "%s\tsubstitutions - duplicate `find` values detected: '%s'",
                red(f),
                paste(
                  x[[trait]] %>% dplyr::group_by(.data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>%
                    dplyr::pull(.data$find) %>% unique(),
                  collapse = "', '")
              )
            )

            # If trait is categorical
            if (!is.null(definitions$elements[[trait]]) && definitions$elements[[trait]]$type == "categorical") {

              # Check replacement values
              to_check <- x[[trait]]$replace %>% unique()
              # Filter out `flowering_time` values from to-check list
              to_check <- to_check[!(grepl("^[YyNn]+$", to_check) & stringr::str_length(to_check) == 12)]
              allowable <- c(definitions$elements[[trait]]$allowed_values_levels %>% names(), NA)
              failing <- to_check[!(
                is.na(to_check) |
                  to_check %in% allowable |
                  to_check %>% sapply(util_check_all_values_in, allowable)
              )]
              expect_length_zero(
                failing,
                info = sprintf(
                  "%s\tsubstitutions - `%s` has invalid replacement values",
                  red(f), trait),
                label = failing %>% paste(collapse = "', '")
              )
            }
          }
        }

        ## Taxonomic updates

        if (!is.na(metadata[["taxonomic_updates"]][1])) {

          expect_no_error(
            x <- metadata[["taxonomic_updates"]] %>% util_list_to_df2(),
            info = paste0(red(f), "\tconverting `taxonomic_updates` to a dataframe")
          )

          # # Check no duplicate `find` values
          # expect_equal(
          #   x %>% dplyr::group_by(.data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>% nrow(),
          #   0, info = sprintf(
          #     "%s\ttaxonomic_updates - duplicate `find` values detected: '%s'",
          #     red(f),
          #     paste(
          #       x %>% dplyr::group_by(.data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>%
          #         dplyr::pull(.data$find) %>% unique(),
          #       collapse = "', '")
          #   )
          # )

           expect_list_elements_exact_names(
             metadata[["taxonomic_updates"]],
             schema$metadata$elements$taxonomic_updates$values %>% names(),
             info = paste0(red(f), "\ttaxonomic_update")
           )

          # This test is commented out because the names in the metadata file already have some standardisations applied (i.e. changing the case of first word)
          # by the time the taxonomic updates are read in and therefore they aren't matching those in the data.csv file.

           # taxon_names <- sapply(metadata[["taxonomic_updates"]], "[[", "find")
           # expect_is_in(
           #   unique(taxon_names) %>% process_standardise_names(), data[[metadata[["dataset"]][["taxon_name"]]]] %>% unique() %>% process_standardise_names(),
           #   info = paste0(red(f), "\ttaxonomic_updates"), label = "`taxon_name`'s"
           # )

        }

        ## Check that special characters do not make it into the data
        expect_no_error(
          parsed_data <- data %>%
            process_parse_data(dataset_id, metadata, contexts, schema),
          info = sprintf("%s\t`process_parse_data`", red(dataset_id)))

        expect_allowed_text(
          parsed_data$traits$value, is_data = TRUE,
          info = sprintf("%s", red(files[1]))
        )

        ## Process data so that you can check excluded observations
        parsed_data <-
          parsed_data$traits %>%
          process_add_all_columns(
            c(names(schema[["austraits"]][["elements"]][["traits"]][["elements"]]),
              "parsing_id", "location_name", "taxonomic_resolution", "methods", "unit_in")
          )

        # Replace original `location_id` with a new `location_id`
        if (!is.null(names(metadata$locations))) {
          parsed_data <-
            parsed_data %>%
            dplyr::select(-dplyr::all_of(c("location_id"))) %>%
            dplyr::left_join(
              by = c("location_name"),
              locations %>% dplyr::select(dplyr::all_of(c("location_name", "location_id"))) %>% dplyr::distinct()
            )
          parsed_data <-
            parsed_data %>%
            mutate(
              location_id = ifelse(.data$entity_type == "species", NA_character_, .data$location_id)
            )
        }

        ## Where missing, fill variables in traits table with values from locations
        # Trait metadata should probably have precedence -- right now trait metadata
        # is being read in during `process_parse_data` and getting overwritten here #TODO
        # If process.R changes, this needs to be updated
        if (!is.null(names(metadata$locations))) {
          vars <- c("basis_of_record", "life_stage", "collection_date",
                    "measurement_remarks", "entity_type")

          for (v in vars) {
            # Merge into traits from location level
            if (v %in% unique(locations$location_property)) {
              traits_tmp <- parsed_data %>%
                dplyr::left_join(
                  by = "location_id",
                  locations %>%
                    tidyr::pivot_wider(names_from = "location_property", values_from = "value") %>%
                    mutate(col_tmp = .data[[v]]) %>%
                    dplyr::select(dplyr::any_of(c("location_id", "col_tmp"))) %>%
                    stats::na.omit()
                )
              # Use location level value if present
              parsed_data[[v]] <- ifelse(!is.na(traits_tmp[["col_tmp"]]), traits_tmp[["col_tmp"]], parsed_data[[v]])
            }
          }
        }

        ## Excluded observations

        if (!is.na(metadata[["exclude_observations"]][1])) {

          expect_no_error(
            x <- metadata[["exclude_observations"]] %>% util_list_to_df2(),
            info = paste0(red(f), "\tconverting `exclude_observations` to a dataframe")
          )

          # Check no duplicate `find` values
          expect_equal(
            x %>% dplyr::group_by(.data$variable, .data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>% nrow(),
            0, info = sprintf(
              "%s\texclude_observations - duplicate `find` values detected: '%s'",
              red(f),
              paste(
                x %>% dplyr::group_by(.data$variable, .data$find) %>% dplyr::summarise(n = dplyr::n()) %>% filter(.data$n > 1) %>%
                  dplyr::pull(.data$find) %>% unique(),
                collapse = "', '")
            )
          )

          expect_list_elements_exact_names(
            metadata[["exclude_observations"]],
            schema$metadata$elements$exclude_observations$values %>% names(),
            info = paste0(red(f), "\texclude_observations")
          )

          # Check for allowable values of categorical variables
          expect_no_error(
            x <- metadata[["exclude_observations"]] %>% util_list_to_df2() %>% split(.$variable),
            info = paste0(red(f), "\tconverting `exclude_observations` to a dataframe and splitting by `variable`")
          )

          for (variable in names(x)) {

            find_values <- x[[variable]][["find"]] %>% unique()

            # If the variable to be excluded is a trait:
            if (variable %in% traits$trait_name) {
              expect_is_in(
                find_values,
                # Extract values from the data for that variable
                parsed_data %>% filter(.data$trait_name == variable) %>% dplyr::pull(.data$value) %>% unique(),
                info = paste0(red(f), "\texclude_observations"), label = sprintf("variable '%s'", variable)
              )
            }
            # If the variable to be excluded is `taxon_name`, `location_name` or other metadata fields
          # This test is commented out because two fixes are requiried.
          # First, the names in the metadata file already have had some standardisations applied (i.e. changing the case of first word)
          # by the time the taxonomic updates are read in and therefore they aren't matching those in the data.csv file.
          # Second, the match is not to `taxon_name` but to the column name in data.csv that maps to `taxon_name`

          #  else {
          #     expect_is_in(
          #       find_values, parsed_data %>% dplyr::pull(variable) %>% unique(),
          #       info = paste0(red(f), "\texclude_observations"), label = sprintf("variable '%s'", variable)
          #     )
          #   }
          }
        }

        ## Check config files contain all relevant columns
        if (metadata[["dataset"]][["data_is_long_format"]]) {

          # For vertical datasets, expect all values of "trait column" found in traits
          var_out <- names(metadata[["dataset"]])
          var_in <- unlist(metadata[["dataset"]])
          i <- match("trait_name", var_out)
          values <- unique(data[[var_in[i]]])
          expect_contains(
            traits[["var_in"]], values,
            info = paste0(red(files[2]), "\ttraits")
          )

        } else {

          # For wide datasets, expect variables in traits are headers in the data
          values <- names(data)
          expect_is_in(
            traits[["var_in"]], values,
            info = paste0(red(files[2]), "\ttraits"), label = "`var_in`"
          )

        }


        ## TODO

        ## Make sure specified columns exist


        ## TODO

        ## For numeric trait data, check it looks reasonable & converts properly



        ## Check traits are not only NAs
        expect_false(
          nrow(metadata[["traits"]] %>% util_list_to_df2() %>% dplyr::filter(!is.na(.data$trait_name))) == 0,
          info = paste0(red(f), "\ttraits - only contain NA `trait_name`'s"))

        if (nrow(metadata[["traits"]] %>% util_list_to_df2() %>% dplyr::filter(!is.na(.data$trait_name))) > 0) {

          # Test build dataset
          expect_no_error(
            dataset <- test_build_dataset(
              file.path(path_data, dataset_id, "metadata.yml"),
              file.path(path_data, dataset_id, "data.csv"),
              dataset_id,
              get_schema("config/traits.yml", "traits"),
              get_unit_conversions("config/unit_conversions.csv"),
              get_schema(),
              get_schema("config/metadata.yml", "metadata"),
              read_csv_char("config/taxon_list.csv")
            ),
            info = sprintf("%s\tbuilding dataset", red(dataset_id)))

          ## Check that traits table is not empty
          expect_false(nrow(dataset$traits) == 0, info = sprintf("%s\t`traits` table is empty", red(dataset_id)))

          ## Check that dataset can pivot wider
          if (nrow(dataset$traits) > 0) {
            expect_true(
              dataset %>% check_pivot_wider(),
              info = sprintf("%s\tduplicate rows detected; `traits` table cannot pivot wider", red(dataset_id))
            )
          }

          # Test `austraits` functions
          # Testing per study, not on all studies combined (is this ideal?)
          # I'm not testing whether the functions work as intended, just that they throw no error

          expect_no_warning(
            dataset_wider <- db_traits_pivot_wider(dataset$traits),
            info = paste0(red(dataset_id), "\t`db_traits_pivot_wider` threw a warning; duplicate rows detected")
          )

          if (exists("dataset_wider")) {
            expect_no_warning(
              expect_no_error(
                dataset_longer <- db_traits_pivot_longer(dataset_wider),
                info = paste0(red(dataset_id), "\t`db_traits_pivot_longer`")),
              info = paste0(red(dataset_id), "\t`db_traits_pivot_longer` threw a warning")
            )
          }
        }
      })
    }

    # Keep this
    context("end")
  }
