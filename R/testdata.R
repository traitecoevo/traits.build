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
           reporter = testthat::default_reporter()) {

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
#' @importFrom testthat local_edition compare expect expect_true expect_false expect_named test_that context expect_silent expect_type
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
    local_edition(2)

    ## New expect_that helper functions; test that a number is in a range,
    ## or that a range contains a number.

    expect_isin <-
      function(object,
               expected,
               ...,
               info = NULL,
               label = NULL,
               expected.label = NULL,
               na.rm = TRUE) {
        if (na.rm)
          object <- object[!is.na(object)]
        i <- object %in% expected

        comp <- compare(all(i), TRUE, ...)
        expect(comp$equal,
               sprintf(
                 "%s - should not contain: %s",
                 info,
                 paste(object[!i], collapse = ", ")
               ))

        invisible(object)
      }

    # Expectation: one set contains the other
    expect_contains <- function(object, expected, ..., info = NULL) {
      i <- expected %in% object

      comp <- compare(all(i), TRUE, ...)
      expect(comp$equal,
             sprintf(
               "%s - does not contain: %s",
               info,
               paste(expected[!i], collapse = ", ")
             ))

      invisible(object)
    }

    # Expectation: one set contains the other
    expect_allowed <- function(object, allowed, ..., info = NULL) {
      i <- object %in% allowed

      comp <- compare(all(i), TRUE, ...)
      expect(comp$equal,
             sprintf(
               "%s - includes invalid terms: %s",
               info,
               paste(object[!i], collapse = ", ")
             ))

      invisible(object)
    }

    expect_not_NA <- function(object, info = NULL, label = NULL) {
      i <- !is.na(object)
      comp <- compare(all(i), TRUE)
      expect(comp$equal,
             sprintf("%s - contains NAs: %s", info, label))
      invisible(object)
    }

    expect_length_zero <- function(object, info = NULL, label = NULL) {
      comp <- compare(length(object), 0)
      expect(comp$equal,
             sprintf("%s: %s", info, label))
      invisible(object)
    }

    expect_unique <- function(object, info = NULL, label = NULL) {
      x <- table(unlist(object))
      i <- x == 1
      comp <- compare(all(i), TRUE)
      expect(comp$equal,
             sprintf("%s - not unique: %s", info, paste(names(x)[!i], collapse = ", ")))
      invisible(object)
    }

    expect_allowed_text <- function(object, is_data = FALSE,
                                    info = NULL,
                                    label = NULL) {

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
          txt <- sprintf("%s\t- ln %s: %s\n",
                         txt,
                         i,
                         colour_characters(object[[i]], which(disallowed[[i]])))
        }

        if (is_data) {
          expect(
            identical(as.vector(all(!check)), TRUE),
            sprintf(
              "%s -- disallowed characters in data detected: %s\n\tPlease replace using `custom_R_code`",
              info, txt
            )
          )
        } else {
          expect(
            identical(as.vector(all(!check)), TRUE),
            sprintf("%s -- disallowed characters detected: %s", info, txt)
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

    check_disallowed_chars <- function(x, exceptions = c("ÁÅÀÂÄÆÃĀâíåæäãàáíÇčóöøéèłńl°êÜüùúû±µµ“”‘’-–—≈˜×")) {

      i <- charToRaw(x)
      # Allow all ascii text
      is_ascii <- i < 0x7F

      # Allow some utf8 characters, those with accents over letters for foreign names
      # List of codes is here: http://www.utf8-chartable.de/
      # Note c3 is needed because this is prefix for allowed UTF8 chars
      is_allowed <- i %in% charToRaw(exceptions)
      !(is_ascii | is_allowed)

    }

    # Better than expect_silent as contains `info` and allows for complete failures
    expect_no_error <-
      function(object, regexp = NULL, ..., info = NULL, label = NULL) {
        error <- tryCatch({
          object
          NULL
        }, error = function(e) {
          e
        })
        expect(is.null(error),
               sprintf(
                 "%s threw an error: %s",
                 label,
                 paste(error$message, collapse = ",")
               ),
               info = info)
        invisible(NULL)
      }

    expect_list_elements_contains_names <- function(object, expected, info) {
      for (i in seq_along(object))
        expect_contains(names(object[[i]]), expected, info = paste(info, i))

      invisible(NULL)
    }

    expect_list_elements_allowed_names <- function(object, allowed, info) {
      for (i in seq_along(object))
        expect_allowed(names(object[[i]]), allowed,  info = paste(info, i))

      invisible(NULL)
    }

    test_dataframe_valid <- function(data, info) {
      expect_not_NA(colnames(data), info = info)
      expect_allowed_text(colnames(data), info = info)
      expect_unique(colnames(data), info = info)
      expect_true(is.data.frame(data), info = info)
    }

    # Function is assigned but not used
    test_dataframe_named <- function(data, expected_colnames, info) {
      test_dataframe_valid(data, info)
      expect_named(data, expected_colnames, info = info)
    }

    test_dataframe_names_contain <-
      function(data, expected_colnames, info) {
        test_dataframe_valid(data, info)
        expect_contains(names(data), expected_colnames, info = info)
      }

    test_list <- function(data, info) {
      expect_true(class(data) == "list", info = info)
    }

    test_list_names_valid <- function(data, info) {
      test_list(data, info)
      expect_not_NA(names(data), info = info)
      expect_allowed_text(names(data), info = info)
      expect_unique(names(data), info = info)
    }

    test_list_named_exact <- function(data, expected_names, info) {
      test_list_names_valid(data, info)
      expect_named(data, expected_names, info = info)
    }

    test_list_named_allowed <- function(data, allowed_names, info) {
      test_list_names_valid(data, info)
      expect_named(data)
      expect_allowed(names(data), allowed_names, info = info)
    }

    # Function is assigned but not used
    test_list_named_contains <- function(data, expected_names, info) {
      test_list_names_valid(data, info)
      expect_named(data)
      expect_contains(names(data), expected_names, info = info)
    }

    # Now run tests for each dataset

    for (dataset_id in test_dataset_ids) {

      s <- file.path(path_data, dataset_id)

      test_that(dataset_id, {

        context(sprintf("%s", dataset_id))

        # Exists
        files <- file.path(s, c("data.csv", "metadata.yml"))
        for (f in files) {
          expect_true(file.exists(f), info = f)
        }

        # Check for other files
        vals <- c("data.csv", "metadata.yml", "raw")
        expect_isin(dir(s), vals, info = paste(f, " disallowed files"))


        # `data.csv`
        f <- files[1]
        # Time columns get reformatted
        expect_silent(data <-
                        read_csv(
                          f,
                          col_types = cols(),
                          guess_max = 1e5,
                          progress = FALSE
                        ))
        # Check no issues flagged when parsing file
        expect_no_error(
          readr::stop_for_problems(data),
          info = sprintf(
            "Problems present when reading data, run `read_csv(%s)` to investigate",
            f
          )
        )

        test_dataframe_valid(data, info = f)

        # Metadata
        f <- files[2]
        expect_allowed_text(readLines(f, encoding = "UTF-8"), info = f)
        expect_silent(metadata <- yaml::read_yaml(f))
        test_list_named_exact(metadata,
                              schema$metadata$elements %>% names(),
                              info = f)

        # Custom R code
        txt <- metadata[["dataset"]][["custom_R_code"]]
        #expect_false(grepl("#", txt), label=paste0(files[3], "-custom_R_code cannot contain comments, except on last line"))
        expect_no_error(process_custom_code(txt)(data),
                        label = paste0(files[3], "-custom_R_code"))

        # Apply custom manipulations
        data <- process_custom_code(txt)(data)

        # Source
        test_list(metadata[["source"]], info = f)
        test_list_names_valid(metadata[["source"]], info = f)

        v <- names(metadata[["source"]])
        i <- grepl("primary", v) | grepl("secondary", v) | grepl("original", v)

        expect_contains(v, "primary", info = f)

        expect_true(
          sum(grepl("primary", v)) <= 1,
          info = paste(
            f,
            "sources can have max 1 type labelled 'primary': ",
            paste(v, collapse = ", ")
          )
        )

        expect_true(all(i),
                    info = paste(
                      f,
                      "sources must be either primary or secondary:",
                      paste(v[!i], collapse = ", ")
                    ))

        vals <- c("key", "bibtype", "author", "title", "year")

        for (bib in names(metadata[["source"]])) {
          expect_contains(names(metadata[["source"]][[bib]]), vals, info = f)
        }

        keys <- unlist(lapply(metadata[["source"]], "[[", "key"))

        expect_unique(keys, info = paste(
          f,
          "sources must have unique keys:",
          paste(keys, collapse = ", ")
        ))

        # People
        test_list(metadata[["contributors"]], info = f)

        test_list_named_allowed(metadata[["contributors"]],
                                schema$metadata$elements$contributors$elements %>% names(),
                                info = f
        )

        # Data collectors
        if (!is.na(metadata[["contributors"]][["data_collectors"]][1])) {
          test_list(metadata[["contributors"]][["data_collectors"]], info = f)

          vars <- schema$metadata$elements$contributors$elements$data_collectors$elements %>% names()
          for (i in seq_along(metadata[["contributors"]][["data_collectors"]])) {
            test_list_named_allowed(
              metadata[["contributors"]][["data_collectors"]][[i]],
              vars, info = paste(f, "data_collector", i)
            )
            expect_contains(metadata[["contributors"]][["data_collectors"]][[i]] %>% names(), vars[1:4])
          }
        }

        # Dataset curators
        expect_true(!is.null(metadata[["contributors"]][["austraits_curators"]]))
        expect_type(metadata[["contributors"]][["austraits_curators"]], "character")

        # Assistants
        if (!is.null(metadata[["contributors"]][["assistants"]][1]))
          expect_type(metadata[["contributors"]][["assistants"]], "character")

        # Dataset

        test_list_named_allowed(metadata[["dataset"]],
                                schema$metadata$elements$dataset$values %>% names(),
                                info = paste0(f, "-dataset"))

        expect_type(metadata[["dataset"]][["data_is_long_format"]], "logical")
        expect_type(metadata[["dataset"]], "list")

        # Locations
        if (length(unlist(metadata[["locations"]])) > 1) {
          test_list(metadata[["locations"]], info = f)

          expect_silent(
            locations <-
              metadata$locations %>%
              process_format_locations(dataset_id, schema) %>%
              process_add_all_columns(names(schema[["austraits"]][["elements"]][["locations"]][["elements"]]))
          )

          test_dataframe_names_contain(
            locations,
            c("dataset_id", "location_name", "location_property", "value"),
            info = paste0(f, " - locations")
          )

          for (v in names(metadata$locations)) {
            test_list(metadata[["locations"]][[v]], info = f)
            expect_contains(
              names(metadata[["locations"]][[v]]),
              c("latitude (deg)", "longitude (deg)"),
              info = paste0(f, " - site: ", v)
            )
          }
        }

        # Contexts
        filename_data <- paste0("data/", dataset_id, "/data.csv")

        traits <-
          # Read all columns as character type to prevent time data types from being reformatted
          readr::read_csv(filename_data, col_types = cols(), guess_max = 100000, progress = FALSE) %>%
          process_custom_code(metadata[["dataset"]][["custom_R_code"]])()

        expect_silent(
          contexts <-
            metadata$contexts %>%
            process_format_contexts(dataset_id, traits)
        )

        ## Check context details load
        if (nrow(contexts) > 0) {
          test_dataframe_names_contain(
            contexts,
            c("context_property", "category", "var_in"),
            info = paste0(f, "-contexts")
          )

          for (i in seq_along(metadata$contexts)) {

            vals <- metadata$contexts[[i]][["values"]]

            if (!is.null(vals)) {

              for (s in seq_along(vals)) {

                # Check that no `find` values are NA
                if (!is.null(vals[[s]][["find"]])) {
                  expect_false(
                    is.na(vals[[s]][["find"]]),
                    info = paste0(
                      f,
                      sprintf("\tcontexts: `find` value in the `context_property` '%s' should not be NA\n\tPlease replace NAs with desired value using `custom_R_code`",
                              metadata$contexts[[i]][["context_property"]]))
                  )
                }

                # Check that there are no `find` fields without accompanying `value` fields
                expect_false(
                  !is.null(vals[[s]][["find"]]) && is.null(vals[[s]][["value"]]),
                  info = paste0(
                    f,
                    sprintf(
                      "\tcontexts: `find: %s` in the `context_property` '%s' is not accompanied by a `value` field",
                      vals[[s]][["find"]], metadata$contexts[[i]][["context_property"]]
                    ))
                )

                # Check that there are no `description` fields with NA `value` fields
                if (!is.null(vals[[s]][["value"]]) && !is.null(vals[[s]][["description"]])) {
                  expect_false(
                    !is.na(vals[[s]][["description"]]) && is.na(vals[[s]][["value"]]),
                    info = paste0(
                      f,
                      sprintf(
                        "\tcontexts: `description: %s` in the `context_property` '%s' should not be accompanied by an NA `value` field\n\tPlease use `custom_R_code` to assign a proper value to NA fields",
                        vals[[s]][["description"]], metadata$contexts[[i]][["context_property"]]
                      ))
                  )
                }

                # Check that there are no `description` fields without accompanying `find` and `value` fields
                expect_false(
                  !is.null(vals[[s]][["description"]]) && is.null(vals[[s]][["find"]]) && is.null(vals[[s]][["value"]]),
                  info = paste0(
                    f,
                    sprintf(
                      "\tcontexts: `description: %s` in the `context_property` '%s' is not accompanied by `find` and `value` fields",
                      vals[[s]][["description"]], metadata$contexts[[i]][["context_property"]]
                    )
                  )
                )
              }
            }
          }
        }

        # Traits
        expect_list_elements_contains_names(metadata[["traits"]],
                                            schema$metadata$elements$traits$elements[1:3] %>% names(),
                                            info = paste0(f, "-traits"))
        expect_list_elements_allowed_names(metadata[["traits"]],
                                           c(schema$metadata$elements$traits$elements %>% names(), unique(contexts$var_in)),
                                           info = paste0(f, "-traits"))
        expect_silent(
          traits <- traits.build::util_list_to_df2(metadata[["traits"]])
        )
        expect_true(is.data.frame(traits))

        expect_isin(traits$trait_name,
                    definitions$elements %>% names(),
                    info = paste0(f, "-traits"))

        # Now that traits loaded, check details of context match
        if (nrow(contexts > 0)) {

          # Check they are in context dataset
          expect_contains(
            c(names(data), names(traits)),
            unique(contexts$var_in),
            info = files[2]
          )

          for (j in unique(contexts[["var_in"]])) {
            contextsub <-
              contexts %>% filter(var_in == j)

            unique2 <- function(x) {unique(x[!is.na(x)])}
            # Context values align either with a column of data or a column of traits table
            # Sophie - Not sure how context values can align with the latter?
            if (is.null(data[[j]])) {
              v <- traits[[j]] %>% unique2()
            } else {
              v <- data[[j]] %>% unique2()
            }

            # `find` will always be non-NA unless both `find` and `value` fields are missing
            # since `process_format_contexts` replaces NA `find` with `value`
            # Look for context values in `find` column
            i <- v %in% contextsub[["find"]]

            expect_true(all(i),
                        info = paste0(
                          f,
                          "- context names from data file not present in metadata contexts: ",
                          v[!i]
                        )
            )
          }
        }

        # Check value types in metadata and any columns of data

        # XXXX To do -- also check for entity type, basis of value and any other columns

        if ("value_type" %in% names(traits)) {
          i <- (traits$value_type %in% names(data))

          value_type_fixed <- traits$value_type[!i] %>% unique()
          value_type_cols <- traits$value_type[i] %>% unique()


          expect_isin(
            value_type_fixed,
            schema$value_type$values %>% names,
            info = paste0(f, "-value types")
          )

          if (length(value_type_cols) > 0) {
            for (v in value_type_cols)
              expect_isin(
                data[[v]] %>% unique(),
                schema$value_type$values %>% names,
                info = paste(f, v, "- value types columns")
              )
          }
        }

        # Substitutions
        if (!is.na(metadata[["substitutions"]][1])) {
          expect_list_elements_contains_names(
            metadata[["substitutions"]],
            schema$metadata$elements$substitutions$values %>% names(),
            "metadata - substitution #"
          )
          trait_names <-
            sapply(metadata[["substitutions"]], "[[", "trait_name")
          expect_isin(unique(trait_names),
                      definitions$elements %>% names(),
                      info = paste0(f, "-substitutions-trait_name"))
          expect_isin(
            unique(trait_names),
            unique(traits$trait_name),
            info = paste0(f, "-substitutions-trait_name")
          )

          # Check for allowable values of categorical variables
          expect_no_error(
            x <- metadata[["substitutions"]] %>% util_list_to_df2() %>% split(.$trait_name))

          for (trait in names(x)) {
            if (!is.null(definitions$elements[[trait]]) &&
                definitions$elements[[trait]]$type == "categorical") {
              to_check <- x[[trait]]$replace %>% unique()
              allowable <-
                c(definitions$elements[[trait]]$allowed_values_levels %>% names(),
                  NA)
              failing <- to_check[!(
                is.na(to_check) |
                  to_check %in% allowable |
                  to_check %>% sapply(util_check_all_values_in, allowable)
              )]
              expect_length_zero(
                failing,
                info = sprintf(
                  "%s - substitutions for `%s` have invalid replacement values",
                  f,
                  trait
                ),
                label = failing %>% paste(collapse = ", ")
              )
            }
          }
        }

        ## Check config files contain all relevant columns
        if (metadata[["dataset"]][["data_is_long_format"]]) {
          # Variable match
          #expect_isin(names(metadata[["dataset"]]), c("taxon_name",  "trait_name", "value","location_name", "individual_id", "context_name", "collection_date"), info=paste0(f, " - variable_match"))

          # For vertical datasets, expect all values of "trait column" found in traits
          var_out <- names(metadata[["dataset"]])
          var_in <- unlist(metadata[["dataset"]])
          i <- match("trait_name", var_out)
          values <- unique(data[[var_in[i]]])
          expect_contains(traits[["var_in"]], values, info = files[2])
        } else {
          # Variable match
          #expect_isin(names(metadata[["dataset"]]), c("taxon_name", "location_name", "individual_id", "context_name", "collection_date"), info=paste0(f, " - variable_match"))

          # For wide datasets, expect variables in traits are header in the data
          values <- names(data)
          expect_isin(traits[["var_in"]], values, info = files[2])
        }


        ## TODO

        ## Make sure specified columns exist


        ## TODO

        ## For numeric trait data, check it looks reasonable & converts properly

        ## Check `location_name`'s are in locations dataset

        if (length(unlist(metadata[["locations"]])) > 1) {
          expect_true(
            !is.null(metadata[["dataset"]][["location_name"]]),
            info = paste0(files[2], " - variable_match -> location_name is missing")
          )

          expect_contains(
            names(data),
            metadata[["dataset"]][["location_name"]],
            info = paste0(files[2], " - column ", metadata[["dataset"]][["location_name"]], "not found in data")
          )

          v <-
            (data[[metadata[["dataset"]][["location_name"]]]] %>% unique %>% na.omit)
          i <- v %in% names(metadata$locations)
          expect_true(all(i),
                      info = paste0(f,  "- site names from data file not present in metadata: ", v[!i]))

          i <- names(metadata$locations) %in% v
          expect_true(all(i),
                      info = paste0(
                        f,
                        "- site names from metadata not present in data file: ",
                        names(metadata$locations)[!i]
                      ))
        }

        ## Check that dataset can pivot wider
        expect_no_error(dataset <- test_build_dataset(
          file.path(path_data, dataset_id, "metadata.yml"),
          file.path(path_data, dataset_id, "data.csv"),
          dataset_id,
          get_schema("config/traits.yml", "traits"),
          get_unit_conversions("config/unit_conversions.csv"),
          get_schema(),
          get_schema("config/metadata.yml",  "metadata"),
          read_csv_char("config/taxon_list.csv")
        ), info = sprintf(" - cannot build %s", dataset_id))
        
        # Check that special characters do not make it into the data
        expect_no_error(
          parsed_data <- data %>%
            process_custom_code(metadata[["dataset"]][["custom_R_code"]])() %>%
            process_parse_data(dataset_id, metadata, contexts),
          info = "`process_parse_data`")

        expect_allowed_text(
          parsed_data$traits$value, is_data = TRUE,
          info = sprintf("%s", files[1])
        )

        expect_equal(
          dataset$traits %>%
          select(
            dplyr::all_of(c("dataset_id", "trait_name", "value", "observation_id", "source_id", "taxon_name",
            "entity_type", "life_stage", "basis_of_record", "value_type", "population_id", "individual_id",
            "temporal_context_id", "method_id", "method_context_id", "entity_context_id", "original_name"))
          ) %>%
          pivot_wider(names_from = "trait_name", values_from = "value", values_fn = length) %>%
          pivot_longer(cols = 16:ncol(.)) %>%
          rename(all_of(c("trait_name" = "name", "number_of_duplicates" = "value"))) %>%
          select(
            all_of(c("dataset_id", "taxon_name", "trait_name", "number_of_duplicates", "observation_id",
            "entity_type", "value_type", "population_id")), everything()
          ) %>%
          filter(number_of_duplicates > 1) %>%
          nrow(),
          0, # Expect nrow() = 0
          info = sprintf("Duplicate rows in %s detected; `traits` table cannot pivot wider", dataset_id)
        )
      })
    }

    # Keep this
    context("end")
  }
