
test_expect_no_error <- function(object, info) {

  error <- tryCatch({
    object
    NULL
  }, error = function(e) {
    e
  })
  testthat::expect(
    is.null(error),
    sprintf("%s threw an error:\n\n" %+% red("%s"), info, paste(error, collapse = ",")))
  invisible(object)
}


test_expect_no_warning <- function(object, info) {
  warning <- tryCatch({
    object
    NULL
  }, warning = function(w) {
    w
  })
  testthat::expect(is.null(warning), info)
}


test_expect_is_in <- function(object, expected, info, label, na.rm = TRUE) {

    if (na.rm)
      object <- object[!is.na(object)]
    i <- object %in% expected

    comp <- testthat::compare(all(i), TRUE)
    testthat::expect(
      comp$equal,
      sprintf(
        "%s - %s should not contain: '%s'",
        info, label,
        paste(object[!i], collapse = "', '")
      ))

    invisible(object)
  }


test_expect_contains <- function(object, expected, info) {

  i <- expected %in% object

  comp <- testthat::compare(all(i), TRUE)
  testthat::expect(
    comp$equal,
    sprintf("%s - does not contain: '%s'", info, paste(expected[!i], collapse = "', '"))
  )

  invisible(object)
}


test_expect_allowed <- function(object, allowed, info, label) {

  i <- object %in% allowed

  comp <- testthat::compare(all(i), TRUE)
  testthat::expect(
    comp$equal,
    sprintf(
      "%s - %s include(s) invalid terms: '%s'",
      info, label,
      paste(object[!i], collapse = "', '")
    ))

  invisible(object)
}


test_expect_equal <- function(object, expected, info) {
  i <- object == expected
  comp <- testthat::compare(all(i), TRUE)
  testthat::expect(comp$equal, info)
}


test_expect_true <- function(object, info) {
  i <- object == TRUE
  comp <- testthat::compare(all(i), TRUE)
  testthat::expect(comp$equal, info)
}


test_expect_false <- function(object, info) {
  i <- object == FALSE
  comp <- testthat::compare(all(i), TRUE)
  testthat::expect(comp$equal, info)
}


test_expect_named <- function(object, expected_names, info, label) {

  if (missing(expected_names)) {
    testthat::expect(
      !identical(names(object), NULL),
      sprintf("%s - %s do not exist", info, label))
  } else {
    testthat::expect(
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


test_expect_type <- function(object, type, info, label) {
  stopifnot(is.character(type), length(type) == 1)
  testthat::expect(
    identical(typeof(object), type),
    sprintf("%s - %s has type %s, not %s", info, label, typeof(object), type)
  )
}


test_expect_not_NA <- function(object, info, label) {
  i <- !is.na(object)
  comp <- testthat::compare(all(i), TRUE)
  testthat::expect(comp$equal, sprintf("%s - %s contain(s) NAs", info, label))
  invisible(object)
}


test_expect_length_zero <- function(object, info, label) {
  comp <- testthat::compare(length(object), 0)
  testthat::expect(comp$equal, sprintf("%s: %s", info, label))
  invisible(object)
}


test_expect_unique <- function(object, info, label) {
  x <- table(unlist(object))
  i <- x == 1
  comp <- testthat::compare(all(i), TRUE)
  testthat::expect(comp$equal, sprintf("%s - %s not unique: '%s'", info, label, paste(names(x)[!i], collapse = "', '")))
  invisible(object)
}


test_expect_allowed_text <- function(object, is_data = FALSE, is_col_names = FALSE, info, label) {

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
      if (is_col_names) {
        txt <- sprintf(
          "%s\t- col %s: %s\n",
          txt, i, colour_characters(object[[i]], which(disallowed[[i]])))
      } else {
        txt <- sprintf(
          "%s\t- ln %s: %s\n",
          txt, i, colour_characters(object[[i]], which(disallowed[[i]])))
      }

    }

    if (is_data) {
      testthat::expect(
        identical(as.vector(all(!check)), TRUE),
        sprintf(
          "%s\tdisallowed characters in data detected: %s\n\tPlease replace using `custom_R_code`",
          info, txt
        )
      )
    } else {
      testthat::expect(
        identical(as.vector(all(!check)), TRUE),
        sprintf("%s - disallowed characters in %s detected: \n%s", info, label, txt)
      )
    }

  }

  invisible(object)

}


#' Split a string into characters
#'
#' Falls back to splitting on bytes when the input is not valid UTF-8, so that
#' text this check exists to catch cannot make the check itself error.
#'
#' @param x A length-1 character vector
#' @return Character vector of the individual characters in `x`
#' @keywords internal
util_split_chars <- function(x) {
  strsplit(x, "", useBytes = !validUTF8(x))[[1]]
}


colour_characters <- function(x, i = NULL) {

  # Split per character, not per byte. Splitting on bytes wrapped the escape
  # codes around the halves of a multi-byte character, so reporting a
  # non-ASCII character produced invalid UTF-8 -- which then errored in
  # whatever tried to print or trim the message.
  chars <- util_split_chars(x)

  # Wrapper around characters to print as colour
  # obtained from crayon::red(x)
  if (!is.null(i))
    chars[i] <- sprintf("\033[31m%s\033[39m", chars[i])

  paste0(chars, collapse = "")
}


check_disallowed_chars <- function(x, exceptions = c("\u00c1\u00c5\u00c0\u00c2\u00c4\u00c6\u00c3\u0100\u00e2\u00ed\u00e5\u00e6\u00e4\u00e3\u00e0\u00e1\u00ed\u00c7\u010d\u00f3\u00f6\u00f8\u00e9\u00e8\u0142\u0144l\u00b0\u00ea\u00dc\u00fc\u00f9\u00fa\u00fb\u00b1\u00b5\u00b5\u201c\u201d\u2018\u2019-\u2013\u2014\u2248\u02dc\u00d7\u2265\u2264")) {

  # Allow some utf8 characters, those with accents over letters for foreign names
  # List of codes is here: http://www.utf8-chartable.de/
  allowed_chars <- util_split_chars(exceptions)

  # Compared whole character against whole character. This used to flatten
  # `exceptions` into an unordered bag of bytes and allow a character when each
  # of its bytes appeared *somewhere* in that bag, which let 1,671 code points
  # through: the 49-character exception list yields only 47 distinct bytes, so
  # anything reassembled from them passed. `ñ`, `É`, `Ø`, `º`, `≠`, `…`, `‰` and
  # a non-breaking space were all silently accepted (#233).
  #
  # Returns one value per character, not per byte, so positions line up with
  # `colour_characters()`.
  vapply(
    util_split_chars(x),
    function(char) {
      bytes <- charToRaw(char)

      # ASCII is always allowed, whatever the exception list says
      if (all(bytes < 0x7F)) {
        return(FALSE)
      }

      # `util_split_chars()` falls back to splitting on bytes for input that is
      # not valid UTF-8, so `char` can be a lone continuation byte. Those are
      # exactly the mojibake this check exists to catch, and comparing them as
      # strings is not meaningful, so treat them as disallowed.
      if (!validUTF8(char)) {
        return(TRUE)
      }

      !(char %in% allowed_chars)
    },
    logical(1),
    USE.NAMES = FALSE
  )
}


test_expect_list_elements_contains_names <- function(object, expected, info) {
  for (i in seq_along(object))
    test_expect_contains(names(object[[i]]), expected, info = paste(info, i))
  invisible(object)
}


test_expect_list_elements_allowed_names <- function(object, allowed, info, label) {
  for (i in seq_along(object))
    test_expect_allowed(names(object[[i]]), allowed, info = paste(info, i), label = "field names")
  invisible(object)
}


test_expect_list_elements_exact_names <- function(object, expected, info) {
  for (i in seq_along(object)) {
    test_expect_contains(names(object[[i]]), expected, info = paste(info, i))
    test_expect_allowed(names(object[[i]]), expected, info = paste(info, i), label = "field names")
  }
  invisible(object)
}


test_expect_dataframe_valid <- function(data, info, label) {
  test_expect_not_NA(colnames(data), info, label)
  test_expect_allowed_text(colnames(data), is_col_names = TRUE, info = info, label = label)
  test_expect_unique(colnames(data), info, label)
  test_expect_true(is.data.frame(data), info = sprintf("%s - is not a dataframe", info))
}


test_expect_dataframe_named <- function(data, expected_colnames, info, label) {
  test_expect_dataframe_valid(data, info, label)
  test_expect_named(data, expected_colnames, info = info, label = label)
}


test_expect_dataframe_names_contain <- function(data, expected_colnames, info, label) {
  test_expect_dataframe_valid(data, info, label)
  test_expect_contains(names(data), expected_colnames, info = info)
}



test_expect_list <- function(data, info) {
  test_expect_true("list" %in% class(data), info = sprintf("%s - is not a list", info))
}


test_expect_list_names_valid <- function(data, info, label) {
  test_expect_list(data, info)
  test_expect_not_NA(names(data), info = info, label = paste0("names of ", label))
  test_expect_unique(names(data), info = info, label = paste0("names of ", label))
}


test_expect_list_names_exact <- function(data, expected_names, info, label) {
  test_expect_list_names_valid(data, info, label = label)
  test_expect_named(data, expected_names, info = info, label = label)
}


test_expect_list_names_allowed <- function(data, allowed_names, info, label) {
  test_expect_list_names_valid(data, info, label = label)
  test_expect_named(data, info = info, label = label)
  test_expect_allowed(names(data), allowed_names, info = info, label = label)
}


test_expect_list_names_contain <- function(data, expected_names, info, label) {
  test_expect_list_names_valid(data, info, label = label)
  test_expect_named(data, info = info, label = label)
  test_expect_contains(names(data), expected_names, info = info)
}


test_build_dataset <- function(
  path_metadata, path_data, info, definitions, unit_conversions, schema, resource_metadata, taxon_list) {

  # Test it builds with no errors
  test_expect_no_error(
    build_config <- dataset_configure(path_metadata, definitions),
    info = "`dataset_configure`"
  )

  test_expect_no_error(
    build_dataset_raw <- dataset_process(path_data, build_config, schema, resource_metadata, unit_conversions),
    info = "`dataset_process`"
  )

  test_expect_no_error(
    build_dataset <- dataset_update_taxonomy(build_dataset_raw, taxon_list),
    info = "`dataset_update_taxonomy`"
  )

  test_expect_structure(build_dataset, info, schema, definitions, single_dataset = TRUE)

  build_dataset
}


test_expect_structure <- function(data, info, schema, definitions, single_dataset = TRUE) {

  vars_austraits <- schema$austraits$elements %>% names()

  vars_tables <-
    vars_austraits %>%
    subset(., !(. %in% c(
      "definitions", "schema", "sources", "metadata",
      "build_info", "taxonomic_updates", "taxa")))

  # Test lists have the right objects
  comparison <- vars_austraits

  test_expect_list_names_exact(data, comparison, info, label = "output tables")

  # Test structure of tables
  for (v in vars_tables) {
    comparison <- schema$austraits$elements[[v]]$elements %>% names()
    test_expect_dataframe_named(data[[v]], comparison, info = info, label = paste0(v, " table column names"))
  }

  # Test that minimum expected columns are in `taxa` and `taxonomic_updates` tables
  test_expect_contains(names(data[["taxa"]]), c("taxon_name", "taxon_rank"), info = paste0(info, "\tnames of `taxa` table"))
  test_expect_contains(
    names(data[["taxonomic_updates"]]),
    c("dataset_id", "original_name", "aligned_name", "taxon_name", "taxonomic_resolution"),
    info = paste0(info, "\tnames of `taxonomic_updates` table")
  )

}


## A helper function to determine if this is being run as part of a test
is_testing_env <- function() {
  # Calling scope
  tb <- .traceback(x = 0)

  # Check if called in `testthat` or interactive
  if (any(unlist(lapply(tb, function(x) any(grepl("test_env", x)))))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
