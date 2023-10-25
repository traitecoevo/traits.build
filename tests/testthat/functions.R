
# Are these functions supposed to be the same as those in testdata.R?

expect_no_error <- function(object, regexp = NULL, ..., info = NULL, label = NULL) {
  error <- tryCatch({
    object
    NULL
  }, error = function(e) {
    e
  })
  if (is.null(label))
    expect(is.null(error), sprintf("%s", paste(error$message, collapse = ", ")), info = info)
  else
    expect(is.null(error), sprintf("%s threw an error: %s", label, paste(error$message, collapse = ", ")), info = info)
  invisible(NULL)
}


expect_unique <- function(object, info = NULL, label = NULL) {
  x <- table(unlist(object))
  i <- x == 1
  comp <- testthat::compare(all(i), TRUE)
  expect(comp$equal,
         sprintf("%s - not unique: %s", info, paste(names(x)[!i], collapse = ", ")))
  invisible(object)
}


expect_is_in <- function(object, expected, ..., info = NULL, label = NULL,
                        expected.label = NULL, na.rm = TRUE) {

  if (na.rm)
    object <- object[!is.na(object)]
  i <- object %in% expected

  comp <- compare(all(i), TRUE, ...)
  expect(
    comp$equal,
    sprintf("%s - should not contain: %s", info, paste(object[!i], collapse = ", "))
  )

  invisible(object)
}


expect_not_NA <- function(object, info = NULL, label = NULL) {

  i <- !is.na(object)
  comp <- compare(all(i), TRUE)
  expect(comp$equal,
         sprintf("%s - object contains NAs", info))
  invisible(object)
}


expect_list <- function(data, info) {
  expect_true(class(data) == "list", info = info)
}


expect_list_names_valid <- function(data, info) {
  expect_list(data, info)
  expect_not_NA(names(data), info = info)
#  expect_allowed_text(names(data), info = info)
  expect_unique(names(data), info = info)
}


expected_named_list <- function(data, expected_names, info) {
  expect_list_names_valid(data, info)
  expect_named(data, expected_names, info = info)
}


expect_list_names_contain <- function(data, expected_names, info) {
  expect_list_names_valid(data, info)
  expect_is_in(names(data), expected_names, info = info)
}


expect_dataframe_valid <- function(data, info) {
  expect_not_NA(colnames(data), info = info)
#  expect_allowed_text(colnames(data), info = info)
  expect_unique(colnames(data), info = info)
  expect_true(is.data.frame(data), info = info)
}


expect_dataframe_named <- function(data, expected_colnames, info) {
  # I think the ordering of naming currently matters, maybe we don't want that?
  # Affected by what order fields are entered into the metadata
  expect_dataframe_valid(data, info)
  expect_named(data, expected_colnames, info = info)
}


test_build_dataset <- function(
  path_metadata, path_data, info, definitions, unit_conversions, schema, resource_metadata, taxon_list) {

  # Test it builds with no errors
  expect_no_error({
    build_config <- dataset_configure(path_metadata, definitions)
  }, info = paste(info, "`dataset_configure`"))

  expect_no_error({
    build_dataset_raw <- dataset_process(path_data, build_config, schema, resource_metadata, unit_conversions)
  }, info = paste(info, "`dataset_process`"))

  expect_no_error({
    build_dataset <- build_update_taxonomy(build_dataset_raw, taxon_list)
  }, info = paste(info, "`build_update_taxonomy`"))

  test_structure(build_dataset, info, schema, definitions, single_dataset = TRUE)

  build_dataset
}


test_structure <- function(
  data, info, schema, definitions, single_dataset = TRUE) {

  vars_austraits <-
    schema$austraits$elements %>% names()

  vars_tables <-
    vars_austraits %>%
    subset(., !(. %in% c("dataset_id", "definitions", "schema", "sources", "metadata", "build_info")))

  # Test lists have the right objects
  comparison <- vars_austraits

  expect_named_list(data, comparison, info = c(info, " - main elements"))

  # Test structure of tables
  for (v in vars_tables) {
    comparison <- schema$austraits$elements[[v]]$elements %>% names()
    expect_dataframe_named(data[[v]], comparison, info = paste(info, "- structure of", v))
  }

  # Contains allowed traits
  expect_is_in(data$traits$trait_name %>% unique(), definitions$elements %>% names(), info = paste("traits ", v))
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
