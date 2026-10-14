## `collection_date` is converted only from a format the dataset declares, never
## one inferred from the values. An inferred reading of an ambiguous value is a
## valid-looking wrong date, which no later check can catch: `2019/20` read as
## a range ending on Excel day 20, `15-Mar` as March 2015, a UTC timestamp as
## the UTC day rather than the local one.


test_that("`util_collection_date_is_valid` accepts only the standard forms", {
  valid <- c(
    "2020", "2020-05", "2020-05-15", "2010-10/2011-03", "2004/2004",
    "1818-10-06", "unknown/2022"
  )
  invalid <- c(
    # Not ISO
    "15/05/2020", "2-Sep-08", "20200515", "2020-5-15", "2019/20",
    # Not a real date, or out of range
    "2020-02-30", "2020-13", "0000", "1699", "2453-01-20",
    # Malformed ranges: `strsplit()` drops the empty piece of `2020/`
    "2020/", "/2020", "2020/2019", "2020/2021/2022", ".na/.na",
    # Whitespace and time of day are not stripped
    " 2020", "2020 ", "2022-06-03 14:00:00",
    "unknown", "unknown/20"
  )

  expect_equal(util_collection_date_is_valid(valid), rep(TRUE, length(valid)))
  expect_equal(util_collection_date_is_valid(invalid), rep(FALSE, length(invalid)))
  expect_equal(util_collection_date_is_valid(c(NA, "2020")), c(NA, TRUE))
})


test_that("`util_format_collection_date` converts from the declared format", {
  expect_equal(
    util_format_collection_date(
      c("2-Sep-08", "15/10/2008", "15.10.2008", "15 October 2008", "15102008"), "dmy"
    ),
    rep(c("2008-09-02", "2008-10-15"), c(1, 4))
  )
  expect_equal(
    util_format_collection_date(c("9/2/2008", "Sep 2, 08", "Sept 2 2008", "Tue Sep 2 2008", "Thr Sep  4 2008"), "mdy"),
    c(rep("2008-09-02", 4), "2008-09-04")
  )
  expect_equal(
    util_format_collection_date(c("19.10.10", "20191010", "2019/10/10"), "ymd"),
    rep("2019-10-10", 3)
  )
  expect_equal(util_format_collection_date(c("Jan-96", "01/1996"), "my"), rep("1996-01", 2))
  expect_equal(util_format_collection_date("2008/09", "ym"), "2008-09")
})


test_that("formats are tried in turn, and the one matched sets the precision", {
  expect_equal(
    util_format_collection_date(c("2-Sep-08", "Oct 2008", "2008"), c("dmy", "my", "y")),
    c("2008-09-02", "2008-10", "2008")
  )
})


test_that("a value the declared format does not match is returned unchanged", {
  # Every one of these was given a confident wrong reading by format inference
  ambiguous <- c("15-Mar", "2019/20", "Sep-Oct 2008", "202001", "5", "Jan-96, Feb-96")
  expect_equal(util_format_collection_date(ambiguous, c("dmy", "my", "y")), ambiguous)

  # Read in the declared format, the date does not exist
  expect_equal(util_format_collection_date("30/02/2008", "dmy"), "30/02/2008")
  expect_equal(util_format_collection_date("13/25/2008", "mdy"), "13/25/2008")
})


test_that("values already standard pass through, whatever the format", {
  standard <- c("2008-09-02", "2008", "2010-10/2011-03", "unknown/2022", NA)
  expect_equal(util_format_collection_date(standard, "mdy"), standard)
  expect_equal(util_format_collection_date(standard), standard)
})


test_that("a two-digit year is the latest year ending in those digits that is not in the future", {
  expect_equal(
    util_format_collection_date(c("12-Mar-55", "21/11/78", "2-Sep-08"), "dmy"),
    c("1955-03-12", "1978-11-21", "2008-09-02")
  )
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  next_year <- sprintf("%02d", (this_year + 1) %% 100)
  expect_equal(
    util_format_collection_date(paste0("1/1/", next_year), "dmy"),
    sprintf("%d-01-01", this_year + 1 - 100)
  )
})


test_that("a local time of day is dropped, a point in time needs a time zone", {
  expect_equal(
    util_format_collection_date(c("9/14/2020 10:01:26", "8/11/2020 8:57"), "mdy"),
    c("2020-09-14", "2020-08-11")
  )

  # 14:00 UTC on 3 June is midnight on 4 June in Sydney
  utc <- c("2022-06-03T14:00:00Z", "2022-10-21T16:22:05Z", "2022-10-08T05:03:17+00:00")
  expect_equal(util_format_collection_date(utc, "ymd"), utc)
  expect_equal(
    util_format_collection_date(utc, timezone = "Australia/Sydney"),
    c("2022-06-04", "2022-10-22", "2022-10-08")
  )

  # readr reads those as `POSIXct`, in UTC
  instants <- as.POSIXct(c("2022-06-03 14:00:00", NA), tz = "UTC")
  expect_equal(
    util_format_collection_date(instants, timezone = "Australia/Sydney"),
    c("2022-06-04", NA)
  )
  expect_false(isTRUE(util_collection_date_is_valid(util_format_collection_date(instants)[1])))
})


test_that("columns readr types as dates or numbers are converted", {
  expect_equal(util_format_collection_date(as.Date(c("2020-01-05", NA))), c("2020-01-05", NA))
  expect_equal(
    util_format_collection_date(c(2012, NA, 20190206), c("ymd", "y")),
    c("2012", NA, "2019-02-06")
  )
})


test_that("an unknown or ambiguous format, or an unknown time zone, is an error", {
  expect_error(util_format_collection_date("1/1/2000", "dym"), "must be one or more of")
  expect_error(util_format_collection_date("1/1/2000", c("dmy", "mdy")), "read the same value two ways")
  expect_error(util_format_collection_date("1/2000", c("ym", "my")), "read the same value two ways")
  expect_error(util_format_collection_date("2000", timezone = "Sydney"), "Olson time zone")

  # As written in `metadata.yml`
  expect_equal(util_check_collection_date_format("dmy my y"), c("dmy", "my", "y"))
  expect_equal(util_check_collection_date_format(".na"), character(0))
  expect_null(util_check_collection_date_timezone(NA))
})


test_that("the build converts the columns `collection_date` names, and only those", {
  data <- tibble::tibble(
    date = c("2-Sep-08", "3-Sep-08"),
    leaf_date = c("4/9/2008", "5/9/2008"),
    other = c("2-Sep-08", "3-Sep-08")
  )
  metadata <- list(
    dataset = list(collection_date = "date", collection_date_format = "dmy"),
    traits = list(
      list(var_in = "a", trait_name = "x", collection_date = "leaf_date"),
      list(var_in = "b", trait_name = "y")
    )
  )

  out <- process_format_collection_date(data, metadata)
  expect_equal(out$date, c("2008-09-02", "2008-09-03"))
  expect_equal(out$leaf_date, c("2008-09-04", "2008-09-05"))
  expect_equal(out$other, data$other)

  # Nothing declared: nothing converted
  metadata$dataset$collection_date_format <- NULL
  expect_equal(process_format_collection_date(data, metadata), data)
})


# Test_2023_7, with `edit` applied to its metadata, in a fresh data folder
dataset_test_dates <- function(edit) {
  path_data <- withr::local_tempdir()
  dir.create(file.path(path_data, "Test_2023_7"))
  file.copy(
    list.files(file.path("examples", "Test_2023_7"), full.names = TRUE),
    file.path(path_data, "Test_2023_7"), recursive = TRUE
  )
  path_metadata <- file.path(path_data, "Test_2023_7", "metadata.yml")
  write_metadata(edit(read_metadata(path_metadata)), path_metadata)

  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )
  out <- capture.output(
    suppressMessages(dataset_test("Test_2023_7", path_data = path_data))
  )
  gsub("\033\\[[0-9]+m", "", out)
}


test_that("`dataset_test` reports dates that are not standard in the built dataset", {
  # Test_2023_7's `Date` column is written `2-Sep-08`
  out <- dataset_test_dates(function(metadata) {
    metadata$dataset$collection_date_format <- NULL
    metadata
  })
  expect_match(
    out, "`collection_date` has 2 values not written as .*'2-Sep-08', '3-Sep-08'",
    all = FALSE
  )
  expect_match(out, "Declare the format they are written in with `collection_date_format`", all = FALSE)

  # Declaring the wrong format converts nothing, so the values are still reported
  out <- dataset_test_dates(function(metadata) {
    metadata$dataset$collection_date_format <- "mdy"
    metadata
  })
  expect_match(out, "'2-Sep-08', '3-Sep-08'", all = FALSE)
})


test_that("`dataset_test` rejects an invalid `collection_date_format`", {
  out <- dataset_test_dates(function(metadata) {
    metadata$dataset$collection_date_format <- "dmy mdy"
    metadata
  })
  expect_match(out, "dataset - `collection_date_format`", all = FALSE)
})
