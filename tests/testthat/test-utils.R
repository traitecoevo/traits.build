# Add a few more tests to ones that are lacking
test_that("`util_replace_null` returns NA", {
  expect_equal(util_replace_null(NULL), NA)
  expect_equal(util_replace_null(NULL, val = "none"), "none")
  expect_equal(util_replace_null(1), 1)
})


test_that("`convert_df_to_list` is working", {
  starwars_list <- suppressWarnings(convert_df_to_list(dplyr::starwars))

  expect_type(starwars_list, "list")
  expect_type(starwars_list, "list")
})


test_that("`convert_list_to_df1` is working", {
  starwars_df <- suppressWarnings(convert_list_to_df1(as.list(dplyr::starwars)[2]))

  expect_equal(starwars_df |> nrow(), 87)
  expect_match(starwars_df$value |> class(), "integer")
  expect_match(starwars_df$key |> unique(), "height")
  expect_named(starwars_df, c("key", "value"))
})


test_that("`convert_list_to_df2` is working", {
  expect_equal(convert_list_to_df2(NULL) |> suppressWarnings(), NA)
  expect_equal(convert_list_to_df2(NA) |> suppressWarnings(), NA)

  my_list <-suppressWarnings(convert_df_to_list(dplyr::starwars))
  expect_match(class(convert_list_to_df2(my_list))[1] |> suppressWarnings(), "tbl_df")
  expect_equal(convert_list_to_df2(my_list, as_character = TRUE)[[1,2]] |> suppressWarnings(), "172")
  expect_equal(convert_list_to_df2(my_list, as_character = FALSE)[[1,2]] |> suppressWarnings(), 172)

  my_list <- list(NA)
  expect_equal(convert_list_to_df2(my_list) |> suppressWarnings(), NA)
})


test_that("`util_df_convert_character` is working", {
  expect_type(dplyr::starwars$height, "integer")
  expect_type(util_df_convert_character(dplyr::starwars)$height, "character")
})


test_that("`util_extract_list_element` is working", {
  test_list <- convert_df_to_list(dplyr::starwars) |> suppressWarnings()
  names(test_list) <- paste("row", seq_len(nrow(dplyr::starwars)))

  expect_type(util_extract_list_element(1, test_list, "height"), "integer")
  expect_equal(util_extract_list_element(1, test_list,  "height"), 172)
  expect_equal(util_extract_list_element("row 2", test_list, "skin_color"), "gold")
  expect_error(util_extract_list_element((length(test_list) + 1), test_list, "height"), "subscript out of bounds")
})


test_that("`util_append_to_list` is working", {
  my_list <- as.list(dplyr::starwars)
  expect_equal(util_append_to_list(my_list, NULL), my_list)
  expect_length(util_append_to_list(my_list, NA), 15)
  expect_gt(length(util_append_to_list(my_list, NA)), length(my_list))
  expect_type(util_append_to_list(my_list[seq(1, length(my_list) - 1)], my_list[14]), "list")
  expect_length(util_append_to_list(my_list[seq(1, length(my_list) - 1)], my_list[14]), 14)
  expect_error(util_append_to_list(my_list), 'argument "to_append" is missing, with no default')
})


test_that("`util_separate_and_sort` returns alphabetically sorted characters", {
  expect_type(util_separate_and_sort("z y x"), "character")
  expect_match(util_separate_and_sort("z y x"), "x y z")
  expect_match(util_separate_and_sort("300 200 100 1 2 3"), "1 100 2 200 3 300")
})


test_that("testing env is working", {
  expect_true(is_testing_env())
})


test_that("`check_disallowed_chars` compares whole characters, not bytes", {
  # The exception list used to be flattened into an unordered bag of bytes, and a
  # character was allowed when each of its bytes appeared *somewhere* in that
  # bag. The 49-character list yields only 47 distinct bytes, so 1,671 code
  # points reassembled from them slipped through (#233).
  f <- check_disallowed_chars

  # Every one of these was silently accepted. Those that are genuine characters
  # are now permitted deliberately, via the schema; the ones below are look-alikes
  # or invisible, and must stay caught.
  leaked <- c("\u00a0", "\u00ba", "\u2260", "\u2022", "\u2020", "\u25e6")
  for (ch in leaked) {
    expect_true(any(f(ch)), info = sprintf("U+%04X", utf8ToInt(ch)))
  }

  # Characters that were already caught must stay caught
  for (ch in c("€", "α", "中")) {
    expect_true(any(f(ch)), info = sprintf("U+%04X", utf8ToInt(ch)))
  }

  # ...and every character in the allowed set must pass, or the check would start
  # rejecting the accented names and symbols it exists to permit
  allowed <- util_split_chars(util_allowed_characters())
  expect_false(any(vapply(allowed, function(ch) any(f(ch)), logical(1))))

  # ASCII is allowed whatever the exception list says
  expect_false(any(f("normal text 123 (a-b) [c] 45%")))

  # One value per character, not per byte, so `colour_characters()` can index it
  expect_length(f("abé"), 3L)

  # Invalid UTF-8 is what this check exists to catch, so a stray Latin-1 byte
  # must not pass. 45 of the 128 non-ASCII byte values used to.
  expect_true(any(f(rawToChar(as.raw(c(0x32, 0x35, 0xb1, 0x31))))))

  # The `is_data = TRUE` path passes `exceptions = ""`, which is ASCII-only.
  # That was already equivalent to a character-wise check, so it must not move.
  expect_true(any(f("é", exceptions = "")))
  expect_false(any(f("abc", exceptions = "")))
})


test_that("the allowed characters come from the schema, and are case-symmetric", {
  allowed <- util_split_chars(util_allowed_characters())

  # The point of defining letters by range: hand-enumeration left 14 letters
  # allowed in lower case but not upper, so `Ósvaldsson` was reported while
  # `ósvaldsson` would not have been.
  letters_only <- allowed[grepl("^\\p{L}$", allowed, perl = TRUE)]
  other_case <- ifelse(letters_only == tolower(letters_only),
                       toupper(letters_only), tolower(letters_only))
  has_case <- tolower(letters_only) != toupper(letters_only)

  # The exceptions are letters whose other case is ASCII or Greek, so they cannot
  # be in a non-ASCII set: U+0130 dotted capital I, U+0131 dotless i, U+017F long
  # s, and U+00B5 micro sign.
  asymmetric <- letters_only[has_case & !(other_case %in% letters_only)]
  expect_setequal(asymmetric, c("İ", "ı", "ſ", "µ"))

  # No duplicates and no ASCII -- both were true of the hand-written list, which
  # listed `í` and `µ` twice and included a stray ASCII `l`
  expect_false(any(duplicated(allowed)))
  expect_false(any(vapply(allowed, utf8ToInt, integer(1)) < 128L))
})


test_that("the ordinal indicators stay disallowed despite being letters", {
  # Unicode classifies U+00AA and U+00BA as letters, so a range-based rule would
  # admit them. They occur only as look-alikes -- U+00BA for the degree sign is
  # the single most common disallowed character in the database -- so the schema
  # lists them under `never_allowed`.
  expect_true(any(check_disallowed_chars("º")))
  expect_true(any(check_disallowed_chars("ª")))
  expect_false(any(check_disallowed_chars("°")))
})


test_that("names and symbols that used to be reported are now allowed", {
  # Measured occurrences in the three downstream databases, all of them genuine
  for (x in c("Ósvaldsson", "Briceño", "Klimešová",
              "16°17′24″S", "(d13C, ‰)", "∼38 Pa")) {
    expect_false(any(check_disallowed_chars(x)), info = x)
  }
})


test_that("util_allowed_characters caches rather than re-reading the schema", {
  first <- util_allowed_characters()
  expect_identical(util_allowed_characters(), first)
  expect_identical(character_cache$allowed, first)
})


# `yaml::read_yaml()` is superlinear in the number of container nodes, so a
# dataset with one location per georeferenced record takes minutes to read
# (#263). `util_read_yaml_chunked()` parses a large `locations:` block in
# chunks; these tests pin it to agreeing with the plain parser.

# Build a copy of a real example metadata file with `n` locations spliced in,
# keeping every other block -- including the `taxonomic_updates:` sequence and
# the unindented lines inside `custom_R_code` -- exactly as it is.
with_n_locations <- function(n, path = "examples/Test_2023_1/metadata.yml") {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  opens <- which(lines == "locations:")

  lat <- -10 - seq_len(n) * 1e-4
  long <- 130 + seq_len(n) * 1e-4
  added <- paste0(
    "  at_", lat, "_deg_lat_and_", long, "_deg_long:\n",
    "    latitude (deg): ", lat, "\n",
    "    longitude (deg): ", long
  )

  out <- withr::local_tempfile(fileext = ".yml", .local_envir = parent.frame())
  writeLines(append(lines, added, after = opens), out)
  out
}


test_that("a large `locations:` block reads the same chunked as whole", {
  f <- with_n_locations(2000)

  expect_identical(util_read_yaml_chunked(f), yaml::read_yaml(f))
  expect_length(util_read_yaml_chunked(f)$locations, 2003)

  # Chunk size is an implementation detail, not a source of answers
  for (chunk in c(1L, 7L, 500L, 5000L)) {
    expect_identical(
      util_read_yaml_chunked(f, chunk = chunk), yaml::read_yaml(f),
      info = paste("chunk =", chunk)
    )
  }
})


test_that("blocks other than `locations:` survive chunking", {
  # The guard that matters: `taxonomic_updates:` is a sequence written with
  # `- find:` at indent 0 and its other keys at indent 2, so treating every
  # indent-2 line as an entry start would split entries and fail to parse
  f <- with_n_locations(2000)
  chunked <- util_read_yaml_chunked(f)
  whole <- yaml::read_yaml(f)

  for (block in setdiff(names(whole), "locations")) {
    expect_identical(chunked[[block]], whole[[block]], info = block)
  }
})


test_that("a small `locations:` block is read by the plain parser", {
  f <- "examples/Test_2023_1/metadata.yml"

  expect_identical(util_read_yaml_chunked(f), yaml::read_yaml(f))
  expect_length(util_read_yaml_chunked(f)$locations, 3)
})


test_that("every example dataset reads identically through the fast path", {
  for (f in list.files("examples", "^metadata\\.yml$",
                       recursive = TRUE, full.names = TRUE)) {
    expect_identical(util_read_yaml_chunked(f), yaml::read_yaml(f), info = f)
  }
})


test_that("a malformed file still reports the real parser's error", {
  f <- with_n_locations(2000)
  lines <- readLines(f)
  # Break a line well past the first chunk boundary
  lines[length(lines) - 2] <- "  \tbad: [unclosed"
  writeLines(lines, f)

  expect_error(util_read_yaml_chunked(f), class = "error")
  expect_error(
    util_read_yaml_chunked(f),
    regexp = as.character(tryCatch(yaml::read_yaml(f), error = conditionMessage)),
    fixed = TRUE
  )
})


test_that("`read_metadata` still preserves custom R code formatting", {
  f <- with_n_locations(2000)
  metadata <- read_metadata(f)

  expect_length(metadata$locations, 2003)
  expect_match(metadata$dataset$custom_R_code, "wood_density_dupe", fixed = TRUE)
  expect_match(metadata$dataset$custom_R_code, "\n", fixed = TRUE)
  expect_identical(
    metadata$dataset$custom_R_code,
    read_metadata("examples/Test_2023_1/metadata.yml")$dataset$custom_R_code
  )
})


test_that("the chunked path is actually taken, and only when it applies", {
  # Everything above would still pass if the fast path fell back every time,
  # so make falling back detectable
  local_mocked_bindings(
    read_yaml = function(...) stop("fell back to the plain parser"),
    .package = "yaml"
  )

  big <- with_n_locations(2000)
  expect_length(util_read_yaml_chunked(big)$locations, 2003)

  # Below the threshold, and for a block that is not a map of locations,
  # falling back is the correct behaviour
  expect_error(util_read_yaml_chunked(big, threshold = 5000L), "fell back")
  expect_error(
    util_read_yaml_chunked("examples/Test_2023_1/metadata.yml"), "fell back"
  )
})
