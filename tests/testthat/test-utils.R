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
  for (ch in c("€", "ж", "中")) {
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

  # The exceptions are letters whose other case is ASCII, which is never in
  # the set (U+0130 dotted capital I, U+0131 dotless i, U+017F long s), and the
  # Greek small letters whose capital is deliberately not allowed, as a
  # look-alike for an ASCII letter (alpha, whose capital looks like A, and so
  # on). Mu and omicron are not among them: both of their cases are look-alikes.
  # The micro sign is, since Unicode gives it GREEK CAPITAL LETTER MU as its
  # capital, which is a look-alike for M; a unit prefix has no capital anyway.
  asymmetric <- letters_only[has_case & !(other_case %in% letters_only)]
  expect_setequal(
    asymmetric,
    c("İ", "ı", "ſ", "µ",
      "α", "β", "ε", "ζ", "η", "ι",
      "κ", "ν", "ρ", "τ", "υ", "χ")
  )

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
              "16°17′24″S", "(d13C, ‰)", "∼38 Pa",
              "δ13C", "δ15N",  # GREEK SMALL LETTER DELTA, isotope composition
              "ΔT")) {         # GREEK CAPITAL LETTER DELTA, "difference"
    expect_false(any(check_disallowed_chars(x)), info = x)
  }
})


test_that("Greek letters are allowed by range, except look-alikes (#257)", {
  # Greek letters carry technical meanings in methods, such as Φ and Ψ for water
  # potential, so the alphabet is allowed by range as Latin letters are
  for (x in c("Φ", "φ", "Ψ", "ψ", "Ψleaf", "ΦPSII", "α", "Ω", "ά", "Ώ", "Ά", "Ό")) {
    expect_false(any(check_disallowed_chars(x)), info = x)
  }

  # GREEK SMALL LETTER MU stays disallowed, as a look-alike for the micro sign
  # units are written with; ANO TELEIA in the range is punctuation, not a letter
  expect_true(any(check_disallowed_chars("\u03bc")))
  expect_false(any(check_disallowed_chars("\u00b5")))
  expect_true(any(check_disallowed_chars("\u0387")))

  # Greek variants outside the alphabet stay disallowed
  expect_true(any(check_disallowed_chars("\u03d5")))  # GREEK PHI SYMBOL

  # So do the 14 capitals and the small omicron that look identical to an ASCII
  # letter, since text holding one would not match the same text typed in ASCII.
  # Their accented forms, such as U+0386 and U+038C, do not look like ASCII and
  # stay allowed (above)
  look_alikes <- intToUtf8(
    c(0x0391, 0x0392, 0x0395, 0x0396, 0x0397, 0x0399, 0x039a, 0x039c,
      0x039d, 0x039f, 0x03a1, 0x03a4, 0x03a5, 0x03a7, 0x03bf),
    multiple = TRUE
  )
  for (x in look_alikes) {
    expect_true(all(check_disallowed_chars(x)), info = sprintf("U+%04X", utf8ToInt(x)))
  }

  # The schema lists exactly these, with mu, as Greek letters never allowed
  never_allowed <- get_schema()$allowed_characters$never_allowed$values
  greek <- names(never_allowed)[strtoi(names(never_allowed), 16L) %in% 0x0370:0x03ff]
  expect_setequal(strtoi(greek, 16L), c(utf8ToInt(paste(look_alikes, collapse = "")), 0x03bc))
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


test_that("`util_flatten_sources` reads the list shape, one row per reference", {
  source <- list(
    dataset = list(key = "Smith_2020", bibtype = "Article"),
    source = list(
      list(key = "Jones_1999", bibtype = "Thesis"),
      list(key = "Brown_2005", bibtype = "Book")
    ),
    secondary = list(list(key = "Smith_2021", bibtype = "Article"))
  )
  out <- util_flatten_sources(source)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("slot", "type", "key", "legacy", "entry"))
  expect_equal(out$type, c("dataset", "source", "source", "secondary"))
  expect_equal(out$key, c("Smith_2020", "Jones_1999", "Brown_2005", "Smith_2021"))
  expect_false(any(out$legacy))
  expect_equal(out$entry[[3]]$bibtype, "Book")

  # A dataset reference alone, and an empty block
  expect_equal(util_flatten_sources(source["dataset"])$type, "dataset")
  expect_equal(nrow(util_flatten_sources(NULL)), 0)
  expect_equal(nrow(util_flatten_sources(list())), 0)
  # `secondary: .na` is no reference at all
  expect_equal(nrow(util_flatten_sources(list(dataset = source$dataset, secondary = NA))), 1)
  # A reference without a key is kept, with an NA key, for `dataset_test()` to report
  expect_equal(util_flatten_sources(list(dataset = list(bibtype = "Article")))$key, NA_character_)
})


test_that("`util_flatten_sources` refuses the old shapes, naming the entry", {
  dataset <- list(key = "Smith_2020", bibtype = "Article")
  old_shapes <- list(
    # the old names
    list(primary = dataset),
    list(dataset = dataset, original_source = list(list(key = "Jones_1999"))),
    # `secondary` as a single mapping rather than a list
    list(dataset = dataset, secondary = list(key = "Smith_2021", bibtype = "Article")),
    # numbered entries
    list(dataset = dataset, secondary_01 = list(key = "Smith_2021"), secondary_2 = list(key = "Smith_2022")),
    list(dataset = dataset, original_01 = list(key = "Jones_1999")),
    list(dataset = dataset, original_source_01 = list(key = "Jones_1999"))
  )
  for (source in old_shapes) {
    expect_error(util_flatten_sources(source), "a shape traits.build no longer reads")
  }
  expect_error(util_flatten_sources(old_shapes[[4]]), "`secondary_01`, `secondary_2`")
  expect_error(util_flatten_sources(old_shapes[[1]]), "metadata_convert_sources()", fixed = TRUE)

  # Leniently, the same entries are flattened and flagged, with their current type
  out <- util_flatten_sources(old_shapes[[1]], strict = FALSE)
  expect_equal(out$type, "dataset")
  expect_equal(out$legacy, TRUE)
  out <- util_flatten_sources(old_shapes[[2]], strict = FALSE)
  expect_equal(out$type, c("dataset", "source"))
  expect_equal(out$legacy, c(FALSE, TRUE))
  out <- util_flatten_sources(old_shapes[[5]], strict = FALSE)
  expect_equal(out$type, c("dataset", "source"))
  expect_equal(out$legacy, c(FALSE, TRUE))
  expect_equal(out$slot, c("dataset", "original_01"))

  out <- util_flatten_sources(old_shapes[[4]], strict = FALSE)
  expect_equal(out$type, c("dataset", "secondary", "secondary"))
  expect_equal(out$key, c("Smith_2020", "Smith_2021", "Smith_2022"))

  # A bare value where a reference belongs is refused too, rather than dropped
  # unseen: `secondary: Smith_2021`, a list of bare keys, or one such item
  bare <- list(
    list(dataset = dataset, secondary = "Smith_2021"),
    list(dataset = dataset, secondary = c("Smith_2021", "Jones_2000")),
    list(dataset = dataset,
         source = list(list(key = "Jones_1999", bibtype = "Thesis"), "Brown_2005")),
    list(dataset = "Smith_2020")
  )
  for (source in bare) {
    expect_error(util_flatten_sources(source), "a shape traits.build no longer reads")
  }
  # Leniently, each is flagged, with no key to report
  out <- util_flatten_sources(bare[[2]], strict = FALSE)
  expect_equal(out$type, c("dataset", "secondary", "secondary"))
  expect_equal(out$legacy, c(FALSE, TRUE, TRUE))
  expect_equal(out$key, c("Smith_2020", NA, NA))
  out <- util_flatten_sources(bare[[3]], strict = FALSE)
  expect_equal(out$key, c("Smith_2020", "Jones_1999", NA))
  expect_equal(out$legacy, c(FALSE, FALSE, TRUE))

  # An unknown type is a different error, and is not `legacy`
  expect_error(util_flatten_sources(list(dataset = dataset, tertiary = list(key = "X_2000"))), "not a type of reference")
  out <- util_flatten_sources(list(dataset = dataset, tertiary = list(key = "X_2000")), strict = FALSE)
  expect_equal(out$type[2], "tertiary")
  expect_false(out$legacy[2])
})


test_that("a reference list can be kept in a .yml or .bib file beside `metadata.yml`", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "metadata.yml")
  metadata <- read_metadata(file.path("examples", "Test_2023_2", "metadata.yml"))
  metadata$source$dataset$key <- "Smith_2020"
  metadata$source$source <- "sources.yml"
  metadata$source$secondary <- "secondary.bib"
  write_metadata(metadata, path)
  writeLines(c(
    "- key: Jones_1999",
    "  bibtype: Thesis",
    "  year: '1999'",
    "  author: Bob Jones",
    "  title: A thesis",
    "- key: Brown_2005",
    "  bibtype: Book",
    "  year: '2005'",
    "  author: Cat Brown",
    "  title: A book"
  ), file.path(dir, "sources.yml"))
  writeLines(c(
    "@Article{Smith_2021,",
    "  author = {Ann Smith},",
    "  title = {A re-use},",
    "  journal = {A Journal},",
    "  year = {2021},",
    "}"
  ), file.path(dir, "secondary.bib"))

  metadata <- read_metadata(path)
  out <- util_flatten_sources(metadata$source)
  expect_equal(out$type, c("dataset", "source", "source", "secondary"))
  expect_equal(out$key, c("Smith_2020", "Jones_1999", "Brown_2005", "Smith_2021"))
  expect_equal(out$entry[[4]]$journal, "A Journal")
  expect_equal(attr(metadata$source$source, "sources_file"), "sources.yml")

  # Written back to their files, and named from `metadata.yml`
  metadata$source$source[[2]]$title <- "A revised book"
  write_metadata(metadata, path)
  expect_true(any(readLines(path) == "  source: sources.yml"))
  expect_true(any(readLines(path) == "  secondary: secondary.bib"))
  again <- read_metadata(path)
  expect_equal(again$source$source[[2]]$title, "A revised book")
  expect_equal(util_flatten_sources(again$source)$key, out$key)
  expect_equal(again$source$secondary[[1]]$title, "A re-use")

  # A missing file is an error naming it
  writeLines(sub("sources.yml", "missing.yml", readLines(path)), path)
  expect_error(read_metadata(path), "missing.yml")
})


test_that("`metadata_convert_sources` converts every old shape, keeping each line", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "Smith_2020"))
  path <- file.path(dir, "Smith_2020", "metadata.yml")
  old <- c(
    "source:",
    "  primary:",
    "    key: Smith_2020",
    "    bibtype: Article",
    "    title: A study",
    "  secondary:",
    "    key: Smith_2021",
    "    bibtype: Article",
    "  original_01:",
    "    key: Jones_1999",
    "    bibtype: Thesis",
    "  secondary_02:",
    "    key: Smith_2022",
    "    bibtype: Article",
    "  original_02:",
    "    key: Brown_2005",
    "    bibtype: Book",
    "contributors:",
    "  data_collectors: .na",
    "dataset:",
    "  source_id: ref",
    "  custom_R_code: .na",
    "  collection_date: 2020"
  )
  writeLines(old, path)
  writeLines(c("ref,x", "Jones_1999,1", "Brown_2005,2", "Other_2000,3"), file.path(dir, "Smith_2020", "data.csv"))

  expect_message(out <- metadata_convert_sources(path_data = dir), "1 of 1 datasets converted")
  expect_true(out$converted)
  # A value naming no reference is reported, not changed
  expect_match(out$issues, "1 `source_id` value(s) name no `source` reference: `Other_2000`", fixed = TRUE)

  expect_equal(readLines(path), c(
    "source:",
    "  dataset:",
    "    key: Smith_2020",
    "    bibtype: Article",
    "    title: A study",
    "  source:",
    "  - key: Jones_1999",
    "    bibtype: Thesis",
    "  - key: Brown_2005",
    "    bibtype: Book",
    "  secondary:",
    "  - key: Smith_2021",
    "    bibtype: Article",
    "  - key: Smith_2022",
    "    bibtype: Article",
    old[18:23]
  ))

  # Converting again changes nothing
  expect_message(out <- metadata_convert_sources(path_data = dir), "0 of 1 datasets converted")
  expect_false(out$converted)

  # A dataset key that is not the dataset_id is reported
  writeLines(sub("key: Smith_2020", "key: Smith_2019", readLines(path)), path)
  out <- suppressMessages(metadata_convert_sources(path_data = dir))
  expect_match(out$issues, "dataset reference key `Smith_2019` is not the dataset_id", fixed = TRUE)
})


test_that("`util_collection_date_is_valid` rejects everything else", {
  expect_false(util_collection_date_is_valid("2020-02-30"))  # no such day
  expect_false(util_collection_date_is_valid("2021-02-29"))  # not a leap year
  expect_false(util_collection_date_is_valid("2020-13-01"))  # no such month
  expect_false(util_collection_date_is_valid("2020-5"))      # month not zero-padded
  expect_false(util_collection_date_is_valid("20-05-15"))    # year not 4 digits
  expect_false(util_collection_date_is_valid("not a date"))
  expect_false(util_collection_date_is_valid("unknown/2022"))  # the build stores it as `2022`
  expect_false(util_collection_date_is_valid("2020-05-15/2020-06-01/2020-07-01"))  # 3-part range
  expect_false(util_collection_date_is_valid(""))
})


test_that("`util_parse_collection_date` resolves unambiguous formats safely", {
  # already valid: passed through unchanged
  expect_equal(util_parse_collection_date("2020-05-15"), "2020-05-15")
  expect_equal(util_parse_collection_date(NA_character_), NA_character_)

  # a named month removes the day/month ambiguity, however it's written
  expect_equal(util_parse_collection_date("2-Sep-08"), "2008-09-02")
  expect_equal(util_parse_collection_date("September 2, 2008"), "2008-09-02")
  expect_equal(util_parse_collection_date("2008-Sep-02"), "2008-09-02")
  expect_equal(util_parse_collection_date("29 Feb 2008"), "2008-02-29")  # leap year

  # a named month with no day resolves to yyyy-mm, not a fabricated day
  expect_equal(util_parse_collection_date("Sep 2008"), "2008-09")
  expect_equal(util_parse_collection_date("2008 Sep"), "2008-09")

  # 2-digit years expand using the POSIX convention (00-68 -> 20xx, 69-99 -> 19xx)
  expect_equal(util_parse_collection_date("1-May-05"), "2005-05-01")
  expect_equal(util_parse_collection_date("1-May-99"), "1999-05-01")

  # Excel serial dates, distinguished from a bare year by digit count
  expect_equal(util_parse_collection_date("39692"), "2008-09-01")

  # a `start/end` range with named-month components on each side
  expect_equal(
    util_parse_collection_date("2 Sep 2008/3 Sep 2008"),
    "2008-09-02/2008-09-03"
  )

  # vectorised, and via a `mutate()` pipeline the way `custom_R_code` uses it
  data <- tibble::tibble(Date = c("2-Sep-08", "3-Sep-08"))
  expect_equal(
    dplyr::mutate(data, Date = util_parse_collection_date(Date))$Date,
    c("2008-09-02", "2008-09-03")
  )
})


test_that("`util_parse_collection_date` leaves genuinely ambiguous or invalid values untouched", {
  # a purely numeric date has no way to tell day-first from month-first
  expect_equal(util_parse_collection_date("01/02/2008"), "01/02/2008")
  expect_equal(util_parse_collection_date("1.2.2008"), "1.2.2008")

  # an impossible day, even alongside a named month, is not guessed at
  expect_equal(util_parse_collection_date("35 Sep 2008"), "35 Sep 2008")
  expect_equal(util_parse_collection_date("29 Feb 2021"), "29 Feb 2021")  # not a leap year

  # more numbers than a day/month/year shape can use
  expect_equal(
    util_parse_collection_date("2 Sep 2008 3 Oct 2009"),
    "2 Sep 2008 3 Oct 2009"
  )

  expect_equal(util_parse_collection_date("not a date"), "not a date")
  expect_equal(util_parse_collection_date("unknown/2022"), "unknown/2022")

  # everything it leaves untouched is exactly what `dataset_test`'s
  # `collection_date`-parses check would still flag -- fixing what's safe to
  # fix should never silently produce something that still fails
  untouched <- c("01/02/2008", "35 Sep 2008", "not a date", "unknown/2022")
  expect_false(any(util_collection_date_is_valid(util_parse_collection_date(untouched))))
})


test_that("`util_parse_collection_date` drops a trailing time-of-day", {
  # ISO 8601 datetime (Stephens_2024_2's `birthtime` column)
  expect_equal(util_parse_collection_date("2021-07-16T22:06:14Z"), "2021-07-16")
  expect_equal(util_parse_collection_date("2021-08-04T23:36:12Z"), "2021-08-04")

  # a plain numeric date with a time suffix (a Google Forms timestamp
  # column, as in Stephens_2020's `Timestamp`) -- resolvable here because
  # `9/14` can only be month-first
  expect_equal(
    util_parse_collection_date(c("8/11/2020 10:09", "9/14/2020 12:06")),
    c("2020-08-11", "2020-09-14")
  )
})


test_that("`util_parse_collection_date` infers a plain numeric date's order from the rest of the column", {
  # `9/14` and `9/15` can only be month-first (no 14th or 15th month), so
  # the otherwise-ambiguous `8/11` and `8/6` are read the same way -- the
  # real shape of Stephens_2020's `Timestamp` column (stripped of time here;
  # the previous test covers the two combined)
  expect_equal(
    util_parse_collection_date(c("8/11/2020", "8/6/2020", "9/14/2020", "9/15/2020")),
    c("2020-08-11", "2020-08-06", "2020-09-14", "2020-09-15")
  )

  # one disambiguating value is enough to resolve the rest of the column,
  # even if it's the second value in the vector rather than the first
  expect_equal(
    util_parse_collection_date(c("01/02/2008", "25/03/2008", "04/05/2008")),
    c("2008-02-01", "2008-03-25", "2008-05-04")  # day-first: 25 can't be a month
  )

  # nothing in the column resolves the order -- every value must stay
  # untouched, not just guessed independently
  expect_equal(
    util_parse_collection_date(c("01/02/2008", "03/04/2009", "05/06/2010")),
    c("01/02/2008", "03/04/2009", "05/06/2010")
  )
})


test_that("`util_parse_collection_date` resolves a value from its own two numbers before deferring to the column", {
  # `austraits.build`'s Coates_2024 genuinely mixes conventions within one
  # column: automated camera-trap timestamps in month-first order alongside
  # hand-entered dates in day-first order. Column-wide agreement (as tested
  # above) can't apply here -- the column disagrees with itself -- but each
  # of these is still unambiguous *on its own*, since one of its two leading
  # numbers exceeds 12.
  expect_equal(
    util_parse_collection_date(c("10/23/2021 1:00:00 AM", "28/11/2021")),
    c("2021-10-23", "2021-11-28")  # month-first and day-first respectively
  )

  # a value that's ambiguous even on this test (both leading numbers <=12)
  # still needs column-wide help, and gets none here (contradictory column)
  expect_equal(
    util_parse_collection_date(c("10/23/2021", "28/11/2021", "1/12/2021")),
    c("2021-10-23", "2021-11-28", "1/12/2021")
  )
})


test_that("`util_parse_collection_date` handles mm-yyyy and dot-separated yy.mm.dd", {
  # `mm-yyyy`, the reverse of the schema's `yyyy-mm` -- unambiguous since a
  # 4-digit year can't be mistaken for anything else. From
  # `austraits.build`'s Sevenello_2026_2, as a `start/end` range.
  expect_equal(util_parse_collection_date("07-2015"), "2015-07")
  expect_equal(
    util_parse_collection_date("07-2015/09-2016"),
    "2015-07/2016-09"
  )

  # dot-separated `yy.mm.dd`, confirmed by checking the middle component is
  # a plausible month and the last a plausible day (from
  # `austraits.build`'s Doyle_2023)
  expect_equal(util_parse_collection_date("19.08.20"), "2019-08-20")
  expect_equal(util_parse_collection_date("19.11.08"), "2019-11-08")

  # a dot-separated triple that *doesn't* fit yy.mm.dd (middle component
  # isn't a plausible month) is left alone rather than guessed at
  expect_equal(util_parse_collection_date("19.20.08"), "19.20.08")
})


test_that("`util_parse_collection_date` takes the curator's `order` for numeric dates the column can't settle", {
  # a lone ambiguous value (Bryant_2021_3's whole `Date` column) -- nothing
  # to infer from, so it stays untouched unless the order is given
  expect_equal(util_parse_collection_date("4/07/2022"), "4/07/2022")
  expect_equal(util_parse_collection_date("4/07/2022", order = "dmy"), "2022-07-04")
  expect_equal(util_parse_collection_date("4/07/2022", order = "mdy"), "2022-04-07")
  expect_equal(util_parse_collection_date("04-04-2019/9-12-2019", order = "dmy"), "2019-04-04/2019-12-09")

  # `order` replaces the column-wide inference...
  expect_equal(
    util_parse_collection_date(c("01/02/2008", "03/04/2009"), order = "dmy"),
    c("2008-02-01", "2009-04-03")
  )

  # ...but a value that settles its own order is still read that way
  expect_equal(util_parse_collection_date("10/23/2021", order = "dmy"), "2021-10-23")

  # two-digit dotted dates: `yy.mm.dd` by default (Doyle_2023), day-first only
  # when the curator says so (Bloomfield_2018's `d.m.yy`)
  expect_equal(util_parse_collection_date("10.3.12"), "2010-03-12")
  expect_equal(
    util_parse_collection_date(c("18.3.12", "10.3.12", "1.4.14"), order = "dmy"),
    c("2012-03-18", "2012-03-10", "2014-04-01")
  )

  # anything other than "dmy"/"mdy" is an error, not silently ignored
  expect_error(util_parse_collection_date("4/07/2022", order = "ymd"))
})


test_that("`util_parse_collection_date` handles dotted d.m.yyyy, yyyymmdd, year spans and lists", {
  # dot-separated `d.m.yyyy`, resolved from the column like `/` and `-`
  # (Lequerica_Tamara_2025)
  expect_equal(
    util_parse_collection_date(c("1.05.2020", "21.11.2019")),
    c("2020-05-01", "2019-11-21")
  )

  # compact `yyyymmdd`, which the Excel serial rule would otherwise read as
  # a date tens of thousands of years from now (Arnold_2021)
  expect_equal(util_parse_collection_date("20190206"), "2019-02-06")
  expect_equal(util_parse_collection_date(20190206), "2019-02-06")
  expect_equal(util_parse_collection_date("20190231"), "20190231")  # no 31 Feb

  # a span of years with a dash rather than `/` (Tsujii_2023, Moore_2019),
  # but not one that runs backwards
  expect_equal(util_parse_collection_date("2019-2020"), "2019/2020")
  expect_equal(util_parse_collection_date("2020-2019"), "2020-2019")

  # a list of dates collapses to the range it spans, or to the one date if
  # they're all the same (Cochrane_2001's `Time of collection`)
  expect_equal(util_parse_collection_date("Jan-98, Feb-98, Mar-98"), "1998-01/1998-03")
  expect_equal(util_parse_collection_date("Oct-96, Oct-96,"), "1996-10")
  expect_equal(util_parse_collection_date("2001-03; 2000-11"), "2000-11/2001-03")

  # a comma inside a single date is not a list -- `September 2` alone would
  # be read as 2002
  expect_equal(util_parse_collection_date("September 2, 2008"), "2008-09-02")

  # a list with any item that doesn't parse is left alone
  expect_equal(util_parse_collection_date("Jan-98, 35 Sep 1998"), "Jan-98, 35 Sep 1998")
})


test_that("util_list_empty_fields names fields with no value, but not `.na` (#307)", {
  entries <- yaml::yaml.load("
- var_in: LMA
  trait_name: leaf_mass_per_area
  unit_in: .na
  replicates:
- var_in: leaf N
  trait_name: ~
- context_property: treatment
  var_in: trt
  values:
  - find: a
    value: control
    description:
")
  expect_equal(
    util_list_empty_fields(entries),
    c("entry 1 (var_in: LMA) `replicates`",
      "entry 2 (var_in: leaf N) `trait_name`",
      "entry 3 (var_in: trt), values entry 1 (find: a) `description`")
  )

  # An empty collection drops its entry just as an empty field does
  entries <- yaml::yaml.load("
- var_in: LMA
  replicates: []
- last_name: Smith
  ORCID: {}
")
  expect_equal(
    util_list_empty_fields(entries),
    c("entry 1 (var_in: LMA) `replicates`", "entry 2 (last_name: Smith) `ORCID`")
  )

  # A section left as `.na`, or one with nothing empty, has nothing to report
  expect_equal(util_list_empty_fields(NA), character(0))
  expect_equal(util_list_empty_fields(list(list(var_in = "LMA"))), character(0))
})
