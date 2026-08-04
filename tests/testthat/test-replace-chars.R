test_that("every replacement is disallowed on the left and allowed on the right", {
  # The two invariants the map lives or dies by. If a key were already allowed
  # the entry would be dead weight; if a value were disallowed the fixer would
  # produce files that still fail `dataset_test()`.
  map <- util_disallowed_char_replacements()

  expect_gt(length(map), 0)
  expect_false(any(duplicated(names(map))))

  for (i in seq_along(map)) {
    key <- names(map)[i]
    value <- map[[i]]
    label <- paste(sprintf("U+%04X", utf8ToInt(key)), collapse = "+")

    expect_true(all(check_disallowed_chars(key)), info = label)

    if (nchar(value) > 0) {
      expect_false(any(check_disallowed_chars(value)), info = label)
    }
  }
})


test_that("a multi-character key is matched before the characters it is built from", {
  # `¬†` is a UTF-8 no-break space (bytes C2 A0) read back as Mac OS Roman. Both
  # halves are themselves disallowed, so a naive per-character pass would
  # rewrite them separately and never recover the space.
  mojibake <- "Coenocharopa¬†yessabahensis"

  expect_equal(util_replace_disallowed_chars(mojibake), "Coenocharopa yessabahensis")
  expect_false(any(check_disallowed_chars(util_replace_disallowed_chars(mojibake))))
})


test_that("the wrong-character cases measured in real datasets are corrected", {
  # Each was found in a downstream `metadata.yml`, and in each the character
  # used is simply not the one that was meant.
  cases <- list(
    c("33.33ºS, 150.44ºE", "33.33°S, 150.44°E"),  # ordinal for degree
    c("inserted at 90◦ to each other", "inserted at 90° to each other"),
    c("Coenocharopa yessabahensis ", "Coenocharopa yessabahensis ")
  )

  for (case in cases) {
    expect_equal(util_replace_disallowed_chars(case[1]), case[2])
    expect_false(any(check_disallowed_chars(case[2])))
  }
})


test_that("GREEK SMALL LETTER MU is normalised to MICRO SIGN", {
  # The two are visually identical, so a unit written with the Greek letter
  # cannot be told from one written with the micro sign by eye. Only the micro
  # sign is allowed, and the whole point of allowing it is that units stay
  # consistent -- so the Greek letter has to converge on it.
  expect_equal(
    util_replace_disallowed_chars("leaf thickness in \u03bcm"),
    "leaf thickness in \u00b5m"
  )
  expect_true(all(check_disallowed_chars("\u03bc")))
  expect_false(any(check_disallowed_chars("\u00b5")))

  # and the normalised form is what the validator accepts
  expect_false(
    any(check_disallowed_chars(util_replace_disallowed_chars("\u03bcg g-1")))
  )
})


test_that("correct typography is reported, never flattened to ASCII", {
  # U+2033 is the right character for arcseconds and U+2030 has a technical
  # meaning, so rewriting either to ASCII would lose information rather than fix
  # a mistake. Retaining these is a curator decision, via `exceptions`.
  for (x in c("16°17'24.8″S", "(∼38 Pa in this study)", "(d13C, ‰)",
              "range 10−20", "a ⁄ b", "and so on…")) {
    expect_equal(util_replace_disallowed_chars(x), x)
  }
})


test_that("replacement never touches a letter", {
  # The map must leave names alone whether or not the check currently allows
  # them -- `ó` is in the exception list, `ñ` and `š` are not, and silently
  # rewriting any of them would corrupt an author's name. An unexpected letter
  # is a decision for the curator, so it gets reported instead.
  # `17°C` guards the other direction: the degree sign is the replacement for
  # two of the map's keys and must not itself be rewritten.
  for (x in c("normal ASCII text 123 (a-b) [c] 45%",
              "Pieter A. Arnold and Verónica F. Briceño",
              "Klimešová et al., 2019",
              "17°C", "")) {
    expect_equal(util_replace_disallowed_chars(x), x)
  }
})


test_that("replacement is idempotent", {
  once <- util_replace_disallowed_chars("33.33ºS ∼38 Pa 24.8″")
  expect_equal(util_replace_disallowed_chars(once), once)
})


test_that("util_locate_disallowed_chars reports one row per character", {
  found <- util_locate_disallowed_chars(c("clean ascii", "33.33ºS and 90◦", "also clean"))

  expect_equal(nrow(found), 2)
  expect_equal(found$line, c(2L, 2L))
  expect_equal(found$char, c("º", "◦"))
  expect_equal(found$code, c("U+00BA", "U+25E6"))

  # Zero rows, with the columns still present, when there is nothing to report
  empty <- util_locate_disallowed_chars(c("all", "ascii", "here"))
  expect_equal(nrow(empty), 0)
  expect_named(empty, c("line", "char", "code"))
})


test_that("util_locate_disallowed_chars applies the data/metadata split", {
  # `dataset_test()` permits accented letters in metadata but nothing outside
  # ASCII in data, and this must mirror that rather than inventing a third rule.
  accented <- "Verónica"

  expect_equal(nrow(util_locate_disallowed_chars(accented, ascii_only = FALSE)), 0)
  expect_equal(nrow(util_locate_disallowed_chars(accented, ascii_only = TRUE)), 1)
})


test_that("a lone invalid byte is described as a byte, not a code point", {
  latin1 <- rawToChar(as.raw(c(0x32, 0x35, 0xb1)))
  found <- util_locate_disallowed_chars(latin1)

  expect_equal(nrow(found), 1)
  expect_equal(found$code, "0xB1")
})


# ---- dataset_replace_disallowed_chars() -------------------------------------

# Writes `metadata.yml` and, optionally, `data.csv` into a throwaway dataset
# folder and returns the containing path, for use as `path_data`.
setup_dataset <- function(metadata, data = NULL) {
  path_data <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(path_data, "Test_2020"))

  write_lines_utf8 <- function(lines, path) {
    con <- file(path, "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(lines, con)
  }

  write_lines_utf8(metadata, file.path(path_data, "Test_2020", "metadata.yml"))
  if (!is.null(data)) {
    write_lines_utf8(data, file.path(path_data, "Test_2020", "data.csv"))
  }

  path_data
}


test_that("dry_run reports without writing", {
  metadata <- c("description: measured at 17ºC", "notes: clean line")
  path_data <- setup_dataset(metadata)
  before <- readLines(file.path(path_data, "Test_2020", "metadata.yml"), encoding = "UTF-8")

  report <- suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data)
  )

  expect_equal(nrow(report), 1)
  expect_equal(report$status, "replaced")
  expect_equal(report$code, "U+00BA")
  expect_equal(report$replacement, "°")
  expect_equal(report$line, 1L)

  # The file must be untouched
  expect_equal(readLines(file.path(path_data, "Test_2020", "metadata.yml"), encoding = "UTF-8"), before)
})


test_that("dry_run = FALSE rewrites only the offending characters", {
  metadata <- c("description: measured at 17ºC", "notes: clean line",
                "taxon: Coenocharopa¬†yessabahensis")
  path_data <- setup_dataset(metadata)

  suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data, dry_run = FALSE)
  )

  after <- readLines(file.path(path_data, "Test_2020", "metadata.yml"), encoding = "UTF-8")

  expect_equal(after, c("description: measured at 17°C", "notes: clean line",
                        "taxon: Coenocharopa yessabahensis"))
  expect_false(any(unlist(lapply(after, check_disallowed_chars))))

  # ...and a second pass finds nothing left to do
  again <- suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data)
  )
  expect_equal(nrow(again), 0)
})


test_that("dry_run = FALSE leaves correct typography in place", {
  # The line is rewritten for the ordinal indicator but must keep its arcseconds
  # mark. `″` U+2033 is an allowed character, so it is not reported at all --
  # the fixer's job is to leave correct typography alone, not to ask about it.
  metadata <- "coords: 16°17\'24.8″S at 17ºC"
  path_data <- setup_dataset(metadata)

  report <- suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data, dry_run = FALSE)
  )

  expect_equal(
    readLines(file.path(path_data, "Test_2020", "metadata.yml"), encoding = "UTF-8"),
    "coords: 16°17\'24.8″S at 17°C"
  )
  expect_equal(report$status[report$char == "º"], "replaced")
  expect_false("″" %in% report$char)
})


test_that("a character with no known replacement is reported, not guessed at", {
  # `†` and `∞` are real symbols rather than typos, so neither is in the map and
  # neither may be altered. Both are disallowed under the character list as it
  # stood before #253 and as it stands after, so this test does not move when
  # the allowed list is widened -- unlike the `‰` and `Ø` it used to use, which
  # #253 made allowed outright.
  metadata <- c("description: growth was unbounded (∞)", "notes: see the note marked †")
  path_data <- setup_dataset(metadata)

  report <- suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data, dry_run = FALSE)
  )

  expect_equal(nrow(report), 2)
  expect_true(all(report$status == "no replacement known"))
  expect_true(all(is.na(report$replacement)))

  # Nothing may have been written
  expect_equal(readLines(file.path(path_data, "Test_2020", "metadata.yml"), encoding = "UTF-8"), metadata)
})


test_that("a resolvable and an unresolvable character on one line are separated", {
  metadata <- "description: 17ºC and unbounded growth (∞)"
  path_data <- setup_dataset(metadata)

  report <- suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data, dry_run = FALSE)
  )

  expect_equal(report$status[report$char == "º"], "replaced")
  expect_equal(report$status[report$char == "∞"], "no replacement known")

  # The known one is fixed and the unknown one survives untouched
  expect_equal(
    readLines(file.path(path_data, "Test_2020", "metadata.yml"), encoding = "UTF-8"),
    "description: 17°C and unbounded growth (∞)"
  )
})


test_that("data.csv is held to ASCII only, and metadata.yml is not", {
  # `ó` is in the exception list, so it is fine in metadata but not in data.
  # (`ñ`, as in Briceño, is in neither -- see the note below.)
  path_data <- setup_dataset(
    metadata = "author: Verónica Arnold",
    data = c("species,temp", "Acacia,17ºC")
  )

  report <- suppressMessages(
    dataset_replace_disallowed_chars(
      "Test_2020", path_data = path_data, files = c("metadata.yml", "data.csv")
    )
  )

  # The accented metadata name is allowed, so only data.csv is reported
  expect_equal(unique(report$file), "data.csv")
  expect_equal(report$code, "U+00BA")
})


test_that("data.csv is left out unless asked for", {
  # Its scope is a much wider sweep than metadata.yml's, so it must not be
  # picked up by the default one-liner. See #251.
  path_data <- setup_dataset(
    metadata = "description: clean ASCII",
    data = c("species,temp", "Acacia,17ºC")
  )

  expect_equal(
    nrow(suppressMessages(
      dataset_replace_disallowed_chars("Test_2020", path_data = path_data)
    )),
    0
  )

  expect_equal(
    nrow(suppressMessages(
      dataset_replace_disallowed_chars("Test_2020", path_data = path_data, files = "data.csv")
    )),
    1
  )
})


test_that("a file that is not valid UTF-8 is reported and skipped", {
  path_data <- withr::local_tempdir()
  dir.create(file.path(path_data, "Test_2020"))
  path <- file.path(path_data, "Test_2020", "metadata.yml")

  # A stray Latin-1 byte, which is the mojibake this check exists to catch.
  # Rewriting it would need the encoding it was written in.
  writeBin(as.raw(c(0x32, 0x35, 0xb1, 0x31, 0x0a)), path)
  before <- readBin(path, "raw", 100)

  report <- suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data, dry_run = FALSE)
  )

  expect_true(all(report$status == "file not valid UTF-8"))
  expect_true(all(is.na(report$replacement)))
  expect_equal(readBin(path, "raw", 100), before)
})


test_that("a clean dataset reports nothing and returns the empty report shape", {
  path_data <- setup_dataset("description: entirely ASCII metadata")

  report <- suppressMessages(
    dataset_replace_disallowed_chars("Test_2020", path_data = path_data)
  )

  expect_equal(nrow(report), 0)
  expect_named(report, names(util_char_report_cols))
})


test_that("a missing dataset folder is skipped rather than an error", {
  path_data <- setup_dataset("description: clean")

  report <- suppressMessages(
    dataset_replace_disallowed_chars(c("Test_2020", "Does_Not_Exist"), path_data = path_data)
  )

  expect_equal(nrow(report), 0)
})


test_that("several datasets are reported together", {
  path_data <- withr::local_tempdir()
  for (id in c("Test_2020", "Test_2021")) {
    dir.create(file.path(path_data, id))
    con <- file(file.path(path_data, id, "metadata.yml"), "w", encoding = "UTF-8")
    writeLines(sprintf("description: %s at 17ºC", id), con)
    close(con)
  }

  report <- suppressMessages(
    dataset_replace_disallowed_chars(c("Test_2020", "Test_2021"), path_data = path_data)
  )

  expect_equal(nrow(report), 2)
  expect_equal(report$dataset_id, c("Test_2020", "Test_2021"))
})
