dewrap_fixture <- function() {
  path_data <- file.path(tempfile("dewrap"), "data")
  dir.create(file.path(path_data, "X"), recursive = TRUE)
  writeLines(c(
    "dataset:",
    "  data_is_long_format: no",
    "  custom_R_code: '",
    "    data %>%",
    "",
    "      mutate(",
    "        x = \"a\"",
    "      )",
    "    methods: a line of code that reads like a field",
    "      but is not one",
    "    '",
    "  sampling_strategy: Plants were collec-",
    "tion at sites",
    "  across NSW: ten sites.",
    "  original_file: x.csv",
    "traits:",
    "- var_in: a",
    "  methods: Leaves were",
    "    measured",
    "- var_in: b",
    "  methods: Single line"
  ), file.path(path_data, "X", "metadata.yml"))
  path_data
}


test_that("`metadata_dewrap_fields` rejoins wrapped fields and nothing else", {
  path_data <- dewrap_fixture()
  path <- file.path(path_data, "X", "metadata.yml")
  before <- readLines(path)

  # A dry run reports but writes nothing
  expect_message(
    utils::capture.output(out <- metadata_dewrap_fields("X", path_data)),
    "Nothing written"
  )
  expect_equal(readLines(path), before)
  expect_equal(out$field, c("sampling_strategy", "methods"))
  expect_equal(out$n_lines_joined, c(3L, 2L))
  expect_equal(out$hyphen_joins, c("collec-tion", NA))

  metadata_dewrap_fields("X", path_data, dry_run = FALSE)
  after <- readLines(path)

  # The hyphen at the break is kept by default, and the value quoted because
  # it contains ": "
  expect_true("  sampling_strategy: 'Plants were collec-tion at sites across NSW: ten sites.'" %in% after)
  expect_true("  methods: Leaves were measured" %in% after)

  metadata <- yaml::read_yaml(path)
  expect_equal(metadata$dataset$sampling_strategy, "Plants were collec-tion at sites across NSW: ten sites.")
  expect_equal(metadata$traits[[2]]$methods, "Single line")
})


test_that("`metadata_dewrap_fields` removes a mid-word hyphen only when asked", {
  path_data <- dewrap_fixture()
  path <- file.path(path_data, "X", "metadata.yml")

  metadata_dewrap_fields("X", path_data, dehyphenate = TRUE, dry_run = FALSE)
  expect_equal(
    yaml::read_yaml(path)$dataset$sampling_strategy,
    "Plants were collection at sites across NSW: ten sites."
  )
})


test_that("`metadata_dewrap_fields` previews the text it would write", {
  path_data <- dewrap_fixture()
  preview <- suppressMessages(utils::capture.output(metadata_dewrap_fields("X", path_data)))
  expect_true(any(grepl("joined at a hyphen: collec-tion", preview, fixed = TRUE)))
  expect_true(any(grepl("'Plants were collec-tion at sites across NSW: ten sites.'", preview, fixed = TRUE)))
})


test_that("`util_dewrap_join` keeps a hyphen, and the space of a suspended one", {
  expect_equal(util_dewrap_join(c("oven-", "dried mass"))$text, "oven-dried mass")
  expect_equal(util_dewrap_join(c("oven-", "dried mass"), dehyphenate = TRUE)$text, "ovendried mass")
  expect_equal(util_dewrap_join(c("rela-", "tively few"), dehyphenate = TRUE)$text, "relatively few")
  expect_equal(util_dewrap_join(c("pre-", "and post-fire"))$text, "pre- and post-fire")
  expect_equal(util_dewrap_join(c("pre-", "and post-fire"), dehyphenate = TRUE)$text, "pre- and post-fire")
  expect_equal(util_dewrap_join(c("from 10 -", "20 m"))$text, "from 10 - 20 m")
  expect_equal(util_dewrap_join(c("14.6 +/-", "0.9%"))$text, "14.6 +/- 0.9%")
  expect_equal(util_dewrap_join(c("range 17.9-", "34.8 degC"))$text, "range 17.9-34.8 degC")
  expect_equal(util_dewrap_join(c("oven-", "dried mass"))$hyphen_joins, "oven-dried")
})


test_that("`util_quote_if_needed` quotes what a plain scalar can't hold", {
  # " #" starts a comment, so unquoted this would be read as "Leaves from plot"
  expect_equal(util_quote_if_needed("Leaves from plot #3 were sampled"), "'Leaves from plot #3 were sampled'")
  for (x in c("- a dash first", "[a bracket", "{a brace", "%a percent", "@an at", "*a star", "> a gt")) {
    expect_equal(yaml::yaml.load(paste0("k: ", util_quote_if_needed(x)))$k, x, info = x)
  }
  expect_equal(util_quote_if_needed("Plain prose, no colon"), "Plain prose, no colon")
})


test_that("`metadata_dewrap_fields` stops at a `traits` field named after a context `var_in`", {
  # A field named after a context's `var_in` can contain a space, so it doesn't
  # look like a `word:` key. It used to be absorbed into the `methods` above it
  # (`Bryant_2024` in `austraits.build`).
  path_data <- file.path(tempfile("dewrap"), "data")
  dir.create(file.path(path_data, "Y"), recursive = TRUE)
  path <- file.path(path_data, "Y", "metadata.yml")
  writeLines(c(
    "contexts:",
    "- context_property: stem type",
    "  category: method_context",
    "  var_in: stem type",
    "traits:",
    "- var_in: ks",
    "  methods: Branch traits were measured on",
    "  sun-exposed branches.",
    "  stem type: branch"
  ), path)

  out <- suppressMessages(utils::capture.output(res <- metadata_dewrap_fields("Y", path_data)))
  expect_equal(res$n_lines_joined, 2L)

  metadata_dewrap_fields("Y", path_data, dry_run = FALSE)
  metadata <- yaml::read_yaml(path)
  expect_equal(metadata$traits[[1]]$methods, "Branch traits were measured on sun-exposed branches.")
  expect_equal(metadata$traits[[1]][["stem type"]], "branch")
})


test_that("`metadata_dewrap_fields` never touches `custom_R_code`", {
  path_data <- dewrap_fixture()
  path <- file.path(path_data, "X", "metadata.yml")
  before <- readLines(path)
  code_block <- before[3:11]

  # Its line breaks are intentional, so it can't be asked for
  expect_error(
    metadata_dewrap_fields("X", path_data, fields = "custom_R_code"),
    "intentional"
  )

  # Including where a line of code looks like one of the fields being dewrapped
  metadata_dewrap_fields("X", path_data, dry_run = FALSE)
  after <- readLines(path)
  expect_equal(after[3:11], code_block)
})
