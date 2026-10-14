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

  metadata_dewrap_fields("X", path_data, dry_run = FALSE)
  after <- readLines(path)

  # Hyphen break undone, and quoted because the joined value contains ": "
  expect_true("  sampling_strategy: 'Plants were collection at sites across NSW: ten sites.'" %in% after)
  expect_true("  methods: Leaves were measured" %in% after)

  metadata <- yaml::read_yaml(path)
  expect_equal(metadata$dataset$sampling_strategy, "Plants were collection at sites across NSW: ten sites.")
  expect_equal(metadata$traits[[2]]$methods, "Single line")
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
