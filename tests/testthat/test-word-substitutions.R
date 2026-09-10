## Tests for `match: word` entries in a dataset's `metadata$substitutions`
## (issue #271).
##
## `metadata$substitutions` normally requires `find` to equal the whole cell
## (`match: value`, the default when `match` is omitted). `match: word` instead
## replaces `find` wherever it occurs as a whole word/phrase within a multi-value
## cell, leaving the rest of the cell untouched -- via `process_word_replace()`,
## the engine shared with the database-wide `traits.yml` synonym feature
## (see test-synonyms.R).
##
## The scenarios below are not hypothetical: they mirror `Brundrett_2024`'s real
## `flower_colour` (123 substitution rows collapsing to a handful of colour-word
## rules) and `dispersal_appendage` (raw values like `wings no wing` and
## `wings winged or not`, whose negation can't be decomposed word-by-word and
## must stay `match: value`) substitution blocks in austraits.build.

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")

trait_data <- function(trait_name, value) {
  tibble::tibble(trait_name = trait_name, value = value)
}

word_replace <- function(trait_name, value, rules) {
  process_word_replace(trait_data(trait_name, value), rules)[["value"]]
}

# `rules` is a tibble of trait_name/find/replace, mirroring the table
# process_parse_data() builds from `metadata$substitutions` rows with `match: word`.
colour_rules <- tibble::tibble(
  trait_name = "flower_colour",
  find       = c("white", "cream", "yellow", "purple", "blue"),
  replace    = c("white_cream", "white_cream", "yellow_orange", "blue_purple", "blue_purple")
)


test_that("a word rule replaces just that word, leaving the rest of the cell alone", {
  expect_equal(word_replace("flower_colour", "yellow white", colour_rules), "yellow_orange white_cream")
})


test_that("a value with nothing to replace is left alone", {
  expect_equal(word_replace("flower_colour", "green", colour_rules), "green")
})


test_that("a find occurring inside a longer word is not replaced", {
  # Whole-word matching: without it, `\\bwing\\b` would corrupt "winged"
  rules <- tibble::tibble(trait_name = "dispersal_appendage", find = "wing", replace = "wings")
  expect_equal(word_replace("dispersal_appendage", "winged", rules), "winged")
})


test_that("a negation stays legible after word substitution", {
  # The hypothesis this feature started from: does `not winged` -> `not wings`
  # stay sensible under word-bounded substitution? Yes -- the boundary excludes
  # "not" from the match entirely.
  rules <- tibble::tibble(trait_name = "dispersal_appendage", find = "winged", replace = "wings")
  expect_equal(word_replace("dispersal_appendage", "not winged", rules), "not wings")
})


test_that("rules are scoped to the trait that declares them", {
  expect_equal(word_replace("dispersal_appendage", "white", colour_rules), "white")
})


test_that("a self-referential find (substring of its own replace) is a single, safe substitution", {
  # `find` = "yellow" is textually a substring of `replace` = "yellow_orange" --
  # this must not loop or double-apply
  expect_equal(word_replace("flower_colour", "yellow", colour_rules), "yellow_orange")
})


test_that("two different words mapping to the same target are de-duplicated", {
  # Both "white" and "cream" -> "white_cream"; without de-duplication this would
  # be "white_cream white_cream"
  expect_equal(word_replace("flower_colour", "white cream", colour_rules), "white_cream")
})


test_that("a multi-word find is matched as one phrase", {
  rules <- tibble::tibble(trait_name = "pollination_vector_possible", find = "ant self", replace = "ant autonomous")
  expect_equal(word_replace("pollination_vector_possible", "ant self", rules), "ant autonomous")
  expect_equal(word_replace("pollination_vector_possible", "bee", rules), "bee")
})


test_that("a rule where find equals replace is a no-op, not an infinite loop", {
  rules <- tibble::tibble(trait_name = "flower_colour", find = "green", replace = "green")
  expect_equal(word_replace("flower_colour", "green", rules), "green")
})


test_that("an empty or column-less replace_table is a no-op", {
  data <- trait_data("flower_colour", c("yellow", "white"))
  expect_equal(process_word_replace(data, tibble::tibble()), data)
  expect_equal(process_word_replace(data, colour_rules[0, ]), data)
})


test_that("known hazard: chained rules within the same trait cascade", {
  # str_replace_all()'s named-vector form is sequential, not simultaneous -- if
  # one rule's `replace` textually equals another rule's `find` (as a whole
  # word) in the same trait, the two rules chain. This is *expected, pinned*
  # behaviour of process_word_replace() itself; guarding against it is
  # dataset_test()'s job (see test-dataset-test.R), not this function's.
  rules <- tibble::tibble(
    trait_name = c("dispersal_appendage", "dispersal_appendage"),
    find       = c("wing", "wings"),
    replace    = c("wings", "winged_thing")
  )
  expect_equal(word_replace("dispersal_appendage", "wing", rules), "winged_thing")
})


test_that("a dataset's match: value substitution resolves before match: word rules see it", {
  # Regression test for the ordering, using a real raw value from the fixture:
  # `Test_2023_1/data.csv` holds "climber" for `plant_growth_form` (the same raw
  # value test-synonyms.R's ordering test relies on). Here it is targeted by
  # *both* an exact substitution (climber -> vine) and a word rule that would
  # also match "climber" directly (climber -> WRONG_VALUE, an unsupported level).
  # If value-match runs first, "climber" becomes "vine" before the word rule
  # ever sees it, so the word rule's pattern no longer matches -- exactly
  # mirroring the existing precedence of substitutions over the database-wide
  # synonym for `vine` (config/traits.yml declares it a synonym of
  # `climber_herbaceous`).
  config <- dataset_configure("examples/Test_2023_1/metadata.yml", definitions)

  config[["metadata"]][["substitutions"]] <-
    c(config[["metadata"]][["substitutions"]],
      list(
        list(trait_name = "plant_growth_form", find = "climber", replace = "vine"),
        list(trait_name = "plant_growth_form", find = "climber", replace = "WRONG_VALUE", match = "word")
      ))

  built <- dataset_process("examples/Test_2023_1/data.csv", config, schema,
                           resource_metadata, unit_conversions)

  growth_form <- built[["traits"]] %>%
    dplyr::filter(.data$trait_name == "plant_growth_form")

  expect_true("climber_herbaceous" %in% growth_form[["value"]])
  expect_false("WRONG_VALUE" %in% growth_form[["value"]])
  expect_false("climber" %in% growth_form[["value"]])
})
