## Tests for the database-wide trait value synonyms declared in `traits.yml`.
##
## A categorical trait value may list synonyms in its description with the
## convention `(Synonyms, first, second)`; `process_replace_synonyms()` replaces
## every listed synonym found in the data with the value that declares it.
##
## The quirks pinned below all occur in `austraits.build`'s real `config/traits.yml`
## (197 synonyms across 27 traits), so they are not hypothetical: synonyms
## separated by a semicolon rather than a comma, synonyms containing a space, and
## a value that lists itself as one of its own synonyms.

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")

# The `elements` of a traits.yml, keyed by trait_name. `process_replace_synonyms()`
# takes this, *not* the whole schema object -- handing it the whole object dies on
# the atomic `description` entry, which is what made #254 hard to read.
test_definitions <- list(
  plant_growth_form = list(
    type = "categorical",
    allowed_values_levels = list(
      climber_herbaceous = "A herbaceous climber. (Synonyms, vine)",
      climber_woody = "A woody climber. (Synonyms, liana)",
      palmoid = "Palm-like habit. (Synonyms, rosette-tree, palm, grass-tree)",
      tree = "A tree. No synonyms are declared for this value."
    )
  ),
  seed_shape = list(
    type = "categorical",
    allowed_values_levels = list(
      comma_shaped = "Comma shaped. (Synonyms, comma_shaped, falcate)",
      ovoid_elongated = "Elongated ovoid. (Synonyms, clavate; obclavate)"
    )
  ),
  leaf_area = list(type = "numeric")
)

trait_data <- function(trait_name, value) {
  tibble::tibble(trait_name = trait_name, value = value)
}

replaced <- function(trait_name, value, definitions = test_definitions) {
  process_replace_synonyms(trait_data(trait_name, value), definitions)[["value"]]
}


test_that("a declared synonym is replaced by the value that declares it", {
  expect_equal(replaced("plant_growth_form", "vine"), "climber_herbaceous")
  expect_equal(replaced("plant_growth_form", "liana"), "climber_woody")
  expect_equal(replaced("seed_shape", "falcate"), "comma_shaped")
})


test_that("a synonym containing a hyphen is matched", {
  expect_equal(replaced("plant_growth_form", "rosette-tree"), "palmoid")
  expect_equal(replaced("plant_growth_form", "grass-tree"), "palmoid")
})


test_that("values that declare no synonyms are left alone", {
  expect_equal(replaced("plant_growth_form", "tree"), "tree")
  expect_equal(replaced("plant_growth_form", "shrub"), "shrub")
  # a numeric trait has no allowed_values_levels at all
  expect_equal(replaced("leaf_area", "12.4"), "12.4")
})


test_that("synonyms are scoped to the trait that declares them", {
  # `falcate` is a seed_shape synonym only; plant_growth_form must not see it
  expect_equal(replaced("plant_growth_form", "falcate"), "falcate")
  expect_equal(replaced("seed_shape", "vine"), "vine")
})


test_that("each value of a multi-value cell is replaced independently", {
  # Categorical values are space-delimited, so a cell may hold several of them
  expect_equal(
    replaced("plant_growth_form", "vine liana"),
    "climber_herbaceous climber_woody"
  )
  expect_equal(
    replaced("plant_growth_form", "tree vine shrub"),
    "tree climber_herbaceous shrub"
  )
})


test_that("a synonym occurring inside a longer word is not replaced", {
  # Whole-word matching: without it, `\\bvine\\b` would corrupt these
  expect_equal(replaced("plant_growth_form", "grapevine"), "grapevine")
  expect_equal(replaced("plant_growth_form", "vineyard"), "vineyard")
  expect_equal(replaced("seed_shape", "obclavate"), "ovoid_elongated")
})


test_that("synonyms separated by a semicolon are parsed, not treated as one", {
  # austraits.build declares `(Synonyms, clavate; obclavate)`; splitting on the
  # comma alone yielded the single unmatchable synonym "clavate; obclavate"
  expect_equal(replaced("seed_shape", "clavate"), "ovoid_elongated")
  expect_equal(replaced("seed_shape", "obclavate"), "ovoid_elongated")
})


test_that("a value listing itself as its own synonym is left unchanged", {
  # `comma_shaped` declares `comma_shaped` among its synonyms; replacing a value
  # with itself must be a no-op rather than an error
  expect_equal(replaced("seed_shape", "comma_shaped"), "comma_shaped")
})


test_that("every row is replaced, and row order is preserved", {
  expect_equal(
    replaced("plant_growth_form", c("vine", "tree", "liana", "vine")),
    c("climber_herbaceous", "tree", "climber_woody", "climber_herbaceous")
  )
})


test_that("definitions carrying no synonyms are a no-op", {
  data <- trait_data("plant_growth_form", c("vine", "tree"))
  expect_equal(process_replace_synonyms(data, NULL), data)
  expect_equal(process_replace_synonyms(data, list()), data)
  expect_equal(
    process_replace_synonyms(data, list(plant_growth_form = list(type = "numeric"))),
    data
  )
  expect_equal(
    process_replace_synonyms(
      data,
      list(plant_growth_form = list(
        allowed_values_levels = list(tree = "A tree, with no synonyms declared.")
      ))
    ),
    data
  )
})


test_that("a trait value carrying no description is tolerated", {
  # None of the three databases has one today, but a curator adding a value with
  # no description would otherwise abort the build with "argument is of length zero"
  data <- trait_data("plant_growth_form", c("vine", "tree"))
  expect_no_error(
    result <- process_replace_synonyms(
      data,
      list(plant_growth_form = list(
        allowed_values_levels = list(
          tree = NULL,
          climber_herbaceous = "A herbaceous climber. (Synonyms, vine)"
        )
      ))
    )
  )
  expect_equal(result[["value"]], c("climber_herbaceous", "tree"))
})


test_that("a dataset substitution is applied before the database-wide synonyms", {
  # Regression test for the ordering. The synonym pass used to run *first*, which
  # silently pre-empted the dataset's own curated substitutions: measured across
  # austraits.build, 196 substitution entries in 21 datasets had a `find` that a
  # synonym had already rewritten, so they never matched.
  #
  # Here `climber` is a plain value in the raw data. Substituting it to `vine`
  # -- which the fixture declares as a synonym of `climber_herbaceous` -- only
  # resolves to `climber_herbaceous` if substitutions run first. Under the old
  # order the value stays `vine`, which is not an allowed level, so the row is
  # dropped into `excluded_data` instead.
  config <- dataset_configure("examples/Test_2023_1/metadata.yml", definitions)

  config[["metadata"]][["substitutions"]] <-
    c(config[["metadata"]][["substitutions"]],
      list(list(trait_name = "plant_growth_form", find = "climber", replace = "vine")))

  built <- dataset_process("examples/Test_2023_1/data.csv", config, schema,
                           resource_metadata, unit_conversions)

  growth_form <- built[["traits"]] %>%
    dplyr::filter(.data$trait_name == "plant_growth_form")

  expect_true("climber_herbaceous" %in% growth_form[["value"]])
  expect_false("climber" %in% growth_form[["value"]])

  # the unresolved synonym must not leak through, nor be excluded as unsupported
  excluded <- built[["excluded_data"]] %>%
    dplyr::filter(.data$trait_name == "plant_growth_form")
  expect_false("vine" %in% excluded[["value"]])
  expect_false("vine" %in% growth_form[["value"]])
})
