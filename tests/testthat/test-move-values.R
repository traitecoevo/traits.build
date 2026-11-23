# The implementations these functions replace, as in austraits.build `R/custom_R_code.R`
# (origin/develop d535de9e). At their defaults, the package's functions must return
# exactly what these do, so that austraits.build can delete its copies with no change to
# its output.
legacy_move_values_to_new_trait <- function(data, original_trait, new_trait, original_values,
                                            values_for_new_trait, values_to_keep) {
  for (j in 1:length(original_values)) { # nolint: seq_linter. Verbatim.
    i <- data[[original_trait]] == original_values[[j]]
    i <- ifelse(is.na(i), "FALSE", i)

    data[[new_trait]] <- ifelse(i, values_for_new_trait[[j]], data[[new_trait]])
    data[[original_trait]] <- ifelse(i, values_to_keep[[j]], data[[original_trait]])
    data
  }

  return(data) # nolint: return_linter. Verbatim.
}

legacy_move_values_to_new_trait_long <- function(data, original_trait, new_trait, traits_column, values_column,
                                                 original_values) {
  i <- data[[values_column]] %in% original_values

  data[[traits_column]] <- ifelse(i, new_trait, data[[traits_column]])

  data
}


growth <- tibble::tibble(
  growth_form = c("shrub", "Epiphyte", "epiphyte", "herb", "epiphytic herb", NA, ""),
  fruit = c("drupe", "", "berry", "capsule", "nut", "pod", NA)
)


# Ported from austraits.build tests/build/test-custom.R
test_that("`move_values_to_new_trait` adds a column, keeping the rows", {
  data <- tibble::tibble(Root = rep("Soil", 10))

  expect_equal(ncol(data), 1)

  expect_equal(ncol(move_values_to_new_trait(data, "Root", "Branch", "Soil", "Leaves", "Soil")), 2)
  expect_equal(nrow(move_values_to_new_trait(data, "Root", "Branch", "Soil", "Leaves", "Soil")), 10)
})


test_that("`move_values_to_new_trait` moves matched values and leaves the rest", {
  data <- growth %>% dplyr::mutate(substrate = NA_character_)

  out <- move_values_to_new_trait(
    data, "growth_form", "substrate", c("epiphyte", "epiphytic herb"), c("epiphyte", "epiphyte"), c("", "herb")
  )

  expect_equal(out$substrate, c(NA, NA, "epiphyte", NA, "epiphyte", NA, NA))
  # Matching is exact and case-sensitive by default, and `NA` never matches
  expect_equal(out$growth_form, c("shrub", "Epiphyte", "", "herb", "herb", NA, ""))
  expect_equal(out$fruit, growth$fruit)
})


test_that("`move_values_to_new_trait` matches values in order, each against the data the last left", {
  data <- tibble::tibble(x = c("a", "b"), y = NA_character_)

  out <- move_values_to_new_trait(data, "x", "y", c("a", "b"), c("A", "B"), c("b", "c"))

  # "a" is rewritten to "b", which the second value then matches
  expect_equal(out$x, c("c", "c"))
  expect_equal(out$y, c("B", "B"))
})


test_that("`move_values_to_new_trait` creates `new_trait` when it is absent", {
  # The austraits.build copy errored here ("replacement has length zero") unless every
  # row matched the first value, so creating the column cannot change an existing call
  expect_error(
    suppressWarnings(legacy_move_values_to_new_trait(growth, "growth_form", "substrate", "epiphyte", "epiphyte", "")),
    "replacement has length zero"
  )

  out <- move_values_to_new_trait(growth, "growth_form", "substrate", "epiphyte", "epiphyte", "")
  expect_type(out$substrate, "character")
  expect_equal(out$substrate, c(NA, NA, "epiphyte", NA, NA, NA, NA))

  # The same as creating the column first, as callers had to
  expect_identical(
    out,
    growth %>%
      dplyr::mutate(substrate = NA_character_) %>%
      move_values_to_new_trait("growth_form", "substrate", "epiphyte", "epiphyte", "")
  )

  # And where every row matches -- the one case the old copy handled -- the same as it
  data <- tibble::tibble(x = rep("a", 3))
  expect_identical(
    move_values_to_new_trait(data, "x", "y", c("a", "b"), c("A", "B"), c("k", "l")),
    legacy_move_values_to_new_trait(data, "x", "y", c("a", "b"), c("A", "B"), c("k", "l"))
  )

  # A data frame with no rows gains the column too
  expect_named(
    move_values_to_new_trait(data.frame(x = character()), "x", "y", "a", "A", ""),
    c("x", "y")
  )
})


test_that("`move_values_to_new_trait` adds to an existing `new_trait`, keeping unmatched values", {
  out <- move_values_to_new_trait(growth, "growth_form", "fruit", c("herb", "shrub"), "dry", c("herb", "shrub"))

  expect_equal(out$fruit, c("dry", "", "berry", "dry", "nut", "pod", NA))
  expect_equal(out$growth_form, growth$growth_form)
})


test_that("`move_values_to_new_trait` with `blank_to_na` clears blanks in `original_trait` only", {
  args <- list(growth, "growth_form", "substrate", c("epiphyte", "epiphytic herb"), c("epiphyte", ""), c("", "herb"))

  # By default a blank stays a blank
  expect_equal(do.call(move_values_to_new_trait, args)$growth_form, c("shrub", "Epiphyte", "", "herb", "herb", NA, ""))

  out <- do.call(move_values_to_new_trait, c(args, blank_to_na = TRUE))

  # Including the blank that was already there
  expect_equal(out$growth_form, c("shrub", "Epiphyte", NA, "herb", "herb", NA, NA))
  expect_type(out$growth_form, "character")
  # Not in `new_trait`, nor in any other column
  expect_equal(out$substrate, c(NA, NA, "epiphyte", NA, "", NA, NA))
  expect_equal(out$fruit, growth$fruit)
})


test_that("`move_values_to_new_trait` with `to_lower` lower-cases and matches ignoring case", {
  out <- move_values_to_new_trait(growth, "growth_form", "substrate", "EPIPHYTE", "epiphyte", "", to_lower = TRUE)

  expect_equal(out$substrate, c(NA, "epiphyte", "epiphyte", NA, NA, NA, NA))
  expect_equal(out$growth_form, c("shrub", "", "", "herb", "epiphytic herb", NA, ""))

  # The column is lower-cased whether or not a value matched
  out <- move_values_to_new_trait(
    tibble::tibble(x = c("Tree", "SHRUB")), "x", "y", "nothing", "z", "", to_lower = TRUE
  )
  expect_equal(out$x, c("tree", "shrub"))

  # Without it, case must match exactly
  out <- move_values_to_new_trait(growth, "growth_form", "substrate", "EPIPHYTE", "epiphyte", "")
  expect_equal(out$substrate, rep(NA_character_, 7))
  expect_equal(out$growth_form, growth$growth_form)
})


test_that("`move_values_to_new_trait` with `to_lower` matches a value `values_to_keep` wrote ignoring case", {
  # "Herb" is rewritten to "Tree", which the second value then matches; before
  # `values_to_keep` was lower-cased, "Tree" no longer matched "tree"
  out <- move_values_to_new_trait(
    tibble::tibble(x = c("Herb", "tree")), "x", "y", c("herb", "Tree"), c("H", "T"), c("Tree", ""),
    to_lower = TRUE
  )

  expect_equal(out$x, c("", ""))
  expect_equal(out$y, c("T", "T"))

  # And the column is left lower case, `NA` kept as `NA`
  out <- move_values_to_new_trait(
    tibble::tibble(x = c("Herb", "Shrub")), "x", "y", c("herb", "shrub"), "z", c("Climber", NA),
    to_lower = TRUE
  )
  expect_identical(out$x, c("climber", NA))
})


test_that("`move_values_to_new_trait` with `to_lower` and `blank_to_na` together", {
  out <- move_values_to_new_trait(
    growth, "growth_form", "substrate", c("epiphyte", "epiphytic herb"), "epiphyte", c("", "herb"),
    blank_to_na = TRUE, to_lower = TRUE
  )

  expect_equal(out$substrate, c(NA, "epiphyte", "epiphyte", NA, "epiphyte", NA, NA))
  expect_equal(out$growth_form, c("shrub", NA, NA, "herb", "herb", NA, NA))
})


test_that("`move_values_to_new_trait` uses a single value for every `original_values`", {
  out <- move_values_to_new_trait(growth, "growth_form", "substrate", c("epiphyte", "epiphytic herb"), "epiphyte", "")

  expect_equal(out$substrate, c(NA, NA, "epiphyte", NA, "epiphyte", NA, NA))
  expect_equal(out$growth_form, c("shrub", "Epiphyte", "", "herb", "", NA, ""))
})


test_that("`move_values_to_new_trait` rejects arguments that do not line up", {
  expect_error(
    move_values_to_new_trait(growth, "growth_form", "substrate", c("a", "b", "c"), c("A", "B"), ""),
    "`values_for_new_trait` has 2 values; it needs one for each of the 3 `original_values`"
  )
  expect_error(
    move_values_to_new_trait(growth, "growth_form", "substrate", "a", "A", c("x", "y")),
    "`values_to_keep` has 2 values"
  )
  expect_error(
    move_values_to_new_trait(growth, "growth form", "substrate", "a", "A", ""),
    "`original_trait` is not a column of `data`: growth form"
  )
})


test_that("`move_values_to_new_trait` at its defaults returns exactly what the old copy did", {
  # Patterns used by the datasets calling it: a logical `NA` column made with
  # `mutate(x = NA)`, numeric values, "NA" written as a string, `new_trait` the same
  # as `original_trait`, and an existing column as `new_trait`
  data <- tibble::tibble(
    a = c("x", "y", NA, "z", "x"),
    n = c(27, 24, 1, NA, 27),
    lgl = NA,
    chr = c("p", "q", "r", "s", "t")
  )

  cases <- list(
    list(data, "a", "lgl", c("x", "z"), c("X", "Z"), c("", "NA")),
    list(data, "n", "lgl", 27, "viviparous", 27),
    list(data, "n", "chr", 24, "spore", "NA"),
    list(data, "a", "a", "y", "Y", ""),
    list(data, "a", "chr", c("x", "y"), c("X", "Y"), c("x", "y")),
    list(data, "a", "lgl", "none", "N", "")
  )

  for (args in cases) {
    expect_identical(do.call(move_values_to_new_trait, args), do.call(legacy_move_values_to_new_trait, args))
  }
})


test_that("`move_values_to_new_trait` is available to `custom_R_code`", {
  # `tests/testthat/R/custom_R_code.R` defines no functions, so this is the package's copy
  code <- paste(
    "data %>%",
    "move_values_to_new_trait('growth_form', 'substrate', 'epiphyte', 'epiphyte', '', blank_to_na = TRUE)"
  )

  out <- process_custom_code(code)(growth)

  expect_equal(out$substrate, c(NA, NA, "epiphyte", NA, NA, NA, NA))
  expect_equal(out$growth_form, c("shrub", "Epiphyte", NA, "herb", "epiphytic herb", NA, NA))
})


texture <- tibble::tibble(
  trait = c(rep("Texture", 5), "Appendage", "Appendage"),
  value = c("plumose", "Tomentose", "smooth", "rough", NA, "plumose", "scales")
)


# Ported from austraits.build tests/build/test-custom.R
test_that("`move_values_to_new_trait_long` reassigns rows, keeping the shape", {
  data <- tibble::tibble(
    trait = rep("Texture", 10),
    value = rep(c("plumose", "tomentose", "smooth", "rough", "spiky"), 2)
  )

  expect_equal(unique(data$trait), "Texture")
  expect_equal(nrow(data), 10)

  out <- move_values_to_new_trait_long(
    data, "Texture", "seed_surface_hairs", "trait", "value", c("plumose", "tomentose")
  )

  expect_equal(unique(out$trait), c("seed_surface_hairs", "Texture"))
  expect_equal(ncol(out), 2)
  expect_equal(nrow(out), 10)
})


test_that("`move_values_to_new_trait_long` matches on value alone, whatever the trait", {
  out <- move_values_to_new_trait_long(texture, "Texture", "seed_surface_hairs", "trait", "value", "plumose")

  # The Appendage row is moved too: `original_trait` is not used (ANBG_2019 relies on this)
  expect_equal(
    out$trait,
    c("seed_surface_hairs", "Texture", "Texture", "Texture", "Texture", "seed_surface_hairs", "Appendage")
  )
  expect_equal(out$value, texture$value)
})


test_that("`move_values_to_new_trait_long` with `to_lower` matches ignoring case, leaving values", {
  expect_equal(
    move_values_to_new_trait_long(texture, "Texture", "hairs", "trait", "value", "tomentose")$trait,
    texture$trait
  )

  out <- move_values_to_new_trait_long(texture, "Texture", "hairs", "trait", "value", "TOMENTOSE", to_lower = TRUE)

  expect_equal(out$trait, c("Texture", "hairs", "Texture", "Texture", "Texture", "Appendage", "Appendage"))
  expect_equal(out$value, texture$value)
})


test_that("`move_values_to_new_trait_long` at its defaults returns exactly what the old copy did", {
  cases <- list(
    list(texture, "Texture", "seed_surface_hairs", "trait", "value", c("plumose", "Tomentose")),
    list(texture, "Appendage", "Texture", "trait", "value", "scales"),
    list(texture, "Texture", "missing", "trait", "value", c("smooth", NA)),
    list(texture, "Texture", "none", "trait", "value", character())
  )

  for (args in cases) {
    expect_identical(do.call(move_values_to_new_trait_long, args), do.call(legacy_move_values_to_new_trait_long, args))
  }
})


test_that("`move_values_to_new_trait_long` rejects a column `data` lacks", {
  expect_error(
    move_values_to_new_trait_long(texture, "Texture", "hairs", "trait_name", "value", "plumose"),
    "Not a column of `data`: trait_name"
  )
})
