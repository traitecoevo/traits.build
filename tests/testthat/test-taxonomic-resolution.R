## The nine example datasets only ever use `taxonomic_resolution: species`, so
## until these tests existed thirteen of the fourteen ranks the real databases
## use were never exercised -- including the six invertebrate ranks in
## `ausinvertraits.build`, which the paper's claim of a workflow generalised
## beyond plants rests on. See #225.

# Ranks used across austraits.build, ausinvertraits.build and AusFizz
botanical_ranks <- c("species", "subspecies", "variety", "form", "series",
                     "genus", "family", "order", "class")

invertebrate_ranks <- c("subfamily", "suborder", "subgenus", "superfamily",
                        "supertribe", "tribe")


test_that("names at or below species rank are matched in full", {
  # These carry a specific epithet, so truncating them to the genus would
  # collapse every species in a genus onto one taxon
  expect_equal(
    util_name_to_match_to("Acacia dealbata", "species"),
    "Acacia dealbata"
  )
  expect_equal(
    util_name_to_match_to("Acacia dealbata subsp. subalpina", "subspecies"),
    "Acacia dealbata subsp. subalpina"
  )
  expect_equal(
    util_name_to_match_to("Eucalyptus baxteri var. pedicellata", "variety"),
    "Eucalyptus baxteri var. pedicellata"
  )
  expect_equal(
    util_name_to_match_to("Banksia integrifolia f. minor", "form"),
    "Banksia integrifolia f. minor"
  )
})


test_that("names above species rank are matched on their first word", {
  expect_equal(util_name_to_match_to("Acacia", "genus"), "Acacia")
  expect_equal(util_name_to_match_to("Fabaceae", "family"), "Fabaceae")
  expect_equal(util_name_to_match_to("Fabales", "order"), "Fabales")

  # A coarse rank recorded against a binomial keeps only the genus, which is
  # the whole point of the branch
  expect_equal(util_name_to_match_to("Acacia dealbata", "genus"), "Acacia")
})


test_that("invertebrate ranks resolve to the genus, not the full name", {
  # `ausinvertraits.build` uses all six of these and none appears in the
  # botanical rank list at the heart of `util_name_to_match_to`. They are all
  # coarser than species, so first-word matching is correct -- this pins that,
  # so that adding a rank *below* species to the databases without adding it to
  # `ranks_at_or_below_species` fails here rather than silently truncating.
  for (rank in invertebrate_ranks) {
    expect_equal(
      util_name_to_match_to("Aphaenogaster longiceps", rank),
      "Aphaenogaster",
      info = rank
    )
  }
})


test_that("every rank the databases use produces a non-empty match name", {
  all_ranks <- c(botanical_ranks, invertebrate_ranks)

  matched <- util_name_to_match_to(
    rep("Aphaenogaster longiceps", length(all_ranks)),
    all_ranks
  )

  expect_equal(length(matched), length(all_ranks))
  expect_false(any(is.na(matched)))
  expect_true(all(nzchar(matched)))
})


test_that("bracketed qualifiers are dropped at every rank", {
  # Manuscript names such as `Acacia sp. [Kimberley Flora]` must not be matched
  # against the taxon list with the bracketed part attached
  expect_equal(
    util_name_to_match_to("Acacia sp. [Kimberley Flora]", "species"),
    "Acacia sp."
  )
  expect_equal(
    util_name_to_match_to("Acacia sp. [Kimberley Flora]", "genus"),
    "Acacia"
  )
})


test_that("an unrecognised rank falls back to first-word matching", {
  # `taxonomic_resolution` is not validated against any vocabulary, so a typo
  # reaches this function. Falling back to the genus is the safe direction, but
  # it is silent -- recorded here so the behaviour is at least known.
  expect_equal(
    util_name_to_match_to("Acacia dealbata", "speceis"),
    "Acacia"
  )
  expect_equal(
    util_name_to_match_to("Acacia dealbata", NA_character_),
    "Acacia"
  )
})
