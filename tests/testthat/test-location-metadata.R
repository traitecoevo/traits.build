## A location can set `basis_of_record`, `life_stage`, `collection_date`,
## `measurement_remarks` and `entity_type` for the records measured there. For
## each record, a value set on the trait's own entry in `metadata.yml` takes
## precedence over the location's, and the location's over the dataset's.
## Where the trait entry reads the field from a column whose cell is empty, the
## location's value fills it (#60).

build_dataset <- function(dataset_id) {
  definitions <- get_schema("config/traits.yml", "traits")
  dataset_process(
    file.path("examples", dataset_id, "data.csv"),
    dataset_configure(file.path("examples", dataset_id, "metadata.yml"), definitions),
    get_schema(),
    get_schema("config/metadata.yml", "metadata"),
    get_unit_conversions("config/unit_conversions.csv")
  )
}

location_id_of <- function(built, location_name) {
  unique(built$locations$location_id[built$locations$location_name == location_name])
}


test_that("trait-level metadata takes precedence over location-level metadata (#60)", {
  # Test_2023_2 is wide. `Cape Tribulation` sets `basis_of_record:
  # field_experiment`, `life_stage: juvenile` and `measurement_remarks`
  built <- build_dataset("Test_2023_2")
  traits <- built$traits %>%
    dplyr::filter(.data$location_id == location_id_of(built, "Cape Tribulation"))
  seed_mass <- traits %>% dplyr::filter(.data$trait_name == "seed_dry_mass")

  # A fixed value on the trait entry beats the location's:
  # `seed_dry_mass` sets `basis_of_record: literature`
  expect_gt(nrow(seed_mass), 0)
  expect_equal(unique(seed_mass$basis_of_record), "literature")
  # and `branch_mass_fraction` sets its own `measurement_remarks`
  expect_equal(
    unique(traits$measurement_remarks[traits$trait_name == "branch_mass_fraction"]),
    "Made-up remarks for `branch_mass_fraction`"
  )

  # A trait entry naming a column beats the location where the cell has a
  # value: `seed_dry_mass` reads `life_stage` from `life_stage_seed_mass`,
  # which is `seedling` for Ixora biflora
  expect_equal(seed_mass$life_stage[seed_mass$taxon_name == "Ixora biflora"], "seedling")

  # ... and the location fills the cell where it is empty, as for Alstonia
  # scholaris
  expect_equal(seed_mass$life_stage[seed_mass$taxon_name == "Alstonia scholaris"], "juvenile")

  # The location still beats the dataset level: `huber_value` declares no
  # `basis_of_record` of its own, and the dataset's is `field`
  expect_equal(
    unique(traits$basis_of_record[traits$trait_name == "huber_value"]),
    "field_experiment"
  )

  # The marks recording which values the trait level set are gone
  expect_false(any(startsWith(names(built$traits), ".trait_level_")))
  expect_false(any(startsWith(names(built$excluded_data), ".trait_level_")))
})


test_that("trait-level metadata takes precedence for long-format data too (#60)", {
  # Test_2023_4 is long. `fruit_dehiscence` sets `life_stage: adult` and reads
  # `measurement_remarks` from a column; `australia` sets `life_stage:
  # juvenile` and `australia_2` sets `measurement_remarks`
  built <- build_dataset("Test_2023_4")
  australia <- location_id_of(built, "australia")
  australia_2 <- location_id_of(built, "australia_2")
  fruit_dehiscence <- built$traits %>% dplyr::filter(.data$trait_name == "fruit_dehiscence")

  # Fixed value
  at_australia <- fruit_dehiscence %>% dplyr::filter(.data$location_id == australia)
  expect_gt(nrow(at_australia), 0)
  expect_equal(unique(at_australia$life_stage), "adult")

  # Column with a value
  at_australia_2 <- fruit_dehiscence %>% dplyr::filter(.data$location_id == australia_2)
  expect_gt(nrow(at_australia_2), 0)
  expect_equal(unique(at_australia_2$measurement_remarks), "Remarks for `fruit_dehiscence`")

  # Location over dataset: `fruit_length` reads `life_stage` only from the
  # dataset level
  fruit_length <- built$traits %>%
    dplyr::filter(.data$trait_name == "fruit_length", .data$location_id == australia)
  expect_equal(unique(fruit_length$life_stage), "juvenile")
})


test_that("`process_add_location_metadata()` decides precedence for each record", {
  locations <- process_format_locations(
    list(site = list(`latitude (deg)` = -30, `longitude (deg)` = 150, life_stage = "juvenile")),
    "Test", get_schema()
  )

  traits <- tibble::tribble(
    ~case,                              ~entity_type, ~life_stage, ~.trait_level_life_stage,
    "trait value",                      "population", "adult",     TRUE,
    "trait column, empty cell",         "population", NA,          TRUE,
    # `custom_R_code` can leave a cell as "" rather than `NA`; it is as empty
    "trait column, empty string",       "population", "",          TRUE,
    "dataset value",                    "population", "adult",     NA,
    "no value",                         "population", NA,          NA,
    "species-level, dataset value",     "species",    "adult",     NA
  ) %>%
    dplyr::mutate(location_name = "site", location_id = NA_character_)

  out <- process_add_location_metadata(traits, locations)

  expect_equal(
    stats::setNames(out$life_stage, out$case),
    c(
      "trait value" = "adult",
      "trait column, empty cell" = "juvenile",
      "trait column, empty string" = "juvenile",
      "dataset value" = "juvenile",
      "no value" = "juvenile",
      # A species-level record is not linked to a location, so takes nothing from it
      "species-level, dataset value" = "adult"
    )
  )
  expect_equal(out$location_id, c(rep("01", 5), NA))
  expect_false(".trait_level_life_stage" %in% names(out))

  # With no locations nothing is filled, but the marks still go
  no_locations <- process_add_location_metadata(traits, locations[0, ])
  expect_equal(no_locations$life_stage, traits$life_stage)
  expect_false(".trait_level_life_stage" %in% names(no_locations))
})
