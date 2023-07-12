
testthat::test_that("test datasets", {

  schema <- get_schema()
  resource_metadata <- get_schema("config/metadata.yml",  "metadata")
  definitions <- get_schema("config/traits.yml", "traits")
  unit_conversions <- traits.build:::get_unit_conversions("config/unit_conversions.csv")
  taxon_list <- read_csv_char("config/taxon_list.csv")
  examples_dir <- "examples"

  # What does the below comment mean?
  # Build example -- this runs a bunch of tests already

  # Example 1 - Test for basis_of_record and life_stage at the dataset level
  # test1-metadata and test1-data are copies of Falster_2005_1
  ex1 <- test_build_dataset(
    file.path(examples_dir, "test1-metadata.yml"),
    file.path(examples_dir, "test1-data.csv"),
    "Example 1", definitions, unit_conversions, schema, resource_metadata, taxon_list
    )

  expect_equal(ex1$traits$basis_of_record %>% unique, "field")
  expect_equal(ex1$traits$life_stage %>% unique, "adult")
  # Are these tests necessary? We know from the above tests that the number of rows will be 406
  expect_equal(ex1$traits %>% filter(basis_of_record == "field") %>% nrow(), 406)
  expect_equal(ex1$traits %>% filter(life_stage == "adult") %>% nrow(), 406)
  expect_equal(nrow(ex1$excluded_data), 0)


  # Example 2 - Test variables are read in at the trait level
  # test2-metadata basis_of_record for Leaf N trait changed to lab, basis_of_record for every other trait
  # has not been specified so should take the dataset level value

  ex2 <- test_build_dataset(
    file.path(examples_dir, "test2-metadata.yml"),
    file.path(examples_dir, "test1-data.csv"),
    "Example 2", definitions, unit_conversions, schema, resource_metadata, taxon_list
    )

  expect_equal(ex2$traits$basis_of_record %>% unique, c("field", "lab"))
  expect_equal(ex2$traits %>% filter(basis_of_record == "field") %>% nrow(), 361)
  expect_equal(ex2$traits %>% filter(basis_of_record == "lab") %>% nrow(), 45)
  expect_equal(
    ex2$traits %>%
    # Is this line necessary?
    select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "leaf_N_per_dry_mass") %>%
    pull(basis_of_record) %>%
    unique,
    "lab"
  )
  expect_equal(
    ex2$traits %>%
    # Is this line necessary?
    select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "leaf_N_per_dry_mass") %>%
    pull(basis_of_record) %>%
    length,
    45
  )


  # Example 3 - Test variables stored as a column in data.csv is read in correctly
  # test2-data basis_of_record column has been added in data.csv
  # test2.1 metadata basis_of_record and life_stage changed to match column name

  # There shouldn't be a dataset level basis_of_record because it's referring to the column right? (Below line)
  # basis_of_record in the column contains "wild" while the dataset level basis_of_record is "field"
  # the values in the column should take precedence over the dataset value

  # basis_of_record for leaf N is still lab, which takes precedence over the basis_of_record column
  # ^ Is this as intended?
  # Should we change "wild" to what's in the schema? Or should there be a test that makes sure
  # these field values are what's in the schema? Also relevant to Example 5 ("Cape_Tribulation")

  ex3 <- test_build_dataset(
    file.path(examples_dir, "test2.1-metadata.yml"),
    file.path(examples_dir, "test2-data.csv"),
    "Example 3", definitions, unit_conversions, schema, resource_metadata, taxon_list
    )

  expect_equal(ex3$traits$basis_of_record %>% unique, c("wild", "lab"))
  expect_equal(ex3$traits %>% filter(basis_of_record == "wild") %>% nrow(), 361)
  expect_equal(ex3$traits$life_stage %>% unique, "seedling")
  expect_equal(ex3$traits %>% filter(life_stage == "seedling") %>% nrow(), 406)
  expect_equal(
    yaml::read_yaml(file.path(examples_dir, "test2.1-metadata.yml"))$dataset$basis_of_record,
    "basis_of_record")
  expect_equal(yaml::read_yaml(file.path(examples_dir, "test2.1-metadata.yml"))$dataset$life_stage, "life_stage")


  # Example 4 - Test variables stored as a column in data.csv are replaced with trait level value
  # test3-data basis_of_record column has been added in data.csv with missing values
  # Also introduced a value at the trait level Leaf_N ~ lab
  # But this was tested in the example above
  # Similar to Example 3 but this time values should be filled in for traits that have a value specified
  # Not really sure what the difference is with Example 3 except the addition of NAs to the basis_of_record column

  ex4 <- test_build_dataset(
    file.path(examples_dir, "test2.1-metadata.yml"),
    file.path(examples_dir, "test3-data.csv"),
    "Example 4", definitions, unit_conversions, schema, resource_metadata, taxon_list
    )

  expect_equal(ex4$traits$basis_of_record %>% unique, c(NA, "lab", "wild"))
  expect_equal(ex4$traits %>% filter(is.na(basis_of_record)) %>% nrow(), 352)
  expect_equal(ex4$traits %>% filter(basis_of_record == "lab") %>% nrow(), 45)
  expect_equal(ex4$traits %>% filter(basis_of_record == "wild") %>% nrow(), 9)

  expect_equal(
    # Is the select part necessary for the below tests?
    ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "leaf_N_per_dry_mass") %>% pull(basis_of_record) %>% unique,
    c("lab")
  )
  expect_equal(
    ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "leaf_N_per_dry_mass") %>% pull(basis_of_record) %>%
    grep(pattern = "lab") %>% length,
    45
  )
  expect_equal(
    ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "leaf_N_per_dry_mass") %>% pull(basis_of_record) %>%
    grep(pattern = "wild") %>% length,
    0
  )
  expect_equal(
    ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "seed_dry_mass") %>% pull(basis_of_record) %>% unique,
    c(NA, "wild")
  )
  expect_equal(
    ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "seed_dry_mass") %>% pull(basis_of_record) %>% is.na %>% sum,
    28
  )
  expect_equal(
    ex4$traits %>% select(dplyr::all_of(c("trait_name", "basis_of_record"))) %>%
    filter(trait_name == "seed_dry_mass") %>% pull(basis_of_record) %>%
    grep(pattern = "wild") %>% length,
    1
  )


  # Example 5 - Test a combination of trait, site and dataset level values
  # Basis of record has been specified for dataset level ~ field, site level ~ Cape_Tribulation
  # trait level ~ leaf_N and column ~ wild
  # Column values should take precedence followed by traits values, followed by locations and then dataset values
  # Example 3 has trait values taking precedence over column values though, and here I think the same
  # thing is happening
  # Also I think locations are taking precedence over column values, and location value is also taking precedence
  # over trait value
  # How can there be a dataset level value if the column is read in at the dataset level?

  ex5 <- test_build_dataset(
    file.path(examples_dir, "test3-metadata.yml"),
    file.path(examples_dir, "test3-data.csv"),
    "Example 5", definitions, unit_conversions, schema, resource_metadata, taxon_list
    )

  # Shouldn't this also have "wild" from the column according to Line 149?
  expect_equal(ex5$traits$basis_of_record %>% unique, c(NA, "lab", "Cape_Tribulation"))
  expect_equal(ex5$traits %>% filter(is.na(basis_of_record)) %>% nrow(), 81)
  expect_equal(ex5$traits %>% filter(basis_of_record == "Cape_Tribulation") %>% nrow(), 315)
  expect_equal(ex5$traits %>% filter(basis_of_record == "lab") %>% nrow(), 10)
  expect_equal(ex5$traits %>% filter(basis_of_record == "wild") %>% nrow(), 0)

  expect_equal(
    # Again, is the select here and in all tests below necessary?
    ex5$traits %>% select(c("taxon_name", "basis_of_record")) %>%
    filter(taxon_name == "Trema aspera") %>% pull(basis_of_record) %>% unique,
    c("Cape_Tribulation"))
  # I think this should be "wild" not "Cape_Tribulation"
  # What is the point of the below test? Doesn't it just test that the number of rows for Trema aspera
  # is 10?
  expect_equal(
    ex5$traits %>% select(c("taxon_name", "basis_of_record")) %>%
    filter(taxon_name == "Trema aspera") %>% pull(basis_of_record) %>% length,
    10)

  # Maybe for a lot of these tests instead of expecting equality to a number it should be something
  # more explicit like expect equal to the number of rows of leaf N values that are not in the Cape Tribulation
  # location (in this example)
  expect_equal(
    ex5$traits %>% select(c("trait_name", "basis_of_record")) %>%
    filter(trait_name == "leaf_N_per_dry_mass") %>% pull(basis_of_record) %>%
    grep(pattern = "lab") %>% length,
    10
  )
  # Needs to be fixed, "wild" should not be 0
  expect_equal(
    ex5$traits %>% select(c("trait_name", "basis_of_record")) %>%
    filter(trait_name == "leaf_N_per_dry_mass") %>% pull(basis_of_record) %>%
    grep(pattern = "wild") %>% length,
    0
  )

  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% unique, c(NA, "lab"))
  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% length, 91)
  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% is.na %>% sum, 81)
  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "01") %>%
                 pull(basis_of_record) %>% grep(pattern = "lab") %>% length, 10)

  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% unique, c("Cape_Tribulation"))
  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% length, 315)
  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% grep(pattern = "Cape_Tribulation") %>% length, 315)
  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% grep(pattern = "lab") %>% length, 0)
  expect_equal(ex5$traits %>% select(c("location_id", "basis_of_record")) %>% filter(location_id == "02") %>%
                 pull(basis_of_record) %>% grep(pattern = "wild") %>% length, 0)

  expect_equal(
    ex5$traits %>% pull(location_id) %>% unique,
    ex5$locations %>% pull(location_id) %>% unique
  )
  # This test feels very similar to the above test
  expect_equal(
    ex5$traits %>% select(c("location_id")) %>% unique() %>% nrow(),
    ex5$locations %>% select(c("location_name")) %>% unique() %>% nrow()
  )


  # Example 6 - Tests focus on context and the various context identifiers
  # Based on Crous_2013
  # Have also added data for sex to test this field (commented out for now)
  # Is the sex data testing old? I think that's an entity_context in the contexts table not the
  # traits table right? Should I remove the tests?

  ex6 <- test_build_dataset(
    file.path(examples_dir, "test4-metadata.yml"),
    file.path(examples_dir, "test4-data.csv"),
    "Example 6", definitions, unit_conversions, schema, resource_metadata, taxon_list
    )

  #expect_equal(ex6$traits$sex %>% unique, c("male", "female"))
  expect_equal(ex6$traits$location_id %>% unique, c("01"))
  #expect_equal(ex6$traits %>% filter(sex == "male") %>% nrow(), 85)
  # Test that distinct combinations of method_id, temporal_id and treatment_id are equal to 36
  expect_equal(ex6$traits %>% distinct(method_id, temporal_id, treatment_id) %>% nrow(), 36)

  expect_equal(ex6$contexts$category %>% unique, c("temporal", "treatment", "method"))
  expect_equal(ex6$contexts %>% nrow(), 9)
  # Test that each row in contexts table has a unique link_id and link_vals combination
  expect_equal(
    ex6$contexts %>% nrow(),
    ex6$contexts %>% group_by(link_id, link_vals) %>% distinct() %>% nrow())
  expect_equal(ex6$contexts %>% pull(context_property) %>% unique() %>% length, 4)

  expect_equal(
    ex6$traits %>% filter(trait_name == "fruit_colour") %>% pull(value) %>% unique,
    c("pink", "black", "red")
  )
  # Why should this be equal to 35? In the cases where it isn't obvious where the numbers
  # come from I feel like it would be useful to implement what I suggested on Line 182-184
  expect_equal(ex6$traits %>% pull(observation_id) %>% unique() %>% length(), 35)

})
