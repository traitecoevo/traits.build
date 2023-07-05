
test_that("test get_schema", {
  expect_silent(x <- get_schema())
  expect_type(x, "list")
  expect_length(x, 6)
  expect_equal(names(x), c("entity_type", "value_type", "basis_of_value", "basis_of_record", "austraits", "metadata"))
  expect_type(get_schema(subsection = "austraits"), "list")
  expect_length(get_schema(subsection = "austraits"), 3)
  expect_length(get_schema(subsection = "metadata"), 3)
  expect_length(get_schema(subsection = "value_type"), 3)
  expect_length(get_schema(subsection = "entity_type"), 3)
  expect_length(get_schema(subsection = "basis_of_record"), 3)
  expect_length(get_schema(subsection = "basis_of_value"), 3)
})
