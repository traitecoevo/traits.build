

austraits <- readRDS("test_austraits.rds")
# This file tests usage of the database
# Note, requires existnec of "test_austraits.rds", generated from `test-process.R`

test_that("plots", {
  expect_no_error(suppressMessages(
    p <- austraits %>% traits.build::plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Test_2022")
  ))
})
