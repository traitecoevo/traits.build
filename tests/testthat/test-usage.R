

test_that("plots", {

  # This file tests usage of the database
  # Note, requires existence of "test_austraits.rds", generated from `test-setup.R`
  expect_silent(austraits <- readRDS("test_austraits.rds"))

  expect_invisible(suppressMessages(
    austraits %>% traits.build::plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Test_2022")
  ))
})
