
# To generate taxon_list-orig.csv

# Use Lizzy's `R/build_update_taxon_list.R` function

# First move the test datasets to the tests/testthat/data folder (and removing existing datasets in the folder)
# Then run `build_setup_pipeline()`, source "build.R" and "R/build_update_taxon_list.R" in "tests/testthat"
# Run the function on the built test database with `replace = TRUE`
# (Remove the tests from the data folder afterwards and discard changes)
# Replace config/taxon_list-orig.csv with config/taxon_list.csv

build_update_taxon_list(database, read_csv("config/taxon_list.csv"), replace = TRUE)
