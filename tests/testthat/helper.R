library(traits.build)
library(crayon)

# `config/taxon_list.csv` is a build artefact, not a repository file:
# `build_setup_pipeline()` creates it and several tests read it. Until now the
# only thing that made those reads work was file ordering -- `test-setup.R`
# generates it as a side effect, and the misspelt `test-xamples.R` happened to
# sort after `test-setup.R` where the correct spelling would have sorted before
# it. Renaming the file to `test-examples.R` broke the suite (#224).
#
# Seeding it here removes the ordering dependency: helper files are sourced
# before any test file runs. `config/taxon_list-orig.csv` is the committed
# fixture that `test-setup.R` copies over the generated file anyway, so nothing
# about what is tested changes.
invisible(
  file.copy("config/taxon_list-orig.csv", "config/taxon_list.csv", overwrite = TRUE)
)
