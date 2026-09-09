# traits.build (development version)

- Added a Claude Code skill, `traits-build-add-dataset`, that drives the add-a-dataset workflow (folder + `data.csv` + `metadata.yml` scaffolding, the test/build/report loop, and review gates) in a database repo. Ships under `inst/skills/` and installs into `.claude/skills/` with the new exported `use_traits_build_skills()`.
- `new_taxa_trait_combinations()` (`R/reports.R`) is now exported, so it can be reached without `:::`. It reads `database$taxa` rather than taking APCalign `resources`, so the "what did this dataset add" report works in `ausinvertraits.build` and `AusFizz`, not just `austraits.build`.
- A context populated by literals set on the metadata `traits` entries no longer has to spell out a `values:` list. Omitting `values:` derives the context values from whatever `var_in` names, and that now covers both of the ways a `var_in` is legitimately satisfied: a column of `data.csv`, or a field set on the `traits` entries naming no column at all (`var_in: method_context` and friends). Previously only the first was, so the second read a `data.csv` column that was never meant to exist and aborted with `rlang_error_data_pronoun_not_found`, naming neither the dataset, the `context_property` nor the `var_in` (#268). Descriptions are still hand-written. A `var_in` matching neither is still rejected, and the message now lists both the columns and the `traits` fields available rather than reading as though a column were the only option.
- A dataset may now keep its locations in a csv file beside `metadata.yml` rather than listing them in it, by setting `locations` to the file's name — `locations: locations.csv`. The file holds one row per location, a `location_name` column and one further column per location property, which is the shape `metadata_add_locations()` is given and the shape `austraits$locations` pivots to. This is for datasets compiled from per-record georeferenced occurrences, which generate one location per record: `AVH_2026` in `austraits.build` has 162,469 of them, making `metadata.yml` 487,518 lines and 17.7 MB, which is neither reviewable nor diffable. Holding them in a csv takes that file to 111 lines, and the dataset's `dataset_configure()` plus `dataset_process()` from about 16 minutes to 17 seconds (#263). Both forms build the same database, and inline `locations:` blocks are unaffected — nothing needs to be converted.
- In a locations csv, an empty cell means the location does not record that property, and `.na` means it is recorded but unknown — the same two states an inline `locations:` block distinguishes by listing a property or not. `metadata_add_locations()` gained a `file` argument for writing this form, and suggests it for a dataset with more than 1000 locations. `build_setup_pipeline()` declares the csv as a dependency of the dataset's `remake` and `targets` targets, so editing it triggers a rebuild.
- `dataset_test()` now decides whether a dataset has locations the same way the build does, from the formatted locations table rather than from `names(metadata$locations)`. The two disagreed for a dataset whose entire `locations:` block held a single property, where `dataset_test()` would join against an empty table that the build had already discarded.
- Reading a `metadata.yml` with a very large `locations:` block is no longer the dominant cost of building a dataset. `yaml::read_yaml()` costs roughly O(n^2) in the number of container nodes a document holds rather than in its size, so a dataset that generates one location per georeferenced record parses far slower than the trait data it describes: `AVH_2026` in `austraits.build`, with 162,469 locations, took 911 seconds to read (#263). `read_metadata()` now parses a large `locations:` block in chunks, which brings that file down to 1.8 seconds and the whole dataset's `dataset_configure()` plus `dataset_process()` from about 16 minutes to 46 seconds. Every chunk still goes through the same YAML parser, and anything unexpected about a file's structure falls back to parsing it whole, so no file is read differently than before — verified against all 574 `metadata.yml` files in `austraits.build` and `ausinvertraits.build`.
- `build_setup_pipeline()` gained `method = "targets"`, which writes a `_targets.R` pipeline. Like `remake` it caches, so editing one dataset rebuilds that dataset rather than all of them — on a 40-dataset compilation a cold build is 33.5s, a rebuild after changing one `data.csv` is 6.9s, and a run with nothing changed is 0.9s. Unlike `remake` it is on CRAN and maintained, and unlike `furrr` it does not rebuild everything. It caches on target *values*, so a metadata edit that does not change a dataset's output stops propagating there rather than rebuilding the compilation. Passing `workers` greater than 1 declares a `crew` controller and builds datasets in parallel. Where `qs2` is installed the pipeline stores its targets with it rather than as gzipped rds, which is internal to `targets` and leaves the exported `.rds` that consumers read unchanged; on that 40-dataset compilation the two together take a cold build to 24.6s and a one-dataset rebuild to 3.6s. The exported `.rds` is not written by `tar_make()` at all: it is around a third of a rebuild and is a publishing step rather than part of checking a dataset, so `build_export()` writes it when it is wanted. That takes a one-dataset rebuild to 2.2s, against 154s for a full build of `austraits.build`. Verified to produce a database equal to the one `method = "base"` produces (#17). Building from `_targets.R` is three steps rather than one, because `tar_make()` neither returns the database nor writes the export, so `build_setup_pipeline()` now names all three — including the `targets::tar_read()` that loads the result — using the `database_name` actually in use, and the generated `_targets.R` says the same at the top. `build_export()` called with a `database_name` the pipeline does not have now explains that the name must match the one the pipeline was set up with, rather than reporting only `target database not found`.
- `dataset_test()` no longer aborts with `the condition has length > 1` for a dataset that declares more than one identifier. `metadata$identifiers` is a list, so `is.na()` on it returns one value per identifier.
- Builds are now reproducible across machines. `observation_id`, `location_id` and the context ids were generated by sorting with `sort()` and `as.factor()`, which collate according to the session's `LC_COLLATE`. The same dataset therefore produced different ids on a contributor's machine than on CI, which runs in the C locale (#29). Ids are now always generated using C-locale collation, matching what previous builds produced on CI, so existing outputs are unchanged. `util_separate_and_sort()` is likewise no longer locale-dependent.
- `process_format_identifiers()` no longer fails with `object 'schema' not found`. Its third argument is the schema, as documented, rather than the trait data, and it now reads the identifiers list it is passed instead of looking one level too deep. `dataset_test()` was erroring on every dataset that declares identifiers.
- A context whose `var_in` names a column that isn't there is now reported as such, naming the dataset, the `context_property` and the missing column, and suggesting the nearest column name. It previously aborted with ``Assigned data `xxx[context_cols[[v]]]` must be compatible with existing data``, which named none of the three and read as a data-length problem rather than a typo (#247). A `var_in` that names a field on the metadata `traits` entries rather than a column of `data.csv` — `var_in: method_context` and friends — is still accepted, as is one created by `custom_R_code`.
- `metadata_add_contexts()` no longer writes time-valued contexts as a number of seconds (`9:00:00` became `32400.0`) when called with `user_responses`, and now reports when a time column has been reformatted so the recorded values can be recognised as matching `data.csv` (#49).
- `metadata_add_contexts()`, `metadata_add_traits()` and `metadata_add_identifiers()` now guess column types from the same number of rows as the build (`guess_max = 100000`), so a sparse column can no longer be typed one way when metadata is written and another way when the database is built.
- `metadata_add_source_doi()` now reports clearly that the optional `rcrossref` package is needed, and offers to install it in interactive sessions, instead of failing with `there is no package called 'rcrossref'` (#178).
- Corrected the link to the AusTraits source repository, which had been misspelled `autraits.build` in `DESCRIPTION`, the package documentation and `NEWS.md`, and pointed at a URL that did not resolve. The Code of Conduct and reference website links in `README.md` were also dead or redirecting, and now resolve directly.
- Added `CITATION.cff` and `inst/CITATION`, so `citation("traits.build")` and GitHub's "Cite this repository" widget both return the Wenk et al. (2024) paper. `CITATION.cff` is excluded from the package tarball via `.Rbuildignore`.
- Removed a duplicated "AusTraits family" section from `README.md`, which had been added twice.

# traits.build 2.1.0

- Identifiers table added, allowing trait values to be linked to a specific identifiers in an herbarium, museum collection, GenBank, or an arboretum. If a data contributor has collected data on the same individual plants across multiple datasets, these can also be linked.
- Methods table documents the dataset's Bibtex types, whether the data are from a Journal article, Online resource, Unpublished dataset, Thesis, etc.
- Additional entity_type values added to schema, included `standard_error`, `standard_deviation`.
- A collection of minor errors have been fixed - including empty datasets breaking the build process, and the wrong location name column being read in

# traits.build 2.0.0

- traits.build paper published in Sep 2024 in Ecological Informatics (DOI: [10.1016/j.ecoinf.2024.102773](https://doi.org/10.1016/j.ecoinf.2024.102773))
- Added standard error and standard deviation as value types
- Moved functions to austraits package and made austraits package a dependancy
- Renamed some of the functions that are now moved to austraits package 
    * `bind_databases` <-- `build_combine`
    * `convert_df_to_list` <-- `util_df_to_list`
    * `convert_list_to_df1` <-- `util_list_to_df1`
    * `convert_list_to_df2` <-- `util_list_to_df2`
- Renamed functions still *also* assigned their old name, with a deprecation warning indicating the new name  
- `plot_trait_distribution_beeswarm`, `trait_pivot_longer` and `trait_pivot_wider` had been in both austraits and traits.build packages and have now been removed from traits.build
- Import new austraits function `flatten_database` (had been suggested to be `database_create_combined_table`)
- Refactoring of test functions used by `dataset_test`
- Added tests using the `dataset_test` function, so it is checked explicitly by traits.build (run on Example datasets)
- Minor bug fixes
- Minor updates to ontology (now version 1.0)

# traits.build 1.1.0

- Small bugfixes in dataset_test
- Add Onotology
- Add Hex sticker


# traits.build 1.0.1

As described in #134, fixes some minor issues with 

- testing of datasets in `dataset_test`
- generating of reports
- standardising of taxonomic names. 

# traits.build 1.0.0

This is the first major release of the {traits.build} package, providing a workflow to harmonise trait data from diverse sources. The code was originally built to support AusTraits (see Falster et al 2021, <doi:10.1038/s41597-021-01006-6>, <https://github.com/traitecoevo/austraits.build>) and has been generalised here to support construction of other trait databases. Detailed instructions are available at

- package website: <https://traitecoevo.github.io/traits.build/>
- package book: <https://traitecoevo.github.io/traits.build-book/>

