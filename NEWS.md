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

This is the first major release of the {traits.build} package, providing a workflow to harmonise trait data from diverse sources. The code was originally built to support AusTraits (see Falster et al 2021, <doi:10.1038/s41597-021-01006-6>, <https://github.com/traitecoevo/autraits.build>) and has been generalised here to support construction of other trait databases. Detailed instructions are available at

- package website: <https://traitecoevo.github.io/traits.build/>
- package book: <https://traitecoevo.github.io/traits.build-book/>

