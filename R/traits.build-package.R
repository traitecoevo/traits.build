#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom crayon %+%
#' @importFrom crayon blue
#' @importFrom crayon green
#' @importFrom crayon red
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
utils::globalVariables(
  c(
    ".",
    ".data",
    "..density..",
    # tidy-eval column names and unqualified `dplyr`/`austraits` calls used
    # unquoted in `R/reports.R` and `R/plot_trait_values.R` -- both packages
    # are in Depends, so these resolve at runtime; this only silences the
    # static-analysis NOTE that can't see that
    "Group",
    "colour",
    "combined",
    "counts",
    "counts_per_value",
    "dataset_id",
    "extract_dataset",
    "extract_trait",
    "family",
    "family_order",
    "group_by",
    "left_join",
    "n",
    "n_taxa",
    "prop",
    "row_counter",
    "row_number",
    "scaled_by_obs",
    "shapes",
    "species_per_family",
    "taxon_name",
    "taxon_rank",
    "taxonomic_status",
    "total",
    "total_counts",
    "total_per_obs",
    "trait_name",
    "ungroup",
    "value",
    "value_type"
  )
)
