# Curator recipes

@ehwenk's working idioms for driving `traits.build` interactively -- the
house practice this skill automates. Preserved here because they are
published nowhere else (not in `traits.build-book`, not in any repo).
Snippets marked **verbatim** are lifted directly from her working script
(`adding datasets scripts.R`); everything else is written for this reference
but follows the same idiom.

The skill drives these same code paths non-interactively through
`scripts/scaffold_metadata.R` and the `metadata_add_*(..., user_responses =
...)` helpers. Read this file when a step needs the interactive/exploratory
form -- checking `excluded_data`, building one dataset for a fast inner
loop, taxonomy alignment, or when something needs a human eyeballing a
`View()` before it goes into a `user_responses` list.

## Load and build

**verbatim**, adapted for a generic database repo (her script hard-codes
`austraits`):

```r
devtools::load_all("../traits.build/")   # dev version of the package
build_setup_pipeline(method = "remake", database_name = "<db>")
database <- remake::make("<db>")          # full build
```

For working on a single dataset, `remake::make(current_study)` builds just
that dataset far faster than a full rebuild -- this is the fast inner loop
for Phase 5/6, not `remake::make("<db>")`:

```r
current_study <- "Wright_2023"
tmp <- remake::make(current_study)
tmp$excluded_data
```

## Always pass `partial_matches_allowed = FALSE`

Her script is inconsistent about this across lines -- some calls have the
flag, some don't -- which is exactly why the skill treats it as a
non-negotiable rather than a habit. Without it, `extract_dataset("Bryant_2021")`
also pulls in `Bryant_2021_2` and `Bryant_2021_3`, silently inflating every
check downstream:

```r
current_dataset <- database %>% extract_dataset(current_study, partial_matches_allowed = FALSE)
```

## Checking what a dataset actually produced

**verbatim** idiom -- `View()` is how she inspects intermediate state; an
agent should read the same tables non-interactively (`dplyr::filter()` +
`print()`, or write to a scratch CSV):

```r
database$traits %>% filter(dataset_id == current_study) %>% View()
database$excluded_data %>% filter(dataset_id == current_study) %>% View()
database$excluded_data %>% filter(stringr::str_detect(error, "unit")) %>% View()
```

## Substitutions round-trip

**verbatim** structure. Her script writes the CSV to `data/<id>/raw/`; the
skill writes it to a scratch directory instead (GATE 1 -- under the `raw/`
policy that folder may not exist for a dataset that didn't need one). The
column shape and the two-step edit-then-reread pattern are otherwise
unchanged:

```r
database$excluded_data %>%
  dplyr::filter(dataset_id == current_study) %>%
  dplyr::filter(error != "Observation excluded in metadata") %>%
  dplyr::distinct(trait_name, value) %>%
  dplyr::arrange(trait_name, value) %>%
  dplyr::rename(find = value) %>%
  dplyr::mutate(replace = NA) %>%
  readr::write_csv("<scratch>/substitutions.csv")

# curator fills `replace` in place, then:
substitutions <- readr::read_csv("<scratch>/substitutions.csv")
metadata_add_substitutions_list(current_study, substitutions)
```

## Taxonomy round-trip

Her script has two variants of the alignment filter, from different points
in her practice. The plan for this skill adopts the *second* (more recent)
one as canonical -- filtering on `original_name != cleaned_name` is more
robust than enumerating specific `alignment_code`s, since new alignment
codes get added to APCalign over time and an enumerated filter silently
stops catching them.

**Canonical (verbatim, her later form):**

```r
library(APCalign)
resources <- load_taxonomic_resources()

taxa_to_check <- (database %>% extract_dataset(current_study, partial_matches_allowed = FALSE))$traits %>%
  distinct(taxon_name)

checked_taxa <- align_taxa(taxa_to_check$taxon_name, resources = resources, identifier = current_study) %>%
  arrange(alignment_code, original_name) %>%
  filter(original_name != cleaned_name) %>%
  filter(!stringr::str_detect(original_name, "\\["))
```

The `!str_detect(original_name, "\\[")` filter drops names that already
carry a `dataset_id` in square brackets -- these are already aligned and
must not be re-aligned; list them at the gate as "already aligned" rather
than silently dropping them.

**Superseded (verbatim, her earlier form)** -- kept here only because it
shows the alternative approach was tried and abandoned; don't use this one:

```r
checked_taxa <- align_taxa(taxa_to_check$taxon_name, resources = resources, identifier = current_study) %>%
  arrange(alignment_code, original_name) %>%
  filter(!stringr::str_detect(alignment_code, "match_01c"), !stringr::str_detect(alignment_code, "match_01d")) %>%
  filter(!stringr::str_detect(original_name, "\\["))
```

Writing the shortlist for confirmation, then applying it once the curator
has confirmed (`aligned_name` is a machine proposal, never applied directly
-- see GATE 2):

```r
checked_taxa %>% readr::write_csv("<scratch>/taxonomic_updates.csv")

taxa_subs <- checked_taxa %>%
  select(find = original_name, replace = aligned_name, reason = aligned_reason,
         taxonomic_resolution = taxon_rank)

metadata_add_taxonomic_changes_list(current_study, taxa_subs)
```

## Locations straight off `data.csv`

**verbatim** (from her `Greening_Australia_2025` line) -- the general shape
for Phase 3's first option (columns already in the main data file):

```r
readr::read_csv("data/<id>/data.csv") %>%
  dplyr::distinct(locality, `Decimal Latitude`, `Decimal Longitude`) %>%
  dplyr::rename(`latitude (deg)` = `Decimal Latitude`, `longitude (deg)` = `Decimal Longitude`) -> locations

metadata_add_locations(current_study, locations, user_responses = list(location_name = "locality", keep = c("latitude (deg)", "longitude (deg)")))
```

## DMS -> decimal degrees

Her notes carry recurring scratch fragments like `42° 49.4'S, 147° 30.6'E`
with no committed conversion snippet -- converting these by hand is a
recurring chore, not a solved one. This is the conversion, not hers
verbatim: degrees + minutes/60, sign flipped for `S`/`W`. Keep the original
string in the notes alongside the converted value so the conversion is
checkable, per Phase 3.

```r
dms_to_decimal <- function(deg, min, hemisphere) {
  sign <- ifelse(toupper(hemisphere) %in% c("S", "W"), -1, 1)
  sign * (deg + min / 60)
}
# "42° 49.4'S" -> dms_to_decimal(42, 49.4, "S")  ==  -42.82333
# "147° 30.6'E" -> dms_to_decimal(147, 30.6, "E") ==  147.51
```

## The test/build/report loop, and where it breaks

**verbatim** shape -- `dataset_test()` first, then narrow a pivot failure
with the two functions below before reaching for `dataset_checks.R`:

```r
dataset_test(current_study)

check_pivot_duplicates(database, current_study)
check_pivot_wider(current_dataset)
```

`check_new_taxa()` / `check_new_taxa_accepted()` are `austraits.build`-only
(see `scripts/probe_repo.R`); where they're absent, use
`new_taxa_trait_combinations()` from `traits.build`'s `R/reports.R` instead
-- it reads `database$taxa` rather than taking APCalign `resources`, so it
works in all three database repos (see Phase 7 in the plan):

```r
check_new_taxa(database, current_study)                       # austraits.build only
check_new_taxa_accepted(database, current_study, resources)   # austraits.build only
new_taxa_trait_combinations(database, current_study)           # works everywhere
```

**Two argument-order/flag traps, both confirmed against her script:**

- `dataset_report(dataset_id, austraits, ...)` -- `dataset_id` first,
  `austraits`/`database` second. `adding_data_long.qmd:1332` shows them
  reversed; the book is wrong, not the function.
- Batch-reporting a compilation that got split into `_2`/`_3` suffixes takes
  a vector of `dataset_id`s in one call, not one call per id:

```r
dataset_report(current_study, database, overwrite = TRUE)
dataset_report(c("Bryant_2021", "Bryant_2021_2", "Bryant_2021_3", "Bryant_2024"), database, overwrite = TRUE)
```
