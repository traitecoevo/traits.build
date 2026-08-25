# Troubleshooting

Reference for GATE 2 and the Phase 5/6 test loop. Organised for lookup, not
reading start to end: the `excluded_data` catalogue below is exhaustive (every
distinct `error` string the build can write, found by grepping `R/process.R`
for `error = ` literals), then common failures from `data_common_issues.qmd`,
then a symptom index. Every error string is quoted exactly as it appears in
source, so it can be grepped against real build output or matched against
`database$excluded_data$error`.

## The `excluded_data` error catalogue

`database$excluded_data` holds every trait observation the build parsed but
did not admit to `database$traits`, one row per observation, with the reason
in `error`. There are exactly eleven distinct `error` values, all assigned in
`R/process.R`. None of this table is a build failure -- `dataset_test()` and
the build both complete; these rows are excluded data to review at GATE 2, not
errors that stop anything.

| `error` string | `R/process.R` | Trigger | Expected or a problem? | Fix |
|---|---|---|---|---|
| `"Trait name not in trait dictionary"` | `process_flag_unsupported_traits()`, line 973 | `trait_name` (post-mapping) is not a key in `config/traits.yml` | Real problem -- either a typo in `trait_mapping.csv`'s `trait_name` cell, or a genuinely new concept | Fix the mapping, or propose a new concept in `config/traits.yml` at GATE 2 (database-wide, needs approval) |
| `"Observation excluded in metadata"` | `process_flag_excluded_observations()`, lines 1012 & 1019 | Value matches a `find` entry under `metadata.yml`'s `exclude_observations` | Expected -- this is the mechanism for deliberately dropping specific values, not an error | None needed; this is a deliberate exclusion. When scanning excluded_data for *unresolved* problems, filter this error out first (see curator-recipes.md's substitutions round-trip) |
| `"Value contains unsupported characters"` | `process_flag_unsupported_characters()`, lines 1064-1065 | `util_check_disallowed_chars()` finds any byte >= `0x7F` in `value` -- data values must be pure ASCII, checked before unit conversion | Real problem | Fix at the source: correct `data.csv` (if the character is a typo/encoding artefact) or add a `custom_R_code` substitution. See "Trait values must be ASCII" below -- this rule is stricter than the metadata.yml character rule |
| `"Missing value"` | `process_flag_unsupported_values()`, line 1163 | `value` is `NA` after parsing | Usually expected (genuine missing data in the source) | None needed if the source really has no value there. If unexpectedly common, check the pivot/column mapping didn't misalign |
| `"Missing species name"` | `process_flag_unsupported_values()`, line 1164 | `taxon_name` is `NA` | Real problem unless the row is legitimately unidentified | Check the taxon-name column mapping; verify `custom_R_code` populated `taxon_name` for every row |
| `"Unsupported trait value"` | `process_flag_unsupported_values()`, line 1181 | For a `categorical` trait, `value` is not among `definitions[[trait]]$allowed_values_levels` in `config/traits.yml` | Real problem -- almost always needs a substitution | `metadata_add_substitution()` / `metadata_add_substitutions_table()` (see below); confirm with `dataset_check_categorical_substitutions()` |
| `"Time can only contain Y & Ns"` | `process_flag_unsupported_values()`, line 1192 | `flowering_time` / `fruiting_time` value contains characters other than `Y`/`y`/`N`/`n` | Real problem | Fix the phenology-string encoding, usually in `custom_R_code` |
| `"Times must be length 12"` | `process_flag_unsupported_values()`, line 1197 | `flowering_time` / `fruiting_time` value is not exactly 12 characters (one per month) | Real problem | Fix the phenology-string construction |
| `"Value does not convert to numeric"` | `process_flag_unsupported_values()`, line 1213 | For a `numeric` trait, `as.numeric(value)` is `NA` (and it isn't a `range`/`bin` value_type written as `lo--hi`) | Real problem -- usually a stray unit suffix, missing-value code (e.g. `"NA"`, `"-"`, `"999"`) not caught by a substitution, or a genuine typo | Substitution, or fix the missing-value code before it reaches `data.csv`'s trait column |
| `"Value out of allowable range"` | `process_flag_out_of_range_values()`, line 1246 | Numeric value falls outside `definitions[[trait]]$allowed_values_min`/`max` in `config/traits.yml` | **A long list here is almost always a units error, not real outliers** -- confirm `unit_in` before treating any as legitimate | Check unit mapping first; if values are genuinely out of range, propose widening the range in `config/traits.yml` at GATE 2 (database-wide, needs approval). Confirm with `dataset_check_numeric_values()` |
| `"Missing unit conversion"` | `process_convert_units()`, line 1322 | `unit_in` -> trait's target unit has no entry in `config/unit_conversions.csv` (or the trait is unrecognised, so `to` can't be resolved) | Real problem | Add the conversion to `config/unit_conversions.csv` at GATE 2 (database-wide, needs approval); or fix a wrong `unit_in` in `trait_mapping.csv`. Cross-check against `units_index.tsv` from `trait_index.R` before the build even runs |

Two of the eleven ("Observation excluded in metadata", "Missing value") are
routinely benign; the rest are worth a look every time they appear.

### No dedicated `dataset_check_*` for most of these

`scripts/dataset_checks.R` only wraps two of the eleven directly
(`dataset_check_categorical_substitutions()` for `"Unsupported trait value"`,
`dataset_check_numeric_values()` for `"Value out of allowable range"`). For
the other nine, filter `excluded_data` directly:

```r
database$excluded_data %>%
  dplyr::filter(dataset_id == current_study, error == "Value does not convert to numeric")
```

or scan everything not-yet-resolved at once (this is the GATE 2 / Phase 6
substitutions-round-trip idiom from `curator-recipes.md`):

```r
database$excluded_data %>%
  dplyr::filter(dataset_id == current_study, error != "Observation excluded in metadata") %>%
  dplyr::count(error, sort = TRUE)
```

## Common failures (from `data_common_issues.qmd`)

### Unsupported categorical values

The book's worked fix, in order of increasing scale:

1. One-off: `metadata_add_substitution(dataset_id, trait_name, find, replace)`.
2. A handful: build a table with columns `dataset_id`, `trait_name`, `find`,
   `replace` and call `metadata_add_substitutions_table()`.
3. Many: `write_csv()` the table, edit `replace` in Excel/a text editor, read
   it back, then `metadata_add_substitutions_table()` -- this is the shape
   GATE 1's `substitutions.csv` and Phase 6's round-trip generalise.

```r
metadata_add_substitution(
  dataset_id = current_study,
  trait_name = "plant_growth_form",
  find = "T",
  replace = "tree"
)
```

`metadata_add_substitutions_table()` requires all four columns
(`dataset_id`, `trait_name`, `find`, `replace`) to exist by those exact names
in the input data frame or it throws
`'<col>' is not a column in the substitutions table` (`R/setup.R:916`).

### Dataset can't pivot wider

`dataset_test()` runs `check_pivot_wider()` (`R/pivot.R`), which requires each
row of `database$traits` to have a unique combination of seven columns:
`dataset_id`, `trait_name`, `observation_id`, `value_type`,
`repeat_measurements_id`, `method_id`, `method_context_id`. A collision on
that combination means the table cannot pivot wider, and `dataset_test()`
fails with:

```
duplicate rows detected; `traits` table cannot pivot wider
```

(`R/testdata.R:904`; the same check also runs a second time as a
`test_expect_no_warning()`, `R/testdata.R:914`.)

Two documented causes:

1. **Individual/population-level measurements mixed with species-level
   measurements** -- a species-level value gets read in once per individual,
   duplicating it. Fix with a `custom_R_code` de-duplication:

   ```r
   data %>%
     group_by(taxon_name) %>%
     mutate(across(c("column 1", "column 2", "column 3"), replace_duplicates_with_NA)) %>%
     ungroup()
   ```

   `replace_duplicates_with_NA` is **not** a `traits.build` function -- it
   lives in the host database repo's `R/custom_R_code.R` (confirmed absent
   from this package's own `R/`). Check `probe_repo.R`'s report before
   assuming it exists; `ausinvertraits.build` and `AusFizz` may not have it.

2. **Rows representing measurements at different times** not distinguished by
   a temporal context or `repeat_measurements_id` -- the book marks this
   "TBC" (unfinished in the source). In practice this usually means a date or
   sampling-round column needs to become a `temporal` context, or
   `repeat_measurements_id` needs to be populated from it.

To find the actual offending rows rather than just get a pass/fail, use
`check_pivot_duplicates(database, dataset_ids)` (`R/process.R:2261`, exported)
against a full build, or `dataset_check_not_pivoting()` from
`scripts/dataset_checks.R` against a single-dataset build -- both return the
duplicate rows with `number_of_duplicates`, `taxon_name`, `original_name`,
`observation_id` attached so the cause is visible immediately rather than
needing to be guessed from the raw pivot failure.

`dataset_check_duplicates_within_dataset()` is a related but weaker signal:
same `(taxon_name, trait_name, entity_type, value)` occurring more than once
within a dataset. This is *expected*, not suspicious, for numeric traits
reported to few significant figures or one bulked measurement reported
against many individuals -- only worth investigating when `n_duplicates` is
large or identical across every taxon for one trait, in which case the fix is
the same `custom_R_code` de-duplication as above, not a metadata edit.

## Disallowed / encoding issues

This is the single most common failure class across the databases and is
covered as a non-negotiable elsewhere in this skill; restated here as the
troubleshooting entry point.

**The rule differs by file.** `data.csv` values must be pure ASCII --
`util_check_disallowed_chars()` (`R/process.R:1034`) flags anything with a
byte >= `0x7F`, no exceptions, and this becomes the `"Value contains
unsupported characters"` excluded_data row. `metadata.yml` allows a defined
set of non-ASCII characters (accented letters, `°`, `±`, `×`, en/em dash,
curly quotes, `~`, `≈`, `≤`, `≥`, …) via `check_disallowed_chars()`
(`R/test_functions.R:285`), built from the `allowed_characters` section of
`inst/support/traits.build_schema.yml`. Don't conflate the two rules: a
character legal in `metadata.yml` can still fail in `data.csv`.

**Known confusables, with known fixes** (`util_disallowed_char_replacements()`,
`R/replace_chars.R:24`):

| Found | Should be | Why |
|---|---|---|
| `º` (U+00BA MASCULINE ORDINAL INDICATOR) | `°` (U+00B0 DEGREE SIGN) | Commonest disallowed character across the databases; schema explicitly calls this out as "used in error for 00B0 DEGREE SIGN" |
| `◦` (U+25E6 WHITE BULLET) | `°` | Visual look-alike for degree sign |
| `∼` (U+223C TILDE OPERATOR) is allowed as-is (meaning "approximately"); a raw `~` used for the same meaning is fine too | -- | Not a fix case, just don't over-correct these |
| `¬†` (bytes `C2 A0` = U+00A0 NBSP, re-decoded as Mac OS Roman) | ` ` (plain space) | Mojibake from a UTF-8 non-breaking space written back out under the wrong encoding. **Replace longest-key-first** -- `util_replace_disallowed_chars()` sorts replacements by descending key length before applying them, because a per-character pass on the two bytes separately reproduces the mangling rather than fixing it |
| U+00A0 NBSP, U+2007, U+2009, U+202F (various space-like characters) | ` ` | Occupy space but aren't a space |
| U+200B ZERO WIDTH SPACE, U+200D ZERO WIDTH JOINER | (removed) | Invisible, no content |
| `μ` (U+03BC GREEK SMALL LETTER MU) | `µ` (U+00B5 MICRO SIGN) | Visually identical; only MICRO SIGN is allowed so a unit isn't written two different ways |

**Never rewritten, because they're meaningful, not errors:** author names and
real symbols -- `ñ`, `š`, `Ó`, `‰` (PER MILLE, used for isotope composition),
`′`/`″` (PRIME/DOUBLE PRIME, arcminutes/arcseconds in coordinates). Letters are
never in the replacement map for exactly this reason -- an unexpected letter
is either a genuine accented name or a mangled symbol, and only a human can
tell which.

**Run `dataset_replace_disallowed_chars(dataset_id)`** (`R/replace_chars.R:203`)
to apply the known replacements automatically and report what it couldn't
resolve. Defaults to `metadata.yml` only and `dry_run = TRUE`; read the report
before passing `files = c("metadata.yml", "data.csv")` or `dry_run = FALSE` --
across the databases this found 95 disallowed characters in `metadata.yml`
files but 170,946 in `data.csv` files (mostly invisible junk in provenance
columns), and `dataset_test()`'s ASCII rule for data almost never surfaces
that volume on its own. The report's `status` column is one of `"replaced"`,
`"no replacement known"` (a genuine character or an uninterpretable mangling
-- needs a human), or `"file not valid UTF-8"` (the file is skipped; recovering
it needs knowing the original encoding, which only the curator has).

Encoding problems that prevent a file from being read as UTF-8 at all belong
to Phase 2 (`data.csv` fidelity), not this reference -- see the plan's list of
legitimate reasons to depart from the provided file. 13 `data.csv` files
across the databases are not valid UTF-8 as supplied.

## Quick-diagnosis index

Scan by symptom.

**`dataset_test()` fails with a `stop()`, before any testthat assertion runs**
(these abort the whole test, not just one dataset's checks):

| Message (verbatim, may include the actual dataset id / column names inline) | Source | Cause |
|---|---|---|
| `Dataset <id> declares identifiers with 'var_in' naming <cols> not present in the data: <cols>.\n  Available columns: <cols>\n  If the column is meant to be created by 'custom_R_code', check that it runs and spells the name identically.` | `R/process.R:116` | `identifiers` block's `var_in` names a column that doesn't exist in `data` after `custom_R_code` runs -- typo in the column name, or `custom_R_code` didn't run / didn't create it |
| `<dataset_id> : missing traits: <cols>` | `R/process.R:1528` | For wide-format data, a `trait_mapping.csv` / metadata `var_in` names a column not in `data.csv`'s header -- typo, or the column really is absent |
| `'<col>' is not a column in the substitutions table` | `R/setup.R:916` | `metadata_add_substitutions_table()` called on a data frame missing one of `dataset_id`, `trait_name`, `find`, `replace` by that exact name |
| `Cannot replace with two names! (for '<find>' -> '<replace>')` | `R/setup.R:1013` | `metadata_add_taxonomic_change()` called with a `replace` vector of length > 1 -- one taxon must map to exactly one name |
| `` `user_responses` needs one `identifier_type` per `var_in` column `` | `R/setup.R:654` | `metadata_add_identifiers(..., user_responses = list(var_in = ..., identifier_type = ...))` -- the two vectors have different lengths |
| `Invalid method selected in 'build_setup_pipeline': <method>` | `R/setup.R:1315` | `build_setup_pipeline(method = ...)` given something other than `"base"`, `"remake"`, `"furrr"` |
| `cannot find data directory: data` | `R/setup.R:1321` | `build_setup_pipeline()` run from somewhere other than the database repo root, or `data/` doesn't exist |

**`dataset_test()` runs but reports failed testthat expectations:**

| Symptom | Cause | Confirm with |
|---|---|---|
| `disallowed characters in data detected` / `disallowed characters in <label> detected` | ASCII-only rule (data) or schema `allowed_characters` rule (metadata) tripped | `dataset_replace_disallowed_chars(dataset_id)` with `dry_run = TRUE` |
| `duplicate rows detected; 'traits' table cannot pivot wider` | See "Dataset can't pivot wider" above | `check_pivot_duplicates(database, dataset_ids)` or `dataset_check_not_pivoting()` |
| `'traits' table is empty` | Every row got excluded, or `trait_name` mapping produced no matches | `database$excluded_data %>% filter(dataset_id == current_study) %>% count(error)` |
| `building dataset` threw an error (via `test_expect_no_error`) | Usually a `custom_R_code` error, or a malformed `metadata.yml` field | Run `metadata_check_custom_R_code(dataset_id)` directly to isolate whether it's the code or the build around it |

**`excluded_data` is unexpectedly long:**

- Filter out `"Observation excluded in metadata"` first (that's deliberate,
  see catalogue above) and `count(error, sort = TRUE)` what's left.
- A long `"Value out of allowable range"` list is a units problem, not real
  outliers -- check `unit_in` against `units_index.tsv` before touching
  `config/traits.yml`.
- A long `"Unsupported trait value"` list for one trait usually means the
  categorical mapping needs a handful of substitutions, not that the trait
  choice is wrong.
- Many `"Value does not convert to numeric"` rows for one trait usually means
  an un-substituted missing-value code (`"NA"`, `"-"`, `"999"`, `"n/a"`) is
  reaching the numeric parser.

**Characters flagged as disallowed:**

- See the confusables table above for the specific character.
- If it's not in that table, decide: genuine content (add to `custom-r-code.md`
  patterns or leave as-is if it's metadata prose) vs. a mangling only the
  curator can interpret (`dataset_replace_disallowed_chars()` reports it as
  `"no replacement known"`).
- Author names and real symbolic characters (`ñ`, `š`, `‰`, `Ó`, `′`, `″`) are
  never wrong -- don't "fix" them.

**The pivot check fails:**

- Run `check_pivot_duplicates(database, current_study)` or
  `dataset_check_not_pivoting(database, current_study)` to see the actual
  duplicate rows rather than the bare pass/fail.
- Species-level values duplicated across individuals -> `custom_R_code`
  de-duplication with `replace_duplicates_with_NA` (host-repo function, check
  `probe_repo.R` first).
- Different sampling times not distinguished -> needs a temporal context or
  `repeat_measurements_id`, per dataset specifics (the book leaves this case
  unfinished; there's no single template fix).

## Not verifiable from source

- The exact eleven-row completeness of the `excluded_data` catalogue is
  confirmed by grep against this version of `R/process.R`; a future version
  could add more `error =` assignments elsewhere. Re-grep
  (`grep -n 'error = ' R/process.R`) before trusting this table against a
  different package version.
- `references/custom-r-code.md`'s exact pattern catalogue (the eleven worked
  patterns named in the plan) is a sibling file not read while writing this
  one -- this document names `replace_duplicates_with_NA` because it appears
  verbatim in `data_common_issues.qmd` and is confirmed absent from this
  package's `R/`, but does not describe the other ten patterns.
- `dataset_check_duplicates_across_datasets()` is documented in
  `check_dataset_functions.qmd` as "TO BE WRITTEN" and unimplemented; there is
  no function to reference for cross-dataset duplicates.
- The book's "different times" pivot-failure case is marked "TBC" in
  `data_common_issues.qmd` itself -- no worked fix exists in source to
  condense here beyond "needs a temporal context or `repeat_measurements_id`".
