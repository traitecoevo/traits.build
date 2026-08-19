# A Claude skill for importing a dataset into a traits.build database

*Plan dated 2026-08-19. Branch: `feature/add-dataset-skill`, cut from `update-AusTraits-report`.*

## Context

Adding a new source to a `traits.build` database (`austraits.build`, `ausinvertraits.build`, `AusFizz`) is a hand process: given one or more data files plus a paper/report PDF, a curator creates `data/<dataset_id>/`, writes `data.csv`, hand-builds `metadata.yml`, sometimes archives the originals under `raw/`, then loops through `dataset_test()` → build → excluded-data review → `dataset_report()`. The procedure is documented — [`adding_data_long.qmd`](https://traitecoevo.github.io/traits.build-book/adding_data_long.html) (1364 lines), seven tutorials, [`check_dataset_functions.qmd`](https://traitecoevo.github.io/traits.build-book/check_dataset_functions.html), [`data_common_issues.qmd`](https://traitecoevo.github.io/traits.build-book/data_common_issues.html) — and there are ~600 worked examples across the three database repos, but none of it is packaged in a form an agent can follow reliably.

**The skill builds the infrastructure; the curator makes the decisions.** The skill does the folder, the near-verbatim `data.csv`, the `metadata.yml` scaffold, the review CSVs with the *from*-side filled in, and the whole test/build/report loop. It does not decide which trait concept a column maps onto, or what a categorical value should be replaced with. Those cells arrive empty and the curator fills them.

Four findings from the exploration shape the design:

- **Every interactive helper accepts `user_responses`.** `metadata_create_template()`, `metadata_add_traits()`, `metadata_add_locations()`, `metadata_add_contexts()`, `metadata_add_identifiers()` all take a named list that substitutes for the `utils::menu()` prompts (`R/setup.R`). The skill drives the sanctioned code path non-interactively rather than hand-writing YAML.
- **`config/traits.yml` is 700 KB / 556 concepts in `austraits.build`.** Even to *show* a curator the plausible options, the skill needs a generated compact index — the dictionary cannot be read into context.
- **@ehwenk's working script** (`adding datasets scripts.R`, unpublished — see the prompt doc for where to get it) is the actual house practice. Its central idiom is that a review step is a CSV the curator edits in Excel, not a chat conversation.
- **Some helpers live in the database repo, not the package.** `check_new_taxa()`, `check_new_taxa_accepted()`, `build_align_taxon_names()`, `build_update_taxon_list()` and the `custom_R_code` helpers are in `austraits.build/R/`. `check_pivot_wider()` is internal here. `extract_dataset()` / `extract_trait()` are in the `austraits` package. The skill must detect what its host repo provides rather than assume or duplicate.

## Decisions already taken

These came from the maintainer during planning and are settled, not open questions.

| Decision | Choice |
|---|---|
| Location | `inst/skills/` (ships with the package) + an exported installer that copies into a database repo's `.claude/skills/` |
| Judgement calls | Skill builds the form with the *from*-side pre-filled; curator fills the *to*-side. Enforced at review gates |
| PDF | Verbatim extraction for `methods` and `sampling_strategy`. The one-sentence `description` is written, not extracted |
| `data.csv` | As close to the provided file as possible, for traceability |
| `raw/` | Archived only when it holds something `data.csv` does not |
| Verification depth | Full loop through `dataset_report()`, with gates before build and before finalising |

## What gets built

### 1. The skill — `inst/skills/traits-build-add-dataset/`

```
SKILL.md                        the workflow, the gates, the non-negotiables
references/metadata-fields.md   field-by-field reference + controlled vocabularies
references/custom-r-code.md     the custom_R_code pattern catalogue
references/curator-recipes.md   @ehwenk's working idioms, verbatim
references/troubleshooting.md   excluded_data error catalogue + common failures
scripts/probe_repo.R            detect which helpers this host repo provides
scripts/trait_index.R           emit a searchable index of this repo's vocabulary
scripts/scaffold_metadata.R     drive the metadata_* helpers from an answers file
scripts/dataset_checks.R        the book's dataset_check_* functions (not exported)
```

**`SKILL.md`** — frontmatter `name: traits-build-add-dataset`, description triggering on "add a dataset", "import this data into AusTraits", "new dataset_id", or a handover of data files plus a paper PDF. Body is the phase/gate workflow below plus the non-negotiables.

**`references/metadata-fields.md`** — condensed from `adding_data_long.qmd`, so the skill need not load a 1364-line chapter. Covers the `source` block shapes (Article/Book/Online/Thesis/Unpublished), `contributors`, every `dataset` field, and the closed vocabularies that cause most errors: `entity_type` (individual/population/species/genus/family/order), `value_type` (mean/minimum/maximum/mode/range/raw/bin), `basis_of_value` (measurement/expert_score/model_derived/literature), `basis_of_record` (field/field_experiment/captive_cultivated/lab/preserved_specimen/literature), `life_stage` (adult/sapling/seedling/juvenile), and the five context categories (treatment/plot/entity/temporal/method).

**`references/custom-r-code.md`** — the eleven worked patterns from the book (NA placeholders, splitting ranges, `replace_duplicates_with_NA`, `move_values_to_new_trait`, mutating `location_name`, date reformatting), plus the rule that `custom_R_code` may not read files. This reference does heavy lifting under the `data.csv` fidelity policy: most reshaping a curator might reflexively do to the CSV belongs here instead.

**`references/curator-recipes.md`** — @ehwenk's idioms, preserved as the canonical snippets since they are published nowhere: the substitutions and taxonomy round-trips (below); locations built straight off `data.csv` with `distinct(locality, lat, long) |> rename("latitude (deg)" = ..., "longitude (deg)" = ...)`; `remake::make(current_study)` to build one dataset and read `tmp$excluded_data`; `extract_dataset(current_study, partial_matches_allowed = FALSE)`; and DMS → decimal degrees conversion (`42° 49.4'S, 147° 30.6'E` is a recurring chore in her notes).

**`scripts/probe_repo.R`** — run first. Reports which of these the host repo provides, so the skill uses the repo's own code instead of duplicating or guessing: `R/custom_R_code.R` (`check_new_taxa`, `check_new_taxa_accepted`, `replace_duplicates_with_NA`, `separate_range`, `move_values_to_new_trait`, …), `R/build_align_taxon_names.R`, `R/build_update_taxon_list.R`, whether `austraits` and `APCalign` are installed, and whether the build is `remake.yml` or `build.R`. `ausinvertraits.build` and `AusFizz` do not have the same helper set as `austraits.build`, so this cannot be assumed.

**`scripts/trait_index.R`** — writes to a scratch directory:

- `traits_index.tsv` — one row per concept in `config/traits.yml`: `trait_name`, `label`, `type`, `units`, `allowed_values_min/max`, allowed values for categoricals, `structure_measured`, `keywords`. This is what lets a curator be *shown* plausible concepts without anyone reading a 700 KB file.
- `units_index.tsv` — `config/unit_conversions.csv`, so a missing conversion is caught before the build rather than after.
- `vocab_index.tsv` — `context_property` × `category` and `location_property` already in use, harvested from `data/*/metadata.yml` (137 distinct context properties in `austraits.build`).

**`scripts/scaffold_metadata.R`** — reads a YAML answers file the agent writes, then calls in order: `metadata_create_template()`, `metadata_add_source_doi()` (or writes a hand-built source block for non-article types), `metadata_add_traits()`, `metadata_add_locations()`, `metadata_add_contexts()`, `metadata_add_identifiers()` — each with its `user_responses` list.

**`scripts/dataset_checks.R`** — `dataset_check_categorical_substitutions()`, `dataset_check_numeric_values()`, `dataset_check_taxonomic_updates()`, `dataset_check_not_pivoting()`, `dataset_check_outlier_by_species()`, `dataset_check_outlier_by_genus()`, `dataset_check_duplicates_within_dataset()`, taken verbatim from `check_dataset_functions.qmd`, where they exist only as code for a curator to copy — the package exports none of them. Guarded so anything `probe_repo.R` finds in the host repo wins.

### 2. The installer — `R/skills.R`

`use_traits_build_skills(path = ".", overwrite = FALSE)`, exported:

- copies every directory under `system.file("skills", package = "traits.build")` into `<path>/.claude/skills/`;
- refuses to clobber an existing skill unless `overwrite = TRUE`, reporting what it skipped;
- errors if `<path>` is not a traits.build database repo (`config/traits.yml` and `data/` must exist);
- returns the installed paths invisibly.

Follows the existing house style: `message()` with the `crayon` colouring used throughout `R/setup.R`, roxygen with `@export`. Needs `NAMESPACE` + `man/` regeneration via `devtools::document()`, a testthat test, and a `NEWS.md` entry.

`inst/` ships with the package, so `.Rbuildignore` needs no change — and it already ignores `^\.claude$`, so the authoring symlink below stays out of the tarball.

### 3. Authoring-side link

`.claude/skills/traits-build-add-dataset` → `../../inst/skills/traits-build-add-dataset`, so the skill is live while being written in this repo without a second copy to keep in sync.

## The workflow SKILL.md encodes

### Phase 0 — Intake

Confirm the working directory is a traits.build database repo and run `probe_repo.R`. Collect the data file(s), the PDF, any DOI. Derive `dataset_id` as `Surname_year`, check `data/` for a collision, suffix `_2`/`_3` as needed. Create `data/<dataset_id>/` and record the incoming filename for `dataset$original_file`.

Do **not** create `raw/` yet. Whether it is needed is not knowable until Phase 2 has established how far `data.csv` had to depart from the source — see Phase 2a.

### Phase 1 — Read the paper

Extract into a scratch notes file, each item carrying its page/section anchor: citation details, `sampling_strategy` (**verbatim**), per-trait `methods` (**verbatim**), locations, treatments/contexts, collection dates, life stage, basis of record.

One exception to verbatim-only: **`description` is a written one-sentence summary of what the study did.** It cannot be lifted cleanly from the abstract, so the skill composes it — and flags it at the gate as generated prose, unlike every other text field.

Anything not found stays `.na` and becomes an entry in the `questions:` block for the contributor. Nothing about methods is paraphrased or invented.

### Phase 2 — Build `data.csv`, as close to the provided file as possible

**The goal is a traceable line back to the file the contributor sent.** `data.csv` should differ from it as little as possible. Do **not** reshape, rename, recode, filter, or summarise in `data.csv` if the same thing can be achieved by `custom_R_code`, a substitution, or a trait mapping — all three are declarative, reviewable, and leave the CSV matching the source.

Legitimate reasons to depart from the provided file, and roughly the whole list:

- the file is not a readable CSV as-is (`.xlsx`/`.xls`/`.ods` with sheets, multi-row or merged headers, metadata blocks above the data);
- special or non-UTF-8 characters that must be resolved before the file will read (13 `data.csv` files in the databases are already not valid UTF-8);
- **multiple files that must be joined**, because `custom_R_code` cannot read files — species-code keys, per-trait spreadsheets, separate location tables.

Everything else — missing-value codes, unit-bearing column headers, categorical spellings, derived columns, de-duplication, date reformatting — stays as supplied and is handled in `metadata.yml`.

### Phase 2a — Archive `raw/` only if it holds something `data.csv` does not

`raw/` exists to preserve information that `data.csv` cannot. If `data.csv` is essentially the file the contributor sent, the archive is a byte-for-byte duplicate and **no `raw/` folder is created**. This follows directly from the fidelity rule above: the better Phase 2 goes, the less there is to archive.

Keep `raw/` when the source genuinely differs:

- **`.xlsx` / `.xls` / `.ods`** — sheets, formatting, formulae, and any sheets not carried into `data.csv`;
- **multiple files merged into one** — each original, plus the `build_data_csv.R` that joined them, so the merge is reproducible;
- **a file that needed character or encoding repair**, where the pre-repair original records what was actually received;
- **supplementary material not otherwise retrievable** — an unpublished report, a data dictionary, a location table supplied separately.

The **published paper is not archived**. It is identified by its DOI in the `source` block, and only 5 of 411 `austraits.build` datasets keep a PDF. Archive a document only when it is the dataset's own supporting material rather than the literature.

Where a departure was needed, `build_data_csv.R` goes in `raw/` alongside the originals, matching the existing convention (146 `.R` files already live in `raw/` folders). Where none was needed, there is no script and no folder — and that is the better outcome.

### Phase 3 — Locations

Locations come from one of four places, in this order. Take the first that has them:

1. **Columns in the main data file** — `distinct()` them out and feed `metadata_add_locations()`.
2. **A secondary file supplied with the dataset** — transcribe into the YAML (the file itself is archived in `raw/`).
3. **The PDF** — a site table or a description in the methods, transcribed with its page anchor.
4. **None of the above** — leave `.na` and raise it in the `questions:` block. Missing location data is a question for the authors, not something to reconstruct.

Coordinates must end up as decimal degrees under `latitude (deg)` / `longitude (deg)`; DMS in the source is converted, and the original string kept in the notes so the conversion is checkable.

**Location properties have no controlled list, but the list should stay short and consistent.** Reuse a property name already in `vocab_index.tsv` wherever one fits. A genuinely new property is fine but is flagged at the gate as new vocabulary, so the choice is deliberate rather than accidental.

### Phase 4 — Scaffold `metadata.yml`

Run `scaffold_metadata.R`, then fill in what is knowable: `source`, `contributors`, the `dataset` block, `methods` (verbatim), `description` (the written sentence), locations, contexts, `replicates`, `unit_in` as given in the source.

`trait_name` is left `.na`. So is `replace` in every substitution. Those are the curator's, and they are the subject of the next gate.

### GATE 1 — mapping review

Stop. The skill writes the review files to a **scratch directory**, each with the *from*-side filled and the decision column empty. They are working files, not dataset records — and under Phase 2a a `raw/` folder may legitimately not exist, so one must not be created just to hold them. @ehwenk's script writes them under `data/<id>/raw/`; the scratch directory is the same workflow without the side effect on the repo. If a particular dataset's decisions are worth preserving, that is a deliberate copy at the end, not the default.

| File | Pre-filled by the skill | Left empty for the curator |
|---|---|---|
| `trait_mapping.csv` | `var_in`, `unit_in` as given, sample values, min/max, n, and candidate concepts from `traits_index.tsv` in a clearly separate `candidates` column | `trait_name`, and the `entity_type` / `value_type` / `basis_of_value` cells |
| `substitutions.csv` | `trait_name`, `find` | `replace` |
| `contexts.csv` | `var_in`, distinct values found, suggested `category` | `context_property`, `value`, `description` |
| `locations.csv` | names, coordinates, properties as found, with new-vocabulary properties marked | any correction |

One file per concern, edited in place — the curator fills the blank column and saves; the skill reads the same file back. No `_needed` / `_added` pair.

Alongside the files, the skill reports in chat: which `unit_in` values have no conversion in `units_index.tsv`, which location/context properties are new vocabulary, the generated `description` sentence for approval, and the list of things the PDF did not answer as draft contributor questions.

The `candidates` column exists because no one can eyeball 556 concepts; it is a shortlist to choose from, never a filled-in answer, and it is trivially droppable if it proves more distracting than useful.

Nothing proceeds until the files come back.

### Phase 5 — Test loop

Round-trip formatting (`read_metadata() |> write_metadata()`), then `metadata_check_custom_R_code()`, then `dataset_test(dataset_id)` — which runs per-dataset without a full build, so this is the fast inner loop. Fix and repeat until clean.

### Phase 6 — Build this dataset, and the second round of substitutions

`build_setup_pipeline(method = "remake", database_name = "<db>")`, then `remake::make(current_study)` to build **just this dataset** and read `tmp$excluded_data` — far faster than a full rebuild, and enough for both round-trips.

**Substitutions**, appending to the same `substitutions.csv` the curator has already seen, `replace` again left blank:

```r
database$excluded_data |>
  filter(dataset_id == current_study, error != "Observation excluded in metadata") |>
  distinct(trait_name, value) |> arrange(trait_name, value) |>
  rename(find = value) |> mutate(replace = NA_character_) |>
  write_csv("<scratch>/substitutions.csv")
# curator fills `replace` in place, then:
metadata_add_substitutions_list(current_study, read_csv("<scratch>/substitutions.csv"))
```

**Taxonomy** — only where the host repo has APCalign (`austraits.build`); elsewhere the book's manual path:

```r
resources <- APCalign::load_taxonomic_resources()
checked <- align_taxa(taxa$taxon_name, resources = resources, identifier = current_study) |>
  arrange(alignment_code, original_name) |>
  filter(original_name != cleaned_name, !str_detect(original_name, "\\["))
```

APCalign's proposed `aligned_name` is written to `taxonomic_updates.csv` for confirmation rather than applied — it is a machine alignment, and the curator confirms it before `metadata_add_taxonomic_changes_list()` runs. The `!str_detect(original_name, "\\[")` filter drops names already carrying a `dataset_id` in square brackets, which must not be re-aligned; those go to the gate listed as "already aligned" rather than silently dropped.

### GATE 2 — excluded data and any config change

Stop and present: values still needing substitutions; out-of-range numerics (a long list means a units error, not real outliers); taxon names not aligning; anything blocking the pivot (`check_pivot_duplicates()`, `dataset_check_not_pivoting()`).

**Any change to `config/traits.yml`, `config/unit_conversions.csv` or `config/taxon_list.csv` is database-wide and is proposed here, never made unilaterally** — a new trait concept, a new unit conversion, a widened allowable range, or new taxon-list rows all need explicit approval. Where the host repo has `build_update_taxon_list()`, propose running it rather than hand-editing the taxon list.

### Phase 7 — Report

Full database rebuild, then `dataset_report(current_study, database, overwrite = TRUE)` — which on this branch renders the new `report_dataset_2026.Rmd`.

Then the "what did this actually add" check, per trait, which is what a curator most wants to see. Prefer `new_taxa_trait_combinations()` from `R/reports.R` on this branch: it reads `database$taxa` rather than APCalign `resources`, so it works in all three database repos. Fall back to the host repo's `check_new_taxa()` / `check_new_taxa_accepted()` where `probe_repo.R` finds them. Note it is currently unexported, so the skill reaches it via `:::` or the branch exports it — worth resolving during implementation rather than leaving to chance.

Two argument traps, both confirmed against @ehwenk's script:

- `dataset_report(dataset_id, austraits, ...)` — `R/reports.R:21`. `adding_data_long.qmd:1332` shows them reversed; the book is wrong. Fixing the book is a separate one-line issue, out of scope here.
- `extract_dataset(current_study, partial_matches_allowed = FALSE)` — without the flag, `Bryant_2021` also pulls in `Bryant_2021_2` and `_3`, silently inflating every check that follows.

### GATE 3 — report review

Stop on: do the locations plot sensibly on the map; do numeric traits sit sensibly against other studies; is the unmatched-species list free of names that should have aligned; is every metadata field populated.

### Phase 8 — Finalise

`dataset_replace_disallowed_chars(dataset_id)`, re-clear formatting, re-run `dataset_test()`, confirm the `questions:` block holds the contributor questions, and hand back a summary plus suggested branch/PR.

## Non-negotiables the skill states explicitly

- **Never fill a decision cell.** `trait_name`, substitution `replace`, and `context_property` arrive empty. The skill fills what the source says, never what the database should say.
- **Never invent method or sampling text.** Verbatim from the PDF, or `.na` plus a question. `description` is the sole written field, and is flagged as such.
- **`data.csv` matches the provided file** unless it cannot be read, has bad characters, or requires a multi-file join. Everything else goes in `custom_R_code`.
- **Never edit `config/*` without approval** — those files affect every dataset in the database.
- **Never write a `trait_name` absent from `config/traits.yml`.** `.na`, and record the column under `questions: additional_traits:`.
- **Never write `individual_id: unknown`** — it silently assigns the whole dataset to one individual. Omit the field instead.
- **Always pass `partial_matches_allowed = FALSE` to `extract_dataset()`.**
- **Reuse existing location and context property names** wherever one fits; a new one is allowed but must be flagged.
- **Degree symbol is U+00B0**, not U+00BA. PDF-extracted text is sanitised for confusables: `º`→`°`, `∼`→`~`, `◦`→`°`, non-breaking spaces, and the `¬†` mojibake (a UTF-8 NBSP decoded as Mac OS Roman — replace longest-first, or a per-character pass mangles it). `º` is the commonest disallowed character across the databases, and the book itself teaches the typo at `adding_data_long.qmd:948`.
- **Trait *values* in `data.csv` must be ASCII**; `metadata.yml` allows a wider set including accented letters. Different rules — don't conflate them.
- **Author names and real symbols are never rewritten** — `ñ`, `š`, `‰`, `Ó` are meaningful, not errors.
- **Round-tripping `metadata.yml` is correct for a new dataset** (it is the documented "clear formatting" step) but is the wrong tool for a one-character fix in an existing file, where it buries the change in an unreviewable reflow.
- **Units follow UCUM**; quote units starting with punctuation (`'{count}/mm2'`); use `neg_MPa` rather than `-MPa`.

## Files

**New**

- `inst/skills/traits-build-add-dataset/SKILL.md`
- `inst/skills/traits-build-add-dataset/references/{metadata-fields,custom-r-code,curator-recipes,troubleshooting}.md`
- `inst/skills/traits-build-add-dataset/scripts/{probe_repo,trait_index,scaffold_metadata,dataset_checks}.R`
- `R/skills.R`
- `tests/testthat/test-skills.R`
- `.claude/skills/traits-build-add-dataset` (symlink)

**Modified**

- `NAMESPACE`, `man/` — regenerated by `devtools::document()`
- `NEWS.md` — one entry
- `AGENTS.md` — a short pointer to the skill

## Branch base

This branch was cut from `update-AusTraits-report`, which at the time was **5 commits ahead of `develop` and 0 behind** — a clean fast-forward, so no merge was needed. It brings:

- `inst/support/report_dataset_2026.Rmd` (889 lines), now the default `input_file` for `dataset_report()` — so Phase 7 exercises the new report, not the old one;
- `new_taxa_trait_combinations()` in `R/reports.R` — a package-level equivalent of `austraits.build`'s `check_new_taxa_accepted()` that reads `database$taxa` instead of taking APCalign `resources`. It needs no APCalign, so Phase 7's "how much does this dataset add" check can work in **all three** database repos rather than only `austraits.build`. It is currently unexported and the file lacks a trailing newline.

Other remote branches in this repo touching reports or check functions are stale and were ruled out of scope by the maintainer; don't build against them.

## Verification

1. **Package integrity** — `devtools::document()`, `devtools::test()`, then `R CMD build` and confirm `inst/skills/` is in the tarball and `.claude/` is not.
2. **Installer** — in a scratch copy of a database repo, `use_traits_build_skills()` creates `.claude/skills/traits-build-add-dataset/`; a second call without `overwrite` skips and says so; calling it outside a database repo errors.
3. **`probe_repo.R` across all three repos** — correctly reports that `austraits.build` has the `check_new_taxa` / `build_align_taxon_names` helpers and that `ausinvertraits.build` and `AusFizz` do not, without erroring on the absence.
4. **`trait_index.R`** — run in all three repos. Expect ~556 rows (`austraits.build`), 60 (`ausinvertraits.build`), 33 (`AusFizz`), and ~137 context properties in `austraits.build`'s `vocab_index.tsv`.
5. **`scaffold_metadata.R` against a known-good answer** — run for `tutorial_dataset_1` in a clone of `traits.build-template` with the answers the tutorial specifies, and diff the generated `metadata.yml` against what `tutorial_dataset_1.qmd` says the interactive session produces. This is the test that the non-interactive path is faithful to the documented one.
6. **`dataset_checks.R`** — source against a built `austraits.build`; each function returns without error on an existing dataset.
7. **The decision cells really are empty** — assert on the generated `trait_mapping.csv` and `substitutions.csv` that `trait_name` and `replace` contain no values. This is the rule most likely to erode as the skill is iterated on, so it gets a test rather than only a sentence in `SKILL.md`.
8. **Full dress rehearsal** — pick an existing `austraits.build` dataset with a `raw/` folder and a DOI (`Richards_2008` also exercises the multi-source `original_01`/`original_02` case), move it aside, and re-import it from its raw files using the skill. Two diffs matter: `data.csv` against the committed one (measuring the fidelity rule in Phase 2), and `metadata.yml` against the committed one (measuring how much scaffolding is genuinely right, and how much correctly arrived empty).
