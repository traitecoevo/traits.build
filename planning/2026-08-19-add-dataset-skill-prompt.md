# Implementation prompt — the add-dataset skill

*Companion to [`2026-08-19-add-dataset-skill.md`](2026-08-19-add-dataset-skill.md). Paste the block below into Claude Code, from the root of a `traits.build` checkout on `feature/add-dataset-skill`.*

## Before you start

**You need more than this repo.** The skill condenses material that lives elsewhere in the AusTraits family. Clone or locate these as siblings — the plan assumes a common parent directory (`~/GitHub/austraits-family/` on the author's machine):

| What | Where | Why you need it |
|---|---|---|
| `traits.build-book` | `traitecoevo/traits.build-book` | `adding_data_long.qmd`, `check_dataset_functions.qmd`, `data_common_issues.qmd`, `tutorial_dataset_1..7.qmd`. The three reference docs and `scripts/dataset_checks.R` are condensed from these. |
| `austraits.build` | `traitecoevo/austraits.build` | `R/custom_R_code.R` (the helpers `probe_repo.R` looks for), `config/traits.yml` (556 concepts — the scale `trait_index.R` must handle), and worked examples `data/Falster_2005_1/metadata.yml` and `data/Richards_2008/metadata.yml`. |
| `ausinvertraits.build`, `AusFizz` | `traitecoevo/…` | The two repos that do *not* have `austraits.build`'s helper set. `probe_repo.R` must handle their absence without erroring. |
| `traits.build-template` | `traitecoevo/traits.build-template` | `data/tutorial_dataset_1/` — the fixture for verification step 5. |
| @ehwenk's `adding datasets scripts.R` | Ask @dfalster — unpublished, not in any repo | The source for `references/curator-recipes.md`. Its idioms are not in the book. Without it, write the reference from the plan's Phase 6 code blocks, which reproduce the substitutions and taxonomy round-trips. |

Do not start `references/curator-recipes.md` without the script or an explicit decision to work from the plan alone.

## The prompt

> Read `planning/2026-08-19-add-dataset-skill.md` in full before writing anything. It is the approved design; implement it rather than re-deciding it.
>
> You are on `feature/add-dataset-skill`, cut from `update-AusTraits-report`. That base is deliberate: `dataset_report()` now defaults to `inst/support/report_dataset_2026.Rmd`, and `R/reports.R` has `new_taxa_trait_combinations()`, which the skill's Phase 7 uses. Other remote branches touching reports or check functions are stale — ignore them.
>
> Build, in this order:
>
> 1. `R/skills.R` with `use_traits_build_skills()`, plus `tests/testthat/test-skills.R`. Doing the installer first means you can install the skill into a database repo and exercise it as you write it.
> 2. `inst/skills/traits-build-add-dataset/scripts/` — `probe_repo.R`, `trait_index.R`, `scaffold_metadata.R`, `dataset_checks.R`. These are testable on their own against the three database repos; get them right before writing prose about them.
> 3. `inst/skills/traits-build-add-dataset/references/` — the four reference docs, condensed from the book chapters listed in the plan.
> 4. `inst/skills/traits-build-add-dataset/SKILL.md` last, once the scripts have settled and you know what it can actually promise.
> 5. The `.claude/skills/traits-build-add-dataset` symlink, `NEWS.md`, and the `AGENTS.md` pointer.
>
> Then work the plan's verification list. Step 5 (`scaffold_metadata.R` reproducing `tutorial_dataset_1`) and step 8 (re-importing `Richards_2008` and diffing) are the two that actually prove the thing works — budget for them rather than treating them as a formality.
>
> Two rules from the plan are the ones most likely to erode as you write. Hold them:
>
> - **Decision cells stay empty.** `trait_name`, substitution `replace`, and `context_property` are the curator's. The skill fills what the *source* says — `var_in`, `unit_in` as given, `find`, sample values — never what the *database* should say. A `candidates` column offering shortlisted concepts is fine and wanted; a filled-in `trait_name` is not. Verification step 7 asserts this.
> - **`data.csv` matches the file the contributor sent.** Depart from it only when it cannot be read as a CSV, has characters or an encoding that block reading, or requires a multi-file join (`custom_R_code` cannot read files). Everything else — missing-value codes, unit-bearing headers, categorical spellings, derived columns, de-duplication, date reformatting — is handled declaratively in `metadata.yml`. And `raw/` is created only when it would hold something `data.csv` does not; if `data.csv` is essentially the source file, no `raw/` folder.
>
> Conventions: default branch is `develop`; `devtools::load_all()`, `devtools::test()`, `devtools::document()`; `NOT_CRAN=true` when touching anything near the `Test_2023_9` snapshot. `AGENTS.md` has the rest, including the commit-message convention — the PR title and body become the permanent commit message, so keep the subject ≤50 characters and put working detail in the first PR comment.
>
> Ask before: changing anything in `DESCRIPTION`'s `Depends` (see the warning in `AGENTS.md` — it breaks ~1,240 `custom_R_code` call sites across 601 datasets), exporting `new_taxa_trait_combinations()`, or widening the skill's scope beyond the plan.

## Open questions the implementer will hit

Flagged here rather than guessed at in the plan.

- **`new_taxa_trait_combinations()` is unexported.** The skill needs it in Phase 7. Either export it on this branch (it has roxygen already, just no `@export`) or reach it with `:::`. Exporting is cleaner but is a public-API change on someone else's branch — worth a word with @ehwenk. Its file also lacks a trailing newline.
- **Where the scratch files live.** The plan says the Gate 1 review CSVs go to a scratch directory rather than `data/<id>/raw/`, because under the `raw/` policy that folder may not exist. If curators would rather have them beside the dataset, that is a one-line change at Gate 1 — but then the skill has to create `raw/` for files that are not archive material, which is what the policy is trying to avoid.
- **Whether the `candidates` column helps or distracts.** It exists because nobody can eyeball 556 trait concepts. If in the dress rehearsal it turns out to anchor the curator on a wrong suggestion, drop it; the plan treats it as separable for that reason.
- **`AusFizz` and `ausinvertraits.build` reporting.** `report_dataset_2026.Rmd` is AusTraits-specific. Phase 7 may not work as-is in the other two repos; find out during verification rather than assuming.
