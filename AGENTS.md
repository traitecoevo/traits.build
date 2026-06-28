# traits.build — agent & contributor guide

`traits.build` is an R package providing a **workflow to harmonise trait data** from diverse
sources into a documented, relational standard structure. It is the generic engine spun out of the
AusTraits project in 2023 (see Wenk et al. 2024, doi:10.1016/j.ecoinf.2024.102773).

## Repo-local guidance

- **Code:** `R/` (functions), `tests/` (testthat), `man/` (generated docs), `NAMESPACE`.
- **Data-model ontology:** `ontology/` documents the ontology of the *data model*
  (entities/relations). This is **not** APD's trait-definition vocabulary — they're parallel, don't
  conflate them (see family context below).
- **Schema:** the relational `traits.build` schema is the source of truth for the data structure
  that other family repos conform to.
- **Docs:** user manual at the [traits.build-book](https://traitecoevo.github.io/traits.build-book/);
  function reference at <http://traitecoevo.github.io/traits.build/>.

Dev follows the standard R-package workflow: `devtools::load_all()`, `devtools::test()`,
`devtools::check()`. Default development branch is `develop`. Note the README's **deprecated**
lifecycle badge — confirm intent before large new features.

> Heads-up: `traits.build` **Imports `austraits`** (it re-exports a few conversion helpers), so the
> R-package install graph runs `traits.build → austraits` even though in the *data* pipeline
> traits.build is upstream of austraits. Run traits.build's tests after touching those helpers.

---

## AusTraits family — cross-package context

`traits.build` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the generic workflow engine + relational
data model/schema. Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction (incl. the reversed `traits.build → austraits`
  edge), source-of-truth rules, cross-boundary artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
