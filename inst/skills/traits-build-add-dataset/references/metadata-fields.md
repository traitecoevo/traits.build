# `metadata.yml` field reference

Condensed from `adding_data_long.qmd` and cross-checked field-by-field against
`inst/support/traits.build_schema.yml` (the authoritative source — the book's
prose drifts). Worked examples are pulled from `austraits.build/data/Falster_2005_1/metadata.yml`
and `austraits.build/data/Richards_2008/metadata.yml`. Read this when filling in
a specific section; it is not meant to be read start to finish.

Top-level sections, in file order: `source`, `contributors`, `dataset`,
`locations`, `contexts`, `traits`, `substitutions`, `taxonomic_updates`,
`exclude_observations`, `questions`.

> **Gotcha — `.na` vs `NA` vs `unknown`.** In plain YAML, a bare `NA` parses to
> the *string* `"NA"`, not to R's real `NA`. Only the R-specific token `.na`
> round-trips to an actual `NA` through `traits.build`'s YAML reader/writer.
> Always write `.na`, never `NA`, when a field has no value. This matters most
> when hand-editing a `metadata.yml` or writing an answers file for
> `scaffold_metadata.R` — a stray `NA` silently becomes a real (wrong) string
> value instead of a missing one. Separately, `unknown` is a real, meaningful
> categorical value in several controlled vocabularies (`value_type`,
> `basis_of_value`, `collection_date: unknown/2009`) — it is not a synonym for
> missing and must not be used as a placeholder for `.na`.

---

## 1. `source`

Citation details, keyed under `primary` (required), optionally `secondary`,
`secondary_2`, and — for compilations of many original studies —
`original_01`, `original_02`, … (see `Richards_2008`, which has 20+).

Preferred path: `metadata_add_source_doi(dataset_id, doi = ...)` (or
`file = "myref.bib"`). Falls back to hand-written blocks for source types a
DOI lookup won't populate correctly (Book, Online, Thesis, Unpublished).

Fields allowed under any source entry, per `schema$metadata.source.primary.values`
(the same list applies to `secondary`): `key`, `bibtype`, `year`, `author`,
`title`, `journal`, `volume`, `number`, `pages`, `doi`, `url`, `type`,
`institution`, `publisher`, `isbn`, `place`, `note`. Not every field applies to
every `bibtype` — see the worked shapes below.

| Field | Meaning |
|---|---|
| `key` | Citation key, `Surname_year`. Check it manually — often wrong for hyphenated surnames. |
| `bibtype` | Entry type: `Article`, `Book`, `Online`, `Thesis`, `Unpublished`, `TechReport`, `Conference`, … (standard BibTeX types). |
| `year` | Publication year (or year written, for unpublished work). |
| `author` | All authors, `and`-separated: `Wendy Cooper and William T. Cooper`. |
| `title` | Sentence case. Watch for ALL-CAPS titles from some journals' DOI metadata. |
| `journal` | Journal name (`Article` only). |
| `volume`, `number` | Volume / issue number (`Article` only). |
| `pages` | `123--134` (double-hyphen, not a single hyphen or en-dash). |
| `doi` | Digital object identifier. |
| `url` | Web page URL (`Online`). |
| `type` | Thesis type: `PhD`, `Masters`, `Honours`. |
| `institution` | Institution sponsoring a thesis or report. |
| `publisher` | Publisher name (`Book`, `Online`). |
| `isbn` | ISBN (`Book`, optional). |
| `place` | Place of publication (optional). |
| `note` | Free-text notes not covered by the other fields (optional). |

Formatting notes carried over from the book: quote a line containing `:` or
`'` (e.g. `title: 'Unpublished data: ...'`); an `Unpublished` title must begin
with the literal words `Unpublished data` and should name the data collector's
affiliation.

**Article:**

```yaml
source:
  primary:
    key: Falster_2005_1
    bibtype: Article
    author: Daniel S. Falster, Mark Westoby
    year: 2005
    title: Alternative height strategies among 45 dicot rain forest species from
      tropical Queensland, Australia
    journal: Journal of Ecology
    volume: 93
    pages: 521--535
    publisher: Wiley-Blackwell
    doi: 10.1111/j.0022-0477.2005.00992.x
```

**Book:**

```yaml
source:
  primary:
    key: Cooper_2013
    bibtype: Book
    year: 2013
    author: Wendy Cooper and William T. Cooper
    title: Australian rainforest fruits
    publisher: CSIRO Publishing
    pages: 272
```

**Online:**

```yaml
source:
  primary:
    key: TMAG_2009
    bibtype: Online
    author: '{Tasmanian Herbarium}'
    year: 2009
    title: Flora of Tasmania Online
    publisher: Tasmanian Museum & Art Gallery (Hobart)
    url: http://www.tmag.tas.gov.au/floratasmania
```

**Thesis:**

```yaml
source:
  primary:
    key: Kanowski_2000
    bibtype: Thesis
    year: 1999
    author: John Kanowski
    title: Ecological determinants of the distribution and abundance of the
      folivorous marsupials endemic to the rainforests of the Atherton uplands,
      north Queensland.
    type: PhD
    institution: James Cook University, Townsville
```

**Unpublished:**

```yaml
source:
  primary:
    key: Ooi_2018
    bibtype: Unpublished
    year: 2018
    author: Mark K. J. Ooi
    title: "Unpublished data: Herbivory survey within Royal National Park,
      University of New South Wales"
```

**Compilation** (multiple original sources, each its own key — see `Richards_2008`
for the full pattern with 20+ `original_NN` entries):

```yaml
source:
  primary:
    key: Richards_2008
    bibtype: Unpublished
    year: '2009'
    author: Anna Richards and Ian J. Wright
    title: 'Unpublished data: Transcription of Australian plant functional trait
      data from Ian Wright''s collection of papers, Macquarie University'
  original_01:
    key: Adams_1984
    bibtype: Article
    year: '1984'
    author: M. A. Adams and P. M, Attiwill
    journal: Australian Journal of Botany
    title: Role of Acacia spp. in nutrient balance and cycling ...
    volume: '32'
    number: '2'
    pages: 205--215
    doi: 10.1071/bt9840205
```

---

## 2. `contributors`

Filled in manually; `metadata_create_template()` only stubs it.

| Sub-field | Meaning |
|---|---|
| `data_collectors` | List of people with a key intellectual role in study design and data collection. Each entry: `last_name`, `given_name`, `affiliation`, `ORCID` (if available). Exactly one should carry `additional_role: contact`. |
| `assistants` | Field/lab assistants — plain name string(s), no structured sub-fields. |
| `dataset_curators` | The person(s) who keyed the data into the database. |

```yaml
contributors:
  data_collectors:
  - last_name: Roderick
    given_name: Michael
    ORCID: 0000-0002-3630-7739
    affiliation: The Australian National University, Australia
    additional_role: contact
  assistants: Michelle Cochrane
  dataset_curators: Elizabeth Wenk
```

Email addresses are never stored in `metadata.yml`.

---

## 3. `dataset`

Every field listed here is either a **literal value** or the **name of a
column in `data.csv`** that supplies that value per-row — the same field can
be either, and `traits.build` figures out which from whether the string
matches a column name. Per `schema$metadata.dataset.values`:

| Field | Auto/manual | Column-or-literal | Notes |
|---|---|---|---|
| `data_is_long_format` | auto | literal (`yes`/`no`) | Set by `metadata_create_template()`. |
| `custom_R_code` | manual, optional | literal R snippet or `.na` | See `references/custom-r-code.md`. Cannot read files. |
| `collection_date` | auto if a column exists, else manual | either | `yyyy-mm-dd`, `yyyy-mm`, or `yyyy`; a study-wide range is `start/end` (e.g. `2010-10/2011-03`); if truly unknown use `unknown/<publication year>`. |
| `taxon_name` | auto | column | The column supplying taxon names. |
| `location_name` | auto | column or literal | If `data.csv` has no such column, mutate one via `custom_R_code` or fill a literal if there's a single location. |
| `source_id` | manual, optional | column | For compilations — links each row to a `source: original_NN` key (see `Richards_2008`: `source_id: AusTraits_key`). |
| `entity_type` | manual, optional | column or literal | See §5. Usually set per-trait instead; a dataset-wide value here is a fallback. |
| `plot_context_id` | manual, optional | column | Only if a column already contains a ready-made plot-context ID. |
| `treatment_context_id` | manual, optional | column | As above, for treatment context. |
| `individual_id` | auto if selected, else manual | column | **See the non-negotiable below.** |
| `observation_id` | manual, optional | column | Rare — normally derived by the pipeline, not supplied. |
| `repeat_measurements_id` | auto (asked at template time) or per-trait | literal `TRUE` or column | Only for response-curve data (e.g. A–Ci curves); links sub-measurements into one observation. |
| `trait_name` | auto | column | Long-format datasets only — names the column holding trait names. |
| `value` | auto | column | Long-format datasets only — names the column holding trait values. |
| `description` | **required, manual** | literal | 1–2 sentence study summary — the one field that is *composed*, not lifted verbatim (see `SKILL.md`). |
| `basis_of_record` | **required, manual** | column or literal | See §5. Dataset-wide value is overridden by a location- or trait-level value if both are set. |
| `life_stage` | **required if applicable, manual** | column or literal | See §5. Same override rule as `basis_of_record`. |
| `replicates` | manual, optional | column or literal | Dataset-wide fallback; usually set per-trait instead. |
| `sampling_strategy` | **required, manual** | literal, **verbatim from the source** | Site selection and overall study design — see the scoping note below. |
| `measurement_remarks` | manual, optional | column or literal | Miscellaneous notes not captured by `methods` or a context. |
| `original_file` | **required, manual** | literal | Filename as submitted by the contributor. |
| `notes` | **required, manual** | literal | Curator notes — flag suspected duplicates with another study, processing quirks, etc. Use `notes: none` when there is nothing to add (see worked examples), not `.na`. |

### Scoping note: `sampling_strategy` vs. per-trait `methods`

The paper's Methods section is not one undifferentiated block to copy twice —
`sampling_strategy` and each trait's `methods` draw from *different parts* of
it:

- `sampling_strategy` is the big-picture study design: site/plot selection
  criteria, how many sites or individuals were chosen and why, the sampling
  design (e.g. randomised, stratified, opportunistic), study period. This is
  the "where and who" — it belongs at dataset level because it applies across
  every trait.
- Each trait's `methods` is the "how measured" for *that trait only* — instrument,
  protocol, sample prep, timing specific to that measurement. Extract just the
  sentences that describe how *this* trait was collected, not the whole
  Methods section pasted into every trait entry. Two different traits
  described in different paragraphs get different `methods` text; do not give
  them both the entire section.

A short passage can legitimately belong in both (e.g. "leaves were collected
from the same individuals sampled for the site survey") — that overlap is
fine and still verbatim — but the *default* is that these two fields contain
different text lifted from different parts of the paper. Ending up with
`sampling_strategy` and every trait's `methods` all containing the same
copy-pasted block is the signal something went wrong: go back to the source
and re-split by what each sentence actually describes.

### Non-negotiable: never write `individual_id: unknown`

`individual_id` should be **omitted entirely**, not set to `"unknown"` or any
other placeholder, whenever the dataset has no real per-individual
identifier. Setting `individual_id: unknown` (or any fixed literal) assigns
**every row in the whole dataset** to one individual named `"unknown"` —
silently collapsing all individual-level observations into a single entity.
Only include the field when there is an actual column linking rows that
belong to the same individual, or when `custom_R_code` builds one. If in
doubt, leave it out: for individual-level measurements, each row is presumed
to be a *different* individual by default, which is almost always what you
want.

---

## 4. `locations`, `contexts`, `traits`

### `locations`

A named list, keyed by `location_name` (must match `data.csv` values exactly,
including case). Per `schema$metadata.locations.elements`, every location
should carry at minimum `latitude (deg)`, `longitude (deg)` (decimal degrees;
convert DMS before writing — see `references/curator-recipes.md`) and
`description`. `locality` is an optional free-text place name. Beyond these,
**location properties have no controlled vocabulary** — but reuse an existing
property name wherever one fits (check `vocab_index.tsv` / `database$locations
%>% distinct(location_property)`) rather than inventing new syntax for the
same concept; a genuinely new property is fine but gets flagged at the review
gate. Set `locations: .na` for botanical-collection or literature-compilation
datasets where a trait value is a range-wide mean, not tied to a site.

```yaml
locations:
  Atherton:
    description: Tropical rain forest vegetation.
    elevation (m): 800
    latitude (deg): -17.1166667
    longitude (deg): 145.65
    precipitation, MAP (mm): 2000
```

`life_stage` and `basis_of_record` can also be set per-location when they
vary by site.

### `contexts`

An array of context-property blocks. Each entry:

| Field | Meaning |
|---|---|
| `context_property` | The property name, free text (no controlled vocabulary — reuse existing terms; check `database$contexts %>% distinct(context_property, category)`). |
| `category` | One of the five categories below. |
| `var_in` | The `data.csv` column holding the raw values. |
| `values` (or inline `find`/`value`/`description`) | Maps raw values to standardised ones. `find` is optional if no substitution is needed. Set `value: .na` for any raw value that is not actually a context (e.g. blank rows). |

**The five context categories** — pick the one matching where the property
sits relative to the entity being measured:

| Category | Applies when… | Example |
|---|---|---|
| `treatment_context` | An experimental manipulation of growing/living conditions. | CO2 or nutrient-addition treatment. |
| `plot_context` | A feature stratified within a location — a subdivision of a site (aspect, block, stand condition) rather than a full location in its own right. | `stand age` (`declining`/`intermediate`/`healthy`) in `Richards_2008`. |
| `entity_context` | Information about the organismal entity itself (not the environment) that isn't a trait measurement — sex, caste, host plant. | An insect's host-plant species. |
| `temporal_context` | Repeat measurements on the same entity across time. | Sampling month/season. |
| `method_context` | The same trait measured on the same entity by more than one method. | `sapwood sampling method` in `Falster_2005_1` (`10mm2 stem cross-sectional area` vs `250mm long branch segment`, etc.). |

```yaml
contexts:
- context_property: sapwood sampling method
  category: method_context
  var_in: method_context
  values:
  - value: 10mm2 stem cross-sectional area
    description: Leaf area was determined for a shoot whose sapwood had a cross
      sectional area of 10 mm2.
- context_property: stand age
  category: plot_context
  var_in: stand_maturity
  values:
  - value: declining
    description: Stand is at a declining stage.
```

> Schema note: `traits.build_schema.yml`'s `austraits.contexts.elements.category`
> description text says `individual_context`; the `metadata.contexts` block and
> every real dataset use `entity_context`. Use `entity_context` — it is what
> the pipeline and every existing dataset actually write.

### `traits`

An array, one entry per mapped column (wide format) or per distinct
`trait_name` value (long format), produced by `metadata_add_traits()`:

```yaml
traits:
- var_in: leaf area (mm2)
  unit_in: .na
  trait_name: .na
  entity_type: .na
  value_type: .na
  basis_of_value: .na
  replicates: .na
  methods: .na
```

| Field | Meaning |
|---|---|
| `var_in` | Column name (wide) or the value found in the `trait_name` column (long). |
| `unit_in` | Unit as supplied in the source. `.na` for categorical traits. See §6. |
| `trait_name` | **Must exist in `config/traits.yml`.** See non-negotiable below. |
| `entity_type` | See §5. |
| `value_type` | See §5. |
| `basis_of_value` | See §5. |
| `replicates` | Number of measurements comprising each value: `1` for raw/individual values, an integer for a mean of n leaves, `unknown` if not stated, a column name if replicate count varies by row, `.na` for categorical traits. |
| `methods` | Verbatim from the source, **trimmed to only the portion describing how this specific trait was measured** — not the whole Methods section. See "Scoping note" below `sampling_strategy`, above. Use YAML anchors (`&method_name` / `*method_name`) to share identical text across traits that were genuinely measured the same way — see `Falster_2005_1`'s `huber_value` block. |

Optional additions: `life_stage`, `basis_of_record` (override the dataset-wide
value for this trait only), `measurement_remarks`, `method_context` /
`temporal_context` (a short phrase naming which context value applies to this
trait's column — must also appear in `contexts:`), `repeat_measurements_id`.

### Non-negotiable: never invent a `trait_name`

`trait_name` must be an existing concept in this database's `config/traits.yml`
— never invented, never a close-enough guess. If no adequate concept exists,
leave `trait_name: .na`, fill in everything else that's knowable (`var_in`,
`unit_in`, `methods`, etc.), and record the column under `questions:
additional_traits:` instead. A future curator fills in the real `trait_name`
once (if) a matching concept is added to the dictionary.

---

## 5. Controlled vocabularies

These four are enforced by `dataset_test()` as `type: categorical` blocks in
`traits.build_schema.yml`; values below are the **complete, current lists** —
don't trust a shorter list from the book.

### `entity_type`

The organismal level the trait measurement applies to — independent of the
taxonomic resolution of the entity's *name*.

`individual`, `population`, `metapopulation`, `subspecies`, `species`,
`subsection`, `section`, `subgenus`, `genus`, `subtribe`, `tribe`,
`supertribe`, `subfamily`, `family`, `superfamily`, `infraorder`, `suborder`,
`order`, `superorder`, `subclass`, `class`, `subphylum`, `phylum`,
`subdivision`, `division`, `kingdom`

In practice the overwhelming majority of datasets use only `individual`,
`population`, `species`, `genus`, `family`, and `order`; the rest of the list
exists for the rare above-family compilation. `metapopulation` covers
multi-location summary statistics; `subspecies` and infraspecific data are
coded `species`.

### `value_type`

The statistical nature of the recorded value.

`raw`, `minimum`, `mean`, `median`, `maximum`, `mode`, `range`, `bin`,
`standard_error`, `standard_deviation`, `unknown`

Categorical traits are almost always `mode` (the most commonly observed
value). For `bin` values, separate the two numbers with a double-hyphen:
`1--10`.

### `basis_of_value`

How the value was obtained.

`measurement`, `expert_score`, `model_derived`, `unknown`

Most categorical traits are `expert_score`; most numeric traits are
`measurement`. (The book's prose lists a fourth option, `literature` — that is
actually a `basis_of_record` value, not `basis_of_value`; the schema does not
allow `literature` here. Don't carry that mistake forward.)

### `basis_of_record`

What kind of specimen/observation the traits were recorded from.

| Value | Meaning |
|---|---|
| `field` | Entities living naturally in the field. |
| `field_experiment` | Entities under experimentally manipulated field conditions. |
| `captive_cultivated` | Entities in a common garden, arboretum, botanical/zoological garden. |
| `lab` | Entities growing in a lab, glasshouse, or growth chamber. |
| `preserved_specimen` | Recorded from a preserved collection specimen (herbarium, museum). |
| `literature` | Sourced from values reported in the literature, basis otherwise unknown. |

### `life_stage`

Unlike the four above, `life_stage` is **not** defined as a `type: categorical`
block in the schema — it's documented only as free text with a stated
convention: "standard values are `adult`, `sapling`, `seedling` and
`juvenile`". Treat these four as the vocabulary to reuse; don't invent a fifth
without checking with a database maintainer first.

---

## 6. Units (UCUM)

`traits.build` databases use the [Unified Code for Units of Measure](https://ucum.org/ucum)
convention for `unit_in` (and the harmonised `unit` in the compiled database).
Each database chooses its own concrete abbreviations, but the syntax rules are
fixed:

- `/` separates numerator and denominator: `mg/mm2`, `umol/m2/s`.
- Curly braces annotate a unit with information that is not itself a unit —
  e.g. what's being counted or measured — without changing the numeric
  conversion: `{count}/mm2`, `umol{CO2}/m2/s`, `mmol{H2O}/m2/s`,
  `nmol{CO2}/g/s`, `umol{chlorophyll}/m2`.
- `a` means `year` (annum) — not obvious, but standard UCUM.
- Check `config/traits.yml` and `config/unit_conversions.csv` before inventing
  a unit string; reuse what's already there.

**House rules** (restated here as the authoritative reference — both are
non-negotiables elsewhere in this skill):

- **Quote a unit that starts with punctuation.** YAML will otherwise
  misparse it: `unit_in: '{count}/mm2'`, not `unit_in: {count}/mm2`.
- **Use `neg_MPa`, never `-MPa`.** A leading `-` reads as a YAML/numeric sign,
  not part of the unit string.

---

## 7. Empty sections

For a first-pass simple dataset, these five sections legitimately have
nothing in them. Leave each as literally `.na` — don't create an empty array
(`[]`) or omit the key:

```yaml
substitutions: .na
taxonomic_updates: .na
exclude_observations: .na
questions: .na
```

(`contexts: .na` as well, when the dataset has no context properties at all.)

Once populated:

- **`substitutions`** — `trait_name` / `find` / `replace` triples aligning a
  contributor's categorical spelling with the trait dictionary's allowed
  values. `trait_name` and `find` are the *from*-side (fillable by the skill);
  `replace` is always the curator's decision and arrives blank.
- **`taxonomic_updates`** — `find` / `replace` / `reason` / `taxonomic_resolution`
  aligning a submitted taxon name to the taxonomic reference.
- **`exclude_observations`** — `variable` / `find` / `reason` for values that
  exist in `data.csv` but must be dropped entirely (e.g. non-native species).
- **`questions`** — free-form contributor questions (first one prefixed
  `contributor:`, subsequent ones `question2:`, etc.) plus, indented under
  `additional_traits:`, any columns worth measuring that have no home in
  `config/traits.yml` yet.
