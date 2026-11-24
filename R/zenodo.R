#' Zenodo deposit metadata for a database release
#'
#' Builds the metadata of a new Zenodo version of a database from the
#' `metadata` block of its `config/metadata.yml`, as copied into the built
#' database, so the deposit no longer has to be typed into Zenodo's web form.
#' Pass the result to [zenodo_deposit()].
#'
#' @section Where each field comes from:
#'
#' * `title`, `version`, `references`, `language` and the licence come from
#'   the fields of the same name. `publication_date` is converted to
#'   `YYYY-MM-DD`; it must parse, so set it before the release build.
#' * The description is `config/zenodo_description.md` when that file exists,
#'   otherwise the `description` field. It is Markdown, rendered to HTML, and
#'   `{{version}}`, `{{doi}}`, `{{publication_date}}` and `{{publication_year}}`
#'   in it are replaced by those fields.
#' * `creators` come from `creators`, as `last_name`, `given_name`, `ORCID`
#'   and `affiliation`.
#' * `contributors` are, in this order:
#'   1. `project_contributors`: people credited on the deposit who are not data
#'      collectors of any dataset, such as project staff and curators. Each has
#'      the fields of a creator plus `role`, a DataCite contributor type such as
#'      `ProjectMember` (the default), `DataManager` or `DataCurator`. Those
#'      with role `DataCollector` are listed among the data collectors.
#'   2. every data collector of every dataset (the database's `contributors`
#'      table), as `DataCollector`, one entry per person;
#'   3. every dataset's `assistants`, as `DataCollector`, unless already listed
#'      as a data collector. These are free text in a dataset's `metadata.yml`,
#'      so they carry a name only;
#'   4. the licence's `rights_holder`, as `RightsHolder`.
#'
#'   A person is listed once per role: entries with the same ORCID, or the same
#'   surname and first given name, are merged, keeping the ORCID and
#'   affiliation where one entry lacks them.
#' * `related_identifiers` come from `related_identifiers`. The relation (in
#'   `relation_type` or `relation`) must be one Zenodo accepts, such as
#'   `isCompiledBy` or `isDescribedBy`; case is corrected. An entry naming the
#'   deposit's own concept DOI (`doi`) is left out, because Zenodo links the
#'   versions of a record itself, and so is a repeat of an identifier with the
#'   same relation (such as the traits.build repository the build adds).
#' * `locations` come from `geo_location$geo_location_place`.
#' * `grants` come from `funding_reference`, but only for entries that give a
#'   `funder_identifier` (the funder's ROR id, such as `038sjwq14` for the
#'   ARDC, or Crossref Funder DOI): Zenodo accepts only awards in its own
#'   vocabulary, identified as `<funder>::<award_number>`. Without any, the
#'   grants are left out, and [zenodo_deposit()] keeps those of the previous
#'   version.
#'
#' Keywords, subjects, communities and notes are not set here, so a new
#' version keeps the previous version's.
#'
#' @param database A built database, such as `austraits`. Its data collectors,
#'   assistants and metadata are used.
#' @param metadata The `metadata` block of `config/metadata.yml`. Defaults to
#'   the copy in `database`, which is the metadata the release was built with.
#' @param description_file Markdown file holding the deposit's description, or
#'   `NULL` to use the `description` field. Defaults to
#'   `config/zenodo_description.md` when that file exists.
#'
#' @return A list in the format of the metadata of Zenodo's deposition API,
#'   for [zenodo_deposit()].
#' @seealso [zenodo_compare()], [zenodo_deposit()]
#' @export
#' @examples
#' \dontrun{
#' metadata <- zenodo_metadata(austraits)
#' str(metadata, max.level = 1)
#' }
zenodo_metadata <- function(database,
                            metadata = database$metadata,
                            description_file = zenodo_default_description()) {
  if (is.null(metadata) || !is.list(metadata)) {
    stop("`metadata` must be the `metadata` block of `config/metadata.yml`.")
  }

  version <- zenodo_scalar(metadata$version)
  if (is.na(version)) stop("The metadata has no `version`.")
  if (grepl("\\.9000$", version)) {
    warning("Version ", version, " is a development version, not a release.")
  }

  publication_date <- zenodo_date(metadata$publication_date)
  creators <- zenodo_people(metadata$creators)
  if (length(creators) == 0) stop("The metadata lists no `creators`.")
  creators <- lapply(creators, \(p) p[setdiff(names(p), "type")])

  out <- list(
    upload_type = "dataset",
    title = zenodo_scalar(metadata$title),
    version = version,
    publication_date = publication_date,
    description = zenodo_description(metadata, description_file, publication_date),
    creators = creators,
    contributors = zenodo_contributors(database, metadata, creators),
    related_identifiers = zenodo_related_identifiers(metadata),
    references = if (is.na(zenodo_scalar(metadata$references))) list() else list(zenodo_scalar(metadata$references)),
    language = zenodo_language(metadata$language),
    access_right = "open",
    license = zenodo_license(metadata$license),
    locations = zenodo_locations(metadata$geo_location),
    grants = zenodo_grants(metadata$funding_reference)
  )
  # Fields with nothing to say are left out, so the previous version's are kept
  out[lengths(out) > 0 & !vapply(out, \(x) is.character(x) && length(x) == 1 && is.na(x), logical(1))]
}

#' Compare a Zenodo version's metadata with the previous version's
#'
#' Lists who and what the new version would lose: creators, contributors,
#' related identifiers and grants on the previous version that are not on the
#' new one. People are matched by ORCID, by surname and first initial, or by
#' the words of their name in any order, so a change of affiliation, role or
#' spelling of a given name is not a loss. A contributor who stays on the
#' deposit in another role is not listed; a creator who is no longer a creator
#' is, even if still a contributor.
#'
#' @param previous Metadata of the previous version, as returned by Zenodo
#'   (`metadata` of a record from its API).
#' @param new Metadata of the new version, from [zenodo_metadata()].
#'
#' @return A list of tibbles: `creators` and `contributors` (columns `name`,
#'   `orcid`, `type`) dropped from the deposit; `renamed`, people whose ORCID
#'   is on both but whose surname differs, with `role` `creator` (often a
#'   given name and surname swapped in `config/metadata.yml`) or `contributor`
#'   (often a spelling corrected in a dataset's `metadata.yml`); a fuller given
#'   name is not listed;
#'   `related_identifiers` and `grants` dropped. `grants` is empty when `new`
#'   has no grants, as the previous ones are then kept.
#' @seealso [zenodo_metadata()], [zenodo_deposit()]
#' @export
zenodo_compare <- function(previous, new) {
  people <- function(x, type = NA_character_) {
    tibble::tibble(
      name = vapply(x, \(p) zenodo_or(p$name, NA_character_), character(1)),
      orcid = vapply(x, \(p) zenodo_or(p$orcid, NA_character_), character(1)),
      type = vapply(x, \(p) zenodo_or(p$type, type), character(1))
    )
  }
  prev_creators <- people(previous$creators, "creator")
  prev_contributors <- people(previous$contributors)
  new_creators <- people(new$creators, "creator")
  new_people <- dplyr::bind_rows(new_creators, people(new$contributors))

  missing_from <- function(x, pool) {
    found <- (!is.na(x$orcid) & x$orcid %in% pool$orcid) |
      zenodo_initial_key(x$name) %in% zenodo_initial_key(pool$name) |
      zenodo_word_key(x$name) %in% zenodo_word_key(pool$name)
    x[!found, ]
  }

  # Same ORCID, different surname, compared within creators and within
  # contributors, so that a creator's swapped names are not hidden by a
  # correct spelling among the data collectors
  renamed_in <- function(before, after) {
    with_orcid <- function(x) {
      x %>% dplyr::filter(!is.na(.data$orcid)) %>% dplyr::distinct(.data$name, .data$orcid)
    }
    with_orcid(before) %>%
      dplyr::inner_join(with_orcid(after), by = "orcid", suffix = c("_previous", "_new"),
                        relationship = "many-to-many") %>%
      dplyr::group_by(.data$orcid) %>%
      dplyr::filter(!any(zenodo_surname(.data$name_previous) == zenodo_surname(.data$name_new))) %>%
      dplyr::ungroup()
  }
  renamed <- dplyr::bind_rows(
    renamed_in(prev_creators, new_creators) %>% dplyr::mutate(role = "creator"),
    renamed_in(prev_contributors, people(new$contributors)) %>% dplyr::mutate(role = "contributor")
  ) %>%
    dplyr::distinct() %>%
    dplyr::select(dplyr::all_of(c("name_previous", "name_new", "orcid", "role")))

  identifiers <- function(x) {
    tibble::tibble(
      identifier = vapply(x, \(i) zenodo_or(i$identifier, NA_character_), character(1)),
      relation = vapply(x, \(i) zenodo_or(i$relation, NA_character_), character(1))
    )
  }
  prev_ids <- identifiers(previous$related_identifiers)
  new_ids <- identifiers(new$related_identifiers)
  id_key <- function(x) paste(tolower(zenodo_bare_identifier(x$identifier)), tolower(x$relation))
  dropped_ids <- prev_ids[!id_key(prev_ids) %in% id_key(new_ids), ]

  grant_code <- function(x) {
    vapply(x, \(g) sub("^.*::", "", zenodo_or(g$id, zenodo_or(g$internal_id, zenodo_or(g$code, NA_character_)))),
           character(1))
  }
  dropped_grants <- if (length(new$grants) == 0) {
    tibble::tibble(grant = character())
  } else {
    prev_grants <- grant_code(previous$grants)
    tibble::tibble(grant = setdiff(prev_grants, grant_code(new$grants)))
  }

  list(
    creators = missing_from(prev_creators, new_creators),
    contributors = missing_from(prev_contributors, new_people),
    renamed = renamed,
    related_identifiers = dropped_ids,
    grants = dropped_grants
  )
}

#' Deposit a database release on Zenodo as a draft
#'
#' Creates a new version of a Zenodo record, or a new record, as a **draft**,
#' sets its metadata, and optionally replaces its files. It never publishes:
#' review the draft on Zenodo and publish it there.
#'
#' Before anything is written, the new metadata is compared with the latest
#' published version's ([zenodo_compare()]). If a creator, contributor or
#' related identifier would be dropped, or a creator's surname changes for the
#' same ORCID, the deposit stops and lists them, unless `allow_removals` is
#' `TRUE`. Add the missing people to `project_contributors` (or the related
#' identifiers to `related_identifiers`) in `config/metadata.yml` instead,
#' where they will be kept for later versions.
#'
#' The new metadata replaces these fields of the draft and keeps the rest from
#' the previous version (such as keywords, communities and notes, and the
#' grants unless [zenodo_metadata()] produced some).
#'
#' Files go up smallest first, and a file already in the draft with the same
#' name and MD5 checksum is not uploaded again, so an interrupted upload can
#' be resumed by running the same call with `draft` set to the draft's id.
#'
#' @param metadata Metadata from [zenodo_metadata()].
#' @param record The concept record id (the number in the concept DOI, such as
#'   `3568417` for AusTraits) or the id of any of its published versions; the
#'   new version follows the latest. `NA` starts a new record instead. There is
#'   no default, so a release cannot start a new record by mistake.
#' @param files Paths of the files the version should hold, or `NULL` to keep
#'   the previous version's files. Files in the draft that are not among these
#'   are deleted from the draft.
#' @param draft The id of a draft to resume, such as one left by an interrupted
#'   upload. `NULL` creates a new draft (for a new version, Zenodo returns the
#'   draft already open for the record, if there is one).
#' @param sandbox `TRUE` (the default) to use <https://sandbox.zenodo.org>, for
#'   trying a deposit; `FALSE` for <https://zenodo.org>.
#' @param token A Zenodo personal access token with scope `deposit:write`.
#'   Defaults to the environment variable `ZENODO_SANDBOX_TOKEN` for the
#'   sandbox and `ZENODO_TOKEN` otherwise; tokens work on one server only.
#' @param allow_removals `TRUE` to deposit even though people or related
#'   identifiers on the previous version are missing from the new one.
#'
#' @return Invisibly, a list: `id` and `url` of the draft, the `comparison`
#'   from [zenodo_compare()] (`NULL` for a new record), and `files`, the names
#'   of the files uploaded.
#' @seealso [zenodo_metadata()], [zenodo_compare()]
#' @export
#' @examples
#' \dontrun{
#' metadata <- zenodo_metadata(austraits)
#' files <- c(list.files("export/data/austraits-8.0.0", full.names = TRUE), "README.md")
#' # Try it on the sandbox first, with a record there
#' zenodo_deposit(metadata, record = NA, files = files, sandbox = TRUE)
#' # Then the real deposit
#' zenodo_deposit(metadata, record = 3568417, files = files, sandbox = FALSE)
#' }
zenodo_deposit <- function(metadata, record, files = NULL, draft = NULL,
                           sandbox = TRUE, token = NULL,
                           allow_removals = FALSE) {
  rlang::check_installed("httr2", "to deposit on Zenodo.")
  if (missing(record)) {
    stop("Give `record`: the concept record id to add a version to, or NA for a new record.")
  }
  if (is.null(token)) {
    token <- Sys.getenv(if (sandbox) "ZENODO_SANDBOX_TOKEN" else "ZENODO_TOKEN")
  }
  if (!nzchar(token)) {
    stop("Set ", if (sandbox) "ZENODO_SANDBOX_TOKEN" else "ZENODO_TOKEN",
         " to a Zenodo personal access token for ",
         if (sandbox) "sandbox.zenodo.org" else "zenodo.org", ", or pass `token`.")
  }
  if (!is.null(files)) {
    if (!all(file.exists(files))) {
      stop("Files not found: ", paste(files[!file.exists(files)], collapse = ", "))
    }
    if (anyDuplicated(basename(files))) {
      stop("Two files share a name: ", paste(unique(basename(files)[duplicated(basename(files))]), collapse = ", "))
    }
    files <- files[order(file.size(files))]
  }

  base <- if (sandbox) "https://sandbox.zenodo.org/api" else "https://zenodo.org/api"
  api <- zenodo_api(base, token)

  # The latest published version, to compare with and to version from
  comparison <- NULL
  latest <- NULL
  if (!is.na(record)) {
    latest <- api(paste0("records/", record, "/versions/latest")) %>% zenodo_perform()
    comparison <- zenodo_compare(latest$metadata, metadata)
    zenodo_report_comparison(comparison, latest)
    lost <- nrow(comparison$creators) + nrow(comparison$contributors) +
      sum(comparison$renamed$role == "creator") + nrow(comparison$related_identifiers) +
      nrow(comparison$grants)
    if (lost > 0 && !isTRUE(allow_removals)) {
      stop("The new version would drop or rename what is listed above. Add them to ",
           "`config/metadata.yml` (people to `project_contributors`) and rebuild, ",
           "or pass `allow_removals = TRUE` if that is intended.", call. = FALSE)
    }
  }

  # The draft
  deposition <- if (!is.null(draft)) {
    api(paste0("deposit/depositions/", draft)) %>% zenodo_perform()
  } else if (is.na(record)) {
    api("deposit/depositions") %>%
      httr2::req_body_json(structure(list(), names = character())) %>%
      zenodo_perform()
  } else {
    versioned <- api(paste0("deposit/depositions/", latest$id, "/actions/newversion")) %>%
      httr2::req_method("POST") %>%
      zenodo_perform()
    draft_url <- zenodo_or(versioned$links$latest_draft, versioned$links$self)
    zenodo_api_url(draft_url, token) %>% zenodo_perform()
  }
  # A published record opened for editing is not a draft: writing to it would
  # change the published version.
  if (!identical(deposition$submitted, FALSE)) {
    stop("Deposition ", deposition$id, " is not an unpublished draft (state '",
         zenodo_or(deposition$state, "?"), "'), so it was left alone.")
  }

  # Metadata: the new fields replace the draft's, the rest are kept
  # (`doi` is left out so that Zenodo mints the version's own DOI, and
  # `relations` is computed by Zenodo.)
  merged <- zenodo_or(deposition$metadata, list())
  merged <- merged[setdiff(names(merged), c("doi", "relations"))]
  for (f in names(metadata)) merged[[f]] <- metadata[[f]]
  deposition <- api(paste0("deposit/depositions/", deposition$id)) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(list(metadata = merged), auto_unbox = TRUE, null = "null") %>%
    zenodo_perform()

  uploaded <- character()
  if (!is.null(files)) {
    uploaded <- zenodo_sync_files(api, token, deposition, files)
  }

  url <- zenodo_or(deposition$links$html, paste0(sub("/api$", "", base), "/deposit/", deposition$id))
  message("Draft ", deposition$id, " is ready for review, and has NOT been published: ", url)
  invisible(list(id = deposition$id, url = url, comparison = comparison, files = uploaded))
}

# ---- helpers ---------------------------------------------------------------

zenodo_or <- function(x, y) if (is.null(x) || length(x) == 0) y else x

zenodo_default_description <- function() {
  if (file.exists("config/zenodo_description.md")) "config/zenodo_description.md" else NULL
}

# One value of a yml field, NA when absent or a placeholder
zenodo_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x[[1]])
  if (is.na(x) || trimws(x) %in% c("", ".na", "NA", "NA_character_", "unknown")) return(NA_character_)
  trimws(x)
}

zenodo_date <- function(x) {
  x <- zenodo_scalar(x)
  if (is.na(x)) stop("The metadata has no `publication_date`; set it to the release date.")
  date <- suppressWarnings(lubridate::parse_date_time(x, c("dmY", "Ymd", "dbY", "mdY"), quiet = TRUE))
  if (is.na(date)) stop("Cannot read `publication_date` '", x, "' as a date.")
  format(as.Date(date), "%Y-%m-%d")
}

zenodo_valid_orcid <- function(x) {
  x <- sub("^.*orcid\\.org/", "", trimws(as.character(x)))
  ifelse(!is.na(x) & grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x), x, NA_character_)
}

# Normalise a name for matching: no accents, punctuation or case
zenodo_fold <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(gsub("[^[:alpha:] ,'-]", " ", x))
  trimws(gsub("\\s+", " ", x))
}

# "Surname, Given Names" -> "surname given", the first given name only
zenodo_name_key <- function(name) {
  parts <- strsplit(zenodo_fold(name), ",", fixed = TRUE)
  vapply(parts, \(p) {
    surname <- trimws(p[1])
    given <- if (length(p) > 1) strsplit(trimws(p[2]), " ")[[1]][1] else ""
    trimws(paste(surname, zenodo_or(given, "")))
  }, character(1))
}

# The words of a name in alphabetical order, so that "McC Overton, Jacob" and
# "Overton, Jacob McC" match
zenodo_word_key <- function(name) {
  vapply(strsplit(gsub(",", " ", zenodo_fold(name)), " +"), \(w) paste(sort(w), collapse = " "), character(1))
}

zenodo_surname <- function(name) trimws(sub(",.*$", "", zenodo_fold(name)))

# "Surname, Given Names" -> "surname g"
zenodo_initial_key <- function(name) {
  parts <- strsplit(zenodo_fold(name), ",", fixed = TRUE)
  vapply(parts, \(p) {
    if (length(p) == 0 || is.na(p[1])) return(NA_character_)
    given <- if (length(p) > 1) trimws(p[2]) else ""
    trimws(paste(trimws(p[1]), substr(given, 1, 1)))
  }, character(1))
}

zenodo_bare_identifier <- function(x) {
  sub("/+$", "", sub("^https?:+/+(dx\\.)?(doi\\.org/)?", "", trimws(x)))
}

# People in config format (last_name, given_name, ORCID, affiliation, role) to
# Zenodo format (name, orcid, affiliation, type)
zenodo_people <- function(x, default_role = NA_character_) {
  if (is.null(x) || all(is.na(unlist(x))) || !is.list(x)) return(list())
  if (!is.null(x$last_name)) x <- list(x)
  out <- lapply(x, \(p) {
    last <- zenodo_scalar(p$last_name)
    if (is.na(last)) stop("A person in the metadata has no `last_name`: ", paste(unlist(p), collapse = ", "))
    given <- zenodo_scalar(p$given_name)
    role <- zenodo_scalar(p$role)
    if (is.na(role)) role <- default_role
    person <- list(
      name = if (is.na(given)) last else paste0(last, ", ", given),
      affiliation = zenodo_scalar(p$affiliation),
      orcid = zenodo_valid_orcid(zenodo_scalar(p$ORCID)),
      type = role
    )
    person[!vapply(person, is.na, logical(1))]
  })
  out
}

zenodo_contributor_types <- function() {
  c("ContactPerson", "DataCollector", "DataCurator", "DataManager", "Distributor",
    "Editor", "HostingInstitution", "Producer", "ProjectLeader", "ProjectManager",
    "ProjectMember", "RegistrationAgency", "RegistrationAuthority", "RelatedPerson",
    "Researcher", "ResearchGroup", "RightsHolder", "Sponsor", "Supervisor",
    "WorkPackageLeader", "Other")
}

# Merge people listed more than once within a role: same ORCID, or same
# surname and first given name (and no conflicting ORCID). The first entry's
# name is kept, and gains an ORCID or affiliation another entry has.
zenodo_merge_people <- function(people) {
  if (length(people) == 0) return(people)
  out <- list()
  keys <- character()
  orcids <- character()
  types <- character()
  for (p in people) {
    key <- zenodo_name_key(p$name)
    type <- zenodo_or(p$type, "")
    orcid <- zenodo_or(p$orcid, NA_character_)
    same <- which(types == type & (
      (!is.na(orcid) & orcids %in% orcid) |
        (keys == key & (is.na(orcid) | is.na(orcids)))
    ))
    if (length(same) > 0) {
      i <- same[1]
      for (f in c("orcid", "affiliation")) {
        if (is.null(out[[i]][[f]]) && !is.null(p[[f]])) out[[i]][[f]] <- p[[f]]
      }
      if (is.na(orcids[i]) && !is.na(orcid)) orcids[i] <- orcid
    } else {
      out[[length(out) + 1]] <- p
      keys <- c(keys, key)
      orcids <- c(orcids, orcid)
      types <- c(types, type)
    }
  }
  out
}

# Free-text names ("Ann Smith, Bo van Dyke and C. Lee") to "Surname, Given"
zenodo_split_names <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(character())
  names <- unlist(strsplit(x, "\\s*(,|;|&|\\band\\b)\\s*"))
  names <- trimws(gsub("\\s+", " ", names))
  names <- names[nzchar(names) & !tolower(names) %in% c("unknown", "na", ".na")]
  vapply(names, \(n) {
    words <- strsplit(n, " ")[[1]]
    if (length(words) == 1) return(words)
    # The surname is the last word, with any lower-case particles before it
    i <- length(words)
    while (i > 2 && grepl("^[[:lower:]]", words[i - 1])) i <- i - 1
    paste0(paste(words[i:length(words)], collapse = " "), ", ", paste(words[seq_len(i - 1)], collapse = " "))
  }, character(1), USE.NAMES = FALSE) %>% unique()
}

zenodo_contributors <- function(database, metadata, creators) {
  project <- zenodo_people(metadata$project_contributors, default_role = "ProjectMember")
  bad <- setdiff(vapply(project, \(p) p$type, character(1)), zenodo_contributor_types())
  if (length(bad) > 0) {
    stop("`project_contributors` role must be one of ", paste(zenodo_contributor_types(), collapse = ", "),
         "; found ", paste(bad, collapse = ", "), ".")
  }

  collectors <- list()
  if (!is.null(database$contributors) && nrow(database$contributors) > 0) {
    tbl <- database$contributors %>%
      dplyr::select(dplyr::any_of(c("last_name", "given_name", "ORCID", "affiliation"))) %>%
      dplyr::distinct()
    collectors <- zenodo_people(
      lapply(seq_len(nrow(tbl)), \(i) as.list(tbl[i, ])), default_role = "DataCollector"
    )
    collectors <- collectors[order(zenodo_fold(vapply(collectors, \(p) p$name, character(1))))]
  }
  collectors <- zenodo_merge_people(collectors)

  assistant_names <- character()
  if (!is.null(database$methods) && "assistants" %in% names(database$methods)) {
    assistant_names <- zenodo_split_names(unique(database$methods$assistants))
  }
  known <- c(zenodo_initial_key(vapply(collectors, \(p) p$name, character(1))),
             zenodo_name_key(vapply(collectors, \(p) p$name, character(1))))
  assistant_names <- assistant_names[!(zenodo_name_key(assistant_names) %in% known |
                                         zenodo_initial_key(assistant_names) %in% known)]
  assistants <- lapply(sort(assistant_names), \(n) list(name = n, type = "DataCollector"))

  # Project contributors who are data collectors are listed among them
  is_collector <- vapply(project, \(p) identical(p$type, "DataCollector"), logical(1))
  data_collectors <- zenodo_merge_people(c(project[is_collector], collectors, assistants))
  project <- project[!is_collector]
  data_collectors <- data_collectors[order(zenodo_fold(vapply(data_collectors, \(p) p$name, character(1))))]

  rights <- list()
  holder <- zenodo_scalar(metadata$license$rights_holder)
  if (!is.na(holder)) {
    match <- Filter(\(p) zenodo_name_key(p$name) == zenodo_name_key(holder), creators)
    person <- if (length(match) > 0) match[[1]] else list(name = holder)
    person$type <- "RightsHolder"
    rights <- list(person)
  }

  out <- zenodo_merge_people(c(project, data_collectors, rights))
  lapply(out, \(p) p[intersect(c("name", "type", "affiliation", "orcid"), names(p))])
}

zenodo_relations <- function() {
  c("isCitedBy", "cites", "isSupplementTo", "isSupplementedBy", "isContinuedBy",
    "continues", "isDescribedBy", "describes", "hasMetadata", "isMetadataFor",
    "isNewVersionOf", "isPreviousVersionOf", "isPartOf", "hasPart", "isReferencedBy",
    "references", "isDocumentedBy", "documents", "isCompiledBy", "compiles",
    "isVariantFormOf", "isOriginalFormOf", "isIdenticalTo", "isAlternateIdentifier",
    "isReviewedBy", "reviews", "isDerivedFrom", "isSourceOf", "requires",
    "isRequiredBy", "isObsoletedBy", "obsoletes", "isPublishedIn", "isVersionOf",
    "hasVersion", "isCollectedBy", "collects", "isTranslationOf", "hasTranslation")
}

zenodo_resource_types <- function() {
  c("publication", "publication-annotationcollection", "publication-book",
    "publication-section", "publication-conferencepaper", "publication-datamanagementplan",
    "publication-article", "publication-patent", "publication-preprint",
    "publication-deliverable", "publication-milestone", "publication-proposal",
    "publication-report", "publication-softwaredocumentation", "publication-taxonomictreatment",
    "publication-technicalnote", "publication-thesis", "publication-workingpaper",
    "publication-other", "publication-standard", "poster", "presentation", "dataset",
    "image", "image-figure", "image-plot", "image-drawing", "image-diagram", "image-photo",
    "image-other", "video", "software", "lesson", "physicalobject", "workflow", "other")
}

zenodo_related_identifiers <- function(metadata) {
  x <- metadata$related_identifiers
  if (is.null(x) || !is.list(x)) return(list())
  if (!is.null(x$identifier)) x <- list(x)
  concept <- zenodo_bare_identifier(zenodo_scalar(metadata$doi))
  relations <- zenodo_relations()
  out <- lapply(x, \(i) {
    identifier <- zenodo_scalar(i$identifier)
    if (is.na(identifier)) return(NULL)
    if (!is.na(concept) && tolower(zenodo_bare_identifier(identifier)) == tolower(concept)) return(NULL)
    relation <- zenodo_scalar(zenodo_or(i$relation_type, i$relation))
    matched <- relations[tolower(relations) == tolower(relation)]
    if (length(matched) == 0) {
      stop("Related identifier ", identifier, " has relation '", relation,
           "', which Zenodo does not accept. Use one of: ", paste(relations, collapse = ", "), ".")
    }
    resource_type <- tolower(zenodo_scalar(i$resource_type))
    entry <- list(identifier = identifier, relation = matched)
    if (!is.na(resource_type)) {
      if (!resource_type %in% zenodo_resource_types()) {
        stop("Related identifier ", identifier, " has resource_type '", resource_type,
             "', which Zenodo does not accept. Use one of: ",
             paste(zenodo_resource_types(), collapse = ", "), ".")
      }
      entry$resource_type <- resource_type
    }
    entry
  })
  out <- Filter(Negate(is.null), out)
  # The build adds traits.build's repository, which the config may also list
  key <- vapply(out, \(i) paste(tolower(zenodo_bare_identifier(i$identifier)), i$relation), character(1))
  out[!duplicated(key)]
}

zenodo_language <- function(x) {
  x <- tolower(zenodo_scalar(x))
  if (is.na(x)) return(NA_character_)
  iso2 <- c(en = "eng", fr = "fra", de = "deu", es = "spa", pt = "por", it = "ita",
            nl = "nld", zh = "zho", ja = "jpn")
  if (x %in% names(iso2)) unname(iso2[x]) else x
}

zenodo_license <- function(x) {
  rights <- zenodo_scalar(x$rights)
  if (is.na(rights)) return(NA_character_)
  tolower(rights)
}

zenodo_locations <- function(x) {
  place <- zenodo_scalar(x$geo_location_place)
  if (is.na(place)) return(list())
  list(list(place = place))
}

zenodo_grants <- function(x) {
  if (is.null(x) || !is.list(x)) return(list())
  if (!is.null(x$award_number)) x <- list(x)
  out <- lapply(x, \(g) {
    funder <- zenodo_scalar(g$funder_identifier)
    award <- zenodo_scalar(g$award_number)
    if (is.na(funder) || is.na(award)) return(NULL)
    funder <- sub("^https?://(ror\\.org/|doi\\.org/)?", "", funder)
    list(id = paste0(funder, "::", award))
  })
  Filter(Negate(is.null), out)
}

zenodo_description <- function(metadata, description_file, publication_date) {
  text <- if (!is.null(description_file)) {
    if (!file.exists(description_file)) stop("Description file not found: ", description_file)
    paste(readLines(description_file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  } else {
    zenodo_scalar(metadata$description)
  }
  if (is.na(text)) stop("The metadata has no `description`.")
  text <- whisker::whisker.render(text, list(
    version = zenodo_scalar(metadata$version),
    doi = zenodo_scalar(metadata$doi),
    publication_date = publication_date,
    publication_year = substr(publication_date, 1, 4)
  ))
  rlang::check_installed("commonmark", "to write the Zenodo description.")
  trimws(commonmark::markdown_html(text))
}

zenodo_report_comparison <- function(comparison, latest) {
  message("Compared with version ", zenodo_or(latest$metadata$version, "?"),
          " (record ", latest$id, "):")
  show <- function(x, what) {
    if (nrow(x) == 0) return(invisible())
    message("  ", nrow(x), " ", what, ":")
    lines <- apply(x, 1, \(r) paste(stats::na.omit(r), collapse = " | "))
    message(paste0("    ", lines, collapse = "\n"))
  }
  show(comparison$creators, "creator(s) would be dropped")
  show(comparison$contributors, "contributor(s) would be dropped from the deposit")
  show(comparison$renamed[comparison$renamed$role == "creator", ], "creator(s) would be renamed (same ORCID)")
  show(comparison$renamed[comparison$renamed$role == "contributor", ],
       "contributor(s) renamed (same ORCID), as the datasets now spell them; not a reason to stop")
  show(comparison$related_identifiers, "related identifier(s) would be dropped")
  show(comparison$grants, "grant(s) would be dropped")
  if (sum(vapply(comparison, nrow, integer(1))) == 0) message("  nobody and nothing dropped.")
}

zenodo_api <- function(base, token) {
  function(path) zenodo_api_url(paste0(base, "/", path), token)
}

zenodo_api_url <- function(url, token) {
  httr2::request(url) %>%
    httr2::req_auth_bearer_token(token) %>%
    httr2::req_retry(
      max_tries = 5,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 502, 503, 504)
    )
}

zenodo_perform <- function(req) {
  req <- httr2::req_error(req, body = \(resp) {
    body <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    if (is.null(body)) return(NULL)
    details <- vapply(zenodo_or(body$errors, list()), \(e) paste0(zenodo_or(e$field, ""), ": ",
      paste(unlist(zenodo_or(e$messages, e$message)), collapse = " ")), character(1))
    c(zenodo_or(body$message, NULL), details)
  })
  resp <- httr2::req_perform(req)
  if (length(resp$body) == 0) return(list())
  httr2::resp_body_json(resp)
}

# Make the draft's files match `files`: delete the others, upload what is
# missing or differs. Returns the names uploaded.
zenodo_sync_files <- function(api, token, deposition, files) {
  present <- api(paste0("deposit/depositions/", deposition$id, "/files")) %>% zenodo_perform()
  local_md5 <- unname(tools::md5sum(files))
  names(local_md5) <- basename(files)
  for (f in present) {
    name <- zenodo_or(f$filename, f$key)
    checksum <- sub("^md5:", "", zenodo_or(f$checksum, ""))
    if (!name %in% names(local_md5) || !identical(unname(local_md5[name]), checksum)) {
      message("Removing ", name, " from the draft")
      api(paste0("deposit/depositions/", deposition$id, "/files/", f$id)) %>%
        httr2::req_method("DELETE") %>%
        zenodo_perform()
    }
  }
  keep <- vapply(present, \(f) {
    name <- zenodo_or(f$filename, f$key)
    name %in% names(local_md5) && identical(unname(local_md5[name]), sub("^md5:", "", zenodo_or(f$checksum, "")))
  }, logical(1))
  kept <- vapply(present[keep], \(f) zenodo_or(f$filename, f$key), character(1))

  bucket <- deposition$links$bucket
  if (is.null(bucket)) stop("Zenodo did not return an upload link (links$bucket) for draft ", deposition$id, ".")
  uploaded <- character()
  for (f in files[!basename(files) %in% kept]) {
    message(format(Sys.time(), "%H:%M:%S"), " uploading ", basename(f), " (",
            round(file.size(f) / 1e6, 1), " MB)")
    # Not retried within the request, which cannot re-read the file: rerun the
    # deposit with `draft` instead, and finished files are skipped.
    resp <- httr2::request(paste0(bucket, "/", utils::URLencode(basename(f), reserved = TRUE))) %>%
      httr2::req_auth_bearer_token(token) %>%
      httr2::req_method("PUT") %>%
      httr2::req_body_file(f, type = "application/octet-stream") %>%
      zenodo_perform()
    checksum <- sub("^md5:", "", zenodo_or(resp$checksum, ""))
    if (nzchar(checksum) && !identical(checksum, unname(local_md5[basename(f)]))) {
      stop("Upload of ", basename(f), " has the wrong checksum; run again with `draft = ", deposition$id, "`.")
    }
    uploaded <- c(uploaded, basename(f))
  }
  uploaded
}
