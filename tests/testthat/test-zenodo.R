# Zenodo deposit (#320). Nothing here talks to Zenodo: the deposit tests mock
# every HTTP request and check what would have been sent.

`%or%` <- function(x, y) if (is.null(x)) y else x

zenodo_test_metadata <- function() {
  list(
    title = "A test database",
    description = "A *test* database.",
    version = "8.0.0",
    doi = "10.5281/zenodo.3568417",
    geo_location = list(geo_location_place = "Australia", geodetic_datum = "EPSG:4326"),
    language = "en",
    related_identifiers = list(
      list(related_identifier_type = "url", identifier = "https://github.com/traitecoevo/traits.build",
           relation_type = "iscompiledby", resource_type = "Software"),
      list(related_identifier_type = "doi", identifier = "10.1038/s41597-021-01006-6",
           relation_type = "isDescribedBy", resource_type = "publication-article"),
      list(related_identifier_type = "doi", identifier = "10.5281/zenodo.3568417",
           relation_type = "isVersionOf", resource_type = "dataset")
    ),
    references = "Falster et al (2021) AusTraits. Scientific Data 8: 254",
    publisher = "Zenodo",
    publication_year = 2026L,
    publication_date = "1 Dec 2026",
    license = list(rights = "CC-BY-4.0", rights_holder = "Falster, Daniel"),
    funding_reference = list(
      list(funder_name = "Australian Research Council", award_number = "FT160100113",
           funder_identifier = "https://ror.org/05mmh0f86"),
      list(funder_name = "ARDC", award_number = "DP720")
    ),
    creators = list(
      list(last_name = "Falster", given_name = "Daniel", ORCID = "0000-0002-9814-092X",
           affiliation = "UNSW Sydney"),
      list(last_name = "Wenk", given_name = "Elizabeth", ORCID = "0000-0001-5640-5910",
           affiliation = "UNSW Sydney")
    ),
    project_contributors = list(
      list(last_name = "Kar", given_name = "Fonti", ORCID = "0000-0002-2760-3974",
           affiliation = "UNSW Sydney"),
      list(last_name = "Wenk", given_name = "Elizabeth", ORCID = "0000-0001-5640-5910",
           affiliation = "UNSW Sydney", role = "DataManager")
    ),
    contributors = NA
  )
}

zenodo_test_database <- function(metadata = zenodo_test_metadata()) {
  list(
    contributors = tibble::tribble(
      ~dataset_id, ~last_name, ~given_name, ~ORCID, ~affiliation, ~additional_role,
      "A_2020", "Clarke", "Peter", NA, "University of New England", NA,
      "B_2021", "Clarke", "Peter J.", NA, NA, "contact",
      "C_2022", "Brodribb", "Timothy", "0000-0002-4964-6107", "University of Tasmania", NA,
      "D_2023", "Brodribb", "Tim", "https://orcid.org/0000-0002-4964-6107", "UTAS", NA,
      "E_2024", "Andrew", "Samuel", "unknown", "CSIRO", NA
    ),
    methods = tibble::tibble(
      dataset_id = c("A_2020", "A_2020", "B_2021", "C_2022"),
      assistants = c("Sophia Amini, Tara Boreham and Marlien van der Merwe",
                     "Sophia Amini, Tara Boreham and Marlien van der Merwe",
                     "P. Clarke; unknown", NA)
    ),
    metadata = metadata
  )
}

zenodo_test_previous <- function() {
  list(
    version = "7.0.0",
    creators = list(
      list(name = "Falster, Daniel", orcid = "0000-0002-9814-092X"),
      list(name = "Wenk, Elizabeth", orcid = "0000-0001-5640-5910"),
      list(name = "Sauquet, Hervé", orcid = "0000-0001-8305-3236")
    ),
    contributors = list(
      list(name = "Kar, Fonti", type = "ProjectMember", orcid = "0000-0002-2760-3974"),
      list(name = "Baxter, Caitlan", type = "ProjectMember"),
      list(name = "Clarke, Peter J.", type = "DataCollector"),
      list(name = "Merwe, Marlien van der", type = "DataCollector"),
      list(name = "Brodribb, Timothy", type = "DataCollector", orcid = "0000-0002-4964-6107")
    ),
    related_identifiers = list(
      list(identifier = "https://github.com/traitecoevo/traits.build", relation = "isCompiledBy"),
      list(identifier = "10.1101/2021.01.04.425314", relation = "isDescribedBy")
    ),
    grants = list(list(code = "FT160100113"), list(code = "DP720A"))
  )
}

test_that("zenodo_metadata() maps config/metadata.yml to Zenodo's fields", {
  m <- zenodo_metadata(zenodo_test_database(), description_file = NULL)

  expect_equal(m$upload_type, "dataset")
  expect_equal(m$version, "8.0.0")
  expect_equal(m$publication_date, "2026-12-01")
  expect_equal(m$language, "eng")
  expect_equal(m$license, "cc-by-4.0")
  expect_equal(m$locations, list(list(place = "Australia")))
  expect_equal(m$references, list("Falster et al (2021) AusTraits. Scientific Data 8: 254"))
  expect_match(m$description, "<em>test</em>", fixed = TRUE)
  expect_equal(m$creators[[2]],
               list(name = "Wenk, Elizabeth", affiliation = "UNSW Sydney", orcid = "0000-0001-5640-5910"))

  # The concept DOI is left out; relation case and resource type are corrected
  expect_length(m$related_identifiers, 2)
  expect_equal(m$related_identifiers[[1]]$relation, "isCompiledBy")
  expect_equal(m$related_identifiers[[1]]$resource_type, "software")

  # Only awards with a funder identifier
  expect_equal(m$grants, list(list(id = "05mmh0f86::FT160100113")))
})

test_that("zenodo_metadata() leaves out grants when no funder identifier is given", {
  md <- zenodo_test_metadata()
  md$funding_reference[[1]]$funder_identifier <- NULL
  m <- zenodo_metadata(zenodo_test_database(md), description_file = NULL)
  expect_false("grants" %in% names(m))
})

test_that("zenodo_metadata() reads the description from a Markdown file", {
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(c("AusTraits version {{version}}, released {{publication_date}}.", "", "- one"), f)
  m <- zenodo_metadata(zenodo_test_database(), description_file = f)
  expect_match(m$description, "<p>AusTraits version 8.0.0, released 2026-12-01.</p>", fixed = TRUE)
  expect_match(m$description, "<li>one</li>", fixed = TRUE)
})

test_that("zenodo_metadata() refuses what Zenodo would reject or misread", {
  md <- zenodo_test_metadata()
  md$related_identifiers[[1]]$relation_type <- "Sources"
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL),
               "relation 'Sources'")

  md <- zenodo_test_metadata()
  md$publication_date <- "unknown"
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL),
               "publication_date")

  md <- zenodo_test_metadata()
  md$project_contributors[[1]]$role <- "Helper"
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL), "Helper")

  md <- zenodo_test_metadata()
  md$version <- "7.0.0.9000"
  expect_warning(zenodo_metadata(zenodo_test_database(md), description_file = NULL),
                 "development version")
})

test_that("zenodo_metadata() lists each contributor once per role", {
  m <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  people <- tibble::tibble(
    name = vapply(m$contributors, \(p) p$name, character(1)),
    type = vapply(m$contributors, \(p) p$type, character(1)),
    orcid = vapply(m$contributors, \(p) p$orcid %or% NA_character_, character(1))
  )

  # Project contributors first, in config order, ProjectMember by default
  expect_equal(people$name[1:2], c("Kar, Fonti", "Wenk, Elizabeth"))
  expect_equal(people$type[1:2], c("ProjectMember", "DataManager"))

  collectors <- people[people$type == "DataCollector", ]
  # Same ORCID, or same surname and first given name: merged
  expect_equal(sum(grepl("^Brodribb", collectors$name)), 1)
  expect_equal(sum(grepl("^Clarke", collectors$name)), 1)
  # "unknown" is not an ORCID
  expect_true(is.na(collectors$orcid[collectors$name == "Andrew, Samuel"]))
  # Assistants, surnames with particles, not duplicating a data collector
  expect_true(all(c("Amini, Sophia", "Boreham, Tara", "van der Merwe, Marlien") %in% collectors$name))
  expect_equal(nrow(collectors), 6)

  # The rights holder, with the creator's ORCID
  holder <- m$contributors[[which(people$type == "RightsHolder")]]
  expect_equal(holder$name, "Falster, Daniel")
  expect_equal(holder$orcid, "0000-0002-9814-092X")
})

test_that("zenodo_compare() lists who and what a new version would drop", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  cmp <- zenodo_compare(zenodo_test_previous(), new)

  # Sauquet stays a data collector nowhere, and is no longer a creator
  expect_equal(cmp$creators$name, "Sauquet, Hervé")
  # Baxter is gone; "Merwe, Marlien van der" matches "van der Merwe, Marlien"
  expect_equal(cmp$contributors$name, "Baxter, Caitlan")
  expect_equal(nrow(cmp$renamed), 0)
  expect_equal(cmp$related_identifiers$identifier, "10.1101/2021.01.04.425314")
  expect_equal(cmp$grants$grant, "DP720A")
})

test_that("zenodo_compare() flags a creator renamed under the same ORCID", {
  md <- zenodo_test_metadata()
  md$creators[[2]][c("last_name", "given_name")] <- list("Elizabeth", "Wenk")
  new <- zenodo_metadata(zenodo_test_database(md), description_file = NULL)
  cmp <- zenodo_compare(zenodo_test_previous(), new)
  expect_equal(cmp$renamed$name_new, "Elizabeth, Wenk")
  expect_equal(cmp$renamed$role, "creator")
  # The ORCID still matches, so she is not "dropped"
  expect_false("Wenk, Elizabeth" %in% cmp$creators$name)
})

test_that("zenodo_compare() matches a mistyped URL scheme", {
  previous <- zenodo_test_previous()
  previous$related_identifiers <- list(list(identifier = "https:://w3id.org/APD", relation = "isCompiledBy"))
  new <- list(related_identifiers = list(list(identifier = "https://w3id.org/APD", relation = "isCompiledBy")))
  expect_equal(nrow(zenodo_compare(previous, new)$related_identifiers), 0)
})

# ---- deposit, with Zenodo mocked -------------------------------------------

# A fake Zenodo: records the requests and answers them
zenodo_fake <- function(previous, draft_files = list(), submitted = FALSE) {
  env <- new.env()
  env$requests <- list()
  env$handler <- function(req) {
    method <- req$method %or% (if (is.null(req$body)) "GET" else "POST")
    url <- req$url
    body <- NULL
    if (!is.null(req$body) && identical(req$body$type, "json")) body <- req$body$data
    env$requests[[length(env$requests) + 1]] <- list(method = method, url = url, body = body)
    json <- function(x, status = 200L) {
      httr2::response(status_code = status, url = url,
                      headers = list(`Content-Type` = "application/json"),
                      body = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")))
    }
    draft <- list(id = 99L, submitted = submitted, state = "unsubmitted",
                  metadata = list(title = "old", keywords = list("plant", "trait"),
                                  doi = "10.5281/zenodo.99", creators = previous$creators),
                  links = list(html = "https://sandbox.zenodo.org/deposit/99",
                               bucket = "https://sandbox.zenodo.org/api/files/bucket-99"))
    if (grepl("/records/.*/versions/latest$", url)) {
      json(list(id = 15L, metadata = previous))
    } else if (grepl("/actions/newversion$", url)) {
      json(list(id = 15L, links = list(latest_draft = "https://sandbox.zenodo.org/api/deposit/depositions/99")))
    } else if (grepl("/depositions/99/files$", url)) {
      json(draft_files)
    } else if (grepl("/depositions/99/files/", url)) {
      httr2::response(status_code = 204L, url = url)
    } else if (grepl("/depositions/99$", url)) {
      if (method == "PUT") draft$metadata <- body$metadata
      json(draft)
    } else if (grepl("/bucket-99/", url)) {
      f <- req$body$data
      json(list(key = basename(url), checksum = paste0("md5:", unname(tools::md5sum(f)))))
    } else {
      json(list(message = paste("unexpected", url)), 404L)
    }
  }
  env
}

test_that("zenodo_deposit() stops before writing anything if people would be dropped", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  fake <- zenodo_fake(zenodo_test_previous())
  httr2::local_mocked_responses(fake$handler)
  expect_error(
    suppressMessages(zenodo_deposit(new, record = 3568417, token = "x")),
    "would drop or rename"
  )
  methods <- vapply(fake$requests, \(r) r$method, character(1))
  expect_true(all(methods == "GET"))
})

test_that("zenodo_deposit() requires a record and a token", {
  new <- list(title = "x")
  expect_error(zenodo_deposit(new, token = "x"), "Give `record`")
  withr::local_envvar(ZENODO_SANDBOX_TOKEN = "", ZENODO_TOKEN = "set")
  expect_error(zenodo_deposit(new, record = NA), "ZENODO_SANDBOX_TOKEN")
})

test_that("zenodo_deposit() creates a draft version, keeps unset fields, and syncs files", {
  dir <- withr::local_tempdir()
  same <- file.path(dir, "dictionary.html")
  writeLines("unchanged", same)
  new_file <- file.path(dir, "austraits-8.0.0.rds")
  writeLines("new data", new_file)
  draft_files <- list(
    list(id = "f1", filename = "dictionary.html", checksum = unname(tools::md5sum(same))),
    list(id = "f2", filename = "austraits-7.0.0.rds", checksum = "abc")
  )

  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  fake <- zenodo_fake(zenodo_test_previous(), draft_files)
  httr2::local_mocked_responses(fake$handler)
  out <- suppressMessages(
    zenodo_deposit(new, record = 3568417, files = c(new_file, same), token = "x",
                   allow_removals = TRUE)
  )

  expect_equal(out$id, 99L)
  expect_equal(out$files, "austraits-8.0.0.rds")
  expect_equal(out$comparison$creators$name, "Sauquet, Hervé")

  calls <- vapply(fake$requests, \(r) paste(r$method, sub("^.*/api/", "", r$url)), character(1))
  expect_true("POST deposit/depositions/15/actions/newversion" %in% calls)
  expect_true("DELETE deposit/depositions/99/files/f2" %in% calls)
  expect_false("DELETE deposit/depositions/99/files/f1" %in% calls)
  expect_true("PUT files/bucket-99/austraits-8.0.0.rds" %in% calls)
  expect_false(any(grepl("publish", calls)))

  put <- Filter(\(r) r$method == "PUT" && grepl("depositions/99$", r$url), fake$requests)[[1]]
  sent <- put$body$metadata
  expect_equal(sent$title, "A test database")
  expect_equal(sent$keywords, list("plant", "trait"))
  expect_null(sent$doi)
})

test_that("zenodo_deposit() leaves a published record alone", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  fake <- zenodo_fake(zenodo_test_previous(), submitted = TRUE)
  httr2::local_mocked_responses(fake$handler)
  expect_error(
    suppressMessages(zenodo_deposit(new, record = 3568417, token = "x", allow_removals = TRUE)),
    "not an unpublished draft"
  )
  methods <- vapply(fake$requests, \(r) r$method, character(1))
  expect_false("PUT" %in% methods)
})

test_that("zenodo_metadata() sorts project contributors who collected data among the data collectors", {
  md <- zenodo_test_metadata()
  md$project_contributors[[3]] <- list(last_name = "Lu-Irving", given_name = "Pat", role = "DataCollector")
  m <- zenodo_metadata(zenodo_test_database(md), description_file = NULL)
  collectors <- vapply(Filter(\(p) p$type == "DataCollector", m$contributors), \(p) p$name, character(1))
  expect_equal(collectors, sort(collectors))
  expect_true("Lu-Irving, Pat" %in% collectors)
  expect_equal(m$contributors[[3]]$type, "DataCollector")
})
