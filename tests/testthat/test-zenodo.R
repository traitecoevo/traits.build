# Zenodo deposit (#320). Nothing here talks to Zenodo: the deposit tests mock
# every HTTP request and check what would have been sent.

`%or%` <- function(x, y) if (is.null(x)) y else x

# Suggested packages, needed only for depositing
skip_if_not_installed("commonmark")
skip_if_not_installed("httr2")
skip_if_not_installed("jsonlite")

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
  # ... and is listed as renamed, as a contributor, which does not stop a deposit
  expect_equal(cmp$renamed$name_new, "van der Merwe, Marlien")
  expect_equal(cmp$renamed$role, "contributor")
  expect_equal(cmp$related_identifiers$identifier, "10.1101/2021.01.04.425314")
  expect_equal(cmp$grants$grant, "DP720A")
})

test_that("zenodo_compare() flags a creator renamed under the same ORCID", {
  md <- zenodo_test_metadata()
  md$creators[[2]][c("last_name", "given_name")] <- list("Elizabeth", "Wenk")
  new <- zenodo_metadata(zenodo_test_database(md), description_file = NULL)
  cmp <- zenodo_compare(zenodo_test_previous(), new)
  creator <- cmp$renamed[cmp$renamed$role == "creator", ]
  expect_equal(creator$name_new, "Elizabeth, Wenk")
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
zenodo_fake <- function(previous, draft_files = list(), submitted = FALSE, concept = "3568417") {
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
    draft <- list(id = 99L, conceptrecid = concept, submitted = submitted, state = "unsubmitted",
                  metadata = list(title = "old", keywords = list("plant", "trait"),
                                  doi = "10.5281/zenodo.99", creators = previous$creators),
                  links = list(html = "https://sandbox.zenodo.org/deposit/99",
                               bucket = "https://sandbox.zenodo.org/api/files/bucket-99"))
    if (grepl("/records/.*/versions/latest$", url)) {
      json(list(id = 15L, conceptrecid = "3568417", metadata = previous))
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

# ---- review findings on PR #334 ----------------------------------------------

test_that("people with the same surname and initial are not confused", {
  people <- list(
    list(name = "Cooper, Wendy", type = "DataCollector"),
    list(name = "Cooper, William", type = "DataCollector"),
    list(name = "Ladd, Phillip", orcid = "0000-0002-7730-9685", type = "DataCollector"),
    list(name = "Ladd, P. G.", type = "DataCollector"),
    list(name = "Wilson, J. Bastow", type = "DataCollector"),
    list(name = "Wilson, J. M.", type = "DataCollector"),
    list(name = "Smith, John A.", orcid = "0000-0001-0000-0001", type = "DataCollector"),
    list(name = "Smith, John B.", type = "DataCollector"),
    list(name = "Brodribb, Tim", orcid = "0000-0002-4964-6107", type = "DataCollector"),
    list(name = "Brodribb, Timothy", orcid = "0000-0002-4964-6107", type = "DataCollector"),
    list(name = "Brodribb, Timothy", type = "DataCollector")
  )
  merged <- traits.build:::zenodo_merge_people(people)
  names <- vapply(merged, \(p) p$name, character(1))
  expect_true(all(c("Cooper, Wendy", "Cooper, William", "Wilson, J. Bastow", "Wilson, J. M.",
                    "Smith, John A.", "Smith, John B.") %in% names))
  # Merged under one ORCID, with the name written most often
  expect_equal(sum(grepl("^Ladd", names)), 1)
  expect_equal(merged[[which(grepl("^Ladd", names))]]$orcid, "0000-0002-7730-9685")
  expect_equal(names[grepl("^Brodribb", names)], "Brodribb, Timothy")

  # One Cooper dropped, the other kept: still reported
  previous <- list(contributors = people[1:2])
  new <- list(contributors = people[2])
  expect_equal(zenodo_compare(previous, new)$contributors$name, "Cooper, Wendy")

  # A different ORCID is a different person, whatever the name
  previous <- list(contributors = list(list(name = "Smith, Benjamin", orcid = "0000-0002-6987-5337")))
  new <- list(contributors = list(list(name = "Smith, Benjamin", orcid = "0000-0001-1111-1111")))
  expect_equal(nrow(zenodo_compare(previous, new)$contributors), 1)
})

test_that("a swapped name without an ORCID is reported as renamed", {
  previous <- list(contributors = list(list(name = "Setterfield, Samantha", type = "DataCollector")))
  new <- list(contributors = list(list(name = "Samantha, Setterfield", type = "DataCollector")))
  cmp <- zenodo_compare(previous, new)
  expect_equal(nrow(cmp$contributors), 0)
  expect_equal(cmp$renamed$name_new, "Samantha, Setterfield")
})

test_that("assistants: placeholders, initials without spaces, and namesakes of data collectors", {
  expect_equal(traits.build:::zenodo_split_names(c("none", "Waltraud Schulze, Inge Schulze, R.J.Williams", ".na.character")),
               c("Schulze, Waltraud", "Schulze, Inge", "Williams, R. J."))
  db <- zenodo_test_database()
  db$methods$assistants[4] <- "Paul Clarke, none"
  m <- zenodo_metadata(db, description_file = NULL)
  names <- vapply(m$contributors, \(p) p$name, character(1))
  expect_true("Clarke, Paul" %in% names)
  expect_false("none" %in% names)
})

test_that("placeholders are empty, and ORCIDs are checked", {
  db <- zenodo_test_database()
  db$contributors$affiliation[5] <- ".na.character"
  db$contributors$ORCID[5] <- "000-0002-6356-962X"
  db$contributors$ORCID[1] <- "0000-0002-9120-472x"
  expect_warning(m <- zenodo_metadata(db, description_file = NULL), "Andrew, Samuel \\(000-0002-6356-962X\\)")
  andrew <- Filter(\(p) p$name == "Andrew, Samuel", m$contributors)[[1]]
  expect_null(andrew$affiliation)
  expect_null(andrew$orcid)
  clarke <- Filter(\(p) grepl("^Clarke", p$name), m$contributors)[[1]]
  expect_equal(clarke$orcid, "0000-0002-9120-472X")
})

test_that("references may be a list, and a dropped one is reported", {
  md <- zenodo_test_metadata()
  md$references <- list("Falster et al (2021) *AusTraits*. Scientific Data", "Wenk et al (2023) APD")
  m <- zenodo_metadata(zenodo_test_database(md), description_file = NULL)
  expect_equal(m$references, list("Falster et al (2021) AusTraits. Scientific Data", "Wenk et al (2023) APD"))
  previous <- list(references = list("Falster et al (2021) AusTraits. Scientific Data", "Another (2020)"))
  expect_equal(zenodo_compare(previous, m)$references$reference, "Another (2020)")
})

test_that("zenodo_carry_over() keeps ORCIDs and affiliations the repository lacks", {
  previous <- list(contributors = list(
    list(name = "Kealley, Ian", type = "DataCollector", orcid = "0000-0003-2747-3927", affiliation = "WA"),
    list(name = "Cooper, Wendy", type = "DataCollector", orcid = "0000-0001-0000-0002")
  ))
  new <- list(contributors = list(
    list(name = "Kealley, Ian", type = "DataCollector"),
    list(name = "Cooper, William", type = "DataCollector")
  ))
  expect_message(out <- traits.build:::zenodo_carry_over(new, previous), "Kealley")
  expect_equal(out$contributors[[1]]$orcid, "0000-0003-2747-3927")
  expect_equal(out$contributors[[1]]$affiliation, "WA")
  expect_null(out$contributors[[2]]$orcid)
})

test_that("zenodo_deposit() refuses directories and a draft of another record", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  dir <- withr::local_tempdir()
  expect_error(zenodo_deposit(new, record = NA, files = dir, token = "x"), "directories")

  fake <- zenodo_fake(zenodo_test_previous(), concept = "424242")
  httr2::local_mocked_responses(fake$handler)
  expect_error(
    suppressMessages(zenodo_deposit(new, record = 3568417, draft = 99, token = "x", allow_removals = TRUE)),
    "not a version of record"
  )
  methods <- vapply(fake$requests, \(r) r$method, character(1))
  expect_false("PUT" %in% methods)
})

test_that("zenodo_deposit() with record = NA creates a new deposition without comparing", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  fake <- zenodo_fake(zenodo_test_previous())
  handler <- function(req) {
    if (grepl("/deposit/depositions$", req$url)) {
      fake$requests[[length(fake$requests) + 1]] <- list(method = "POST", url = req$url, body = NULL)
      return(httr2::response(status_code = 201L, url = req$url,
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw('{"id": 99, "submitted": false, "metadata": {}, "links": {"bucket": "b"}}')))
    }
    fake$handler(req)
  }
  httr2::local_mocked_responses(handler)
  out <- suppressMessages(zenodo_deposit(new, record = NA, token = "x"))
  expect_null(out$comparison)
  calls <- vapply(fake$requests, \(r) paste(r$method, sub("^.*/api/", "", r$url)), character(1))
  expect_equal(calls, c("POST deposit/depositions", "PUT deposit/depositions/99"))
})

test_that("the metadata Zenodo receives has arrays where Zenodo needs them", {
  m <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  m$creators <- m$creators[1]
  m$references <- m$references[1]
  json <- jsonlite::fromJSON(
    as.character(jsonlite::toJSON(list(metadata = m), auto_unbox = TRUE, null = "null")),
    simplifyVector = FALSE
  )$metadata
  for (f in c("creators", "contributors", "related_identifiers", "references", "locations", "grants")) {
    expect_type(json[[f]], "list")
    expect_null(names(json[[f]]))
  }
  expect_type(json$title, "character")
  expect_type(json$creators[[1]]$name, "character")
})

test_that("allow_removals can accept named losses and still stop on the rest", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  fake <- zenodo_fake(zenodo_test_previous())
  httr2::local_mocked_responses(fake$handler)
  # Sauquet, Baxter, a preprint and a grant are dropped; accepting only two
  expect_error(
    suppressMessages(zenodo_deposit(new, record = 3568417, token = "x",
                                    allow_removals = c("Sauquet, Hervé", "Baxter, Caitlan"))),
    "Not yet accepted: 10.1101/2021.01.04.425314"
  )
  out <- suppressMessages(zenodo_deposit(new, record = 3568417, token = "x",
    allow_removals = c("Sauquet, Hervé", "Baxter, Caitlan", "10.1101/2021.01.04.425314", "DP720A")))
  expect_equal(out$id, 99L)
})

test_that("zenodo_metadata() stops on what it cannot deposit", {
  db <- zenodo_test_database()
  expect_error(zenodo_metadata(db, metadata = "x", description_file = NULL), "metadata` block")
  md <- zenodo_test_metadata(); md$version <- NULL
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL), "no `version`")
  md <- zenodo_test_metadata(); md$creators <- NA
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL), "no `creators`")
  md <- zenodo_test_metadata(); md$creators[[1]]$last_name <- "unknown"
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL), "no `last_name`")
  md <- zenodo_test_metadata(); md$publication_date <- "sometime soon"
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL), "Cannot read")
  md <- zenodo_test_metadata(); md$description <- ".na"
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL), "no `description`")
  expect_error(zenodo_metadata(db, description_file = "nowhere.md"), "not found")
  md <- zenodo_test_metadata(); md$related_identifiers[[1]]$resource_type <- "toolbox"
  expect_error(zenodo_metadata(zenodo_test_database(md), description_file = NULL), "resource_type 'toolbox'")
})

test_that("zenodo_metadata() reads single entries and leaves out empty fields", {
  md <- zenodo_test_metadata()
  md$creators <- md$creators[[1]]
  md$related_identifiers <- list(identifier = "https://w3id.org/APD", relation_type = "isCompiledBy")
  md$funding_reference <- list(award_number = "DE170100208", funder_identifier = "doi.org/10.13039/501100000923/")
  md$language <- "fr"
  md$license <- list(rights = "unknown")
  md$geo_location <- list(geo_location_place = ".na")
  md$project_contributors <- NULL
  m <- zenodo_metadata(zenodo_test_database(md), description_file = NULL)
  expect_equal(m$creators[[1]]$name, "Falster, Daniel")
  expect_equal(m$related_identifiers, list(list(identifier = "https://w3id.org/APD", relation = "isCompiledBy")))
  expect_equal(m$grants, list(list(id = "10.13039/501100000923::DE170100208")))
  expect_equal(m$language, "fra")
  expect_false(any(c("license", "locations") %in% names(m)))

  # A single-word assistant is kept as written
  expect_equal(traits.build:::zenodo_split_names("Bob"), "Bob")
  expect_equal(traits.build:::zenodo_split_names(NA), character())
})

test_that("zenodo_deposit() checks its files before calling Zenodo", {
  new <- list(title = "x")
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a", "data.csv"); b <- file.path(dir, "b", "data.csv")
  dir.create(dirname(a)); dir.create(dirname(b)); writeLines("1", a); writeLines("2", b)
  expect_error(zenodo_deposit(new, record = NA, files = file.path(dir, "missing.csv"), token = "x"), "not found")
  expect_error(zenodo_deposit(new, record = NA, files = c(a, b), token = "x"), "share a name: data.csv")
})

test_that("zenodo_deposit() reports Zenodo's errors, a missing upload link and a bad checksum", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  dir <- withr::local_tempdir()
  f <- file.path(dir, "austraits-8.0.0.rds")
  writeLines("data", f)
  json <- function(url, x, status = 200L) {
    httr2::response(status_code = status, url = url, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE)))
  }
  draft <- list(id = 99L, submitted = FALSE, metadata = list(), links = list(bucket = "https://z/api/files/b"))

  # Zenodo's validation errors are shown
  httr2::local_mocked_responses(function(req) {
    if (grepl("depositions$", req$url)) return(json(req$url, draft, 201L))
    json(req$url, list(message = "Validation error.", errors = list(list(field = "metadata.creators", messages = list("Missing data")))), 400L)
  })
  expect_error(suppressMessages(zenodo_deposit(new, record = NA, token = "x")), "metadata.creators: Missing data")

  # No upload link
  httr2::local_mocked_responses(function(req) {
    if (grepl("/files$", req$url)) return(json(req$url, list()))
    json(req$url, list(id = 99L, submitted = FALSE, metadata = list(), links = list()))
  })
  expect_error(suppressMessages(zenodo_deposit(new, record = NA, files = f, token = "x")), "upload link")

  # The upload's checksum differs
  httr2::local_mocked_responses(function(req) {
    if (grepl("/files$", req$url)) return(json(req$url, list()))
    if (grepl("/files/b/", req$url)) return(json(req$url, list(checksum = "md5:0000")))
    json(req$url, draft)
  })
  expect_error(suppressMessages(zenodo_deposit(new, record = NA, files = f, token = "x")), "wrong checksum")
})

test_that("zenodo_deposit() says so when nothing is dropped", {
  new <- zenodo_metadata(zenodo_test_database(), description_file = NULL)
  previous <- list(version = "7.0.0", creators = new$creators, contributors = new$contributors)
  fake <- zenodo_fake(previous)
  httr2::local_mocked_responses(fake$handler)
  messages <- capture_messages(zenodo_deposit(new, record = 3568417, token = "x"))
  expect_true(any(grepl("nobody and nothing dropped", messages)))
})

test_that("data collectors are sorted by surname, then given names, the same in every locale", {
  people <- lapply(c("Smith-Martin, Chris", "smith, Anna", "Ziemińska, Kasia", "Smith, Benjamin",
                     "van der Merwe, Marlien", "Ábrego, Nerea"), \(n) list(name = n))
  sorted <- vapply(traits.build:::zenodo_sort_people(people), \(p) p$name, character(1))
  expect_equal(sorted, c("Ábrego, Nerea", "smith, Anna", "Smith, Benjamin", "Smith-Martin, Chris",
                         "van der Merwe, Marlien", "Ziemińska, Kasia"))
  withr::with_locale(c(LC_COLLATE = "C"), {
    expect_equal(vapply(traits.build:::zenodo_sort_people(people), \(p) p$name, character(1)), sorted)
  })
})
