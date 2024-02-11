library(dplyr)
library(rdflib)

output_path <- "ontology/output/ontology"
data_path <- "ontology/data"

ontology_csv <- readr::read_csv(file.path(data_path, "traits.build_ontology.csv"))
resource_triples <- readr::read_csv(file.path(data_path, "traits.build_resource.csv"))
published_classes_csv <- readr::read_csv(file.path(data_path, "traits.build_ontology_published_classes.csv"))

convert_to_triples <- function(ontology_csv, published_classes_csv) {

  published_classes_csv <- 
      published_classes_csv %>%
      dplyr::mutate(identifier2 = paste0(prefix,":",identifier))
  
  vocabularies <- 
      published_classes_csv %>%
      dplyr::select(dplyr::all_of(c("vocabulary", "prefix", "skos:inScheme"))) %>%
      dplyr::distinct(vocabulary, .keep_all = TRUE)
  
  reformatted_classes <-
    published_classes_csv %>%
      dplyr::select(dplyr::all_of(c(
        "Entity", "skos:prefLabel", "dcterms:description", "rdfs:comment", "skos:inScheme"
      ))) %>%
      dplyr::rename(dplyr::all_of(c(
        "Subject" = "Entity",
        "<http://www.w3.org/2004/02/skos/core#prefLabel>" = "skos:prefLabel", 
        "<http://purl.org/dc/terms/description>" = "dcterms:description",
        "<http://www.w3.org/2000/01/rdf-schema#comment>" = "rdfs:comment",
        "<http://www.w3.org/2004/02/skos/core#inScheme>" = "skos:inScheme"
      )))  %>%
    tidyr::pivot_longer(cols = -Subject) %>% 
    dplyr::rename(
      Predicate = name,
      Object = value
    ) %>% 
    dplyr::filter(!is.na(Object)) %>%
    dplyr::mutate(
      Subject = paste0("<",Subject,">"),
      Object = ifelse(stringr::str_detect(Predicate,"inScheme"),paste0("<", Object, ">"), paste0("\"", Object, "\"@en"))
      )

  reformatted_ontology <- 
        ontology_csv %>%
      dplyr::select(-all_of(c("contextof"))) %>%
      dplyr::rename(all_of(c(
        "Subject" = "Class",
        "<http://www.w3.org/2004/02/skos/core#prefLabel>" = "prefLabel",
        "<http://www.w3.org/2004/02/skos/core#altLabel>" = "altLabel",
        "<http://www.w3.org/2004/02/skos/core#exactMatch>" = "exactMatch",
        "<http://www.w3.org/2004/02/skos/core#broader>" = "broader",
        "<http://semanticscience.org/resource/SIO_000602>" = "computational_entity",
        "<http://www.w3.org/2004/02/skos/core#closeMatch>" = "closeMatch",
        "<http://www.w3.org/2004/02/skos/core#relatedMatch>" = "relatedMatch",
        "<http://rs.tdwg.org/dwc/terms/attributes/organizedInClass>" = "organizedInClass",
        "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasContext>" = "hascontext",
        "<http://www.w3.org/2000/01/rdf-schema#label>" = "label",
        "<http://vocab.fairdatacollective.org/gdmt/Cites>" = "Cites",
        "<http://semanticscience.org/resource/SIO_000628>" = "refers to",
        "<http://semanticscience.org/resource/SIO_000673>" = "has unique identifier",
        "<http://semanticscience.org/resource/SIO_000674>" = "is unique identifier for",
        "<http://semanticscience.org/resource/SIO_000671>" = "has identifier",
        "<http://semanticscience.org/resource/SIO_000672>" = "is identifier for",
        "<http://semanticscience.org/resource/SIO_000255>" = "has annotation",
        "<http://semanticscience.org/resource/SIO_000254>" = "is annotation of",
        "<http://semanticscience.org/resource/SIO_000223>" = "has property",
        "<http://semanticscience.org/resource/SIO_000224>" = "is property of",
        "<http://semanticscience.org/resource/SIO_001096>" = "is specialisation of",
        "<http://semanticscience.org/resource/SIO_000642>" = "is base for",
        "<http://semanticscience.org/resource/SIO_000641>" = "has basis",
        "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#characteristicFor>" = "oboe-core:characteristicFor",
        "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#ofEntity>" = "oboe-core:ofEntity",
        "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasValue>" = "oboe-core:hasValue",
        "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#measurementFor>" = "oboe-core:measurementFor",
        "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasMeasurement>" = "oboe-core:hasMeasurement",
        "<http://purl.org/dc/terms/description>" = "dcterms:description",
        "<http://www.w3.org/2000/01/rdf-schema#comment>" = "rdfs:comment",
        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype>" = "rdf:datatype",
        "<http://purl.org/dc/terms/created>" = "dcterms:created"
      ))) %>%
    dplyr::mutate(
      `<http://www.w3.org/2000/01/rdf-schema#label>` = `<http://www.w3.org/2004/02/skos/core#prefLabel>`,
      `<http://www.w3.org/2002/07/owl#sameAs>` = `<http://www.w3.org/2004/02/skos/core#exactMatch>`,
      `<http://www.w3.org/2004/02/skos/core#inScheme>` = "traits.build"
    ) %>%
    dplyr::mutate(
      Subject = paste0("<https://w3id.org/traits.build/",Subject,">")
    ) %>%
    tidyr::pivot_longer(cols = -Subject) %>% 
    dplyr::rename(
      Predicate = name,
      Object_tmp = value
    ) %>% 
    dplyr::filter(!is.na(Object_tmp)) %>%
    dplyr::mutate(
      Object_tmp = stringr::str_split(Object_tmp, "\\;\\ ")
    ) %>%
    tidyr::unnest_longer(Object_tmp) %>%
    dplyr::mutate(
      Object_tmp = stringr::str_trim(Object_tmp)
    ) %>%
    dplyr::mutate(
      Object = ifelse(stringr::str_detect(Predicate, "description|[Ll]abel|[Cc]omment"), 
                                  paste0("\"", Object_tmp, "\"@en"), 
                                  NA),
      Object = ifelse(!stringr::str_detect(Object_tmp, "\\:") & !stringr::str_detect(Predicate, "description|label|comment")  & is.na(Object), 
                       paste0("<https://w3id.org/traits.build/",stringr::str_trim(Object_tmp),">"), 
                       Object),
      Object = ifelse(stringr::str_detect(Object_tmp, "\\:") & is.na(Object),
                       paste0("<", published_classes_csv$Entity[match(Object_tmp, published_classes_csv$identifier2)],">"), 
                       Object),
      Object = ifelse(stringr::str_detect(Predicate,"\\#datatype"), 
                       paste0("<", Object_tmp, ">"), 
                       Object),
      Object = ifelse(stringr::str_detect(Predicate,"inScheme"), 
                      "<https://w3id.org/traits.build/>", 
                      Object),
      Object = ifelse(stringr::str_detect(Predicate,"created"), 
                      paste0("\"", Object_tmp, "\"", "^^<xsd:date>"),
                      Object),
      Subject = ifelse(Subject == "<https://w3id.org/traits.build/traits.build>", "<https://w3id.org/traits.build>", Subject)
      ) %>%
    dplyr::select(-Object_tmp)
  
  triples <- 
    reformatted_ontology %>%
      bind_rows(reformatted_classes) %>%
      bind_rows(resource_triples) %>%
      #mutate(Object = iconv(Object, from="UTF-8", to="ASCII", sub="Unicode")) %>%
      mutate(graph = ".")

  triples
}

triples <- convert_to_triples(ontology_csv, published_classes_csv)

triples %>%
  readr::write_delim(file.path(output_path, "traits.build.nq"), col_names=FALSE, escape="none", quote="none")

triples %>%
  select(-graph) %>%
  readr::write_delim(file.path(output_path, "traits.build.nt"), col_names = FALSE, escape = "none", quote = "none")

triples <- triples %>%
  dplyr::mutate(
    predicate_tmp = stringr::str_replace(Predicate, "<", ""),
    predicate_tmp = stringr::str_replace(predicate_tmp, ">", ""),
    object_tmp = stringr::str_replace_all(Object, "<", ""),
    object_tmp = stringr::str_replace_all(object_tmp, ">", ""),
    object_tmp = stringr::str_replace(object_tmp, "\\^\\^", ""),
    object_tmp = stringr::str_replace(object_tmp, "xsd:date", ""),
    object_tmp = stringr::str_replace(object_tmp, "xsd:anyURI", ""),
    object_tmp = stringr::str_replace(object_tmp, "xsd:Name", ""),
    object_tmp = stringr::str_replace(object_tmp, "xsd:int", ""),
    object_tmp = stringr::str_replace(object_tmp, "xsd:decimal", ""),
    object_tmp = stringr::str_replace(object_tmp, "@en", ""),
    object_tmp = stringr::str_replace(object_tmp, "[:punct:]$",""),
    object_tmp = stringr::str_replace(object_tmp, "^[:punct:]",""),
    subject_tmp = stringr::str_replace(Subject, "<https://w3id.org/traits.build/", ""),
    subject_tmp = stringr::str_replace(subject_tmp, "<", ""),
    subject_tmp = stringr::str_replace(subject_tmp, ">", "")
  )

labels <- triples %>%
  filter(predicate_tmp %in% c("http://www.w3.org/2004/02/skos/core#prefLabel", "http://www.w3.org/2004/02/skos/core#altLabel")) %>%
  mutate(
    subject_tmp = stringr::str_replace(Subject, "<", ""),
    subject_tmp = stringr::str_replace(subject_tmp, ">", "")
  )

triples <- triples %>%
  mutate(
    Predicate_labels = published_classes_csv$`skos:prefLabel`[match(predicate_tmp, published_classes_csv$Entity)],
    Object_labels = ifelse(stringr::str_detect(object_tmp, "^http"),
                           labels$object_tmp[match(object_tmp, labels$subject_tmp)],
                           object_tmp),
    Object_labels = ifelse(is.na(Object_labels), object_tmp, Object_labels),
    Subject_labels = ontology_csv$prefLabel[match(subject_tmp, ontology_csv$Class)]
  )
  
triples %>%
  readr::write_csv(file.path(output_path, "build_triples.csv"))


# prove this parses correctly
true_triples <- read_nquads(file.path(output_path, "traits.build.nq"))

# serialize to any format
rdflib::rdf_serialize(true_triples, file.path(output_path, "traits.build.ttl"),
                      namespace = c(traits.build = "https://w3id.org/traits.build/",
                                    dc = "http://purl.org/dc/elements/1.1/",
                                    skos = "http://www.w3.org/2004/02/skos/core#",
                                    dwcattributes = "http://rs.tdwg.org/dwc/terms/attributes/",
                                    dwc = "http://rs.tdwg.org/dwc/terms/",
                                    dcterms = "http://purl.org/dc/terms/",
                                    ets = "http://terminologies.gfbio.org/terms/ETS/",
                                    obo = "http://purl.obolibrary.org/obo/",
                                    oboe = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#",
                                    owl = "http://www.w3.org/2002/07/owl#",
                                    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
                                    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                    datacite = "http://purl.org/datacite/v4.4/",
                                    SIO = "http://semanticscience.org/resource/",
                                    gdmt = "http://vocab.fairdatacollective.org/gdmt/")
)
rdflib::rdf_serialize(true_triples, file.path(output_path, "traits.build.json"), format="jsonld")

# Put a copy of ontology in place found by pkgdown
quarto::quarto_render("ontology/index.qmd", output_format = "html")
version <- rmarkdown::yaml_front_matter("ontology/index.qmd")$params$version

file.copy("ontology/index.html", "ontology/output/ontology/index.html", overwrite = TRUE)
unlink("ontology/index.html")

# Copy into folder for specific release
files <- c("traits.build.json", "traits.build.nq", "traits.build.nt", "traits.build.ttl", "index.html")
to_path <- file.path("ontology/output/ontology/release", version)
dir.create(to_path, FALSE, TRUE)
purrr::walk(files, ~ file.copy(file.path("ontology/output/ontology", .x), file.path(to_path, .x), overwrite = TRUE))

# **IMPORTANT FINAL STEP**
# After generating the files within the ontology folder, 
# a copy of the files needs to be copied across to build the website using:
# pkgdown::build_site()