library(dplyr)
library(rdflib)

ontology_csv <- readr::read_csv("traits_build_ontology.csv")
published_classes_csv <- readr::read_csv("traits_build_ontology_published_classes.csv")

convert_to_triples <- function(ontology_csv, published_classes_csv) {

  published_classes_csv <- 
      published_classes_csv %>%
      dplyr::mutate(identifier2 = paste0(prefix,":",identifier))
  
  vocabularies <- 
      published_classes_csv %>%
      dplyr::select(dplyr::all_of(c("vocabulary", "prefix", "skos:inScheme"))) %>%
      dplyr::distinct(vocabulary, .keep_all = TRUE)
  
  reformat_classes <-
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
      Object = ifelse(stringr::str_detect(Object,"inScheme"),paste0("<", Object, ">"), Object)
      )

# map in:
  #http://www.w3.org/2000/01/rdf-schema#label
  #owl:sameas
  
  reformatted_ontology <- 
        ontology_csv %>%
      dplyr::select(-all_of(c("contextof"))) %>%
      dplyr::rename(all_of(c(
        "Subject" = "Class",
        "<http://www.w3.org/2004/02/skos/core#prefLabel>" = "prefLabel",
        "<http://www.w3.org/2004/02/skos/core#exactMatch>" = "exactMatch",
        "<http://www.w3.org/2000/01/rdf-schema#subClassOf>" = "subClassOf",
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
        "<http://purl.org/dc/terms/description>" = "dcterms:description",
        "<http://www.w3.org/2000/01/rdf-schema#comment>" = "rdfs:comment",
        "<http://www.w3.org/2000/01/rdf-schema#Datatype>" = "rdf:datatype"
      ))) %>%
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
                                  paste0("\"", Object_tmp, "\""), 
                                  NA),
      Object = ifelse(!stringr::str_detect(Object_tmp, "\\:") & !stringr::str_detect(Predicate, "description|label|comment")  & is.na(Object), 
                       paste0("<https://w3id.org/traits.build/",stringr::str_trim(Object_tmp),">"), 
                       Object),
      Object = ifelse(stringr::str_detect(Object_tmp, "\\:") & is.na(Object),
                       paste0("<", published_classes_csv$Entity[match(Object_tmp, published_classes_csv$identifier2)],">"), 
                       Object),
      Object = ifelse(stringr::str_detect(Predicate,"schema\\#Datatype"), 
                       paste0("<", Object_tmp, ">"), 
                       Object)
      ) %>%
    dplyr::select(-Object_tmp)
  
  triples <- 
    reformatted_ontology %>%
      bind_rows(reformat_classes) %>%   
      #mutate(Object = iconv(Object, from="UTF-8", to="ASCII", sub="Unicode")) %>%
      mutate(graph = ".")

  triples
}

triples %>%
  readr::write_delim("traits_build.nq", col_names=FALSE, escape="none", quote="none")

triples %>%
  select(-graph) %>%
  readr::write_delim("traits_build.nt", col_names = FALSE, escape = "none", quote = "none")

# prove this parses correctly
true_triples <- read_nquads("traits_build.nq")

# serialize to any format
rdflib::rdf_serialize(true_triples, "traits_build.ttl",
                      namespace = c(traits_build = "https://w3id.org/traits.build/",
                                    dc = "http://purl.org/dc/elements/1.1/",
                                    skos = "http://www.w3.org/2004/02/skos/core#",
                                    dwcattributes = "http://rs.tdwg.org/dwc/terms/attributes/",
                                    dwc = "http://rs.tdwg.org/dwc/terms/"
                                    dcterms = "http://purl.org/dc/terms/",
                                    ets = "http://terminologies.gfbio.org/terms/ETS/",
                                    obo = "http://purl.obolibrary.org/obo/",
                                    oboecore = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#",
                                    owl = "http://www.w3.org/2002/07/owl#",
                                    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
                                    datacite = "http://purl.org/datacite/v4.4/",
                                    SIO = "http://semanticscience.org/resource/",
                                    gdmt = "http://vocab.fairdatacollective.org/gdmt/"
)
rdflib::rdf_serialize(true_triples, "APD.json", format="jsonld")

  