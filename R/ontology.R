library(dplyr)
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
      Subject = paste0("https://w3id.org/traits.build/",Subject)
    ) %>%
    tidyr::pivot_longer(cols = -Subject) %>% 
    dplyr::rename(
      Predicate = name,
      Object = value
    ) %>% 
    dplyr::filter(!is.na(Object)) %>%
    dplyr::mutate(
      Object = stringr::str_split(Object, "\\;")
    ) %>%
    tidyr::unnest_longer(Object) %>%
    dplyr::mutate(
      Object = stringr::str_trim(Object)
    )
  
  reformatted_ontology <- reformatted_ontology %>%
    dplyr::mutate(
      Object2 = ifelse(!stringr::str_detect(Object, "\\:") & 	!stringr::str_detect(Predicate, "description|label|comment"), 
                       paste0("<https://w3id.org/traits.build/",stringr::str_trim(Object),">"), 
                       NA),
      Object2 = ifelse(!stringr::str_detect(Object, "\\:") & 	stringr::str_detect(Predicate, "description|label|comment"), 
                         paste0(Object), 
                         Object2),
      Object2 = ifelse(stringr::str_detect(Predicate, "Datatype"), 
                       paste0("<", Object,">"), 
                       Object2),
      Object2 = ifelse(stringr::str_detect(Object, "\\:"),
                       paste0("<", published_classes_csv$Entity[match(Object, published_classes_csv$identifier2)],">"), 
                       Object2)
      )
  