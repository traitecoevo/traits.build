
add_row <- function(data, name, description) {
  if(!("html" %in% class(name)))
    name <- gt::html(name)
  if (!("html" %in% class(description))) {
    description <- gt::html(description)
  }

  bind_rows(
    data, 
    tibble(name = list(name), description = list(description))
  )
}

create_term_table <- function(thisterm, triples) {

  trait_i <- 
    triples %>% 
    filter(subject_tmp == thisterm) %>%
    mutate(property_link = NA, value_link = NA)
  
  
  for (i in seq_along(1:nrow(trait_i))) {
    trait_i$property_link[i] <- make_link(trait_i$Predicate_labels[i], trait_i$Predicate[i])
    trait_i$value_link[i] = ifelse(stringr::str_detect(trait_i$Object[i], "http"), make_link(trait_i$Object_labels[i], trait_i$Object[i]), trait_i$Object_labels[i])
  }
  
  output <- tibble(name =  list(), description = list())

  output <- 
    add_row(output, "URI", thisterm)
  
  # label
    label <- trait_i %>% filter(Predicate_labels == "preferred label")
      
    output <-
        add_row(output,
                label$property_link,
                label$value_link
                )
  
  # alternative label
    altlabel <- trait_i %>% filter(Predicate_labels == "alternative label")
    
    output <-
      add_row(output,
              altlabel$property_link,
              altlabel$value_link
              )
  
  # description
    description <- trait_i %>% filter(Predicate_labels == "description")
    
    output <-
      add_row(output, 
              description$property_link, 
              description$value_link
              )

  # comments
    comments_tmp <- trait_i %>% filter(Predicate_labels == "comment")
    
    output <-
      add_row(output, 
              comments_tmp$property_link, 
              comments_tmp$value_link
              )
  
  # computational entity
    computational_entity <- trait_i %>% filter(Predicate_labels == "computational entity")
    
    output <-
      add_row(output,
              computational_entity$property_link,
              computational_entity$value_link
      )

  # organizedInClass
    organizedInClass <- trait_i %>% filter(Predicate_labels == "organizedInClass")  
    
    output <-
      add_row(output,
              make_link("organizedInClass", "http://rs.tdwg.org/dwc/terms/attributes/organizedInClass"),
              print_list2(organizedInClass$value_link)
      )
      
  # exact match
    exact_match <- trait_i %>% filter(Predicate_labels == "has exact match")
    
    output <-
      add_row(output,
              make_link("has exact match", "http://www.w3.org/2004/02/skos/core#exactMatch"),
              print_list2(exact_match$value_link)
      )
  
  # close match
    close_match <- trait_i %>% filter(Predicate_labels == "has close match")
    
    output <-
      add_row(output,
              make_link("has close match", "http://www.w3.org/2004/02/skos/core#closeMatch"),
              print_list2(close_match$value_link)
      )
  
  # related match
    related_match <- trait_i %>% filter(Predicate_labels == "has related match")
    
    output <-
      add_row(output,
              make_link("has related match", "http://www.w3.org/2004/02/skos/core#relatedMatch"),
              print_list2(related_match$value_link)
      )
    
  # has broader
    broader <- trait_i %>% filter(Predicate_labels == "has broader")
    
    output <-
      add_row(output,
              make_link("has broader", "http://www.w3.org/2004/02/skos/core#broader"),
              print_list2(broader$value_link)
      )
    
  # has unique identifier
    has_unique_identifier <- trait_i %>% filter(Predicate_labels == "has unique identifier")
    
    output <-
      add_row(output,
              make_link("has unique identifier", "http://semanticscience.org/resource/SIO_000673"),
              print_list2(has_unique_identifier$value_link)
      )
    
  # is unique identifier for
    is_unique_identifier <- trait_i %>% filter(Predicate_labels == "is unique identifier for")
    
    output <-
      add_row(output,
              make_link("is unique identifier for", "http://semanticscience.org/resource/SIO_000674"),
              print_list2(is_unique_identifier$value_link)
      )    
    
  # has identifier
    has_identifier <- trait_i %>% filter(Predicate_labels == "has identifier")
    
    output <-
      add_row(output,
              make_link("has identifier", "http://semanticscience.org/resource/SIO_000671"),
              print_list2(has_identifier$value_link)
      )
    
  # is identifier for
    is_identifier <- trait_i %>% filter(Predicate_labels == "is identifier for")
    
    output <-
      add_row(output,
              make_link("is identifier for", "http://semanticscience.org/resource/SIO_000672"),
              print_list2(is_identifier$value_link)
      )
    
  # of entity
    of_entity <- trait_i %>% filter(Predicate_labels == "of entity")
    
    output <-
      add_row(output,
              make_link("of entity", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#ofEntity"),
              print_list2(of_entity$value_link)
      )
    
  # has value
    has_value <- trait_i %>% filter(Predicate_labels == "has value")
    
    output <-
      add_row(output,
              make_link("has value", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasValue"),
              has_value$value_link
      )
    
  # has measurement
    has_measurement <- trait_i %>% filter(Predicate_labels == "has measurement")
    
    output <-
      add_row(output,
              make_link("has measurement", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasMeasurement"),
              has_measurement$value_link
      )
    
  # measurement for
    measurement_for <- trait_i %>% filter(Predicate_labels == "measurement for")
    
    output <-
      add_row(output,
              make_link("measurement for", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#measurementFor"),
              measurement_for$value_link
      )
    
  # has context
    has_context <- trait_i %>% filter(Predicate_labels == "has context")
    
    output <-
      add_row(output,
              make_link("has context", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasContext"),
              print_list2(has_context$value_link)
      )
    
  # characteristic for
    characteristic_for <- trait_i %>% filter(Predicate_labels == "characteristic for")
    
    output <-
      add_row(output,
              make_link("characteristic for", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#characteristicFor"),
              print_list2(characteristic_for$value_link)
      )
    
  # is specialisation of
    is_specialisation_of <- trait_i %>% filter(Predicate_labels == "is specialisation of")
    
    output <-
      add_row(output,
              make_link("is specialisation of", "http://semanticscience.org/resource/SIO_001096"),
              print_list2(is_specialisation_of$value_link)
      )
    
  # has annotation
    has_annotation <- trait_i %>% filter(Predicate_labels == "has annotation")
    
    output <-
      add_row(output,
              make_link("has annotation", "http://semanticscience.org/resource/SIO_000255"),
              print_list2(has_annotation$value_link)
      )
    
  # is annotation of
    is_annotation_of <- trait_i %>% filter(Predicate_labels == "is annotation of")
    
    output <-
      add_row(output,
              make_link("is annotation of", "http://semanticscience.org/resource/SIO_000254"),
              print_list2(is_annotation_of$value_link)
      )
    
  # refers to
    refers_to <- trait_i %>% filter(Predicate_labels == "refers to")
    
    output <-
      add_row(output,
              make_link("refers to", "http://semanticscience.org/resource/SIO_000628"),
              print_list2(refers_to$value_link)
      )
    
  # Cites
    cites <- trait_i %>% filter(Predicate_labels == "Cites")
    
    output <-
      add_row(output,
              make_link("cites", "http://vocab.fairdatacollective.org/gdmt/Cites"),
              print_list2(cites$value_link)
      )
    
  # has basis
    has_basis <- trait_i %>% filter(Predicate_labels == "has basis")
    
    output <-
      add_row(output,
              make_link("has basis", "http://semanticscience.org/resource/SIO_000641"),
              print_list2(has_basis$value_link)
      )
    
  # is base for
    is_base_for <- trait_i %>% filter(Predicate_labels == "is base for")
    
    output <-
      add_row(output,
              make_link("is base for", "http://semanticscience.org/resource/SIO_000642"),
              print_list2(is_base_for$value_link)
      )
    
  # has property
    has_property <- trait_i %>% filter(Predicate_labels == "has property")
    
    output <-
      add_row(output,
              make_link("has property", "http://semanticscience.org/resource/SIO_000223"),
              print_list2(has_property$value_link)
      )
    
    
  # is property of
    is_property_for <- trait_i %>% filter(Predicate_labels == "is property for")
    
    output <-
      add_row(output,
              make_link("is property for", "http://semanticscience.org/resource/SIO_000224"),
              print_list2(is_property_for$value_link)
      )
    
  # created
    created_tmp <- trait_i %>% filter(Predicate_labels == "created")
    
    output <-
      add_row(output,
              created_tmp$property_link,
              created_tmp$value_link
      )
    
  # is in scheme
    scheme_tmp <- trait_i %>% filter(Predicate_labels == "is in scheme")
    
    output <-
      add_row(output,
              scheme_tmp$property_link,
              scheme_tmp$value_link
      )
    
  # datatype
    datatype_tmp <- trait_i %>% filter(Predicate_labels == "datatype")
    
    output <-
      add_row(output,
              datatype_tmp$property_link,
              datatype_tmp$value_link
      )
  
  output
}
