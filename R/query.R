## define the globals
 globalVariables(c("efo", "efo_df", "efo_oi", "efo_tc", "mlogp"))

## internal helper function used to build the two data resources - this
## function needs to be run and the data updated whenever a new database is obtained

## we probably want to refactor the synonym and matching code to use a helper function
## we probably need some testing here for completeness and accuracy
## we probably need to figure out if we can drop some of the columns etc - these might be
## getting pretty big
.makeCorpus <- function(path = getwd(), use_stemming = TRUE, remove_stop_words = TRUE, save = TRUE) {
  efo_df <- dbGetQuery(gwasCatSearch_dbconn(), "SELECT * from efo_labels")
  row.names(efo_df) = efo_df$Subject
 # gwtrait_df <- dbGetQuery(gwasCatSearch_dbconn(),
 #                         "SELECT \"DISEASE.TRAIT\", MAPPED_TRAIT FROM gwascatalog_metadata")
  
  ## Synonyms
  efo_syn <- dbGetQuery(gwasCatSearch_dbconn(), "SELECT * from efo_synonyms")
  ##unroll this and then paste together all synonyms - use punctuation as a separator since
  ##is should get ignored by corpus tools
  sp1 = split(efo_syn[,2], efo_syn[,1], drop=TRUE)
  ##only keep terms that correspond to something we mapped
  hasMatch = names(sp1) %in% efo_df[,1]
  sp2 = sp1[hasMatch]
  ##collapse terms into a single value
  sp3 = sapply(sp2, function(x) paste(x, collapse=" *:* "))
  mm = match(names(sp3), row.names(efo_df))
  efo_df$Synonyms = ""
  efo_df$Synonyms[mm]=sp3
  ##Build up a set of matched traits
  ##split them by the CURIE they map to
  xx =dbGetQuery(gwasCatSearch_dbconn(), "SELECT * from gwascatalog_mappings")
  sp1 = split(xx$DISEASE.TRAIT , xx$MAPPED_TRAIT_CURIE)
  hasMatch = names(sp1) %in% efo_df$Subject
  sp2=sp1[hasMatch]
  sp3 = sapply(sp2, function(x) paste(x, collapse=" *:* ") )
  mm = match(names(sp3), row.names(efo_df))
  efo_df$Matches = ""
  efo_df$Matches[mm] = sp3
  
  ##at some point try setting remember_spaces=TRUE - this should retain the column
  ##where the match occurs - and I am guessing we can filter on that
  efo_tc <- corpustools::create_tcorpus(efo_df, doc_column = "Subject", 
                                        text_columns = c("Object", "Synonyms", "Matches"),
                                        remember_spaces=TRUE, split_sentences=TRUE)
  efo_tc$preprocess(use_stemming = use_stemming, remove_stopwords = remove_stop_words)

  efo_df = efo_df[,c("Subject","Object","IRI","DiseaseLocation","Direct","Inherited")]
  if (save) {
    save(efo_tc, file = paste0(path, "/efo_tc.rda"), compress = "xz")
    save(efo_df, file = paste0(path, "/efo_df.rda"), compress = "xz")
  }
  return(efo_tc)
}

##stash a few commands to get data in R
## gwascatmeta = RSQLite::dbGetQuery(gwasCatSearch:::gwasCatSearch_dbconn(), 
##                 "SELECT * from gwascatalog_metadata")
## gwascatsyn = RSQLite::dbGetQuery(gwasCatSearch:::gwasCatSearch_dbconn(), "SELECT * from efo_synonyms")

## dbcon=gwasCatSearch:::gwasCatSearch_dbconn()
##gwascatsynQ = function(query) RSQLite::dbGetQuery(gwasCatSearch:::gwasCatSearch_dbconn(), query)


#' A function to query the efo_synonyms table and return the set of known synonyms for the input ontology terms
#' @description
#' This function provides an interface to the SQL database containing ontology term synonyms. These are 
#' synonyms provided as part of the ontology.
#' @param Ontonames a character vector of the ontology CURIE symbols
#' @details The function selects the appropriate rows and then splits them according to the input ontology terms.
#' Any ontology term without synonyms will be dropped.
#' @return A named list, each element of which is named with the CURIE symbol, and the elements are the text
#' of the synonyms
#' @author Robert Gentleman
#' @examples 
#' getSynonyms(c("EFO:0000094", "EFO:0000095"))
#' @export
getSynonyms = function(Ontonames) {
  if( !is.character(Ontonames) || (length(Ontonames)<1) )
    stop("incorrect input")
  query = paste0( "SELECT * FROM efo_synonyms WHERE Subject IN ('", paste(Ontonames, collapse="','"), "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), query)
  return(split(ans$Object, ans$Subject))
}

#' A function to return the set of traits that were mapped to a given set of EFO terms
#' @description
#' This function provides an interface to the SQL table of matched traits to terms
#' @param Ontonames a character vector of the ontology CURIE symbols
#' @details The function extracts the matches, groups them by EFO CURIE label and returns a named list.
#' @return A named list, each element of which is named with the CURIE symbol, and the elements are the text
#' descriptions of the GWAS Catalog traits that mapped to that symbol.
#' @author Robert Gentleman
#' @examples 
#' getMatchedTraits(c("EFO:0000094", "EFO:0000095"))
#' @export
getMatchedTraits = function(Ontonames) {
  if( !is.character(Ontonames) || (length(Ontonames)<1) )
    stop("incorrect input")
  query = dbGetQuery(gwasCatSearch_dbconn(), "SELECT * from gwascatalog_mappings")
  sp1 = split(query$DISEASE.TRAIT , query$MAPPED_TRAIT_CURIE)
  ans = sp1[Ontonames]
  return(ans) 
}

#' A function to return the version information from the Ontology/EBI databases
#' @description
#' This function returns data.frame with one row for each resource that gives version information.
#' @details The version number for the resource, if it has one, is given. Otherwise the download
#' is given.
#' @return A data.frame with one row per resource.
#' @author Robert Gentleman
#' @examples
#' # example code
#' getVersionInfo()
#' @export
getVersionInfo = function() {
  query="SELECT * from version_info"
  dbGetQuery(gwasCatSearch_dbconn(),query)
}

#' A function to return the disease locations from the efo_labels table.
#' @description
#' This function takes an input vector of ontology CURIE symbols and returns the inferred disease locations.
#' @param Ontonames a character vector of the ontology CURIE symbols
#' @details The function selects the appropriate values from the efo_labels table.
#' @return A named vector of disease locations.
#' @author Robert Gentleman
#' @examples
#' getDiseaseLocation(c("EFO:0000094", "EFO:0000095"))
#' @export
getDiseaseLocation = function(Ontonames) {
  if (!is.character(Ontonames) | length(Ontonames) < 1)
    stop("incorrect input")
  query = paste0("SELECT DiseaseLocation FROM efo_labels WHERE Subject IN ('",
                 paste(Ontonames, collapse = "','"),
                 "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), query)
  return(ans)
}
  
#' 
#' A function to query the efo_edges table and return the set of parent terms for the input ontology terms
#' @description
#' This function provides an interface to the SQL database containing ontology edges. These are 
#' @param Ontonames a character vector of the ontology CURIE symbols
#' @details The function selects the appropriate rows from the efo_edges table.
#' @return A named list, names are from the input Ontonames and the values are the set of parent terms.
#' @author Robert Gentleman
#' @examples 
#' getParents(c("EFO:0000094", "EFO:0000095"))
#' @export
getParents = function(Ontonames) {
  if (!is.character(Ontonames) | length(Ontonames) < 1)
    stop("incorrect input")
  query = paste0("SELECT * FROM efo_edges WHERE Subject IN ('",
                 paste(Ontonames, collapse = "','"),
                 "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), query)
  return(split(ans$Object, ans$Subject))
}

#' 
#' A function to query the uberon_labels table and return the text descriptions for the input Uberon terms
#' @description
#' This function provides an interface to the SQL database containing Uberon labels. 
#' @param UberonIDs a character vector of the ontology CURIE symbols
#' @details The function selects the appropriate rows from the uberon_labels table.
#' @return A named vector, names are from the input UberonIDs and the values are the corresponding text descriptions.
#' @author Robert Gentleman
#' @examples 
#' getUberonText(c("UBERON:0001004", "UBERON:0000474"))
#' @export
getUberonText = function(UberonIDs) {
  if (!is.character(UberonIDs) | length(UberonIDs) < 1)
    stop("incorrect input")
  query = paste0("SELECT * FROM uberon_labels WHERE Subject IN ('",
                 paste(UberonIDs, collapse = "','"),
                 "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), query)
  rval=ans$Object
  names(rval) = ans$Subject
  notmapped = setdiff(UberonIDs, names(rval))
  rval[notmapped] = NA
  return(rval)
}

##    property     class
## 1        id character
## 2      name character
## 3   parents      list
## 4  children      list
## 5 ancestors      list
## 6  obsolete   logical

##The other way around—in our DB each row represents a child-parent relationship, 
##where Subject is the child and Object is the parent. Otherwise you are correct, 
##from efo_edges we get explicit relationships, and from efo_entailed_edges we get 
##all (explicit+implicit) edges, which allows us to get both ancestors and descendants 
##of any term.

#' 
#' A function to query the efo_edges table and return the parent terms for the input EFO IDs
#' @description
#' This function provides an interface to the SQL database containing EFO edges (parent child relationships). 
#' @param EFOID a character vector of the EFO CURIE symbols
#' @details The function returns the set of parents for the input terms.
#' @return A named vector, names are from the input EFOIDs and the values are the 
#' corresponding CURIEs for the parent terms. There can be zero or more matches for each input ID.
#' @author Robert Gentleman
#' @examples 
#' EFOparents(c("EFO:0000768", "MONDO:0002429"))
#' @export
EFOparents = function( EFOID) {
  if (!is.character(EFOID) | length(EFOID) < 1)
    stop("incorrect input")
  
  q1 = paste0("SELECT * from efo_edges where Subject IN ('",
  paste(EFOID, collapse = "','"),
  "')")
 ans = dbGetQuery(gwasCatSearch_dbconn(), q1)
 av = ans$Object
 names(av) = ans$Subject
 return(av)
}

#'
#'A function to access the ancestors of the input EFO IDs
#' @description
#' This function provides an interface to the SQL database containing EFO entailed edges.
#' @param EFOID a character vector of the EFO CURIE symbols
#' @details The function returns the set of ancestors for the input terms. The ancestors are all nodes that are parents, or parents of parents and so on, up to the root node of the ontology. These edges are entailed as there are some relationships that can be
#' inferred that would not be detected by simply recursively finding parents.
#' @return A named vector, names are from the input EFOIDs and the values are the corresponding CURIEs for the ancestor terms. There can be zero or more matches for each input ID.
#' @author Robert Gentleman
#' #' @param EFOID a character vector of the EFO CURIE symbols
#' @examples 
#' tancestors = EFOancestors(c("EFO:0000768", "MONDO:0002429"))
#' table(names(tancestors))
#' @export
EFOancestors = function(EFOID) {
  if (!is.character(EFOID) | length(EFOID) < 1)
    stop("incorrect input")
  
  q1 = paste0("SELECT * from efo_entailed_edges where Subject IN ('",
              paste(EFOID, collapse = "','"),
              "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), q1)
  av = ans$Object
  names(av) = ans$Subject
  return(av)
}

#' 
#' A function to query the efo_edges table and return the child terms for the input EFO IDs
#' @description
#' This function provides an interface to the SQL database containing EFO edges (parent child relationships). 
#' @param EFOID a character vector of the EFO CURIE symbols
#' @details The function returns the set of children (more specific) for the input IDs.
#' @return A named vector, names are from the input EFOIDs and the values are the corresponding CURIEs for the child terms.
#' @author Robert Gentleman
#' @examples 
#' EFOchildren("EFO:0009448")  ##should contain EFO:0000768
#' EFOchildren(c("EFO:0000768", "MONDO:0002429"))
#' @export
EFOchildren = function( EFOID) {
  if (!is.character(EFOID) | length(EFOID) < 1)
    stop("incorrect input")
  
  q1 = paste0("SELECT * from efo_edges where Object IN ('",
              paste(EFOID, collapse = "','"),
              "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), q1)
  av = ans$Subject
  names(av) = ans$Object
  return(av)
}

#' 
#' A function to query the efo_edges table and return the descendant terms for the input EFO IDs
#' @description
#' This function provides an interface to the SQL database containing EFO entailed edges. 
#' @param EFOID a character vector of the EFO CURIE symbols
#' @details The function returns the set of descendents for the input terms.
#' @return A named vector, names are from the input EFOIDs and the values are the corresponding CURIEs for the 
#' descendant terms.  These are basically the children, the children of the children and so on.
#' We use entailed edges so there may be terms in the descendents that have been identified
#' by logic based on the ontology and may not be detected by the recursive application of EFOchildren.
#' @author Robert Gentleman
#' @examples 
#' ##leukemia
#' dec1 = EFOdescendants("EFO:0000565")  
#' table(names(dec1))
#' dec2 = EFOdescendants(c("EFO:0000768", "MONDO:0002429"))
#' @export
EFOdescendants = function( EFOID) {
  if (!is.character(EFOID) | length(EFOID) < 1)
    stop("incorrect input")
  
  q1 = paste0("SELECT * from efo_entailed_edges where Object IN ('",
              paste(EFOID, collapse = "','"),
              "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), q1)
  av = ans$Subject
  names(av) = ans$Object
  return(av)
}

#' A function to query the efo_labels table and return the term labels for the input EFO IDs
#' @description
#' This function provides an interface to the SQL database containing EFO entailed edges. 
#' @param EFOID a character vector of the EFO CURIE symbols
#' @details The function returns the set of descendents for the input terms.
#' @examples
#' EFOlabels(c("EFO:0000768", "MONDO:0002429"))
#' @export
EFOlabels = function (EFOID) 
{
    if (!is.character(EFOID) | length(EFOID) < 1) 
        stop("incorrect input")
    q1 = paste0("SELECT * from efo_labels where Subject IN ('", 
        paste(EFOID, collapse = "','"), "')")
    ans = dbGetQuery(gwasCatSearch_dbconn(), q1)
    av = ans$Object
    names(av) = ans$Subject
    return(av)
}

