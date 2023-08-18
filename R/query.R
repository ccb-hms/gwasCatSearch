## internal helper function used to build the two data resources - this
## function needs to be run and the data updated whenever a new database is obtained


## README: we will get two more tables in the EFO db, one with all synonyms for each term
## and one with the set of input text descriptions mapped to each EFO term (they are essentially
## like synonyms.). We will just past together each group (with a label for the group)
## might also want to try to take some more advantage of the corpus stuff...
## we probably want to refactor the synonym and matching code to use a helper function
## we probably need some testing here for completeness and accuracy
## we probably need to figure out if we can drop some of the columns etc - these might be
## getting pretty big
.makeCorpus <- function(path = getwd(), use_stemming = TRUE, remove_stop_words = TRUE, save = TRUE) {
  efo_df <- dbGetQuery(gwasCatSearch_dbconn(), "SELECT * from efo_labels")
  row.names(efo_df) = efo_df$Subject
  gwtrait_df <- dbGetQuery(gwasCatSearch_dbconn(),
                           "SELECT \"DISEASE.TRAIT\", MAPPED_TRAIT FROM gwascatalog_metadata")
  
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
  
  efo_tc <- corpustools::create_tcorpus(efo_df, doc_column = "Subject", 
                                        text_columns = c("Object", "Synonyms", "Matches"))
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
  if( !is.character(Ontonames) | length(Ontonames)<1 )
    stop("in correct input")
  query = paste0( "SELECT * FROM efo_synonyms WHERE Subject IN ('", paste(Ontonames, collapse="','"), "')")
  ans = dbGetQuery(gwasCatSearch_dbconn(), query)
  return(split(ans$Object, ans$Subject))
}

