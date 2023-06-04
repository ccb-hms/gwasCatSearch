## internal helper function used to build the two data resources - this
## function needs to be run and the data updated whenever a new database is obtained
.makeCorpus <- function(path = getwd(), use_stemming = TRUE, remove_stop_words = TRUE, save = TRUE) {
  efo_df <- dbGetQuery(gwasCatSearch_dbconn(), "SELECT * from efo_labels")
  efo_tc <- corpustools::create_tcorpus(efo_df, doc_column = "Subject", text_columns = "Object")
  efo_tc$preprocess(use_stemming = use_stemming, remove_stopwords = remove_stop_words)

  if (save) {
    save(efo_tc, file = paste0(path, "/efo_tc.rda"), compress = "xz")
    save(efo_df, file = paste0(path, "/efo_df.rda"), compress = "xz")
  }
  return(efo_tc)
}
