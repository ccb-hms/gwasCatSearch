#' A function that takes the output of query against the EFO corpus and creates a datatable object
#' @import corpustools
#' @description
#' Users query the corpus to find EFO ontology terms that match the query. Those hits can be processed
#' by `hits2DT` to produce a datatable which then provides some amount of interactivity.
#' @param hits A `featureHits` object from the corpustools package.
#' @param efoDF A dataframe containing the information about the EFO terms.
#' @param tc A `tCorpus` object that was built using the same dataframe that is used for `efoDF`
#' @details Once hits have been found, this function obtains that actual text string in the EFO term that was matched
#' to the query.  Then using the `iri` column in `efoDF` a link to the EFO on-line resource for the term is created.
#' Information about how many phenotypes map to the term, both directly and via inheritance are reported.
#' @return
#' A `datatable` object with columns:
#' \describe{
#'  \item{EFO}{The EFO label for the term and link.}
#'  \item{Text}{The text of the EFO term.}
#'  \item{Direct}{The number of phenotypes that map directly to that term.}
#'  \item{Inherited}{The number of phenotypes that are inherited by mapping to children of the term.}
#'  \item{Others}{One column for each labeled search term in the call that created the `hits` object. If
#'  those terms were labeled then the columns will have the same label. The value in the column is the actual word
#'  in the EFO term that was matched on.}
#'  \item{field}{If there is a column in the `tcorpus$tokens` table named `field`, the values in it
#'  are aligned with the documents and returned. If there is no column named `field` then nothing
#'  is added.}
#' }
#' @author R. Gentleman
#' @seealso [corpustools::tCorpus()], [efo_df], [DT::datatable()]
#' @examples
#' hits <- search_features(efo_tc, query = c("Infect# infectious*", "Pancreas# pancrea*"))
#' summary(hits)
#' hitsasDT <- hits2DT(hits, efo_df, efo_tc)
#' @export
hits2DT <- function(hits, efoDF, tc) {
  if (nrow(hits$hits) == 0) {
    return(NULL)
  }
  ## fix up the tc so it only has the HITS...
  .i <- tc$get_token_id(doc_id = hits$hits$doc_id, token_id = hits$hits$token_id)
  .value <- as.character(hits$hits$code)
  tc$set("HITS", value = .value, subset = .i, subset_value = FALSE)
  keep <- rep(FALSE, nrow(tc$tokens))
  keep[.i] <- TRUE
  sub_tc <- tc$subset(
    subset = keep, subset_meta = unique(hits$hits$doc_id %in% tc$meta$doc_id),
    copy = TRUE
  )

  hit_index <- match(unique(hits$hits$doc_id), efoDF$Subject)
  matchedEFO <- efoDF[hit_index, ]
  EFO <- paste0("<a href= \"", matchedEFO$IRI, "\">", matchedEFO$Subject, "</a>")
  EFOtext <- matchedEFO$Object
  Direct <- matchedEFO$"Direct"
  Inherited <- matchedEFO$"Inherited"
  outDF <- data.frame(EFO = EFO, Text = EFOtext, Direct = Direct, Inherited = Inherited, row.names = matchedEFO$Subject)
  ## now look at how many things were queried and make a vector for each one
  ## that gives the token for the doc
  Queries <- split(sub_tc$tokens, sub_tc$tokens$HITS)
  for (i in 1:length(Queries)) {
    tmp <- rep(NA, nrow(matchedEFO))
    names(tmp) <- matchedEFO$Subject
    sp1 = split(Queries[[i]]$token, Queries[[i]]$doc_id)
    sp2 = sapply(sp1, function(x) { x = unique(tolower(x)); paste(x, collapse=", ")})
    tmp[names(sp2)] = sp2
    #docID <- as.character(Queries[[i]]$doc_id)
    #tmp[docID] <- as.character(Queries[[i]]$token)
    outDF[[names(Queries[i])]] <- tmp
  }
  ##see if there is info about where the map occurred and add it in
  if( "field" %in% colnames(sub_tc$tokens)) {
    sx1 = split(sub_tc$tokens$field, sub_tc$tokens$doc_id)
    sx2 = sapply(sx1, function(x) {x = unique(tolower(x)); paste(x, collapse=", ")})
    ##possibly paranoid, but make sure we align
    tmp <- rep(NA, nrow(matchedEFO))
    names(tmp) <- matchedEFO$Subject
    tmp[names(sx2)] = sx2
    outDF[["field"]] = tmp
  }
  return(outDF)
}

#' A function that adds the field values to a `featureHits` object.
#' @import corpustools
#' @description
#' Users query the corpus to find EFO ontology terms that match the query. This function adds the `field` information
#' from the `tCorpus` to the `featureHits` object.
#' @param hits A `featureHits` object from the corpustools package.
#' @param tc A `tCorpus` object that was built using the same dataframe that is used for `efoDF`
#' @details The hits are aligned with the appropriate rows in the `tc$tokens` data.frame and 
#' the `field` values extracted and entered added to the `hits` dataframe of the `hits` object.
#' @return
#' A `featureHits` object a column added to the `hits` component providing information on which 
#' field (subject, synonym or match) the hit was found in.
#' @author R. Gentleman
#' @seealso [corpustools::tCorpus()], [efo_df]
#' @examples
#' hits <- search_features(efo_tc, query = c("Infect# infectious*", "Pancreas# pancrea*"))
#' summary(hits)
#' hitswF <- addField2Hits(hits, efo_tc)
#' @export
addField2Hits = function(hits, tc) {
  if (nrow(hits$hits) == 0) {
    return(NULL)
  }
  if (!("field" %in% names(tc$tokens)))
    stop("no \'field\' column in tc$tokens")
  ## find the indices in the tc for each hit
  .i <- tc$get_token_id(doc_id = hits$hits$doc_id,
                    token_id = hits$hits$token_id)
  hits$hits = cbind(hits$hits, field = tc$tokens[.i, "field"])
  return(hits)
}

#' A function that takes the output of query against the GWAS Catalog corpus and creates a datatable object
#' @import corpustools
#' @description
#' Users query the `gwc_tc` corpus to find GWAS studies that match the query. Those hits can be processed
#' by `hits2DTGWC` to produce a datatable which then provides some amount of interactivity.
#' @param hits A `featureHits` object from the corpustools package.
#' @param gwc_df A dataframe containing the information about the GWAS catalog studies.
#' @param tc A `tCorpus` object that was built using the `gwc_df` dataframe.
#' @details Once hits have been found, this function obtains that actual text string and field where 
#' the match occurred.
#' @return
#' A `datatable` object with columns:
#' \describe{
#'  \item{Study}{The GWAS study ID.}
#'  \item{Text}{The set of CURIE IDs mapped to that trait.}
#'  \item{DiseaseTrait}{The text label for the GWAS trait for the Study.}
#'  \item{Others}{One column for each labeled search term in the call that created the `hits` object. If
#'  those terms were labeled then the columns will have the same label. The value in the column is the actual string
#'  that was matched on.}
#'  \item{field}{If there is a column in the `tcorpus$tokens` table named `field`, the values in it
#'  are aligned with the documents and returned. If there is no column named `field` then this column
#'  is missing.}
#' }
#' @author R. Gentleman
#' @seealso [corpustools::tCorpus()], [efo_df], [DT::datatable()]
#' @examples
#' htl = search_features(gwc_tc, query="leuk*")
#' htlf = addField2Hits(htl, gwc_tc)
#' 
#' summary(htlf)
#' gwhitsDT <- hits2DTGWC(htlf, gwc_df, gwc_tc)
#' @export
hits2DTGWC <- function(hits, gwc_df, tc) {
  if (nrow(hits$hits) == 0) {
    return(NULL)
  }
  ## fix up the tc so it only has the HITS...
  .i <- tc$get_token_id(doc_id = hits$hits$doc_id, token_id = hits$hits$token_id)
  .value <- as.character(hits$hits$code)
  tc$set("HITS", value = .value, subset = .i, subset_value = FALSE)
  keep <- rep(FALSE, nrow(tc$tokens))
  keep[.i] <- TRUE
  sub_tc <- tc$subset(
    subset = keep, subset_meta = unique(hits$hits$doc_id %in% tc$meta$doc_id),
    copy = TRUE
  )
  ##now things are different - our gwc_df can have multiple rows per study
  ##this is to unroll the EFO labels. So first just get the easy one - get
  ##only the first match
  hit_index <- match(unique(hits$hits$doc_id), gwc_df$STUDY.ACCESSION)
  matchedGWC <- gwc_df[hit_index, ]

  ##now there can be more than one EFO term per STUDY.ACCESSION so we need to
  ##find and collapse those
  id2 = gwc_df$STUDY.ACCESSION %in% unique(hits$hits$doc_id)
  allms = gwc_df[id2,]
  curies = split(allms$MAPPED_TRAIT_CURIE, allms$STUDY.ACCESSION)
  Curies = sapply(curies, function(x) paste(x, collapse=" *:* "))
  Study <- matchedGWC$STUDY.ACCESSION
  DiseaseTrait <- matchedGWC$DISEASE.TRAIT
  ##we keep the StudyID, the Curies and the GWC trait - 
  outDF <- data.frame(Study=Study, Curies=Curies, DiseaseTrait = DiseaseTrait)
  
  ## now look at how many things were queried and make a vector for each one
  ## that gives the token for the doc
  Queries <- split(sub_tc$tokens, sub_tc$tokens$HITS)
  for (i in 1:length(Queries)) {
    tmp <- rep(NA, nrow(matchedGWC))
    
    sp1 = split(Queries[[i]]$token, Queries[[i]]$doc_id)
    sp2 = sapply(sp1, function(x) { x = unique(tolower(x)); paste(x, collapse=", ")})
    dfx = data.frame(Study=names(sp2), Tokens=sp2)
    names(dfx)[2] = names(Queries[i])
    outDF = merge(outDF, dfx)
  }
  ##see if there is info about where the map occurred and add it in
  if( "field" %in% colnames(sub_tc$tokens)) {
    sx1 = split(sub_tc$tokens$field, sub_tc$tokens$doc_id)
    sx2 = sapply(sx1, function(x) {x = unique(tolower(x)); paste(x, collapse=", ")})
    fieldDF = data.frame(Study=names(sx2), field = sx2)
    outDF = merge(outDF, fieldDF)
  }
  return(outDF)
}

# vv = hits2DTGWC(htlf, gwc_df, tc=gwc_tc)
