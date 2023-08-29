
#' query2table
#' @export
query2table = function(query="(kidney AND disease)") {
  data("efo_tc", package = "gwasCatSearch")
  data("efo_df", package = "gwasCatSearch")
    sout <- corpustools::search_features(tc = efo_tc, query = query)
    if (nrow(sout$hits) < 1) stop("no hits, please try a different query")
    gwasCatSearch::hits2DT(sout, efo_df, efo_tc)
}


#' processtable
#' @export
process_annotated = function (query = "(kidney AND disease)", 
     include_subclasses=TRUE, direct_subclasses_only=FALSE, sels = NULL) 
{
    tab <- query2table(query)
    rn = rownames(tab)
    if (is.null(sels)) 
        sels = seq_len(nrow(tab))
    rn = rn[sels]
    u <- unique(rn)
    last <- gwasCatSearch::resources_annotated_with_term(u, 
        include_subclasses = include_subclasses, 
            direct_subclasses_only = direct_subclasses_only)
    dups <- which(duplicated(last$STUDY.ACCESSION))
    if (length(dups) > 0) 
        last <- last[-dups, ]
    last$PUBMEDID <- sprintf("<A href='https://pubmed.ncbi.nlm.nih.gov/%s/'>%s</A>", 
        last$PUBMEDID, last$PUBMEDID)
    last$MAPPED_TRAIT_URI <- sprintf("<A href='%s'>%s</A>", last$MAPPED_TRAIT_URI, 
        last$MAPPED_TRAIT_URI)
    last$accstr = last$STUDY.ACCESSION
    last$STUDY.ACCESSION <- sprintf("<A href='https://www.ebi.ac.uk/gwas/studies/%s' target='_blank'>%s</A>", 
        last$STUDY.ACCESSION, last$STUDY.ACCESSION)
    last
}
