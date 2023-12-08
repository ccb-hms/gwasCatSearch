#' given an accession, tabulate variants from GWAS catalog
#' @param acc character() accession number(s) by EBI
#' @return a data.frame with relevant information about hits reported in the selected studies
#' @examples
#' tab = variants_from_study(acc="GCST007087")
#' dim(tab)
#' head(tab)
#' @export
variants_from_study = function (acc = "GCST90131906") 
{
    con = gwasCatSearch:::.datacache$dbconn
    assocdf = as.data.frame(dplyr::tbl(con, "gwascatalog_associations"))
    suppressWarnings({
        assocdf$CHR_POSN = as.numeric(assocdf$CHR_POS)
    })
    bad = which(is.na(assocdf$CHR_POSN))
    if (length(bad) > 1) 
        assocdf = assocdf[-bad, ]
    dplyr::filter(assocdf, STUDY.ACCESSION %in% acc)
}


