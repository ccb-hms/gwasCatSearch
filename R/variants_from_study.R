#' given a set of Study Accession IDs extract the corresponding variants from GWAS catalog
#' @param acc character() accession number(s) by EBI
#' @return a data.frame with relevant information about hits reported in the selected studies
#' @examples
#' tab = variants_from_study(acc="GCST007087")
#' dim(tab)
#' head(tab)
#' @export
variants_from_study = function (acc) 
{
    if (!is.character(acc) | length(acc) < 1)
     stop("incorrect input")
    con = gwasCatSearch_dbconn()
    q1 = paste0("SELECT * FROM gwascatalog_associations WHERE [STUDY.ACCESSION] IN ('",
                paste(acc, collapse = "','"),
                "')")
    assocdf = dbGetQuery(con, q1)
    suppressWarnings({
        assocdf$CHR_POSN = as.numeric(assocdf$CHR_POS)
    })
    bad = which(is.na(assocdf$CHR_POSN))
    if (length(bad) > 1) 
        assocdf = assocdf[-bad, ]
    assocdf
}


