
#' build a GRanges with all GWAS associations from SQLite db
#' @importFrom dplyr tbl
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols DataFrame metadata
#' @note Uses gwascatalog_associations table in the GWASCatalogSearchDB.
assocgr_from_db = function() {
 con = gwasCatSearch:::.datacache$dbconn
 assocdf = as.data.frame(dplyr::tbl(con, "gwascatalog_associations"))
 suppressWarnings({
   assocdf$CHR_POSN = as.numeric(assocdf$CHR_POS) # POS includes NA and a x b strings
 })
 bad = which(is.na(assocdf$CHR_POSN))
 if (length(bad)>1) assocdf = assocdf[-bad,]
 gr = GenomicRanges::GRanges(assocdf$CHR_ID, 
    IRanges::IRanges(assocdf$CHR_POSN, width=1))
 S4Vectors::mcols(gr) = S4Vectors::DataFrame(assocdf)
 S4Vectors::metadata(gr) = list(bad_pos=assocdf[bad,])
 gr
}
