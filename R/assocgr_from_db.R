
#' build a GRanges with all GWAS associations from SQLite db
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols DataFrame metadata
#' @param acc character() study accession number(s) from EBI
#' @return a data.frame with relevant information about hits reported in the selected studies
#' @note Uses gwascatalog_associations table in the GWASCatalogSearchDB.
#' @examples
#' tab = granges_from_study(acc="GCST007087")
#' @export
granges_from_study = function(acc) {
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
  if (length(bad) > 0) 
    assocdf = assocdf[-bad, ]
 gr = GenomicRanges::GRanges(assocdf$CHR_ID, 
    IRanges::IRanges(assocdf$CHR_POSN, width=1))
 S4Vectors::mcols(gr) = S4Vectors::DataFrame(assocdf)
 S4Vectors::metadata(gr) = list(bad_pos=assocdf[bad,])
 gr
}
