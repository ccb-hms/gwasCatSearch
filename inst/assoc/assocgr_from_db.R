
library(gwasCatSearch)
library(RSQLite)
library(dplyr)
library(S4Vectors)
library(GenomicRanges)
assocgr_from_db = function() {
 con = gwasCatSearch:::.datacache$dbconn
 assocdf = as.data.frame(tbl(con, "gwascatalog_associations"))
 suppressWarnings({
   assocdf$CHR_POSN = as.numeric(assocdf$CHR_POS) # POS includes NA and a x b strings
 })
 bad = which(is.na(assocdf$CHR_POSN))
 if (length(bad)>1) assocdf = assocdf[-bad,]
 gr = GRanges(assocdf$CHR_ID, IRanges(assocdf$CHR_POSN, width=1))
 mcols(gr) = DataFrame(assocdf)
 metadata(gr) = list(bad_pos=assocdf[bad,])
 gr
}
