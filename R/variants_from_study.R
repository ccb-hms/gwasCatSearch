#' given an accession, tabulate variants from GWAS catalog
#' @param acc character() accession number(s) by EBI
#' @param gwdat instance of GRanges with `STUDY ACCESSION` and other locus-level metadata
#' @return a data.frame with relevant information about hits reported in the selected studies
#' @examples
#' data(gwc_gr)
#' tab = variants_from_study(acc="GCST007087", gwdat=gwc_gr)
#' dim(tab)
#' head(tab)
#' @export
variants_from_study = function(acc, gwdat) {
 mydf = as.data.frame(gwdat)  # substitutes period
 mydf[which(mydf$STUDY.ACCESSION %in% acc),]
}

