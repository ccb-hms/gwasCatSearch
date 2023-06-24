#' given an accession, tabulate variants from GWAS catalog
#' @param acc character() accession number(s) by EBI
#' @param gwcat instance of gwascat's gwaswloc class
#' @return a data.frame with all relevant information about hits reported in the selected studies
#' @examples
#' data(gwascat_2023_06_24)
#' tab = variants_from_study(acc="GCST007087", gwcat=gwascat_2023_06_24)
#' dim(tab)
#' head(tab[,1:8])
#' @export
variants_from_study = function(acc, gwcat) {
 mydf = as.data.frame(gwcat)
 mydf[which(mydf$STUDY.ACCESSION %in% acc),]
}

