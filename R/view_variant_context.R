#

#' utility for extracting information about GWAS hits near a position,
#' uses a snapshot of EBI GWAS catalog
#' @importFrom GenomicRanges start
#' @importFrom IRanges subsetByOverlaps
#' @importFrom methods as
#' @param chr chromosome identifier
#' @param pos numeric(1) position of variant of interest
#' @param radius numeric(1) radius of interval to present
#' @param focal_rec NULL or data.frame with one row with elements CHR_ID, CHR_POS, PVALUE_MLOG, STUDY.ACCESSION,
#' MAPPED_TRAIT, SNP_ID_CURRENT, PUBMEDID; this will be used to identify and annotate the "focal"
#' SNP for the visualization with a blue dot
#' @param gwdat instance of GRanges with mcols PVALUE_MLOG, `STUDY ACCESSION`,
#' MAPPED_TRAIT, SNP_ID_CURRENT, PUBMEDID
#' @return a data.frame with fields PVALUE_MLOG, STUDY.ACCESSION, MAPPED_TRAIT,
#' SNP_ID_CURRENT, PUBMEDID, pos, mlogp, and tag, the latter an HTML string
#' for use in tooltips
#' @examples
#' data(gwc_gr)
#' mydf = get_variant_context(gwdat = gwc_gr)
#' head(mydf[, 1:6])
#' @export
get_variant_context = function(chr=15, pos=69e6, radius=5e5, focal_rec=NULL, gwdat) {
 keyvars = c("PVALUE_MLOG", "STUDY ACCESSION", "MAPPED_TRAIT", "SNP_ID_CURRENT", "PUBMEDID")
 stopifnot(all(keyvars %in% names(S4Vectors::mcols(gwdat))))
 tt = IRanges::subsetByOverlaps(gwdat, 
       GenomicRanges::GRanges(chr, IRanges::IRanges(pos,width=1)+radius))
 mydf = as.data.frame(S4Vectors::mcols(tt))[,make.names(keyvars)]
 mydf$pos = GenomicRanges::start(tt)
 mydf$mlogp = pmin(70, mydf$PVALUE_MLOG)
 mydf$focal = FALSE
 if (!is.null(focal_rec)) {
   focal_rec$pos = focal_rec$CHR_POS
   focal_rec$mlogp = pmin(70, focal_rec$PVALUE_MLOG)
   focal_rec$focal = TRUE
   mydf = rbind(mydf, focal_rec[c("PVALUE_MLOG", "STUDY.ACCESSION", "MAPPED_TRAIT",
           "SNP_ID_CURRENT", "PUBMEDID", "pos", "mlogp", "focal")])
   }
 mydf$tag = paste0("snp: ", mydf$SNP_ID_CURRENT, 
                 "<br>", "trait :", mydf$MAPPED_TRAIT, 
                 "<br>", "acc: ", mydf$STUDY.ACCESSION,
                 "<br>", "pmid:", mydf$PUBMEDID)
 mydf
}

#' interactive visualization of GWAS hits near a position
#' @importFrom plotly ggplotly
#' @import ggplot2
#' @param chr chromosome identifier
#' @param pos numeric(1) position of variant of interest
#' @param radius numeric(1) radius of interval to present
#' @param focal_rec NULL or data.frame with one row with elements CHR_ID, CHR_POS, PVALUE_MLOG, STUDY.ACCESSION,
#' MAPPED_TRAIT, SNP_ID_CURRENT, PUBMEDID; this will be used to identify and annotate the "focal"
#' SNP for the visualization with a blue dot
#' @param gwdat instance of GRanges with GWAS catalog p-values etc.
#' @param main character(1) title for plot
#' @return ggplotly is called
#' @examples
#' data(gwc_gr)
#' foc = S4Vectors::mcols(IRanges::subsetByOverlaps(gwc_gr,
#'      GenomicRanges::GRanges("15:69287238")))
#' foc = as.data.frame(foc)  # will fix names
#' foc$CHR_POS = 69287238
#' foc$CHR_ID = 15
#' view_variant_context(gwdat=gwc_gr, focal_rec=foc)
#' @export
view_variant_context = function(chr=15, pos=69e6, radius=5e5, focal_rec=NULL, gwdat,
  main="Manhattan plot for GWAS catalog hits, mouseover for details") {
 mydf = get_variant_context(chr=chr, pos=pos, radius=radius, focal_rec=focal_rec, gwdat=gwdat)
 pl = ggplot(mydf, aes(x=pos, y=mlogp, text=tag)) + geom_point(colour="lightgrey") + xlab(sprintf("Pos on chr %s", chr)) +
         ylab("min(70, -log10 p)") + ggtitle(main)
 focind = which(mydf$focal)
 if (length(focind)>0) {
     ndf = mydf[focind[1],]
     pl = pl + geom_point(data=ndf, aes(x=pos,
                    y=mlogp), colour="red", size=1.5)
     }
 plotly::ggplotly(pl)
}

#> names(tib)
# [1] "DATE.ADDED.TO.CATALOG"      "PUBMEDID"                  
# [3] "FIRST.AUTHOR"               "DATE"                      
# [5] "JOURNAL"                    "LINK"                      
# [7] "STUDY"                      "DISEASE.TRAIT"             
# [9] "INITIAL.SAMPLE.SIZE"        "REPLICATION.SAMPLE.SIZE"   
#[11] "REGION"                     "CHR_ID"                    
#[13] "CHR_POS"                    "REPORTED.GENE.S."          
#[15] "MAPPED_GENE"                "UPSTREAM_GENE_ID"          
#[17] "DOWNSTREAM_GENE_ID"         "SNP_GENE_IDS"              
#[19] "UPSTREAM_GENE_DISTANCE"     "DOWNSTREAM_GENE_DISTANCE"  
#[21] "STRONGEST.SNP.RISK.ALLELE"  "SNPS"                      
#[23] "MERGED"                     "SNP_ID_CURRENT"            
#[25] "CONTEXT"                    "INTERGENIC"                
#[27] "RISK.ALLELE.FREQUENCY"      "P.VALUE"                   
#[29] "PVALUE_MLOG"                "P.VALUE..TEXT."            
#[31] "OR.or.BETA"                 "X95..CI..TEXT."            
#[33] "PLATFORM..SNPS.PASSING.QC." "CNV"                       
#[35] "MAPPED_TRAIT"               "MAPPED_TRAIT_URI"          
#[37] "STUDY.ACCESSION"            "GENOTYPING.TECHNOLOGY"     
