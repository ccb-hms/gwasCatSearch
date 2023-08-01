#' A data frame containing the IDs and Labels for all EFO ontology classes.
#' @docType data
#' @format a data frame with observations on
#' \describe{
#'    \item{\code{Subject}}{The EFO label.}
#'    \item{\code{Object}}{The EFO text string for the ontology entry.}
#'    \item{\code{IRI}}{A URL linking to the EFO page for that term.}
#'    \item{\samp{Direct}}{The count of openGWAS phenotypes that were mapped directly to the EFO term.}
#'    \item{\samp{Inherited}}{The number of openGWAS phenotypes that were inferred to have the EFO term.}
#' }
#' @note We use mappings from study defined traits to the EFO ontology provided by the EBI/EMBL withthe GWAS Catalog.  Any phenotype mapping directly to the `object` label is a direct map.  Since the ontology has notions of parent and child, terms from any child nodes are inferred to be mapped to the node, the number of these is given in the `inferred mapping` column.
#' @source EFO: https://www.ebi.ac.uk/efo/
#' @keywords datasets
#' @examples
#' data(efo_df)
#' dim(efo_df)
#' efo_df[11:15, ]
"efo_df"

#' A tCorpus object built using EFO labels.
#' @docType data
#' @format a tCorpus R6 object where documents are EFO labels suitable for searching
#' @seealso [efo_df], [corpustools::tCorpus()]
#' @source The data are created from `efo_df`
#' @references EFO: https://www.ebi.ac.uk/efo/
#' @examples
#' efo_tc
#' ht2 <- search_features(efo_tc, query = "infectious* AND pancrea*")
#' summary(ht2)
"efo_tc"

#' A snapshot of the EBI GWAS catalog made 2023-06-24, as an extended GRanges instance
#' @name gwascat_2023_06_24
#' @docType data
#' @format The format is: Formal class 'gwaswloc' [package "gwascat"] with 8
#' slots: extractDate, seqnames, ranges, strand, seqinfo, elementMetadata,
#' elementType, metadata.  S4Vectors::mcols can be applied to this object to
#' yield a DataFrame with 499094 observations
#' on the following 38 variables.
#' \describe{ \item{"DATE.ADDED.TO.CATALOG"}{a Date}
#' \item{"PUBMEDID"}{a numeric vector} \item{list("FIRST.AUTHOR")}{a
#' character vector} \item{"DATE"}{a Date} \item{list("JOURNAL")}{a
#' character vector} \item{"LINK"}{a character vector}
#' \item{"STUDY"}{a character vector} \item{list("DISEASE.TRAIT")}{a
#' character vector} \item{"INITIAL.SAMPLE.SIZE"}{a character vector}
#' \item{"REPLICATION.SAMPLE.SIZE"}{a character vector}
#' \item{"REGION"}{a character vector} \item{list("CHR_ID")}{a character
#' vector} \item{"CHR_POS"}{a numeric vector}
#' \item{"REPORTED.GENE.S."}{a character vector}
#' \item{"MAPPED_GENE"}{a character vector}
#' \item{"UPSTREAM_GENE_ID"}{a character vector}
#' \item{"DOWNSTREAM_GENE_ID"}{a character vector}
#' \item{"SNP_GENE_IDS"}{a character vector}
#' \item{"UPSTREAM_GENE_DISTANCE"}{a character vector}
#' \item{"DOWNSTREAM_GENE_DISTANCE"}{a character vector}
#' \item{"STRONGEST.SNP.RISK.ALLELE"}{a character vector}
#' \item{"SNPS"}{a character vector} \item{list("MERGED")}{a character
#' vector} \item{"SNP_ID_CURRENT"}{a character vector}
#' \item{"CONTEXT"}{a character vector} \item{list("INTERGENIC")}{a
#' character vector} \item{"RISK.ALLELE.FREQUENCY"}{a numeric vector}
#' \item{"P.VALUE"}{a numeric vector} \item{list("PVALUE_MLOG")}{a
#' numeric vector} \item{"P.VALUE..TEXT."}{a character vector}
#' \item{"OR.or.BETA"}{a numeric vector} \item{list("X95..CI..TEXT.")}{a
#' character vector} \item{"PLATFORM..SNPS.PASSING.QC."}{a character
#' vector} \item{"CNV"}{a character vector} \item{list("MAPPED_TRAIT")}{a
#' character vector} \item{"MAPPED_TRAIT_URI"}{a character vector}
#' \item{"STUDY.ACCESSION"}{a character vector}
#' \item{"GENOTYPING.TECHNOLOGY"}{a character vector} }
#' @source This was created using makeCurrentGwascat in the Bioconductor gwascat package.
#' @keywords datasets
#' @examples
#' data(gwascat_2023_06_24)
#' tail(sort(table(S4Vectors::mcols(gwascat_2023_06_24)$MAPPED_GENE)))
"gwascat_2023_06_24"
