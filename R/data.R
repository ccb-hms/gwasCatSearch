#' A data frame containing the IDs and Labels for all EFO ontology classes
#' @docType data
#' @format a data frame with observations on
#' \describe{
#'    \item{\code{Subject}}{The CURIE Identifier.}
#'    \item{\code{Object}}{The EFO text description for the ontology entry.}
#'    \item{\code{IRI}}{A URL linking to the EFO page for that term.}
#'    \item{\code{DiseaseLocation}}{A comma separated list of CURIE identifiers for annotated disease locations.}
#'    \item{\samp{Direct}}{The count of openGWAS phenotypes that were mapped directly to the EFO term.}
#'    \item{\samp{Inherited}}{The count of openGWAS phenotypes that were inferred to have the EFO term.}
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

#' A tCorpus object built using GWAS Catalog Studies.
#' @docType data
#' @format a tCorpus R6 object where documents are GWAS Catalog studies, suitable for searching
#' @seealso [efo_tc], [corpustools::tCorpus()]
#' @source The data are created from `gwc_tc`
#' @references GWAS catalog: https://www.ebi.ac.uk/gwas/
#' @examples
#' gwc_tc
#' htg <- search_features(gwc_tc, query = "leuk*")
#' summary(htg)
"gwc_tc"


#' 
#' A data frame containing the GWAS catalog studies and EFO mappings
#' @docType data
#' @format A data frame with observations on
#' \describe{
#'    \item{\code{STUDY.ACCESSION}}{The GWAS Catalog ID.}
#'    \item{\code{DISEASE.TRAIT}}{The text description for the trait being studied.}
#'    \item{\code{MAPPED_TRAIT}}{The text description of EFO term mapped to that trait.}
#'    \item{\code{MAPPED_TRAIT_URI}}{The URI for the mapped EFO term.}
#'    \item{\samp{MAPPED_TRAIT_CURIE}}{The CURIE for the mapped EFO term.}
#' }
#' @note We use mappings from study defined traits to EFO as provided by the GWAS Catalog
#' @source EFO: https://www.ebi.ac.uk/efo/
#' @keywords datasets
#' @export
"gwc_df" 

