\name{efo_df}
\alias{efo_df}
\docType{data}
\title{
A data frame containing the IDs and Labels for all EFO ontology classes.
}
\description{
All EFO
}
\usage{data("efo_df")}
\format{
  A data frame with 28428 observations on the following 5 variables.
  \describe{
    \item{\code{Subject}}{The EFO label.}
    \item{\code{Object}}{The EFO text string for the ontology entry.}
    \item{\code{IRI}}{A URL linking to the EFO page for that term.}
    \item{\samp{Direct}}{The count of openGWAS phenotypes that were mapped directly to the EFO term.}
    \item{\samp{Inherited}}{The number of openGWAS phenotypes that were inferred to have the EFO term.}
  }
}
\details{
Separately we have mapped all openGWAS traits to EFO ontology terms using the `text2term` (https://text2term.hms.harvard.edu/) mapper.  Any phenotype mapping directly to the `object` label is a direct map.  Since the ontology has notions of parent and child, terms from any child nodes are inferred to be mapped to the node, the number of these is given in the `inferred mapping` column.
}
\source{
EFO: https://www.ebi.ac.uk/efo/
}
\examples{
data(efo_df)
efo_df[11:15,]
}
\keyword{datasets}
