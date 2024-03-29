% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{EFOancestors}
\alias{EFOancestors}
\title{A function to access the ancestors of the input EFO IDs}
\usage{
EFOancestors(EFOID)
}
\arguments{
\item{EFOID}{a character vector of the EFO CURIE symbols}
}
\value{
A named vector, names are from the input EFOIDs and the values are the corresponding CURIEs for the ancestor terms. There can be zero or more matches for each input ID.
}
\description{
This function provides an interface to the SQL database containing EFO entailed edges.
}
\details{
The function returns the set of ancestors for the input terms. The ancestors are all nodes that are parents, or parents of parents and so on, up to the root node of the ontology. These edges are entailed as there are some relationships that can be
inferred that would not be detected by simply recursively finding parents.
}
\examples{
tancestors = EFOancestors(c("EFO:0000768", "MONDO:0002429"))
table(names(tancestors))
}
\author{
Robert Gentleman
#' @param EFOID a character vector of the EFO CURIE symbols
}
