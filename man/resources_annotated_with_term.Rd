\name{resources_annotated_with_term}
\alias{resources_annotated_with_term}

\title{
A function that finds which phenotypes from a collection of phenotypes (openGWAS for this package)
map to specific terms in an ontology (EFO for this package).
}
\description{
This function relies on a SQLite database that contains mappings from a set of phenotypes to one or more
ontologies.  The database contains information about the organization of the ontology and hence can find
phenotypes that map directly to a term or those that are inherited at a term by virtue of having mapped
directly to one of the chilren or other more specific descriptions.
}
\usage{
resources_annotated_with_term(search_term, include_subclasses = TRUE,
                  direct_subclasses_only = FALSE)
}

\arguments{
  \item{search_term}{
A label for the term (EFO in this case) that is of interest.
}
  \item{include_subclasses}{
If `TRUE` then all terms mapped to subclasses (children) of the term are included.
}
  \item{direct_subclasses_only}{
If `TRUE` then only terms that map to direct children (first step only) are included.
}
}
\details{
The mapping of openGWAS traits/phenotypes to EFO ontology terms was performed by `text2term`. The results were
then organized, together with information on the sturcture of the ontology (EFO in this case) to facilitate retrieval.
Included in the return values is the confidence score provided by `text2term` which gives some indication of how syntactically
similar the phenotype description was to the ontology term label.
}
\value{
A `data.frame` with columns:
[1] "OpenGWAS ID"        "OpenGWAS Trait"     "Ontology Term"      "Ontology Term ID"   "Mapping Confidence"
  \item{OpenGWAS ID}{The openGWAS ID for the trait.}
  \item{OpenGWAS Trait}{The openGWAS text label for that trait.}
  \item{Ontology Term}{The text description/label of the ontology term.}
  \item{Ontology Term ID}{The label for the ontology term.}
  \item{Mapping Confidence}{The confidence score from mapping the phenotype label to the ontology label.}
}
\references{
`text2term`
}
\author{
Robert Gentleman
}

\examples{
ematch = resources_annotated_with_term("EFO:0009605", include_subclasses=FALSE)
dim(ematch)
ematch[1,]
}
