\name{efo_tc}
\alias{efo_tc}
\docType{data}
\title{
A tCorpus object built using EFO labels.
}
\description{
A tCorpus object where documents are EFO lables that is suitable for searching
}
\usage{data("efo_tc")}
\format{
  The format is:
Classes 'tCorpus', 'R6' <tCorpus>
}
\source{
The data are created from `efo_df`
}
\seealso{
  \code{\link{efo_df}}
  \code{\link[corpustools]{tCorpus}}
}
\references{
 EFO: https://www.ebi.ac.uk/efo/
}
\examples{
efo_tc
ht2 = search_features(efo_tc, query = "infectious* AND pancrea*")
summary(ht2)
}
\keyword{datasets}
