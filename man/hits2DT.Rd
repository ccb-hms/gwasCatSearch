\name{hits2DT}
\alias{hits2DT}
\title{
A function that takes the output of query against the EFO corpus and creates a datatable object.
}
\description{
Users query the corpus to find EFO ontology terms that match the query. Those hits can be processed
by `hits2DT` to produce a datatable which then provides some amount of interactivity.
}
\usage{
hits2DT(hits, efoDF, tc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hits}{
A `featureHits` object from the corpustools package.
}
  \item{efoDF}{
A dataframe containing the information about the EFO terms.
}
  \item{tc}{
A `tCorpus` object that was built using the same dataframe that is used for `efoDF`
}
}
\details{
Once hits have been found, this function obtains that actual text string in the EFO term that was matched
to the query.  The using the `iri` column in `efoDF` a link to the EFO on-line resource for the term is created.
Information about how many phenotypes map to the term, both directly and via inheritance are reported.
}
\value{
A `datatable` object with columns:
"EFO"      "Text"     "Direct"   "Inherited" Others
  \item{EFO}{The EFO label for the term and link.}
  \item{Text}{The text of the EFO term.}
  \item{Direct}{The number of phenotypes that map directly to that term.}
  \item{Inherited}{The number of phenotypes that are inherited by mapping to children of the term.}
  \item{Others}{One column for each labeled search term in the call that created the `hits` object. If
  those terms were labeled then the columns will have the same label. The value in the column is the actual word
  in the EFO term that was matched on.}
}
\author{
R. Gentleman
}

\seealso{
 \code{\link[corpustools]{tCorpus}}, \code{\link{efo_df}},
\code{\link[DT]{datatable}}
}
\examples{
hits = search_features(efo_tc, query = c('Infect# infectious*','Pancreas# pancrea*'))
summary(hits)
hitsasDT = hits2DT(hits, efo_df, efo_tc)
}
