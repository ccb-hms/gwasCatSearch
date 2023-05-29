## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
library("gwasCatSearch")

## -----------------------------------------------------------------------------
hits = search_features(efo_tc, query = c('Granuloma# granulo*', 'Rheumatic# rheum*'))
summary(hits)
hitsasDT = hits2DT(hits, efo_df, efo_tc)
datatable(hitsasDT, escape=FALSE, rownames=FALSE)

## -----------------------------------------------------------------------------
ht2 = search_features(efo_tc, query = "granulo* AND rheum*")
summary(ht2)

## -----------------------------------------------------------------------------
ematch = resources_annotated_with_term("EFO:0007987", include_subclasses=FALSE)
dim(ematch)
allmatch = resources_annotated_with_term("EFO:0007987", include_subclasses = TRUE)
dim(allmatch)
datatable(head(allmatch,n=20))

