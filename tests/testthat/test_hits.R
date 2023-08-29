context("check hits processing")

test_that("hits processing succeeds", {
  hits <- search_features(efo_tc, query = c("Infect# infectious*", "Pancreas# pancrea*"))
  hitsasDT <- hits2DT(hits, efo_df, efo_tc)
  expect_true( nrow(hitsasDT) == 384) 
  expect_true( all(names(hits$hits) %in%
     c("code", "feature", "doc_id", "sentence", "token_id", "hit_id")) )
})

