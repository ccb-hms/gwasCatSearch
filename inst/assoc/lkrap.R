library(gwasrapidd)
rapvdemo = get_variants(study_id = 'GCST001085', warnings = FALSE)
str(rapvdemo)



library(RSQLite)
library(dplyr)
library(readr) # necessary because of embedded EOF
if (!exists("loctab")) {
 tab = suppressWarnings(read_tsv("ebi_gwcat_alternative_20230901.gz"))
 mycon = dbConnect(SQLite(), ":memory:")
 assoctab = dbWriteTable(mycon, "assoc", tab)
 loctab = tbl(mycon, "assoc")
}
locvdemo = tbl(mycon, "assoc") |> filter(`STUDY ACCESSION` == 'GCST001085')

gwrdd = gwasrapidd::get_studies(efo_trait = 'autoimmune disease')
gwrdd@studies$study_id


loctab |> filter(MAPPED_TRAIT == "autoimmune disease") |> select(`STUDY ACCESSION`) |> group_by(`STUDY ACCESSION`) |> summarise(n=n())
