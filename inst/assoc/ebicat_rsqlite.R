
library(RSQLite)
library(dplyr)
library(readr) # necessary because of embedded EOF
tab = suppressWarnings(read_tsv("ebi_gwcat_alternative_20230901.gz"))
mycon = dbConnect(SQLite(), ":memory:")
assoctab = dbWriteTable(mycon, "assoc", tab)
tbl(mycon, "assoc")
