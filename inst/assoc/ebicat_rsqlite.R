
# this is one way to produce a comprehensive SQLite representation
# of the GWAS catalog.  you will still have to deal with the fact
# that many CHR_POS are non-numeric 
library(RSQLite)
library(dplyr)
library(readr) # necessary because of embedded EOF
tab = suppressWarnings(read_tsv([path to alternative tsv]))
mycon = dbConnect(SQLite(), ":memory:")
assoctab = dbWriteTable(mycon, "assoc", tab)
tbl(mycon, "assoc")
