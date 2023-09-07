
library(gwasCatSearch)
library(RSQLite)
library(dplyr)
con = gwasCatSearch:::.datacache$dbconn
dbListTables(con)
