the contents of this folder give a way of producing a SQLite database
from EBI gwascat "alternative" text, which has various problems

- EOF embedded in string
- a x b type addresses for epistasis studies

the R script uses readr::read_tsv to get around these; records with problems are
annotated in a 'problems' attribute that is ignored when the good records are
ingested
