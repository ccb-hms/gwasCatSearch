# gwasCatSearch

An R package to provide free text search, and ontology lookup for GWAS Catalog phenotypes

To install the package, use `BiocManager::install("ccb-hms/gwasCatSearch")`.

A shiny app is available at [https://vjcitn.shinyapps.io/gwasCatSearch](https://vjcitn.shinyapps.io/gwasCatSearch) and [https://ccb-rstudio-connect.hms.harvard.edu/gwasCatSearch/](https://ccb-rstudio-connect.hms.harvard.edu/gwasCatSearch/)
---

The representation of the [EBI GWAS catalog](https://www.ebi.ac.uk/gwas/) used
in this package is managed at [Harvard Medical School Center for Computational Biomedicine](https://github.com/ccb-hms/GWASCatalogSearchDB).

- gwasCatSearch redistributes a compressed SQLite database that includes
extracts from the [Semantic SQL](https://github.com/INCATools/semantic-sql) reprsentation of the [Experimental Factor Ontology](https://www.ebi.ac.uk/efo/),
along with extracts from the EBI GWAS catalog.
- To leverage existing R programming methods with the ontology and the
GWAS catalog, we provide functions `make_oi` and `assocgr_from_db`.
- `make_oi` produces an [ontologyIndex](https://cran.r-project.org/web/packages/ontologyIndex/index.html) to simplify visualization and traversal of ontological hierarchies.
- `assocgr_from_db` produces a [GRanges](https://bioconductor.org/packages/devel/bioc/vignettes/GenomicRanges/inst/doc/GenomicRangesIntroduction.html) representation of the GWAS catalog to simplify interrogation of GWAS hits in genomic coordinates.
