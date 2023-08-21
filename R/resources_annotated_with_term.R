#' A function that finds which phenotypes from a collection of phenotypes (The GWAS Catalog for this package)
#' map to specific terms in an ontology (EFO for this package)
#' @description
#' This function relies on a SQLite database that contains mappings from a set of phenotypes to one or more
#' ontologies.  The database contains information about the organization of the ontology and hence can find
#' phenotypes that map directly to a term or those that are inherited at a term by virtue of having mapped
#' directly to one of the chilren or other more specific descriptions.
#' @param search_terms character() A label for the term (EFO in this case) that is of interest.
#' @param include_subclasses logical(1) defaults to TRUE; if `TRUE` then all terms mapped to subclasses (children) of the term are included.
#' @param direct_subclasses_only logical(1) defaults to FALSE; If `TRUE` then only terms that map to direct children (first step only) are included.
#' @details The mapping of openGWAS traits/phenotypes to EFO ontology terms was performed by `text2term`. The results were
#' then organized, together with information on the sturcture of the ontology (EFO in this case) to facilitate retrieval.
#' Included in the return values is the confidence score provided by `text2term` which gives some indication of how syntactically
#' similar the phenotype description was to the ontology term label.
#' @return
#' A `data.frame` with columns:
#' \describe{
#'   \item{STUDY.ACCESSION}{The GWAS catalog ID for the study.}
#'   \item{PUBMEDID}{The PUBMEDID for the Study}
#'   \item{DISEASE_TRAIT}{The GWAS Catalog text label for that trait.}
#'   \item{MAPPED_TRAIT}{The text description/label of the ontology term.}
#'   \item{MAPPED_TRAIT_CURIE}{The CURIE symbol for the mapped trait.}
#'   \item{MAPPED_TRAIT_URI}{The URI for the ontology term.}
#' }
#' @references `text2term`
#' @author Robert Gentleman
#' @examples
#' ematch <- resources_annotated_with_term("EFO:0005297", include_subclasses = FALSE)
#' dim(ematch)
#' ematch[1, ]
#' @export
resources_annotated_with_term <-
  function(search_terms,
           include_subclasses = TRUE,
           direct_subclasses_only = FALSE) {
    if (!is.character(search_terms) || length(search_terms) < 1)
      stop("invalid search_terms input")
    
    nsearch = length(search_terms)
    ##SQLite has an expression depth of 1000 - so only about 200 terms at a time seems to work
    ##now we split the search_terms into groups
    
    ngroups = floor(nsearch / 200) + 1
    
    gps = rep(1:ngroups, each = 200, length.out = nsearch)
    search_terms = split(search_terms, gps)
    
    con <- gwasCatSearch_dbconn()
    
    if (include_subclasses) {
      if (direct_subclasses_only) {
        ontology_table <- "efo_edges"
      } else {
        ontology_table <- "efo_entailed_edges"
      }
    } else {
      ontology_table <- "efo_edges"
    }
    for (i in seq_along(1:ngroups)) {
      stsub = search_terms[[i]]
      
      query <- paste0(
        "SELECT DISTINCT
                      study.`STUDY.ACCESSION`,
                      study.`DISEASE.TRAIT`,
                      study.MAPPED_TRAIT,
                      study.PUBMEDID,
                      study.MAPPED_TRAIT_CURIE,
                      study.MAPPED_TRAIT_URI
                    FROM `gwascatalog_metadata` study
                    WHERE
                      study.`STUDY.ACCESSION` IN (
                       SELECT DISTINCT mapping.`STUDY.ACCESSION`
                       FROM `gwascatalog_mappings` mapping
                        LEFT JOIN ",
        ontology_table,
        " ee ON (mapping.MAPPED_TRAIT_CURIE = ee.Subject)"
      )
      
      testWhere = paste0("mapping.MAPPED_TRAIT_CURIE = \'", stsub,"\'")
      if(length(testWhere) > 1)
        testWhere = paste0(c("",rep("OR ",length(testWhere)-1)), testWhere)
      if(include_subclasses) {
        tWhere2 = paste0(" OR ee.Object = \'", stsub, "\'")
        testWhere = paste0(testWhere, tWhere2, collapse=" ")
      }

      #Rafael's version - but the for loop is a bit messy and possibly inefficient
      # index <- 0
      # where_clause <- "\nWHERE ("
      # for (term in stsub) {
      #   if (index == 0) {
      #     where_clause <-
      #       paste0(where_clause,
      #              "mapping.MAPPED_TRAIT_CURIE = \'",
      #              term,
      #              "\'")
      #   } else {
      #     where_clause <-
      #       paste0(where_clause,
      #              " OR mapping.MAPPED_TRAIT_CURIE = \'",
      #              term,
      #              "\'")
      #   }
      #   if (include_subclasses) {
      #     where_clause <-
      #       paste0(where_clause, " OR ee.Object = \'", term, "\'")
      #   }
      #   index <- index + 1
      # }
      #query <- paste0(query, where_clause, ") \n )")
      query <- paste0(query, "\nWHERE (", testWhere, ") \n )")
      results <- dbGetQuery(con, query)
      if (i == 1)
        ans = results
      else
        ans = rbind(ans, results)
    }
    return(ans)
  }
