#' Simple app for interrogating an ontology-mapped phenotype term corpus derived from EBI/NHGRI GWAS catalog
#' @importFrom utils data packageVersion
#' @import DT
#' @return Used only for side effect of running a shiny app.
#' @seealso A [shinyapps.io deployment](https://vjcitn.shinyapps.io/gwasCatSearch) is available.
#' @export
search_gwascat = function() {
 shiny::runApp(list(ui=ui, server=server))
}
