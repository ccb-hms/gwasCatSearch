#' present a graphical view of selected ontology terms
#' @param tags character() vector of EFO tags
#' @examples
#' tags = c("EFO:0003884", "EFO:0004531", "EFO:0005208", "EFO:0009909", "EFO:0021409", "EFO:0021433")
#' plotTags(tags)
#' @export
plotTags = function(tags) {
    if (!exists("efo")) efo <<- ontoProc::getOnto("efoOnto")
    ontoProc::onto_plot2(efo, tags)
}

