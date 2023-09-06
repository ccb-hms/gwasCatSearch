#' produce an ontology_index instance from GWASCatalogSearchDB sqlite
#' @importFrom ontologyIndex ontology_index
#' @importFrom dplyr tbl
#' @param con SQLite connection via RSQLite/DBI dbConnect
#' @examples
#' rcon = gwasCatSearch:::.datacache$dbconn
#' efo = make_oi(rcon)
#' efo$name[1:5]
#' efo$children[1:3] # do not disconnect or check will error
#' @export
make_oi = function(con) {
 ll = as.data.frame(tbl(con, "efo_labels"))
 nn = split(ll$Object, ll$Subject)
 ed = as.data.frame(tbl(con, "efo_edges"))
 pl = split(ed$Object, ed$Subject)[names(nn)]
 ontology_index(name=nn, parents=pl)
}

