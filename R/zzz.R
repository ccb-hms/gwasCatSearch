#' @import RSQLite
#' @import DBI
#' @import utils

.datacache <- new.env(parent = emptyenv())

#' A function that allows the user to access the SQLite database tables provided with gwasCatSearch
#' @description
#' An instance of the SQLiteConnection class that has been opened on the database tables provided
#' @examples
#' RSQLite::dbListTables(gwasCatSearch_dbconn())
#' @export
gwasCatSearch_dbconn <- function() .datacache$dbconn

gwc_df <-NULL

.onLoad <- function(libname, pkgname) {
  ## Connect to the SQLite DB
  ## shipped in package as a gzipped tarfile that will be untarred into temp dir
  tarfile <- system.file("extdata", "gwascatalog_search.db.tar.xz", package=pkgname, lib.loc=libname)
  td = tempdir()
  untd <- utils::untar(tarfile, list=FALSE, exdir=td)
  dbfile <- paste(td, "gwascatalog_search.db", sep="/")
  if (!file.exists(dbfile)) {
    stop("DB file'", dbfile, "' not found")
  }
  assign("dbfile", dbfile, envir = .datacache)
  dbconn <- DBI::dbConnect(RSQLite::SQLite(),
    dbname = dbfile, cache_size = 64000L,
    synchronous = "off", flags = RSQLite::SQLITE_RO
  )
 
  assign("dbconn", dbconn, envir = .datacache)
  ##no idea why we can't do this one
  ns = topenv()
  ns$gwc_df = dbGetQuery(dbconn, "SELECT * from gwascatalog_mappings")

## drop caching of the large granges object
#  assoc_gr = assocgr_from_db()
#  assign("gwc_gr", assoc_gr, envir = .datacache)
}

.onUnload <- function(libpath) {
  DBI::dbDisconnect(gwasCatSearch_dbconn())
}
