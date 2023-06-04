#' @import RSQLite
#' @import DBI

.datacache <- new.env(parent = emptyenv())

gwasCatSearch_dbconn <- function() .datacache$dbconn

.onLoad <- function(libname, pkgname) {
  ## Connect to the SQLite DB
  dbfile <- system.file("extdata", "gwascatalog_search.db", package = pkgname, lib.loc = libname)
  if (!file.exists(dbfile)) {
    stop("DB file'", dbfile, "' not found")
  }
  assign("dbfile", dbfile, envir = .datacache)
  dbconn <- DBI::dbConnect(RSQLite::SQLite(),
    dbname = dbfile, cache_size = 64000L,
    synchronous = "off", flags = RSQLite::SQLITE_RO
  )
  assign("dbconn", dbconn, envir = .datacache)
}

.onUnload <- function(libpath) {
  DBI::dbDisconnect(gwasCatSearch_dbconn())
}
