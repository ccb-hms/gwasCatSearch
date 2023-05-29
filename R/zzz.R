.datacache <- new.env(parent=emptyenv())

gwasCatSearch_dbconn <- function() .datacache$dbconn

.onLoad <- function(libname, pkgname)
{
  ## Connect to the SQLite DB
  dbfile <- system.file("extdata", "gwascatalog_search.db", package=pkgname, lib.loc=libname)
  if (!file.exists(dbfile))
    stop("DB file'", dbfile, "' not found")
  assign("dbfile", dbfile, envir=.datacache)
  dbconn <- dbConnect(SQLite(), dbname = dbfile, cache_size = 64000L,
                      synchronous = "off", flags = SQLITE_RO)
  assign("dbconn", dbconn, envir=.datacache)
}

.onUnload <- function(libpath)
{
  dbDisconnect(gwasCatSearch_dbconn())
}
