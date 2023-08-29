
#' get path to SQLite database for gwascatSearch, adding it (from Bioconductor Open Storage Network) to BiocFileCache if absent 
#' @import BiocFileCache
#' @param cache instance of BiocFileCache
#' @return NULL if gwascatalog_search.db is already present in cache, otherwise named string
#' with path to SQLite database in cache
#' @export
gwascatdb_path = function(cache=BiocFileCache::BiocFileCache()) {
  path = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocGWASDB/gwascatalog_search.db"
  chk = BiocFileCache::bfcquery( cache, "gwascatalog_search.db" )
  nrec = nrow(chk)
  if (nrec == 0) return(BiocFileCache::bfcadd( cache, 
          rname="gwascatalog_search.db", fpath=path, rtype="web", action="copy" ))
  if (nrec>1) message("multiple gwascatalog_search.db found in cache, returning last.")
  return(chk[nrow(chk),]$rpath)
  }

#' @import RSQLite
#' @import DBI

.datacache <- new.env(parent = emptyenv())

#' A function that allows the user to access the SQLite database tables provided with gwasCatSearch
#' @description
#' An instance of the SQLiteConnection class that has been opened on the database tables provided
#' @examples
#' RSQLite::dbListTables(gwasCatSearch_dbconn())
#' @export
gwasCatSearch_dbconn <- function() .datacache$dbconn

.onLoad <- function(libname, pkgname) {
  ## Connect to the SQLite DB
  dbfile <- gwascatdb_path()
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
