\name{openGWASSearch_dbconn}
\alias{openGWASSearch_dbconn}
\title{
A function that provides access to the RSQLite database containing EFO and other data.
}
\description{
A connection is created to the DB at load time and returned when the package is unloaded. Different
functions within the package make use of the connection and there are times when the user might want
to query the DB directly.
}
\usage{
openGWASSearch_dbconn()
}
\value{
A connection to the DB of class "SQLiteConnection"
}
\author{
Robert Gentleman
}
