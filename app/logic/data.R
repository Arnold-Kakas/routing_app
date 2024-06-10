box::use(
  DBI[dbConnect, dbWriteTable, dbSendQuery, dbFetch, dbClearResult, dbListTables],
  RSQLite[SQLite]
)

#' @export
conn <- dbConnect(SQLite(), "app/logic/dframe.sqlite")


if (!'dframe' %in% dbListTables(conn)) {
  dframe <- data.frame(destination = "",
                       duration = 30)
  dbWriteTable(conn, "dframe", dframe, overwrite = TRUE)
}


#' @export
getDFrame <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM dframe")
  dframe <- dbFetch(res)
  dbClearResult(res)
  return(dframe)
}