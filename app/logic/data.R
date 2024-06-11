# box::use(
#   DBI[dbConnect, dbWriteTable, dbSendQuery, dbExecute, dbFetch, dbClearResult, dbListTables],
#   RSQLite[SQLite]
# )

# conn <- dbConnect(SQLite(), "app/logic/dframe.sqlite")
# 
# getDFrame <- function() {
#   # Remove all records from the dframe table
#   dbExecute(conn, "DELETE FROM dframe")
#   res <- dbSendQuery(conn, "SELECT * FROM dframe")
#   dframe <- dbFetch(res)
#   dbClearResult(res)
#   return(dframe)
# }

dframe <- data.frame(
  názov = NA_character_,
  adresa = NA_character_,
  trvanie = NA_integer_,
  štart = TRUE,
  koniec = FALSE,
  latitude = NA_real_,
  longitude = NA_real_
)