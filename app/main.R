box::use(
  shiny[bootstrapPage, div, moduleServer, NS, tags],
)

box::use(
  app/logic/data[dframe],
  app/view/table
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    tags$div(
      table$ui(ns("table1")),
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    table$server("table1", dframe)
  })
}
