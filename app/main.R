box::use(
  shiny[bootstrapPage, div, moduleServer, NS, tags],
)

box::use(
  app/logic/data[conn, getDFrame],
  app/view/mainpage,
)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    tags$head(
      tags$title("cleandata Router Optimizer"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Maven+Pro:wght@400;500;700&display=swap",
        rel = "stylesheet"
      )
    ),
    tags$body(
      tags$h1(
        class = "main-heading",
        style = "display: flex;
                justify-content: space-between;",
        tags$span("Router Optimizer"),
        tags$a(
          style = "margin-right: 20px;",
          href = "https://www.cleandata.sk",
          tags$img(
            src = "static/cleandata_logo.png",
            alt = "cleandata logo",
            height = "58px",
            width = "220px"
          ),
          target = "_blank"
        )
      ),
      tags$div(
        mainpage$ui(ns("mainpage"))
        )
      )
    )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mainpage$server("mainpage", dframe)
  })
}
