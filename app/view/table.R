box::use(
  rhandsontable[hot_col, hot_to_r, renderRHandsontable, rHandsontableOutput, rhandsontable],
  shiny[moduleServer, NS, plotOutput, reactive, reactiveValues, tags, tagList],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3(
      class = "card-title",
      "Fill in you destinations and details"),
    tags$div(
      class = "table-container",
      "Table",
      rHandsontableOutput(ns("table1"))
      )
    )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
    values = reactiveValues()
    
      data_table = reactive({
        if (!is.null(input$table1)) {
          DF = hot_to_r(input$table1)
        } else {
          if (is.null(values[["DF"]]))
            DF = data
          else
            DF = values[["DF"]]
        }
      
      values[["DF"]] = DF
      DF
    })
  
    output$table1 <- renderRHandsontable({
      DF = data_table()
      
      if (!is.null(DF))
        rhandsontable(DF, width = 550,
                      height = 300) |>
        hot_col("duration", format = "0")
      
  })
  })
}
