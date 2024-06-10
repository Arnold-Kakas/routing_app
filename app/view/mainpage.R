box::use(
  leaflet[leafletOutput, renderLeaflet, addPolygons, addProviderTiles, addLegend, addTiles, colorNumeric, labelFormat, labelOptions, leaflet, leafletOptions, setView],
  rhandsontable[hot_col, hot_to_r, renderRHandsontable, rHandsontableOutput, rhandsontable],
  shiny[moduleServer, NS, plotOutput, reactive, reactiveValues, tags, tagList],
  shiny.semantic[grid, semanticPage],
  shinyjs[useShinyjs],
)

box::use(
  app/logic/data[conn, getDFrame],
  app/logic/grid[myGridTemplate],
  app/logic/dtedit[dtedit]
)

##### Callback functions.
destination.insert.callback <- function(data, row) {
  query <- paste0("INSERT INTO dframe (destination, duration) VALUES (",
                  "", paste0(data[row,]$destination[[1]], collapse = ';'), "', ",
                  "'", as.integer(data[row,]$duration), "' ",
                  ")")
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getDFrame())
}

destination.update.callback <- function(data, olddata, row) {
  query <- paste0("UPDATE dframe SET ",
                  "destination = '", paste0(data[row,]$destination[[1]], collapse = ';'), "', ",
                  "duration = '", as.integer(data[row,]$duration), "' ",
                  "WHERE id = ", data[row,]$id)
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getDFrame())
}

destination.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM dframe WHERE id = ', data[row,]$id)
  dbSendQuery(conn, query)
  return(getDFrame())
}

#' @export
ui <- function(id) {

  ns <- NS(id)
  semanticPage(
    useShinyjs(),
    grid(
      myGridTemplate,
      inputtable = 
        tagList(
          tags$h3(
            class = "card-title",
            "Fill in you destinations and details"),
          tags$div(
            class = "table-container",
            rHandsontableOutput(ns("table1"))
            )
          ),
      map = 
        tagList(
          tags$h3(
            class = "card-title",
            "Your route"),
          tags$div(
            class = "map-container",
            leafletOutput(ns("map"),
                          height = 350)
            )
          ),
      outputtable = 
        tagList(
          tags$h3(
            class = "card-title",
            "Map details"),
          tags$div(
            class = "table-container",
            rHandsontableOutput(ns("table2"))
          )
        )
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
    
    output$map <- renderLeaflet({
      leaflet() |>
                     addTiles() |>
                     setView(lng = 19.696058, lat = 48.6737532, zoom = 7)
    })
    
    
    output$table2 <- renderRHandsontable({
      DF = data_table()
      
      if (!is.null(DF))
        rhandsontable(DF, width = 550,
                      height = 300) |>
        hot_col("duration", format = "0")
    })
    
  })
}
