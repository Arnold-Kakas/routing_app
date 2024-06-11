box::use(
  dplyr[coalesce, filter, join_by, left_join, mutate, select],
  leaflet[addMarkers, leafletOutput, renderLeaflet, addPolygons, addProviderTiles, addLegend, addTiles, colorNumeric, labelFormat, labelOptions, leaflet, leafletOptions, setView],
  rhandsontable[hot_col, hot_cols, hot_to_r, renderRHandsontable, rHandsontableOutput, rhandsontable],
  shiny[moduleServer, NS, plotOutput, reactive, reactiveValues, tags, tagList, uiOutput],
  shiny.semantic[grid, semanticPage],
  shinyjs[useShinyjs],
  tidygeocoder[geo],
)

box::use(
  app/logic/grid[myGridTemplate],
)

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
            "Vyplnte údaje"),
          tags$div(
            class = "card",
            rHandsontableOutput(ns("table1"))
            )
          ),
      map = 
        tagList(
          tags$h3(
            class = "card-title",
            "Miesta a trasa"),
          tags$div(
            class = "card",
            leafletOutput(ns("map"),
                          height = 350)
            )
          ),
      outputtable = 
        tagList(
          tags$h3(
            class = "card-title",
            "Detaily"),
          tags$div(
            class = "card",
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
          
          DF_to_geocode = DF |> 
            filter(is.na(latitude)) |>
            select(adresa)

          DF_geo = geo(
            address = DF_to_geocode$adresa, method = "osm",
            lat = latitude0,
            long = longitude0,
            quiet = TRUE
          ) |> 
            filter(!is.na(address)) |> 
            mutate(address = as.character(address))
          
          print(DF_geo)

          DF = DF |>
            left_join(DF_geo, by = join_by(adresa == address), keep = FALSE) |>
            mutate(latitude = coalesce(latitude, latitude0),
                   longitude = coalesce(longitude, longitude0)
            )
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
        rhandsontable(DF[1:7], 
                      width = 550,
                      height = 300,
                      rowHeaders = NULL) |>
        hot_col("trvanie", format = "0") |> 
        hot_cols(colWidths = ifelse(names(DF) %in% c("longitude", "latitude"), 
                                    0.1, 
                                    ifelse(names(DF) == "adresa", 
                                           150, 
                                           ifelse(names(DF) == "názov", 
                                                  120, 
                                                  50))))
    })

    output$map <- renderLeaflet({
      DF = data_table()
      
      if (all(is.na(DF$longitude))) {
        leaflet() |>
          addTiles() |>
          setView(lng = 19.696058, lat = 48.6737532, zoom = 7)
      } else {
        leaflet(DF) |>
          addTiles() |>
          addMarkers(~longitude, ~latitude, label = ~as.character(názov))
      }
    })
    
    
    output$table2 <- renderRHandsontable({
      DF = data_table()

      if (!is.null(DF))
        rhandsontable(DF[1:7], 
                      width = 550,
                      height = 300,
                      rowHeaders = NULL) |>
        hot_col("trvanie", format = "0") |> 
        hot_cols(colWidths = ifelse(names(DF) %in% c("longitude", "latitude"), 
                                    0.1, 
                                    ifelse(names(DF) == "adresa", 
                                           150, 
                                           ifelse(names(DF) == "názov", 
                                                  120, 
                                                  50))))
    })
  })
}
