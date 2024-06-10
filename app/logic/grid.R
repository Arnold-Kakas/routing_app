box::use(
  shiny.semantic[grid_template],
)

#' @export
myGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("inputtable", "map"),
      c("inputtable", "map"),
      c("inputtable", "outputtable"),
      c("inputtable", "outputtable")
    ),
    cols_width = c("2fr", "4fr"),
    rows_height = c("200px", "200px", "auto")
  ),
  mobile = list(
    areas = rbind(
      "inputtable",
      "map",
      "outputtable"
    ),
    rows_height = c("auto", "auto", "auto"),
    cols_width = c("100%")
  )
)