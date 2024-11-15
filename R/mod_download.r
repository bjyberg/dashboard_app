### ----- UI ---- ###
downloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("download_checkbox"),
      label = "Download Data?",
      FALSE
    ),
    conditionalPanel(
      condition = "input.download_checkbox == 1",
      virtualSelectInput(ns("downloadType"), "Download Type",
        choices = c(
          "CSV" = ".csv",
          "Geopackage" = ".gpkg",
          "FlatGeobuf" = ".fgb"
        ),
        multiple = FALSE
      ),
      downloadButton(ns("download"), "Download Data"),
      ns = ns
    )
  )
}

### ----- Server ---- ###

downloadServer <- function(id, iso3, country_bounds, weighted_data, admin_sel) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download <- downloadHandler(
        filename = function() {
          paste0(iso3(), "_vulnerability_data", input$downloadType)
        },
        content = function(file) {
          weighted_data <- subset(weighted_data(), GID_2 %in% admin_sel())
          if (input$downloadType == ".csv") {
              write.csv(weighted_data, file)
          } else if (input$downloadType %in% c('.gpkg', '.fgb')) {
            mergeCols <- c("admin1_name", "admin2_name", "GID_2", "NAME_2",
             "NAME_1", "HASC_2")
            country_bounds()[country_bounds()$GID_2 %in% admin_sel(), ] |>
              terra::merge(weighted_data, by = mergeCols) |>
              writeVector(file)
          }
        }
      )
    }
  )
}