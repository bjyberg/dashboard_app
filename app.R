library(shiny)
library(shinyWidgets)
library(plotly)
library(arrow)
library(leaflet)
library(terra)

### ----- UI ---- ###
ui <- fluidPage(
  titlePanel(
    title = div(
      style = "display: flex; align-items: center; justify-content: space-between;",
      h1("AMD Adaptive Capacity Dashboard", style = "font-size: 40px; margin-right: 20px;"),
      div(
        img(src = "amd_logo.png", style = "height: 75px; margin-left: 10px;"),
        img(src = "alliance_logo.png", style = "height: 100px; padding-top: 5px; margin-left: 5px;")
      )
    ),
    windowTitle = "Adaptive Capacity Dashboard"
    ),
    sidebarLayout(
      sidebarPanel(
          countryUI('country-sel'),
          adminUI('admin-sel'),
          variableUI('variable-selection'),
          plotweightUI('capital_weightPlot'),
          likertUI("likert_weights"),
          hr(),
          downloadUI("AC_download"),
          hr(),
          alertUI("more_info"),
        ),
        mainPanel(
          leafletUI("map"),
        )
      )
)

### ----- SERVER ---- ###
server <- function(input, output, session) {
  alertServer("more_info")
  country_data <- countryServer('country-sel')
  weights <- likertServer("likert_weights", reactive(country_data()$lookup))
  admin_sel <- adminServer('admin-sel', reactive(country_data()$bound))
  var_sel <- variableServer('variable-selection', reactive(country_data()$lookup))

  plotweightServer('capital_weightPlot', reactive(weights()$capital))

# # Weighting Values
  weighted_index_df <- reactive({
    req(weights())
    req(country_data())
    lookup <- country_data()$lookup
    data <- country_data()$data
    var_weights <- weights()$variable
    capital_weights <- weights()$capital
    for (c in na.omit(unique(lookup$group))) {
      cols <- na.omit(lookup[lookup$group == c, "clean_name"])
      cols_longname <- na.omit(lookup[lookup$group == c, "standardized_name"])
      data_weighted <- min_max_scale(data[cols] * var_weights[cols_longname])
      index <- min_max_scale(rowSums(data_weighted, na.rm = TRUE))
      data[paste0(tolower(c), "_index")] <- min_max_scale(index * capital_weights[c])
      ac_index <- min_max_scale(rowSums(data[grep('index', names(data))]))
      data['ac_index'] <- ac_index
    }
    return(data)
  })


  leafletServer("map",
    reactive(country_data()),
    reactive(weighted_index_df()),
    reactive(admin_sel()),
    reactive(var_sel())
  )

  downloadServer("AC_download",
    reactive(country_data()$iso3),
    reactive(country_data()$bound),
    reactive(weighted_index_df()),
    reactive(admin_sel())
  )

}

## ----- APP ---- ###
shinyApp(ui, server)