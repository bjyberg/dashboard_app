library(shiny)
library(shinyWidgets)
library(plotly)
library(arrow)
library(leaflet)
library(terra)

### ----- UI ---- ###
ui <- fluidPage(
  navbarPage("AMD Adaptive Capacity",
  tags$style(type = 'text/css',
                                ".nav.navbar-nav {visibility: hidden;}",
                                ".navbar-brand {font-size: 45px; float: left; margin-left: 20 px; padding-top:40px;}",
                                ".navbar-nav {float: left; margin-right: 20 px;}"),
  tags$script(HTML("var header = $('.navbar > .container-fluid');
    header.append('<div style=\"float:right\"><ahref=\"url\"><img src=\"alliance_logo.png\" style=\"float:right;height:100px;padding-top:5px;\"> </a></div>');
    header.append('<div style=\"float:right\"><ahref=\"url\"><img src=\"amd_logo.png\" style=\"float:right;height:100px;padding-top:5px;\"> </a></div>');")
  ),
    tabPanel("Explore",
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
      ),
    ),
  )
)

### ----- SERVER ---- ###
server <- function(input, output, session) {
  alertServer("more_info")
  weights <- likertServer("likert_weights")
  # observe(print(weights()))
  country_data <- countryServer('country-sel')
  admin_sel <- adminServer('admin-sel', reactive(country_data()$bound))
  var_sel <- variableServer('variable-selection', reactive(country_data()$lookup))
  plotweightServer('capital_weightPlot', reactive(weights()$capital))

  # observe(print(var_sel()))
# Weighting Values
  weighted_index_df <- reactive({
    req(weights())
    req(country_data())
    lookup <- country_data()$lookup
    data <- country_data()$data
    var_weights <- weights()$variable
    capital_weights <- weights()$capital
    for (c in na.omit(unique(lookup$group))) {
      clean_capital <- tolower(c)
      cols <- na.omit(lookup[lookup$group == c, "clean_name"])
      cols_longname <- na.omit(lookup[lookup$group == c, "standardized_name"])
      data_weighted <- min_max_scale(data[cols] * var_weights[cols_longname])
      index <- min_max_scale(rowSums(data_weighted, na.rm = TRUE))
      data[paste0(clean_capital, "_index")] <- min_max_scale(index * capital_weights[clean_capital]) # this needs to equal 1 at no weighting
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