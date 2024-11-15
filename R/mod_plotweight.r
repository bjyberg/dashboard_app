### ----- UI ---- ###

plotweightUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("weight_plot"), height = "250px")
}

### ----- Server ---- ###

plotweightServer <- function(id, weight_ls) {
  moduleServer(
    id,
    function(input, output, session, weights = weight_ls) {
      observe({
        weights <- weights()
        radar_weights <- plot_ly(
          type = "scatterpolar",
          mode = "markers",
          r = weights,
          theta = tools::toTitleCase(names(weights)),
          name = "Weights",
          tooltip = NULL,
          fill = "toself",
          hovertext = paste(
            "<b>Dimension: </b>", tools::toTitleCase(names(weights)),
            "<br><b>Weight: </b>", weights
          ),
          hoverinfo = "text"
        ) |>
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            margin = list(l = 0, r = 0),
            dragmode = FALSE,
            polar = list(
              angularaxis = list(
                rotation = 24
              ),
              radialaxis = list(
                visible = F
                #     range = c(0, max(ac_weightings[input$ac_weight][[1]]))
              )
            ),
            showlegend = FALSE
          ) |>
          config(
            displayModeBar = FALSE
          )
        output$weight_plot <- renderPlotly(radar_weights)
      })
    }
  )
}