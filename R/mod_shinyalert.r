library(shiny)
library(shinyalert)

alertUI <- function(id) {
  ns <- NS(id)
  div(
    class = "input-group",
    tags$span(
      style = "vertical-align: bottom;",
      actionButton(ns("about_b"), "More Information",
        icon = icon("circle-question", "lg")
      )
    )
  )
}

alertServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$about_b, {
      shinyalert(
        html = TRUE,
        closeOnClickOutside = TRUE,
        showConfirmButton = FALSE,
        size = "l",
        text = paste(
          "<h3 style='font-size:20px; text-align: left;'><strong>",
          "Three key points to consider when reading the adaptive",
          "capacity analysis and interpreting the results include:</strong>",
          "</h3>",
          "<ul style = 'font-size:16px; text-align: left;''>",
          "<li><strong>",
          "The adaptive capacity indicators and indexes are scored on a",
          "scale of 0 (light orange) to 1 (red), with 0 representing a low",
          "level of adaptive capacity and 1 representing a high level of",
          "adaptive capacity.",
          "</strong>",
          "This normalization process was done to support the easy",
          "comparison of different data sets.",
          "</li>",
          "<li><strong>",
          "The results for the adaptive capacity analysis should",
          "only be used to compare different districts within the same",
          "country and not used for cross country comparisons",
          "</strong>",
          "There are a couple of reasons for this. Firstly, while attempts",
          "were made to use similar indicators in each of the target",
          "countries, there are some differences in the indicators used",
          "based on data availability and the likely contribution they have",
          "to adaptive capacity. Secondly, and most importantly, the",
          "analysis is designed in such a way as to determine adaptive",
          "capacity based on the performance of a district against that of",
          "the other districts included in the analysis for that country.",
          "The benefit of this is that it allows for a straightforward",
          "comparison of the different districts in a target region,",
          "analyzing the performance of a district against its peers which",
          "helps to steer investment decisions within that region. However,",
          "a limitation of this approach is that the best performing",
          "district within that region will, through the nature of the",
          "analysis, have a high adaptive capacity even if the district",
          "performs badly based on international standards.",
          "</li>",
          "<li><strong>",
          "While the results are disaggregated by district, they do",
          "not take into account any of the heterogeneity within districts.",
          "</strong>",
          "This is due to most of the data that is used in the analysis only",
          "being available at the district or in some instances provincial",
          "level. For the districts that had insufficient data, we tried to",
          "impute those missing values. However, not all variables will",
          "share a relationship in a spatial or provincial region. The",
          "impact of this is that the analysis may miss certain vulnerable",
          "groups within districts that could be the target of certain",
          "investments.",
          "</li>",
          "</ul>"
        )
      )
    })
  })
}
