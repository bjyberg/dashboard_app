library(shiny)
library(terra)
library(arrow)

### ----- Datasets  ---- ###
boundary_list <- list.files("data/", pattern = '.fgb', full.names = T)
data_list <- list.files('data/', pattern = "transformed", full.names = T)

### ----- Helper Functions ---- ###

handle_a2_duplicates <- function(data) {
  dupes <- which(duplicated(data$NAME_2))
  if (length(dupes) > 0) {
    n2 <- data[dupes, ]$NAME_2
    n1 <- data[dupes, ]$NAME_1
    n1_add <- gsub("\\b(\\pL)\\pL{1,}|.", "\\U\\1", n1, perl = TRUE)
    new_n2 <- paste0(n2, "-", n1_add)
    data[dupes, ]$NAME_2 <- new_n2
  }
}


### ----- UI ---- ###

countryUI <- function(id) {
  ns <- NS(id)
  pickerInput(ns("Country"), "Select Country",
    choices = c("Vietnam", "Cambodia", "Bangladesh"),
    selected = "Vietnam"
  )
}


### ----- Server ---- ###

countryServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      iso3 <- reactive({
        req(input$Country)
        if (input$Country == "Vietnam") {
          "VNM"
        } else if (input$Country == "Cambodia") {
          "KHM"
        } else if (input$Country == "Bangladesh") {
          "BGD"
        }
      })

      country_data <- reactive({
        req(iso3)
        # country_bounds = vect(grep(iso3(), boundary_list, value = T))
        return(
          list(
          iso3 = iso3(),
          data = read_parquet(grep(iso3(), data_list, value = T)),
          bound = vect(grep(iso3(), boundary_list, value = T)),
          lookup = subset(read.csv("data/name_lookup.csv"), iso3 == iso3())
        ))
      })
      return(country_data)
    }
  )
}
