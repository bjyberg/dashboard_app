library(leaflet)
library(DT)
library(formattable)

### ----- UI ---- ###

leafletUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")),
    hr(),
    # conditionalPanel(
    #       condition = "input.explore_dims != 1",
    DTOutput(ns("varTable"))
  )
}

### ----- Server ---- ###
leafletServer <- function(id, data, weighted_data, admin_sel, variable_sel) {
  moduleServer(
    id,
    function(input, output, session) {
      index_lookup <- list(
        ac_index = "Adaptive Capacity", human_index = "Human Index",
        social_index = "Social Index", physical_index = "Physical Index",
        economic_index = "Economic Index")

      output$map <- renderLeaflet({
        req(admin_sel)
        if (is.null(admin_sel())) {
          return(leaflet() |> addTiles())
        }

        bounds <- data()$bound
        if (!is.null(admin_sel())) {
          admins <- admin_sel()
          bounds <- bounds[bounds$GID_2 %in% admins]
        }

        map_data <- terra::merge(
          bounds['GID_2'],
          weighted_data(),
          by = 'GID_2'
        )
        variable <- grep(variable_sel()$variable, names(map_data), value = T)

        lookup <- data()$lookup
          if(variable_sel()$type == "capital") {
            colors <- "BuPu" # need to add another color just for ac capital
            variable_name <- paste0(strsplit(variable, "_")[[1]][1], " Index")
          }  else {
            colors <- "YlOrRd"
            variable_name <- lookup[lookup$clean_name == variable, "final_name"]
          }
        leaf_pal <- colorNumeric(colors, map_data[[variable]], na.color = "transparent")
        map_data$selected_var <- map_data[[variable]]

        a1_name <- lookup[lookup$clean_name == "NAME_1", "final_name"]

        # variable_name <- lookup[lookup$clean_name == variable(), "final_name"]
        if(variable == "ac_index") {
          variable_text <- ''
        } else {
          variable_text <- paste0("<br/>", variable_name, ": ", round(map_data[[variable]], 3))
        }

        pop_content <- paste0(
          "<b>", map_data$NAME_2, "</b>",
          paste0("<br/>", a1_name, ": "),
          map_data$NAME_1,
          "<br/>Total Adaptive Capacity: ", round(map_data$ac_index, 3),
          variable_text
        )

        leaflet() |>
          addTiles() |>
          addPolygons(
            data = map_data,
            layerId = map_data$GID_2,
            fillColor = ~ leaf_pal(selected_var),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            popup = pop_content
          ) |>
          addLegend(
            pal = leaf_pal,
            values = map_data$selected_var,
            opacity = 0.7,
            title = variable_name
            )
      })

      dt_table <- reactive({
        req(admin_sel())
        req(weighted_data())
        bounds <- data()$bound
        weighted_data <- weighted_data()
        if (!is.null(admin_sel())) {
          admins <- admin_sel()
          bounds <- bounds[bounds$GID_2 %in% admins]
        }
        index_cols <- grep("index", names(weighted_data), value = TRUE)
        dt_table <- weighted_data[, c("NAME_1", "NAME_2", index_cols, "GID_2")]
        dt_table <- rapply(dt_table, round, classes = "numeric",
          how = "replace", digits = 2)
        dt_table_spatial <- terra::merge(
          bounds['GID_2'],
          dt_table,
          by = 'GID_2'
        )
        return(dt_table_spatial)
      })

      output$varTable <- renderDataTable({
        req(admin_sel())
        lookup <- data()$lookup
        a1_name <- subset(lookup, clean_name == "NAME_1", "final_name")[[1]]
        a2_name <- subset(lookup, clean_name == "NAME_2", "final_name")[[1]]
        df <- as.data.frame(dt_table())[c("NAME_1", "NAME_2", names(index_lookup))]
        names(df) <- c(a1_name, a2_name, unlist(index_lookup))
        formatted_dt <- formattable(
          df,
          list(
            `Adaptive Capacity` = color_tile("#FDD49E", "#D7301Fa4"),
            `Economic Index` = color_tile("#D0D1E6", "#0570B0a4"),
            `Human Index` = color_tile("#D0D1E6", "#0570B0a4"),
            `Social Index` = color_tile("#D0D1E6", "#0570B0a4"),
            `Physical Index` = color_tile("#D0D1E6", "#0570B0a4")
          )
        ) |>
          as.datatable(escape = FALSE,
            selection = "single",
            options = list(scrollX = TRUE, stateSave = TRUE,
              paging = TRUE, pageLength = 5,
              columnDefs = list(list(targets = c(3, 4, 5, 6), orderable = FALSE))),
            rownames = FALSE)
        
        return(formatted_dt)
      })

    # This is now all for the interactive table/map
      prev_row <- reactiveVal()

  observeEvent(input$varTable_rows_selected, {
    lookup <- data()$lookup
    row_selected <- dt_table()[input$varTable_rows_selected, ]
    if (!is.null(prev_row())) {
      leafletProxy("map") |>
        removeShape(layerId = "selected") |>
        addPolygons(data = row_selected, color = "#005eff", layerId = "selected")
    } else {
      leafletProxy("map") |>
        addPolygons(data = row_selected, color = "#005eff", layerId = "selected") 
    }
    # set new value to reactiveVal
    prev_row(row_selected)
  })

  observeEvent(input$map_shape_click, {
    clickId <- input$map_shape_click$id
    id2 <- which(dt_table()$GID_2 == clickId)
    pg_length <- input$varTable_state$length
    rel_row <- (which(input$varTable_rows_all == id2) - 1)
    dataTableProxy("varTable") |>
      selectRows(which(dt_table()$GID_2 == clickId)) |>
      selectPage(rel_row %/% pg_length + 1)
  })
    }
  )
}
