library(leaflet)
library(DT)
library(formattable)

### ----- UI ---- ###

leafletUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = 650),
    hr(),
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

        observe({print(data()$data)})

      output$map <- debounce(
        renderLeaflet({
        req(admin_sel)
        req(data()$bound)
        req(weighted_data())
        req(variable_sel())
        bounds <- data()$bound


        if (is.null(admin_sel()) || is.null(variable_sel())) {
          return(leaflet() |> addTiles())
        } else {
          admins <- admin_sel()
          bounds <- bounds[bounds$GID_2 %in% admins]
        }

        if (nrow(bounds) == 0) {
          return(leaflet() |> addTiles())
        }

        map_data <- terra::merge(
          bounds['GID_2'],
          weighted_data(),
          by = 'GID_2'
        )
        
        if (variable_sel()$type == "capital") {
          variable <- paste0(tolower(variable_sel()$variable), "_index")
        } else {
          variable <- variable_sel()$variable
        }
        # variable <- grep(variable_sel()$variable, names(map_data), value = T) #rembember, if there is an error - blame regex

        lookup <- data()$lookup
          if(variable_sel()$type == "capital") {
            colors <- c("#E76254", "#EE8648", "#F5A354", "#FCC468", "#FFDD9A",
                "#D4E1CB", "#93CFDB", "#68AEC8", "#4C86A8", "#346391", "#1E466E") # need to add another color just for ac capital
            variable_name <- paste0(strsplit(variable, "_")[[1]][1], " Index")
          }  else {
            colors <- c("#E76254", "#EE8648", "#F5A354", "#FCC468", "#FFDD9A",
                "#D4E1CB", "#93CFDB", "#68AEC8", "#4C86A8", "#346391", "#1E466E")
            variable_name <- lookup[lookup$clean_name == variable, "final_name"]
          }
        # leaf_pal <- colorNumeric(colors, map_data[[variable]], na.color = "transparent")
        leaf_pal <- colorNumeric(colors, c(0,1), na.color = "transparent")
        map_data$selected_var <- map_data[[variable]]

        a1_name <- lookup[lookup$clean_name == "NAME_1", "final_name"]

        if (grepl('index', variable)) {
          variable_name <- index_lookup[[variable]]
        } else {
          variable_name <- lookup[lookup$clean_name == variable, "final_name"]
        }

        if(variable == "ac_index") {
          variable_text <- ''
        } else if (grepl('index', variable)) {
          variable_text <- paste0("<br/>", "<b>", variable_name,": </b>", round(map_data[[variable]][[1]], 3))
        } else {
          variable_text <- paste0("<br/>", "<b>", variable_name,": </b>",
            "<br/> Index Value: ", round(map_data[[variable]][[1]], 3), 
            "<br/> Actual Value: ", round(data()$data[[variable]][[2]], 3))
        }

        pop_content <- paste0(
          "<b>", map_data$NAME_2, "</b>",
          paste0("<br/>", a1_name, ": "),
          map_data$NAME_1,
          "<br/>Total Adaptive Capacity: ", round(map_data$ac_index, 3),
          variable_text
        )
        legend_title <- if (variable_name == "Adaptive Capacity") {
          variable_name
        } else {
          HTML(paste0(
            "Adaptive Capacity",
            "<br/>",
            "<span style='font-size: 12px; font-weight: normal;'>", variable_name, "</span>"
          ))
        }

        leaflet() |>
          addTiles() |> 
          addPolygons(
            data = map_data,
            layerId = map_data$GID_2,
            fillColor = ~ leaf_pal(selected_var),
            fillOpacity = 0.8,
            color = "white",
            weight = 1,
            popup = pop_content
          ) |>
          addLegend(
            pal = leaf_pal,
            # values = map_data$selected_var,
            values = c(0,1),
            labFormat = function(type, breaks) {
              return(c("Low", "", "Mid.", "", "High"))
            },
            opacity = 0.8,
            title = legend_title
            )
      }),
      200)


      dt_table <- reactive({
        req(admin_sel())
        req(weighted_data())
        bounds <- data()$bound
        weighted_data <- weighted_data()
        if (!is.null(admin_sel())) {
          admins <- admin_sel()
          bounds <- bounds[bounds$GID_2 %in% admins]
        }

        if (nrow(bounds) == 0) return(NULL)

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
        req(dt_table())
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
