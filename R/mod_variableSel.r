library(shinyWidgets)


### ----- UI ---- ###
variableUI <- function(id) {
  ns <- NS(id)
  capital_choices <- prepare_choices(
    data.frame(
      names = c("Adaptive Capacity", "Human", "Social", "Physical", "Economic"),
      value = c("ac", "human", "social", "physical", "economic")
    ),
    label = names,
    value = value
  )
  tagList(
    conditionalPanel(
      condition = "input.explore_dims == 0",
      virtualSelectInput(ns("capital"), "Select Capital Index",
        choices = capital_choices,
        selected = "ac"
      ),
      ns = ns
    ),
    conditionalPanel(
      condition = "input.explore_dims == 1",
      virtualSelectInput(ns("dim_attr"), "Select Attribute",
        choices = NULL,
        multiple = FALSE,
        search = TRUE
      ),
      ns = ns
    ),
    checkboxInput(ns("explore_dims"),
      label = "Explore Individual Attributes?",
      FALSE
    )
  )
}

### ----- Server ---- ###

variableServer <- function(id, lookup_table) {
  moduleServer(
    id,
    function(input, output, session) {
      selected <- reactive({
        if (input$explore_dims == 0) {
          var_type <- 'capital'
          selected_variable <- input$capital
        } else {
          var_type <- 'variable'
          selected_variable <- input$dim_attr
        }
        return(list(variable = selected_variable, type = var_type))
      })
      observe({
        lookup_df <- lookup_table()[!is.na(lookup_table()$group), ]
        dim_list <- prepare_choices(lookup_df, label = final_name, value = clean_name, group_by = group)
        updateVirtualSelect(
          session = session,
          inputId = "dim_attr",
          choices = dim_list, selected = lookup_df$clean_name[1]
        )
      })
      return(selected)
    }
  )
}
