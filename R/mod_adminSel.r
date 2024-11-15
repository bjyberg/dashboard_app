library(shinyWidgets)

### ----- UI ---- ###
adminUI <- function(id) {
  ns <- NS(id)
  virtualSelectInput(ns("admin"), "Admin Region",
    choices = NULL,
    multiple = TRUE,
    search = TRUE
  )
}

### ----- Server ---- ###

adminServer <- function(id, country_bounds) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        admin_df <- country_bounds()
        admin_list <- prepare_choices(admin_df, label = NAME_2, value = GID_2, group_by = NAME_1)
        updateVirtualSelect(
          session = session, inputId = 'admin', 
          choices = admin_list, selected = admin_df$GID_2
        )
      })
      return(reactive(input$admin))
    }
  )
}
