### ----- Datasets ---- ###

lik_questions <- read.csv("data/likert_questions.csv")
lik_assignment <- read.csv("data/likert_assignment.csv")

### ----- Helper Functions ---- ###
aggregate_weight <- function(name, type = c("capital", "variable"), weight_df, fun = "mean") {
  w_ids <- unique(lik_assignment[lik_assignment[type] == name, "w_id"])
  values <- subset(weight_df, w_id %in% w_ids)$value
  agg_function <- switch(fun,
    "mean" = \(x) mean(x, na.rm = T),
    "mean_drop0" = \(x) {
      x[which(x == 0)] <- NA
      mean(x, na.rm = T)
    },
    "geo_mean" = \(x) exp(mean(log(x)))
  )
  return(agg_function(values))
}

### ----- UI ---- ###

likertUI <- function(id) {
  ns <- NS(id)
  div(
    class = "input-group",
    tags$span(
      style = "vertical-align: bottom;",
      actionButton(ns("weight_qs"), "Custom Weights"
      )
    ),
    conditionalPanel(
      condition = "output.initial_weights == false",
      actionButton(ns("reset_weights"), "Reset Weights"),
      ns = ns
    )
  )
}

### ----- Server ---- ###

likertServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      lik_sels <- reactiveValues(data = lik_questions[c("w_id", "q_id", "value")])
      ns <- session$ns
      observeEvent(input$weight_qs, {
        lik_names <- c("Not at all", "A little bit", "Somewhat", "Quite a bit", "Extremely")
        lik_vals <- c(1, 2, 3, 4, 5)
        showModal(
          modalDialog(
            title = "Define Your Focus",
            easyClose = TRUE,
            # size = "l",
            lapply(1:nrow(lik_questions), function(i) {
              radioGroupButtons(
                inputId = ns(paste0("lik_Q", i)),
                # label = paste0("Q", i),
                label = lik_questions[lik_questions$q_id == paste0("Q", i), "question"],
                choiceValues = lik_vals,
                selected = lik_sels$data[lik_sels$data$q_id == paste0("Q", i), "value"],
                choiceNames = lik_names
              )
            })
          )
        )
      })
      observe(
        lapply(1:nrow(lik_questions), function(i) { ## Dynamically generate the likert listeners and update the selections
          observeEvent(input[[paste0("lik_Q", i)]], {
            # return(as.numeric((input[[paste0("lik_Q", i)]])))
            lik_sels$data[lik_sels$data$q_id == paste0("Q", i), "value"] <- as.numeric(input[[paste0("lik_Q", i)]])
          })
        })
        )

      observeEvent(input[["reset_weights"]], {
        lik_sels$data$value <- 3
      })

      weights <- reactive({
        q_weights <- aggregate(value ~ w_id, data = lik_sels$data, mean)
        capitals <- sapply(unique(lik_assignment$capital), aggregate_weight, "capital", q_weights)
        variables <- sapply(unique(lik_assignment$variable), aggregate_weight, "variable", q_weights)
        list(capital = capitals, variable = variables)
      })

      weights_debounced <- debounce(weights, 1000)

      output$initial_weights <- reactive({
        return(all(lik_sels$data$value == 3))
      })
      outputOptions(output, "initial_weights", suspendWhenHidden = FALSE)
      
      return(weights_debounced)
    }
  )
}
