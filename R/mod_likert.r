### ----- Datasets ---- ###

lik_questions <- read.csv("data/likert_questions.csv")

 html_likert_intro <- HTML("
 The below questions have been crafted to support practitioners in identifying the factors that are likely to influence the successful implementation of their project activities/investments. 
 The answers to these questions are used to adjust the weighting of indicators creating an adaptive capacity map that is tailored to their particular activities/investments. 
<b>As a practitioner, please answer the following questions based on your proposed activities/investments, to develop your own custom adaptive capacity map</b>
 ")

### ----- Helper Functions ---- ###
get_likert_value <- function(q_ids, weight_df) {
    q_id_list <- strsplit(q_ids, ",")
    value <- mean(weight_df[weight_df$q_id %in% q_id_list[[1]], "value"], na.rm = T)
    return(value)
}

# The second is to calculate the actual weights
aggregate_weight <- function(name, type = c("capital", "variable"), weight_lookup, weight_df, fun = "mean") {
    if (type == "capital") {
      type <- "group" # lazy fix bc time constraints > fix by refactor col name across code
    } else {
      type <- "standardized_name" 
    }
    q_ids <- weight_lookup[weight_lookup[[type]] == name, "question_id"]
    values <- unlist(lapply(q_ids, get_likert_value, weight_df))
  agg_function <- switch(fun,
    "mean" = \(x) mean(x, na.rm = T),
    "mean_drop0" = \(x) {
      x[which(x == 0)] <- NA
      mean(x, na.rm = T)
    },
    "geo_mean" = \(x) exp(mean(log(x)))
  )
  return(round(agg_function(values), 3))
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

likertServer <- function(id, country_lookup) {
  moduleServer(
    id,
    function(input, output, session) {
      lik_sels <- reactiveValues(data = lik_questions[c("q_id", "value")])
      ns <- session$ns
      observeEvent(input$weight_qs, {
        lik_names <- c("Not at all", "A little bit", "Somewhat", "Quite a bit", "Extremely")
        lik_vals <- c(1, 2, 3, 4, 5)
        showModal(
          modalDialog(
            title = html_likert_intro,
            easyClose = TRUE,
            # size = "l",
            lapply(1:nrow(lik_questions), function(i) {
              radioGroupButtons(
                inputId = ns(paste0("lik_Q", i)),
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
        debounce(
          observeEvent(input[[paste0("lik_Q", i)]], {
            # return(as.numeric((input[[paste0("lik_Q", i)]])))
            lik_sels$data[lik_sels$data$q_id == paste0("Q", i), "value"] <- as.numeric(input[[paste0("lik_Q", i)]])
          }), 400)
        })
        )

      observeEvent(input[["reset_weights"]], {
        lik_sels$data$value <- 3
      })

      w_lookup <- reactive(country_lookup()[!is.na(country_lookup()$question_id), ])

      weights <- reactive({
        # q_weights <- aggregate(value ~ w_id, data = lik_sels$data, mean)
        capitals <- sapply(unique(w_lookup()$group), aggregate_weight, "capital", w_lookup(), lik_sels$data)
        variables <- sapply(unique(w_lookup()$standardized_name), aggregate_weight, "variable", w_lookup(), lik_sels$data)
        list(capital = capitals, variable = variables)
      })

      weights_debounced <- debounce(weights, 1000)

      output$initial_weights <- reactive({
        return(all(lik_sels$data$value == 3))
      })
      outputOptions(output, "initial_weights", suspendWhenHidden = FALSE)
      
      return(weights_debounced)
      # return(lik_sels)
    }
  )
}
