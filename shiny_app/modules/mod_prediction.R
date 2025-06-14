mod_prediction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Test Set Predictions"),
    div(class = "table-container", tableOutput(ns("test_preview"))),
    actionButton(ns("run_prediction"), "Run Prediction"),
    div(class = "table-container", tableOutput(ns("prediction_output")))
  )
}


mod_prediction_server <- function(id, model, test_data) {
  moduleServer(id, function(input, output, session) {
    output$test_preview <- renderTable({
      head(test_data, 5)
    })
    
    prediction_result <- eventReactive(input$run_prediction, {
      pred <- predict(model, newdata = test_data)
      tibble(Prediction = pred)
    })
    
    output$prediction_output <- renderTable({
      prediction_result()
    })
  })
}
