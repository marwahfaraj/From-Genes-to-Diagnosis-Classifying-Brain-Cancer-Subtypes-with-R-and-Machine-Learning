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
      # Select only Sample and PC columns for a cleaner preview
      if ("Sample" %in% names(test_data)) {
        preview_data <- test_data[, c("Sample", "PC1", "PC2", "PC3", "PC4")]
        head(preview_data, 5)
      } else {
        head(test_data, 5)
      }
    })
    
    prediction_result <- eventReactive(input$run_prediction, {
      req(model, test_data)
      sample_names <- test_data$Sample

      # Remove both Sample and type columns
      cols_to_remove <- c("Sample", "type")
      prediction_data <- test_data[, !(names(test_data) %in% cols_to_remove)]

      print("Columns used for prediction:")
      print(names(prediction_data))

      print("Prediction data columns:")
      print(names(prediction_data))
      print("Model structure:")
      print(str(model))

      # Run the prediction
      pred <- predict(model, newdata = prediction_data)

      # Combine the Sample column with the prediction results
      tibble(Sample = sample_names, Prediction = pred)
    })
    
    output$prediction_output <- renderTable({
      # Ensure a result is available before rendering
      req(prediction_result())
      prediction_result()
    })

    print("Prediction done")
  })
}