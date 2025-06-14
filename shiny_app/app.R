source("global.R")
source("modules/mod_prediction.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    style = "background-color: rgba(0, 0, 0, 0.7); padding: 40px; border-radius: 12px; margin: 60px auto; max-width: 900px;",
    h2("🧠 Brain Tumor Classification Tool"),
    mod_prediction_ui("predict_module")
  )
)

server <- function(input, output, session) {
  mod_prediction_server("predict_module", model = final_model, test_data = test_data)
}

shinyApp(ui = ui, server = server)
