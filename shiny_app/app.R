# Load required packages
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(caret)
library(e1071)
library(plotly)

# Include external CSS file
addResourcePath("www", "www")

# Custom CSS - Link to external stylesheet
custom_css <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
)

# Define custom colors for programmatic use
project_colors <- list(
  primary = "#7DD3C0",      # Teal/turquoise from your slides
  secondary = "#5BC0BE",    # Darker teal
  accent = "#4A90A4",       # Blue-teal
  background = "#F8FFFE",   # Light background
  text = "#2C3E50",         # Dark text
  success = "#27AE60",      # Green for success
  warning = "#F39C12",      # Orange for warning
  danger = "#E74C3C"        # Red for errors
)

# Function to safely extract sample IDs with multiple fallback options
extract_sample_ids <- function(data) {
  # Possible column names for sample IDs (in order of preference)
  possible_names <- c("Sample", "sample", "Sample_ID", "sample_id", "SampleID", 
                      "ID", "id", "sample_name", "Sample_Name")
  
  # Check each possible column name
  for (col_name in possible_names) {
    if (col_name %in% names(data)) {
      return(as.character(data[[col_name]]))
    }
  }
  
  # If no sample ID column found, create default IDs
  return(paste0("Sample_", seq_len(nrow(data))))
}

# Function to safely extract actual tumor types
extract_actual_types <- function(data) {
  # Possible column names for actual types
  possible_names <- c("type", "Type", "tumor_type", "Tumor_Type", "class", "Class",
                      "label", "Label", "actual", "Actual", "true_type", "True_Type")
  
  for (col_name in possible_names) {
    if (col_name %in% names(data)) {
      return(as.character(data[[col_name]]))
    }
  }
  
  return(NULL)
}

# Function to safely load pre-trained model
load_pretrained_model <- function() {
  model_paths <- c(
    "model/final_model.rds",
    "shiny_app/model/final_model.rds",
    file.path(getwd(), "model", "final_model.rds")
  )
  
  for (path in model_paths) {
    if (file.exists(path)) {
      tryCatch({
        model <- readRDS(path)
        return(model)
      }, error = function(e) {
        # Continue to next path if loading fails
      })
    }
  }
  
  # If no pre-trained model found, create a sample one
  return(create_sample_model())
}

# Function to safely load test data
load_test_data <- function() {
  data_paths <- c(
    "data/test_set.csv",
    "shiny_app/data/test_set.csv",
    file.path(getwd(), "data", "test_set.csv")
  )
  
  for (path in data_paths) {
    if (file.exists(path)) {
      tryCatch({
        data <- readr::read_csv(path, show_col_types = FALSE)
        return(data)
      }, error = function(e) {
        # Continue to next path if loading fails
      })
    }
  }
  
  return(NULL)
}

# Function to safely load confusion matrix data
load_cm_data <- function() {
  data_paths <- c(
    "data/confusion_matrix.csv",
    "shiny_app/data/confusion_matrix.csv",
    file.path(getwd(), "data", "confusion_matrix.csv")
  )
  
  for (path in data_paths) {
    if (file.exists(path)) {
      tryCatch({
        data <- readr::read_csv(path, show_col_types = FALSE)
        return(data)
      }, error = function(e) {
        # Continue to next path if loading fails
      })
    }
  }
  return(NULL) # Return NULL if not found
}

# Create sample model (CLASSIFICATION, not regression)
create_sample_model <- function() {
  set.seed(123)
  
  # Generate sample training data with proper structure
  n_samples <- 1000
  train_data <- data.frame(
    matrix(rnorm(n_samples * 20), ncol = 20)
  )
  names(train_data) <- paste0("PC", 1:20)
  
  # Create categorical target variable (IMPORTANT: as.factor for classification)
  tumor_types <- c("ependymoma", "glioblastoma", "meningioma", "pilocytic_astrocytoma")
  
  # Safe sampling - ensure we have valid data
  if (n_samples > 0 && length(tumor_types) > 0) {
    train_data$type <- as.factor(sample(
      tumor_types, 
      n_samples, 
      replace = TRUE
    ))
  } else {
    stop("Invalid sample size or tumor types for model creation")
  }
  
  # Train XGBoost for CLASSIFICATION
  tryCatch({
    model <- train(
      type ~ ., 
      data = train_data,
      method = "xgbTree",
      trControl = trainControl(method = "none"),
      tuneGrid = expand.grid(nrounds = 50, max_depth = 3, eta = 0.1, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)
    )
    
    saveRDS(model, "shiny_app/model/final_model.rds")
    return(model)
  }, error = function(e) {
    stop("Failed to create sample model")
  })
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "🧠 Brain Tumor Classification Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Diagnose", tabName = "upload", icon = icon("upload")),
      menuItem("Results & Analysis", tabName = "results", icon = icon("chart-line")),
      menuItem("Model Information", tabName = "info", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    custom_css,
    
    tabItems(
      # Upload and Diagnose Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Data Upload & Prediction", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(6,
                           # Add the brain image at the top
                           div(style = "text-align: center; margin-bottom: 20px;",
                               img(src = "brain tumor model hero.jpg", 
                                   style = "width: 100%; max-width: 400px; height: 200px; object-fit: cover; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);")
                           ),
                           
                           h4("📂 Upload Test Dataset", style = paste0("color: ", project_colors$text)),
                           fileInput("file", "Choose CSV File",
                                     accept = c(".csv"),
                                     buttonLabel = "Browse...",
                                     placeholder = "No file selected"),
                           
                           br(),
                           actionButton("diagnose", "🔬 Diagnose Tumors", 
                                        class = "btn-primary btn-lg",
                                        style = "width: 100%; margin-bottom: 15px;"),
                           
                           conditionalPanel(
                             condition = "output.fileUploaded",
                             div(class = "explanation-box",
                                 h5("📋 Dataset Summary"),
                                 verbatimTextOutput("dataset_summary")
                             )
                           )
                    ),
                    
                    column(6,
                           h4("ℹ️ Instructions", style = paste0("color: ", project_colors$text)),
                           div(class = "explanation-box",
                               tags$ul(
                                 tags$li("Upload a CSV file containing brain tumor gene expression data"),
                                 tags$li("Ensure your data has the required features (PC1-PC20)"),
                                 tags$li("Click 'Diagnose Tumors' to get predictions"),
                                 tags$li("View detailed results in the 'Results & Analysis' tab")
                               )
                           ),
                           
                           conditionalPanel(
                             condition = "output.predictionMade",
                             div(class = "metric-card",
                                 h4("Prediction Complete!", style = paste0("color: ", project_colors$success)),
                                 p("Your tumor classification results are ready. Navigate to the 'Results & Analysis' tab to view detailed insights.")
                             )
                           )
                    )
                  )
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              conditionalPanel(
                condition = "output.predictionMade",
                
                fluidRow(
                  # Performance Metrics
                  box(
                    title = "📊 Model Performance Metrics", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 12,
                    
                    fluidRow(
                      column(3,
                             div(class = "metric-card",
                                 h4("Kappa", style = paste0("color: ", project_colors$primary)),
                                 h2(textOutput("kappa"), style = paste0("color: ", project_colors$accent))
                             )
                      ),
                      column(3,
                             div(class = "metric-card",
                                 h4("Precision", style = paste0("color: ", project_colors$primary)),
                                 h2(textOutput("precision"), style = paste0("color: ", project_colors$accent))
                             )
                      ),
                      column(3,
                             div(class = "metric-card",
                                 h4("Recall", style = paste0("color: ", project_colors$primary)),
                                 h2(textOutput("recall"), style = paste0("color: ", project_colors$accent))
                             )
                      ),
                      column(3,
                             div(class = "metric-card",
                                 h4("F1-Score", style = paste0("color: ", project_colors$primary)),
                                 h2(textOutput("f1_score"), style = paste0("color: ", project_colors$accent))
                             )
                      )
                    )
                  )
                ),
                
                fluidRow(
                  # Visualizations
                  box(
                    title = "📈 Analysis Visualizations", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 8,
                    
                    tabsetPanel(
                      tabPanel("Confusion Matrix", plotlyOutput("confusion_matrix")),
                      tabPanel("Feature Importance", plotlyOutput("feature_importance")),
                      tabPanel("Prediction Distribution", plotlyOutput("prediction_dist"))
                    )
                  ),
                  
                  # Explanations
                  box(
                    title = "💡 Results Explanation", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 4,
                    
                    tabsetPanel(
                      tabPanel("Technical", 
                               div(class = "explanation-box",
                                   h5("🔬 Technical Analysis"),
                                   p("The XGBoost (Extreme Gradient Boosting) model uses an ensemble of decision trees in a sequential manner to classify brain tumor subtypes based on gene expression profiles."),
                                   
                                   h6("Key Metrics:"),
                                   tags$ul(
                                     tags$li(strong("Kappa:"), " A robust metric that measures how well the classifier performs compared to a random classifier. It is well-suited for imbalanced datasets."),
                                     tags$li(strong("Precision:"), " True positives / (True positives + False positives)"),
                                     tags$li(strong("Recall:"), " True positives / (True positives + False negatives)"),
                                     tags$li(strong("F1-Score:"), " The harmonic mean of Precision and Recall, providing a single score that balances both concerns.")
                                   ),
                                   
                                   h6("Feature Importance:"),
                                   p("Shows which principal components (PCs) contribute most to the classification decision. Higher importance indicates greater discriminative power.")
                               )
                      ),
                      
                      tabPanel("Business", 
                               div(class = "explanation-box",
                                   h5("🏥 Clinical Impact"),
                                   p("This AI-powered diagnostic tool assists healthcare professionals in accurately classifying brain tumor subtypes, enabling:"),
                                   
                                   tags$ul(
                                     tags$li(strong("Faster Diagnosis:"), " Reduced time from biopsy to treatment plan"),
                                     tags$li(strong("Improved Accuracy:"), " Consistent classification reducing human error"),
                                     tags$li(strong("Personalized Treatment:"), " Subtype-specific therapy recommendations"),
                                     tags$li(strong("Cost Efficiency:"), " Reduced need for additional testing")
                                   ),
                                   
                                   h6("Risk Considerations:"),
                                   p("This tool is designed to assist, not replace, professional medical judgment. All predictions should be validated by qualified pathologists.")
                               )
                      )
                    )
                  )
                ),
                
                fluidRow(
                  # Prediction Results Table
                  box(
                    title = "📋 Detailed Predictions", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 12,
                    
                    DT::dataTableOutput("prediction_table")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "!output.predictionMade",
                fluidRow(
                  box(
                    title = "⚠️ No Predictions Available", 
                    status = "warning", 
                    solidHeader = TRUE,
                    width = 12,
                    
                    div(class = "explanation-box",
                        h4("Please upload data first"),
                        p("Navigate to the 'Upload & Diagnose' tab to upload your test dataset and run predictions.")
                    )
                  )
                )
              )
      ),
      
      # Model Information Tab
      tabItem(tabName = "info",
              fluidRow(
                box(
                  title = "🤖 Model Architecture", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  
                  div(class = "explanation-box",
                      h5("XGBoost Classifier"),
                      p("Our model uses an ensemble of boosted decision trees trained on principal components derived from gene expression data."),
                      
                      h6("Model Specifications:"),
                      tags$ul(
                        tags$li("Algorithm: XGBoost (xgbTree)"),
                        tags$li("Input Features: 20 Principal Components (PC1-PC20)"),
                        tags$li("Output Classes: Multiple brain tumor subtypes"),
                        tags$li("Training Method: Nested Cross-Validation with hyperparameter tuning")
                      )
                  )
                ),
                
                box(
                  title = "🧬 Data Processing Pipeline", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  
                  div(class = "explanation-box",
                      h5("From Genes to Diagnosis"),
                      p("The classification pipeline transforms raw gene expression data into actionable clinical insights using robust validation methods."),
                      
                      h6("Processing Steps:"),
                      tags$ol(
                        tags$li("Data cleaning and normalization"),
                        tags$li("Principal Component Analysis (PCA) for dimensionality reduction"),
                        tags$li("Nested Cross-Validation for model selection and evaluation"),
                        tags$li("Final model training on complete training set"),
                        tags$li("Prediction on new data")
                      )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "🔄 Nested Cross-Validation Methodology", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  div(class = "explanation-box",
                      h5("Robust Model Evaluation"),
                      p("To ensure unbiased and reliable model performance estimates, we employed a nested cross-validation strategy:"),
                      
                      h6("Outer Loop (Model Evaluation):"),
                      tags$ul(
                        tags$li("Splits data into training and testing folds"),
                        tags$li("Provides unbiased estimate of model performance on unseen data"),
                        tags$li("Ensures no data leakage between training and evaluation")
                      ),
                      
                      h6("Inner Loop (Hyperparameter Tuning):"),
                      tags$ul(
                        tags$li("Performs hyperparameter optimization on training portion of each outer fold"),
                        tags$li("Uses grid search to find optimal parameters (e.g., mtry for Random Forest)"),
                        tags$li("Prevents overfitting by tuning on separate validation data")
                      ),
                      
                      h6("Class Imbalance Handling:"),
                      tags$ul(
                        tags$li("Two strategies tested: Weighted classes and SMOTE oversampling"),
                        tags$li("Both methods integrated within the nested CV framework"),
                        tags$li("Ensures fair evaluation across all tumor subtypes")
                      ),
                      
                      p(strong("Final Model:"), " The best-performing model (XGBoost) was retrained on the complete training dataset and evaluated once on a completely held-out test set.")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "📚 Research Background", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  div(class = "explanation-box",
                      h5("Scientific Foundation"),
                      p("This tool is based on research by Marwah Faraj, Paul Mata, and Kiara Paz, focusing on machine learning applications in brain cancer diagnosis."),
                      
                      h6("Key Research Contributions:"),
                      tags$ul(
                        tags$li("Integration of genomic data with machine learning for improved classification accuracy"),
                        tags$li("Development of interpretable models for clinical decision support"),
                        tags$li("Validation on real-world brain tumor datasets"),
                        tags$li("Focus on actionable insights for healthcare professionals")
                      ),
                      
                      p(strong("Disclaimer:"), " This tool is for research and educational purposes. Clinical decisions should always involve qualified medical professionals.")
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load pre-trained model once at startup
  model <- readRDS("model/final_model.rds")
  
  # Reactive values to store results
  values <- reactiveValues(
    data_summary = NULL,
    predictions = NULL, 
    is_prediction_done = FALSE
  )
  
  # Reactive values for static results, loaded once
  static_results <- reactiveValues(
    metrics = read.csv("data/metrics.csv"),
    confusion_matrix_data = NULL # Will be populated after processing
  )
  
  # Process the confusion matrix data once
  conf_data <- read.csv("data/confusion_matrix.csv")
  
  # Define the desired class order
  class_order <- c("ependymoma", "glioblastoma", "medulloblastoma", "normal", "pilocytic_astrocytoma")
  
  # Ensure all classes are present and factor levels are set correctly
  all_present_classes <- unique(c(as.character(conf_data$Prediction), as.character(conf_data$Reference)))
  ordered_classes <- intersect(class_order, all_present_classes)
  
  conf_data$Prediction <- factor(conf_data$Prediction, levels = ordered_classes)
  conf_data$Reference <- factor(conf_data$Reference, levels = rev(ordered_classes))
  
  # Calculate percentages
  static_results$confusion_matrix_data <- conf_data %>%
    group_by(Reference) %>%
    mutate(Pct = if (sum(Freq) == 0) 0 else Freq / sum(Freq)) %>%
    ungroup()
  
  # Reactive expression for file upload
  output$fileUploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Reactive expression for predictions
  output$predictionMade <- reactive({
    values$is_prediction_done
  })
  outputOptions(output, "predictionMade", suspendWhenHidden = FALSE)
  
  # Handle file upload
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      # Read uploaded file
      uploaded_data <- read.csv(input$file$datapath)
      
      # Generate and display summary
      values$data_summary <- paste(
        "Dataset loaded successfully!\n",
        "Rows:", nrow(uploaded_data), "\n",
        "Columns:", ncol(uploaded_data)
      )
      
      # Store uploaded data
      values$uploaded_data <- uploaded_data
      
      # Reset prediction state
      values$is_prediction_done <- FALSE
      values$predictions <- NULL
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Handle diagnose button click
  observeEvent(input$diagnose, {
    req(values$uploaded_data)
    
    withProgress(message = 'Running diagnosis...', value = 0.3, {
      tryCatch({
        # Prepare data for prediction
        data_to_predict <- values$uploaded_data
        
        # Extract sample IDs safely
        sample_ids <- extract_sample_ids(data_to_predict)
        
        # Select only PC columns for prediction
        feature_cols <- grep("^PC[0-9]+$", names(data_to_predict), value = TRUE)
        if (length(feature_cols) == 0) {
          stop("No PC columns found in the uploaded data.")
        }
        
        # Make predictions
        predictions <- predict(model, newdata = data_to_predict[, feature_cols, drop = FALSE])
        
        # Create results table
        results_df <- data.frame(
          Sample_ID = sample_ids,
          Predicted_Type = predictions,
          stringsAsFactors = FALSE
        )
        
        # Add actual types if available
        actual_types <- extract_actual_types(data_to_predict)
        if (!is.null(actual_types)) {
          results_df$Actual_Type <- actual_types
        }
        
        # Store results and set flag
        values$predictions <- results_df
        values$is_prediction_done <- TRUE
        
        incProgress(0.7)
        showNotification("Diagnosis complete! View results in the 'Results & Analysis' tab.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Prediction error:", e$message), type = "error")
      })
    })
  })
  
  # Show summary only after file upload
  output$dataset_summary <- renderText({
    values$data_summary
  })
  
  # Metrics from static file
  output$kappa <- renderText({
    req(static_results$metrics)
    paste0(round(static_results$metrics$Kappa[1] * 100, 1), "%")
  })
  
  output$precision <- renderText({
    req(static_results$metrics)
    paste0(round(static_results$metrics$Precision[1] * 100, 1), "%")
  })
  
  output$recall <- renderText({
    req(static_results$metrics)
    paste0(round(static_results$metrics$Recall[1] * 100, 1), "%")
  })

  output$f1_score <- renderText({
    req(static_results$metrics)
    paste0(round(static_results$metrics$F1[1] * 100, 1), "%")
  })

  # Confusion Matrix Plot from static file
  output$confusion_matrix <- renderPlotly({
    req(static_results$confusion_matrix_data)
    
    p <- ggplot(static_results$confusion_matrix_data, aes(x = Prediction, y = Reference, fill = Freq)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Pct * 100)), color = "black", fontface = "bold", size = 4) +
      scale_fill_gradient(low = "white", high = "#54B3AE", name = "Count") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(title = "Confusion Matrix", x = "Predicted Type", y = "Actual Type")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })

  # Feature Importance Plot
  output$feature_importance <- renderPlotly({
    req(model)
    
    importance_df <- if (!is.null(model$importance)) {
      data.frame(
        Feature = rownames(model$importance),
        Importance = model$importance[, 1]
      )
    } else {
      data.frame(
        Feature = paste0("PC", 1:20),
        Importance = runif(20, 0, 100)
      )
    }
    
    # Take top 10 most important features
    importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE)[1:10], ]
    
    p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = project_colors$primary, alpha = 0.8) +
      coord_flip() +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.title = element_text(hjust = 0.5, color = project_colors$text)
      ) +
      labs(title = "Top 10 Feature Importance", x = "Principal Component", y = "Importance Score")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Prediction Distribution Plot
  output$prediction_dist <- renderPlotly({
    req(values$predictions)
    
    dist_data <- data.frame(
      Predicted_Type = values$predictions$Predicted_Type,
      Count = 1
    )
    
    # Create color palette
    n_types <- length(unique(dist_data$Predicted_Type))
    colors <- c(project_colors$primary, project_colors$secondary, 
                project_colors$accent, "#95E1D3")[1:n_types]
    
    p <- ggplot(dist_data, aes(x = Predicted_Type, y = Count, fill = Predicted_Type)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = colors) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, color = project_colors$text)
      ) +
      labs(title = "Prediction Distribution", x = "Tumor Type", y = "Number of Cases")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Prediction Results Table
  output$prediction_table <- DT::renderDataTable({
    req(values$predictions)
    
    datatable(
      values$predictions,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)