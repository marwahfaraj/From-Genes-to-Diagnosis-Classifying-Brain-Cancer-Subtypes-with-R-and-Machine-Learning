# Load libraries
library(shiny)
library(readr)
library(dplyr)
library(randomForest)

# Debug: Print working directory info
message("Global.R - Current working directory: ", getwd())
message("Global.R - Available files: ", paste(list.files(), collapse = ", "))
message("Global.R - Available directories: ", paste(list.dirs(recursive = FALSE), collapse = ", "))

# Set working directory to project root if we're in shiny_app folder
if(basename(getwd()) == "shiny_app") {
  setwd("..")
  message("Global.R - Changed working directory to: ", getwd())
}

# Function to safely load model
load_model_safely <- function() {
  model_paths <- c(
    "model/final_model.rds",
    "shiny_app/model/final_model.rds",
    file.path(getwd(), "model", "final_model.rds"),
    file.path(dirname(getwd()), "model", "final_model.rds")
  )
  
  message("Global.R - Attempting to load model from paths:")
  for (path in model_paths) {
    message("  Checking: ", path)
    if (file.exists(path)) {
      tryCatch({
        model <- readRDS(path)
        message("Successfully loaded model from: ", path)
        
        # Validate model
        if (is.null(model)) {
          message("Warning: Model is NULL")
          next
        }
        
        if (!inherits(model, "randomForest")) {
          message("Warning: Model is not a randomForest object")
        }
        
        message("Model validation passed")
        return(model)
      }, error = function(e) {
        message("Failed to load model from ", path, ": ", e$message)
      })
    } else {
      message("  File does not exist: ", path)
    }
  }
  
  message("No pre-trained model found. Will create sample model when needed.")
  return(NULL)
}

# Function to safely load test data
load_test_data_safely <- function() {
  data_paths <- c(
    "data/test_set.csv",
    "shiny_app/data/test_set.csv",
    file.path(getwd(), "data", "test_set.csv"),
    file.path(dirname(getwd()), "data", "test_set.csv")
  )
  
  message("Global.R - Attempting to load test data from paths:")
  for (path in data_paths) {
    message("  Checking: ", path)
    if (file.exists(path)) {
      tryCatch({
        data <- readr::read_csv(path, show_col_types = FALSE)
        message("Successfully loaded test data from: ", path)
        
        # Validate data (This block is now completed)
        if (is.null(data) || nrow(data) == 0) {
          message("Warning: Loaded test data is NULL or empty.")
          next
        }
        return(data)
        
      }, error = function(e) {
        message("Failed to load data from ", path, ": ", e$message)
      })
    } else {
      message("  File does not exist: ", path)
    }
  }
  message("No test data found.")
  return(NULL)
}