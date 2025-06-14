# Load libraries
library(shiny)
library(readr)
library(dplyr)

# Load trained model
model_path <- "model/final_model.rds"
final_model <- readRDS(model_path)

# Load test data
test_data <- readr::read_csv("data/test_set.csv")
