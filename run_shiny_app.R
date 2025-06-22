#!/usr/bin/env Rscript

# Install and load required packages
required_packages <- c(
  "shiny", 
  "shinydashboard", 
  "DT", 
  "ggplot2", 
  "dplyr", 
  "readr",
  "caret",
  "e1071"
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", package, "\n")
      install.packages(package, repos = "https://cran.rstudio.com/")
      library(package, character.only = TRUE)
    } else {
      library(package, character.only = TRUE)
    }
  }
}

# Install and load packages
cat("Loading required packages...\n")
install_if_missing(required_packages)

# Set the working directory to the project root
setwd("/Users/marwahfaraj/Desktop/ms_degree_application_and_doc/final_projects/503_final_project/From-Genes-to-Diagnosis-Classifying-Brain-Cancer-Subtypes-with-R-and-Machine-Learning")

cat("Starting Shiny app...\n")
# Run the Shiny app
shiny::runApp("shiny_app", port = 3838, host = "0.0.0.0") 