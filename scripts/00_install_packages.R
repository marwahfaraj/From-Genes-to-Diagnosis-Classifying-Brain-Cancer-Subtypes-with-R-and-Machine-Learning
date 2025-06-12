# 00_install_packages.R
# Install all required packages for the project

required_packages <- c(
  # Core data wrangling and visualization
  "tidyverse",    # includes ggplot2, dplyr, tidyr, readr, purrr
  "data.table",   # Fast data manipulation
  "janitor",      # Cleaning data
  
  # Machine learning pipeline & preprocessing
  "caret",        # ML workflow and nested CV
  "recipes",      # Data preprocessing recipes
  "smotefamily",  # SMOTE sampling
  "dplyr",        # Data wrangling
  
  # Dimensionality reduction
  "glmnet",       # LASSO, Ridge Regression
  "Rtsne",        # t-SNE
  "FactoMineR",   # PCA
  "factoextra",   # PCA visualization
  
  # Models
  "randomForest", # Random forest
  "xgboost",      # Gradient boosting
  "svmRadial",    # SVM (caret interface via e1071)
  "e1071",        # SVM, Naive Bayes
  "nnet",         # Neural networks
  "klaR",         # Naive Bayes (caret interface)
  
  # Evaluation
  "pROC",         # ROC curves and AUC
  "MLmetrics",    # Accuracy, precision, recall, F1-score
  "yardstick",    # Tidy metrics
  
  # Visualization and plots
  "ggpubr",       # Publication-ready plots
  "cowplot",      # Combine ggplots
  "gridExtra",    # Arrange multiple plots
  
  # Reporting
  "rmarkdown",    # Dynamic documents
  "knitr",        # Report rendering
  "DALEX",
  
  # Heatmaps and clustering
  "pheatmap"      # Heatmap visualizations
)

  # Smote
  "themis"


installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}
