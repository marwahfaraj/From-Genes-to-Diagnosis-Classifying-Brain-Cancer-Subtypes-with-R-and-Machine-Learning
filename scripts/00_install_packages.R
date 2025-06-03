# 00_install_packages.R
# Install all required packages for the project

required_packages <- c(
  "tidyverse",    # Core data wrangling and visualization
  "data.table",   # Fast data manipulation
  "janitor",      # Cleaning data
  "caret",        # ML pipeline
  "recipes",      # Data preprocessing
  "e1071",        # SVM, Naive Bayes
  "glmnet",       # LASSO, Ridge
  "Rtsne",        # t-SNE
  "FactoMineR",   # PCA
  "factoextra",   # PCA visualization
  "randomForest", # Random forest
  "xgboost",      # Gradient boosting
  "nnet",         # Neural nets (optional)
  "pROC",         # ROC and AUC
  "MLmetrics",    # Performance metrics
  "yardstick",    # Tidy metrics
  "ggpubr",       # Publication plots
  "cowplot",      # Combine plots
  "gridExtra",    # Plot layout
  "rmarkdown",    # Reports
  "knitr"         # Rendering reports
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install)) {
  install.packages(to_install, dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}
