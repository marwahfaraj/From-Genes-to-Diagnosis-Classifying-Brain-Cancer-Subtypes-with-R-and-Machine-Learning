# 04_eval_svm.R

suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(smotefamily)
  library(e1071)
  library(doParallel)
  library(themis)
  library(recipes)
})

# Source the custom summary function
source("scripts/utils.R")

# ------------------------------------------------------------
# Create output directory
# ------------------------------------------------------------
if (!dir.exists("output/model/metrics")) {
  dir.create("output/model/metrics", recursive = TRUE)
}

# ------------------------------------------------------------
# Load PCA-transformed data
# ------------------------------------------------------------
df <- read.csv("output/dim_reduction/pca_scores.csv", stringsAsFactors = FALSE)
class_col <- if ("Class" %in% names(df)) "Class" else "type"
df[[class_col]] <- as.factor(df[[class_col]])

# ------------------------------------------------------------
# Weighted SVM (Radial)
# ------------------------------------------------------------
cat("Running weighted SVM on PCA data...\n")

weights_vec <- max(table(df[[class_col]])) / table(df[[class_col]])
sample_weights <- weights_vec[df[[class_col]]]

ctrl_wt <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = custom_summary,
  allowParallel = TRUE
)

x_train <- df[, -which(names(df) == class_col)]
y_train <- df[[class_col]]

fit_wt <- train(
  x = x_train,
  y = y_train,
  method = "svmRadial",
  trControl = ctrl_wt,
  tuneLength = 3,
  weights = sample_weights,
  metric = "Kappa"
)

# ------------------------------------------------------------
# SMOTE SVM (Radial)
# ------------------------------------------------------------
cat("Running SMOTE SVM on PCA data...\n")

ctrl_sm <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = custom_summary,
  sampling = "smote",
  allowParallel = TRUE
)

fit_sm <- train(
  x = x_train,
  y = y_train,
  method = "svmRadial",
  trControl = ctrl_sm,
  tuneLength = 3,
  metric = "Kappa"
)

# ------------------------------------------------------------
# Collect and summarize results
# -------------------------------------------------------------------------
# Save metrics
# ------------------------------------------------------------
# Get the full row of results for the best tune of each model, selected by Kappa
results_wt <- fit_wt$results[which.max(fit_wt$results$Kappa.Kappa), ]
results_sm <- fit_sm$results[which.max(fit_sm$results$Kappa.Kappa), ]

# Combine the results and add a 'Strategy' column
summary_df <- bind_rows(
  mutate(results_wt, Strategy = "Weighted"),
  mutate(results_sm, Strategy = "SMOTE")
)

# Save the summary
write.csv(summary_df, "output/model/metrics/summary_svm_pca.csv", row.names = FALSE)

cat("Saved SVM performance metrics with PCA using Weighted + SMOTE.\n")
