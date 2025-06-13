# 04_eval_nn.R

suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(nnet)
  library(smotefamily)
})

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
# Weighted Neural Net
# ------------------------------------------------------------
cat("Running weighted neural net on PCA data...\n")

weights_vec <- max(table(df[[class_col]])) / table(df[[class_col]])
sample_weights <- weights_vec[df[[class_col]]]

ctrl_wt <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

fit_wt <- train(
  form = as.formula(paste(class_col, "~ .")),
  data = df,
  method = "nnet",
  trControl = ctrl_wt,
  tuneLength = 3,
  preProcess = c("nzv", "center", "scale"),
  weights = sample_weights,
  trace = FALSE
)

acc_wt <- mean(fit_wt$resample$Accuracy)
sd_wt  <- sd(fit_wt$resample$Accuracy)

# ------------------------------------------------------------
# SMOTE Neural Net
# ------------------------------------------------------------
cat("Running SMOTE neural net on PCA data...\n")

ctrl_sm <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  sampling = "smote",
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

fit_sm <- train(
  form = as.formula(paste(class_col, "~ .")),
  data = df,
  method = "nnet",
  trControl = ctrl_sm,
  tuneLength = 3,
  preProcess = c("nzv", "center", "scale"),
  trace = FALSE
)

acc_sm <- mean(fit_sm$resample$Accuracy)
sd_sm  <- sd(fit_sm$resample$Accuracy)

# ------------------------------------------------------------
# Save metrics
# ------------------------------------------------------------
summary_df <- tibble(
  Method   = "pca",
  Strategy = c("Weighted", "SMOTE"),
  MeanAcc  = c(acc_wt, acc_sm),
  SDAcc    = c(sd_wt, sd_sm)
)

write.csv(
  summary_df,
  file = "output/model/metrics/summary_nn_pca.csv",
  row.names = FALSE
)

cat("Saved NN performance metrics with PCA using Weighted + SMOTE.\n")
