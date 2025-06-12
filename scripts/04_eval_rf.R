# 04_eval_rf.R

suppressPackageStartupMessages({
  if (!requireNamespace("themis", quietly = TRUE)) {
    install.packages("themis", repos = "https://cloud.r-project.org")
  }
  library(themis)
  library(caret)
  library(dplyr)
  library(ggplot2)
})

# ------------------------------------------------------------
# Create directories for outputs if they don't exist
# ------------------------------------------------------------
if (!dir.exists("output/model/metrics")) dir.create("output/model/metrics", recursive = TRUE)
if (!dir.exists("output/model/graphs"))  dir.create("output/model/graphs",  recursive = TRUE)

# ------------------------------------------------------------
# Define base colors matching the project theme
# ------------------------------------------------------------
base_colors <- c("#54B3AE", "#2C3A4B", "#A5D5D1", "#e8eadF", "#7FB6B0")

# ------------------------------------------------------------
# Load full data and define label column
# ------------------------------------------------------------
full_df   <- read.csv("output/cleaned_data.csv", stringsAsFactors = FALSE)
class_col <- if ("Class" %in% names(full_df)) "Class" else "type"
full_df[[class_col]] <- as.factor(full_df[[class_col]])

# ------------------------------------------------------------
# Setup methods to evaluate
# ------------------------------------------------------------
methods <- c("pca", "tsne")
results_list <- list()

for (method in methods) {
  
  if (method == "pca") {
    dr_df <- full_df
    preprocess_steps <- c("nzv", "center", "scale", "pca")
  } else if (method == "tsne") {
    tsne_df <- read.csv("output/dim_reduction/tsne_coordinates.csv")
    tsne_df$Class <- full_df[[class_col]][1:nrow(tsne_df)]  # Align rows
    dr_df <- tsne_df
    preprocess_steps <- NULL  # No preprocessing for t-SNE
  }
  
  dr_df$Class <- as.factor(dr_df$Class)
  
  if (any(is.na(dr_df$Class))) {
    warning(paste("Skipping", method, "- Class column has NA values"))
    next
  }
  
  weights <- max(table(dr_df$Class)) / table(dr_df$Class)
  
  ctrl_wt <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    classProbs = TRUE,
    summaryFunction = defaultSummary
  )
  
  fit_wt <- train(
    Class ~ .,
    data = dr_df,
    method = "rf",
    trControl = ctrl_wt,
    tuneLength = 3,
    preProcess = preprocess_steps,
    weights = weights[dr_df$Class]
  )
  
  acc_wt <- mean(fit_wt$resample$Accuracy)
  sd_wt  <- sd(fit_wt$resample$Accuracy)
  
  if (acc_wt >= 0.999) {
    warning(paste("Unrealistic high accuracy in weighted RF for", method, "- possible data leakage"))
  }
  
  ctrl_sm <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    sampling = "smote",
    classProbs = TRUE,
    summaryFunction = defaultSummary
  )
  
  fit_sm <- train(
    Class ~ .,
    data = dr_df,
    method = "rf",
    trControl = ctrl_sm,
    tuneLength = 3,
    preProcess = preprocess_steps
  )
  
  acc_sm <- mean(fit_sm$resample$Accuracy)
  sd_sm  <- sd(fit_sm$resample$Accuracy)
  
  summary_df <- tibble(
    Method   = method,
    Strategy = c("Weighted", "SMOTE"),
    MeanAcc  = c(acc_wt, acc_sm),
    SDAcc    = c(sd_wt, sd_sm)
  )
  
  write.csv(summary_df,
            file = paste0("output/model/metrics/summary_rf_", method, ".csv"),
            row.names = FALSE)
  
  p <- ggplot(summary_df, aes(x = Strategy, y = MeanAcc, fill = Strategy)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = MeanAcc - SDAcc, ymax = MeanAcc + SDAcc),
                  width = 0.2) +
    scale_fill_manual(values = base_colors) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(
      title = paste("RF Accuracy (", toupper(method), ")", sep = ""),
      x = "Strategy", y = "Mean Accuracy"
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("output/model/graphs/rf_accuracy_", method, ".png"),
    plot = p, dpi = 300, width = 8, height = 5
  )
  
  results_list[[method]] <- summary_df
}
# ------------------------------------------------------------
# Combine SMOTE results only for fair comparison
# ------------------------------------------------------------
combined <- bind_rows(results_list) %>% filter(Strategy == "SMOTE")

write.csv(
  combined,
  file = "output/model/metrics/summary_rf_all_methods.csv",
  row.names = FALSE
)

p_all <- ggplot(combined, aes(x = Method, y = MeanAcc, fill = Method)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = MeanAcc - SDAcc, ymax = MeanAcc + SDAcc),
                width = 0.2) +
  scale_fill_manual(values = base_colors) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "RF Accuracy Comparison Across DR Methods (SMOTE)",
    x = "DR Method", y = "Mean Accuracy"
  ) +
  theme_minimal()

ggsave(
  filename = "output/model/graphs/rf_accuracy_comparison.png",
  plot     = p_all,
  dpi      = 300,
  width    = 8,
  height   = 5
)

cat("RF evaluation complete using PCA (in-train) and t-SNE.\n")
