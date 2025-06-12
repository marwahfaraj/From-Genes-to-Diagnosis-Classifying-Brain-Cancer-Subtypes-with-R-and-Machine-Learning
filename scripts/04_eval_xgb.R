# 04_eval_xgb.R

suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(ggplot2)
  library(smotefamily)
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
# Load labels and identify DR files
# ------------------------------------------------------------
full_df   <- read.csv("output/cleaned_data.csv", stringsAsFactors = FALSE)
class_col <- if ("Class" %in% names(full_df)) "Class" else "type"

dr_dir   <- "output/dim_reduction"
dr_files <- list.files(dr_dir,
                       pattern    = "pca_scores\\.csv$|tsne_coordinates\\.csv$",
                       full.names = TRUE)

# ------------------------------------------------------------
# Container for summaries
# ------------------------------------------------------------
results_list <- list()

# ------------------------------------------------------------
# Loop over each DR method
# ------------------------------------------------------------
for (dr_path in dr_files) {
  method <- if (grepl("pca_scores", dr_path)) "pca" else "tsne"
  dr_df  <- read.csv(dr_path, stringsAsFactors = FALSE)
  dr_df$Class <- as.factor(full_df[[class_col]])
  
  # compute class weights
  weights <- max(table(dr_df$Class)) / table(dr_df$Class)
  
  # Weighted XGB
  ctrl_wt <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3
  )
  fit_wt <- train(
    Class ~ .,
    data       = dr_df,
    method     = "xgbTree",
    trControl  = ctrl_wt,
    tuneLength = 3,
    preProcess = c("nzv", "center", "scale"),
    weights    = weights[dr_df$Class]
  )
  acc_wt <- mean(fit_wt$resample$Accuracy)
  sd_wt  <- sd(fit_wt$resample$Accuracy)
  
  # SMOTE XGB
  ctrl_sm <- trainControl(
    method   = "repeatedcv",
    number   = 5,
    repeats  = 3,
    sampling = "smote"
  )
  fit_sm <- train(
    Class ~ .,
    data       = dr_df,
    method     = "xgbTree",
    trControl  = ctrl_sm,
    tuneLength = 3,
    preProcess = c("nzv", "center", "scale")
  )
  acc_sm <- mean(fit_sm$resample$Accuracy)
  sd_sm  <- sd(fit_sm$resample$Accuracy)
  
  # Build summary table
  summary_df <- tibble(
    Method   = method,
    Strategy = c("Weighted", "SMOTE"),
    MeanAcc  = c(acc_wt, acc_sm),
    SDAcc    = c(sd_wt, sd_sm)
  )
  
  # Save metrics
  write.csv(summary_df,
            file      = paste0("output/model/metrics/summary_xgb_", method, ".csv"),
            row.names = FALSE)
  
  # Plot and save
  p <- ggplot(summary_df, aes(x = Strategy, y = MeanAcc, fill = Strategy)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = MeanAcc - SDAcc, ymax = MeanAcc + SDAcc),
                  width = 0.2) +
    scale_fill_manual(values = base_colors) +
    labs(title = paste("XGBoost Accuracy (", toupper(method), ")", sep = ""),
         x     = "Strategy",
         y     = "Mean Accuracy") +
    theme_minimal()
  
  ggsave(filename = paste0("output/model/graphs/xgb_accuracy_", method, ".png"),
         plot     = p,
         dpi      = 300,
         width    = 8,
         height   = 5)
  
  results_list[[method]] <- summary_df
}

# ------------------------------------------------------------
# Compare across DR methods
# ------------------------------------------------------------
combined <- bind_rows(results_list)
write.csv(combined,
          file      = "output/model/metrics/summary_xgb_all_methods.csv",
          row.names = FALSE)

p_all <- ggplot(combined, aes(x = Method, y = MeanAcc, fill = Method)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = MeanAcc - SDAcc, ymax = MeanAcc + SDAcc),
                width = 0.2) +
  scale_fill_manual(values = base_colors) +
  labs(title = "XGBoost Accuracy Comparison Across DR Methods",
       x     = "DR Method",
       y     = "Mean Accuracy") +
  theme_minimal()

ggsave(filename = "output/model/graphs/xgb_accuracy_comparison.png",
       plot     = p_all,
       dpi      = 300,
       width    = 8,
       height   = 5)

cat("XGBoost evaluation complete for PCA and t-SNE.\n",
    "Metrics in output/model/metrics/, graphs in output/model/graphs/.\n")
