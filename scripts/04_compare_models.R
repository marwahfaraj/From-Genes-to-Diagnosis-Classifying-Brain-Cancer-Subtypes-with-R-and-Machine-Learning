# 04_compare_models.R

suppressPackageStartupMessages({
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
# Files containing the "all methods" summaries for each model
# ------------------------------------------------------------
model_files <- c(
  RF  = "output/model/metrics/summary_rf_all_methods.csv",
  XGB = "output/model/metrics/summary_xgb_all_methods.csv",
  SVM = "output/model/metrics/summary_svm_all_methods.csv",
  NB  = "output/model/metrics/summary_nb_all_methods.csv",
  NN  = "output/model/metrics/summary_nn_all_methods.csv"
)

# ------------------------------------------------------------
# Read each file, pick best MeanAcc across strategies & DR methods, combine
# ------------------------------------------------------------
comparison_list <- lapply(names(model_files), function(mod) {
  df <- read.csv(model_files[[mod]])
  best_row <- df[which.max(df$MeanAcc), ]
  best_row$Model <- mod
  best_row
})
comparison_df <- bind_rows(comparison_list)

# Save combined metrics
write.csv(comparison_df,
          file      = "output/model/metrics/summary_models_comparison.csv",
          row.names = FALSE)

# ------------------------------------------------------------
# Plot comparison bar chart
# ------------------------------------------------------------
p <- ggplot(comparison_df, aes(x = Model, y = MeanAcc, fill = Model)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = MeanAcc - SDAcc, ymax = MeanAcc + SDAcc),
                width = 0.2) +
  scale_fill_manual(values = base_colors) +
  labs(title = "Best Mean Accuracy by Model",
       x     = "Model",
       y     = "Mean Accuracy") +
  theme_minimal()

# Save plot
ggsave(filename = "output/model/graphs/models_comparison.png",
       plot     = p,
       dpi      = 300,
       width    = 8,
       height   = 5)

cat("Model comparison plot saved to output/model/graphs/models_comparison.png\n")
