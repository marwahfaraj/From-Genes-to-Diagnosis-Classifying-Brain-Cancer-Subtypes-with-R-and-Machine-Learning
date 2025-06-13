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
strategy_colors <- c("SMOTE" = "#2C3A4B", "Weighted" = "#54B3AE")

# ------------------------------------------------------------
# Files containing the model performance summaries (only PCA)
# ------------------------------------------------------------
model_files <- list(
  RF  = "output/model/metrics/summary_rf_pca.csv",
  XGB = "output/model/metrics/summary_xgb_pca.csv",
  SVM = "output/model/metrics/summary_svm_pca.csv",
  NB  = "output/model/metrics/summary_nb_pca.csv",
  NN  = "output/model/metrics/summary_nn_pca.csv"
)

# ------------------------------------------------------------
# Read and prepare data
# ------------------------------------------------------------
combined_df <- bind_rows(
  lapply(names(model_files), function(model) {
    df <- read.csv(model_files[[model]])
    df$Model <- model
    df
  })
)

write.csv(combined_df,
          file = "output/model/metrics/summary_all_models_grouped.csv",
          row.names = FALSE)

# ------------------------------------------------------------
# Plot grouped comparison with percent labels above bars
# ------------------------------------------------------------
p_grouped <- ggplot(combined_df, aes(x = Model, y = MeanAcc, fill = Strategy)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(MeanAcc, accuracy = 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 3.5) +
  scale_fill_manual(values = strategy_colors) +
  labs(
    title = "Model Accuracy by Strategy (PCA)",
    x = "Model",
    y = "Mean Accuracy",
    fill = "Strategy"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.1)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/model/graphs/comparison_grouped_by_model.png",
  plot = p_grouped,
  dpi = 300,
  width = 9,
  height = 6
)

cat("Updated grouped model comparison plot saved.\n")
