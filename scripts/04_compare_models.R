# 04_compare_models.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(janitor) # For cleaning names
})

# ----------------------------------------------------------------
# Setup
# ----------------------------------------------------------------
# Directories for outputs
output_metrics_dir <- "output/model/metrics"
output_graphs_dir <- "output/model/graphs"
dir.create(output_graphs_dir, recursive = TRUE, showWarnings = FALSE)

# Colors for plotting
strategy_colors <- c("SMOTE" = "#2C3A4B", "Weighted" = "#54B3AE")

# ----------------------------------------------------------------
# Load and Combine Model Performance Data
# ----------------------------------------------------------------
# Find all PCA model summary files
model_files <- list.files(
  path = output_metrics_dir,
  pattern = "summary_.*_pca\\.csv",
  full.names = TRUE
)

# Read, clean, and combine the files into a single data frame
combined_data <- model_files %>%
  set_names() %>%
  map_dfr(~read_csv(.x, col_types = cols()) %>% clean_names(), .id = "source_file") %>%
  mutate(
    Model = toupper(str_extract(basename(source_file), "(?<=summary_).*(?=_pca)"))
  ) %>%
  # Select and standardize only the columns we need for the plot
  select(
    Model,
    Strategy = strategy,
    Accuracy = accuracy_accuracy,
    Kappa = kappa_kappa,
    F1 = f1,
    Recall = recall,
    Precision = precision
  )

# Save the newly combined and cleaned summary file
write_csv(combined_data, file.path(output_metrics_dir, "summary_all_models_grouped.csv"))

# ----------------------------------------------------------------
# Create and Save the Faceted Performance Plot
# ----------------------------------------------------------------
# Reshape data for faceted plotting with ggplot
plot_data <- combined_data %>%
  pivot_longer(
    cols = c(Accuracy, Kappa, F1, Recall, Precision),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    # Create clean, consistent metric names for plot labels
    Metric = case_when(
      Metric == "F1" ~ "F1 Score",
      TRUE ~ Metric
    )
  )

# Define the order of facets for the plot
plot_data$Metric <- factor(plot_data$Metric, levels = c("Accuracy", "Kappa", "F1 Score", "Precision", "Recall"))

# Create the faceted plot
p_faceted <- ggplot(plot_data, aes(x = Model, y = Value, fill = Strategy)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(Value, 2)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5,
    color = "black"
  ) +
  facet_wrap(~Metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = strategy_colors) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Model Performance Comparison by Metric and Strategy",
    subtitle = "Metrics derived from cross-validation on the training set using PCA features",
    x = "Model",
    y = "Mean Value",
    fill = "Balancing Strategy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15)),
    strip.background = element_rect(fill = "#4A90A4", color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "top",
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the final plot
ggsave(
  filename = file.path(output_graphs_dir, "models_comparison.png"),
  plot = p_faceted,
  width = 14,
  height = 8,
  dpi = 300
)

cat("✅ Successfully generated and saved the model comparison plot.\n")
cat("You can find it at: output/model/graphs/models_comparison.png\n")
