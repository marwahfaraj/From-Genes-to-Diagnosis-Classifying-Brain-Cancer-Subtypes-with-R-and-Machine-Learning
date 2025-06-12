# 02_exploration_visualization.R

# --------------------------------------------------------------------------------
# This script performs extended EDA on the cleaned gene‐expression dataset:
#   1) Sample‐Clustering Heatmap (with Class annotation on rows)
#   2) Gene‐Correlation Heatmap (top 50 most variable genes)
#   3) Class‐Distribution Bar Plot
#   4) Total Expression per Sample (Jitter Plot)
#   5) Violin Plots for Top 10 Most Variable Genes
# --------------------------------------------------------------------------------

# 0) Load required packages
library(ggplot2)
library(reshape2)
library(pheatmap)

# 1) Load the cleaned data
cat("Loading cleaned data...\n")
flush.console()
data <- read.csv("output/cleaned_data.csv", stringsAsFactors = FALSE)

# 2) Create output directory if it doesn't exist
if (!dir.exists("output/graphs")) {
  dir.create("output/graphs", recursive = TRUE)
}

# 3) Detect which column holds the class/label
class_col <- if ("Class" %in% names(data)) "Class" else "type"

# 4) Identify the top 50 most variable numeric features
all_numeric   <- data[, sapply(data, is.numeric)]
variances     <- apply(all_numeric, 2, var, na.rm = TRUE)
top_50_genes  <- names(sort(variances, decreasing = TRUE))[1:50]
numeric_data  <- all_numeric[, top_50_genes]

# 5) Define color palette for class
class_levels   <- sort(unique(data[[class_col]]))
base_colors    <- c("#54B3AE", "#2C3A4B", "#A5D5D1", "#fbfcee", "#7FB6B0")
tableau_colors <- setNames(base_colors[seq_along(class_levels)], class_levels)

# --------------------------------------------------------------------------------
# (1) Sample‐Clustering Heatmap
# --------------------------------------------------------------------------------
cat(" Running sample‐clustering heatmap...\n")
flush.console()

rownames(numeric_data) <- paste0("Sample_", seq_len(nrow(numeric_data)))
sample_cor <- cor(t(numeric_data), use = "pairwise.complete.obs")

annotation_df <- data.frame(Class = factor(data[[class_col]], levels = class_levels))
rownames(annotation_df) <- rownames(numeric_data)
annotation_colors <- list(Class = tableau_colors)

pheatmap(
  mat               = sample_cor,
  annotation_row    = annotation_df,
  annotation_colors = annotation_colors,
  show_rownames     = FALSE,
  show_colnames     = FALSE,
  main              = "Sample Clustering Heatmap (Top 50 Most Variable Genes)",
  color             = colorRampPalette(c("#9fa9a5", "#A5D5D1", "#2C3A4B"))(50),
  filename          = "output/graphs/sample_clustering_heatmap.png",
  width             = 8,
  height            = 6
)


# --------------------------------------------------------------------------------
# (2) Gene‐Correlation Heatmap
# --------------------------------------------------------------------------------
cat(" Running gene‐correlation heatmap on top 50 genes...\n")
flush.console()

gene_cor <- cor(numeric_data, use = "pairwise.complete.obs")

pheatmap(
  mat           = gene_cor,
  show_rownames = FALSE,
  show_colnames = FALSE,
  color         = colorRampPalette(c("#9fa9a5", "#A5D5D1", "#2C3A4B"))(50),
  filename      = "output/graphs/gene_correlation_heatmap.png",
  width         = 8,
  height        = 6
)

# --------------------------------------------------------------------------------
# (3) Class Distribution Bar Plot
# --------------------------------------------------------------------------------
cat(" Plotting class distribution...\n")
flush.console()

p2 <- ggplot(data, aes_string(x = class_col, fill = class_col)) +
  geom_bar() +
  scale_fill_manual(values = tableau_colors) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background  = element_rect(fill = "gray95", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    axis.text.x      = element_text(angle = 35, hjust = 1)
  ) +
  labs(
    title = "Class Distribution",
    x     = "Class",
    y     = "Count"
  )

ggsave(
  filename = "output/graphs/class_distribution.png",
  plot     = p2,
  dpi      = 300,
  width    = 6,
  height   = 4
)

# --------------------------------------------------------------------------------
# (4) Total Expression per Sample (Jitter Plot)
# --------------------------------------------------------------------------------
cat(" Plotting total expression per sample...\n")
flush.console()

sample_sums <- rowSums(all_numeric, na.rm = TRUE)
summary_df  <- data.frame(
  TotalExpression = sample_sums,
  Class           = data[[class_col]]
)

p_sum <- ggplot(summary_df, aes(x = Class, y = TotalExpression, color = Class)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_color_manual(values = tableau_colors) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background  = element_rect(fill = "gray95", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    axis.text.x      = element_text(angle = 35, hjust = 1)
  ) +
  labs(
    title = "Total Expression per Sample by Class",
    x     = "Class",
    y     = "Sum of Expression"
  )

ggsave(
  filename = "output/graphs/sample_total_expression.png",
  plot     = p_sum,
  dpi      = 300,
  width    = 8,
  height   = 5
)

# --------------------------------------------------------------------------------
# (5) Violin Plots for Top 10 Most Variable Genes
# --------------------------------------------------------------------------------
cat(" Plotting top 10 most variable genes (violin)...\n")
flush.console()

top_10_genes  <- names(sort(variances, decreasing = TRUE)[1:10])
top_df        <- data[, c(class_col, top_10_genes)]
top_df_melted <- melt(top_df, id.vars = class_col)

p_violin <- ggplot(
  top_df_melted,
  aes_string(x = class_col, y = "value", fill = class_col)
) +
  geom_violin(trim = TRUE, alpha = 0.6) +
  facet_wrap(~ variable, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = tableau_colors) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    strip.background = element_rect(fill = "gray90", color = NA),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  ) +
  labs(
    title = "Violin Plots of Top 10 Most Variable Genes",
    x     = NULL,
    y     = "Expression"
  )

ggsave(
  filename = "output/graphs/top10_variable_genes_violin.png",
  plot     = p_violin,
  dpi      = 300,
  width    = 10,
  height   = 6
)

# --------------------------------------------------------------------------------
# (End) Final status message
# --------------------------------------------------------------------------------
cat("All EDA plots saved to output/graphs directory.\n")
flush.console()
