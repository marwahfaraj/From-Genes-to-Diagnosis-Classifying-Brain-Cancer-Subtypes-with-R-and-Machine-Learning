# 03_dimensionality_reduction.R

library(ggplot2)
library(FactoMineR)     # For PCA
library(factoextra)     # For scree plots
library(Rtsne)          # For t-SNE
library(dplyr)

# Load cleaned data
data <- read.csv("output/cleaned_data.csv", stringsAsFactors = FALSE)

# Output dirs
dir.create("output/dim_reduction", recursive = TRUE, showWarnings = FALSE)
dir.create("output/dim_reduction/plots", showWarnings = FALSE)

# Class column
class_col <- if ("Class" %in% names(data)) "Class" else "type"
class_vector <- data[[class_col]]
numeric_vars <- data[, sapply(data, is.numeric)]

# Define color palette
base_colors <- c("#54B3AE", "#2C3A4B", "#A5D5D1", "#e8eadF", "#7FB6B0")
custom_colors <- setNames(base_colors[1:length(unique(class_vector))], sort(unique(class_vector)))

# ---------------------------------------
# 1. PCA
# ---------------------------------------

cat("Performing PCA with variance threshold...\n")

# Run PCA with up to 20 components
pca_model <- PCA(numeric_vars, scale.unit = TRUE, ncp = 20, graph = FALSE)

# Prepare scree data
eig_df <- as.data.frame(pca_model$eig)
eig_df$PC <- factor(1:nrow(eig_df))
eig_df$label <- paste0(round(eig_df$`percentage of variance`, 1), "%")

# Apply repeating base color palette
bar_colors <- rep(base_colors, length.out = nrow(eig_df))

# Plot scree with styling
scree_plot <- ggplot(eig_df[1:20, ], aes(x = PC, y = `percentage of variance`)) +
  geom_col(fill = bar_colors[1:20]) +
  geom_line(aes(group = 1), color = "black") +
  geom_point(color = "black") +
  geom_text(aes(label = label), vjust = -1.2, size = 3.2) +
  labs(
    title = "PCA Scree Plot: Variance Explained",
    x = "Principal Components",
    y = "Variance (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave("output/dim_reduction/plots/pca_scree.png", scree_plot, width = 8, height = 6, dpi = 300)

# Get cumulative variance
var_explained <- eig_df$`percentage of variance`
cum_var <- cumsum(var_explained)

# Find number of PCs to reach 95% variance
n_components <- which(cum_var >= 95)[1]
max_components <- ncol(pca_model$ind$coord)
n_components <- min(n_components, max_components)

cat("Selected", n_components, "components explaining", round(cum_var[n_components], 2), "% variance.\n")

# Extract selected components
pca_scores <- as.data.frame(pca_model$ind$coord[, 1:n_components])
colnames(pca_scores) <- paste0("PC", 1:n_components)
pca_scores[[class_col]] <- class_vector

# Save scores for modeling
write.csv(
  x = data.frame(Sample = rownames(pca_scores), pca_scores),
  file = "output/dim_reduction/pca_scores.csv",
  row.names = FALSE
)

# Save selected component count
writeLines(
  text = paste("PCA Components Chosen:", n_components, "\nCumulative Variance:", round(cum_var[n_components], 2), "%"),
  con = "output/dim_reduction/pca_components_info.txt"
)

# PCA 2D scatter plot
ggsave(
  filename = "output/dim_reduction/plots/pca_scatter.png",
  plot = ggplot(pca_scores, aes(x = PC1, y = PC2, color = .data[[class_col]])) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_manual(values = custom_colors) +
    theme_minimal() +
    labs(title = "PCA Projection (PC1 vs PC2)", color = "Class"),
  width = 7, height = 5, dpi = 300
)

# ---------------------------------------
# 2. t-SNE (for visualization only)
# ---------------------------------------

cat("Running t-SNE for EDA visualization...\n")
set.seed(123)

tsne_out <- Rtsne(scale(numeric_vars), dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)
tsne_df <- data.frame(Dim1 = tsne_out$Y[, 1], Dim2 = tsne_out$Y[, 2], Class = class_vector)

# t-SNE plot
ggsave(
  filename = "output/dim_reduction/plots/tsne_scatter.png",
  plot = ggplot(tsne_df, aes(x = Dim1, y = Dim2, color = Class)) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_manual(values = custom_colors) +
    theme_minimal() +
    labs(title = "t-SNE Projection (2D)", color = "Class"),
  width = 7, height = 5, dpi = 300
)

write.csv(tsne_df, file = "output/dim_reduction/tsne_coordinates.csv", row.names = FALSE)

cat("PCA and t-SNE complete. Ready for modeling.\n")
