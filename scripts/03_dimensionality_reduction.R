# 03_dimensionality_reduction.R

# --------------------------------------------------------------------------------
# This script applies dimensionality‐reduction techniques on the cleaned dataset:
#   1) Principal Component Analysis (PCA)
#   2) t‐Distributed Stochastic Neighbor Embedding (t‐SNE)
#   3) LASSO (glmnet) for feature selection (multinomial if >2 classes)
#
# Outputs:
#   • PCA eigenvalues (scree) plot and PCA scores saved to CSV
#   • t‐SNE 2D embedding plot and coordinates saved to CSV
#   • LASSO feature coefficients and selected feature list saved to CSV
#
# All plots are saved under “output/dim_reduction/” (300 dpi PNG).
# --------------------------------------------------------------------------------

# 0) Load required packages
library(ggplot2)
library(FactoMineR)      # for PCA
library(factoextra)      # for PCA plotting
library(Rtsne)           # for t‐SNE
library(glmnet)          # for LASSO
library(dplyr)           # for data wrangling

# 1) Load the cleaned data
cat("Loading cleaned data...\n")
flush.console()
data <- read.csv("output/cleaned_data.csv", stringsAsFactors = FALSE)

# 2) Create output subdirectories if they don't exist
if (!dir.exists("output/dim_reduction")) {
  dir.create("output/dim_reduction", recursive = TRUE)
}
if (!dir.exists("output/dim_reduction/plots")) {
  dir.create("output/dim_reduction/plots", recursive = TRUE)
}

# 3) Detect which column holds the class/label (could be "Class" or "type")
class_col <- if ("Class" %in% names(data)) "Class" else "type"

# 4) Extract numeric matrix and class vector
numeric_vars <- data[, sapply(data, is.numeric)]
class_vector <- data[[class_col]]

# 5) Define class‐level colors (match the greenish EDA palette)
class_levels <- sort(unique(class_vector))
# Base colors from EDA: greenish and complementary pastel
base_colors <- c("#54B3AE", "#2C3A4B", "#A5D5D1", "#fbfcee", "#7FB6B0")
# Use only as many colors as there are distinct classes
custom_colors <- base_colors[1:length(class_levels)]
names(custom_colors) <- class_levels

# --------------------------------------------------------------------------------
# (1) PRINCIPAL COMPONENT ANALYSIS (PCA)
# --------------------------------------------------------------------------------
cat("🔍 Performing PCA...\n")
flush.console()

# Center and scale numeric variables
pca_res <- PCA(numeric_vars, scale.unit = TRUE, ncp = 10, graph = FALSE)

# Scree plot of eigenvalues
png("output/dim_reduction/plots/pca_scree.png", width = 800, height = 600, res = 300)
fviz_screeplot(pca_res, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("PCA Scree Plot: Variance Explained") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )
dev.off()

# Extract PCA scores for first two components
pca_scores <- as.data.frame(pca_res$ind$coord[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")
pca_scores[[class_col]] <- class_vector

# PCA scatter plot (PC1 vs PC2) with custom colors
p_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = .data[[class_col]])) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "PCA: PC1 vs PC2",
    x     = paste0("PC1 (", round(100 * pca_res$eig[1, 2], 1), "%)"),
    y     = paste0("PC2 (", round(100 * pca_res$eig[2, 2], 1), "%)"),
    color = "Class"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.position  = "right"
  )

ggsave(
  filename = "output/dim_reduction/plots/pca_scatter.png",
  plot     = p_pca,
  dpi      = 300,
  width    = 7,
  height   = 5
)

# Save PCA scores to CSV
write.csv(
  x         = data.frame(Sample = rownames(pca_scores), pca_scores),
  file      = "output/dim_reduction/pca_scores.csv",
  row.names = FALSE
)


# --------------------------------------------------------------------------------
# (2) t‐DISTRIBUTED STOCHASTIC NEIGHBOR EMBEDDING (t‐SNE)
# --------------------------------------------------------------------------------
cat("🔍 Performing t‐SNE (2D)...\n")
flush.console()

# Set a random seed for reproducibility
set.seed(123)

# Run t‐SNE on the scaled numeric data
tsne_obj <- Rtsne(
  X                 = scale(numeric_vars),
  dims              = 2,
  perplexity        = 30,
  verbose           = TRUE,
  max_iter          = 1000,
  check_duplicates  = FALSE
)

# Collect t‐SNE coordinates
tsne_df <- data.frame(
  Dim1  = tsne_obj$Y[, 1],
  Dim2  = tsne_obj$Y[, 2],
  Class = class_vector
)

# t‐SNE scatter plot with the same greenish palette
p_tsne <- ggplot(tsne_df, aes(x = Dim1, y = Dim2, color = Class)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "t‐SNE: 2D Embedding",
    x     = "t‐SNE 1",
    y     = "t‐SNE 2",
    color = "Class"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.position  = "right"
  )

ggsave(
  filename = "output/dim_reduction/plots/tsne_scatter.png",
  plot     = p_tsne,
  dpi      = 300,
  width    = 7,
  height   = 5
)

# Save t‐SNE coordinates to CSV
write.csv(
  x         = data.frame(Sample = rownames(tsne_df), tsne_df),
  file      = "output/dim_reduction/tsne_coordinates.csv",
  row.names = FALSE
)


# --------------------------------------------------------------------------------
# (3) LASSO FEATURE SELECTION (glmnet)
#    - If >2 classes, use multinomial; if exactly 2, use binomial.
# --------------------------------------------------------------------------------
cat("🔍 Running LASSO for feature selection...\n")
flush.console()

# Prepare X (matrix) and y (factor‐encoded outcome)
X_matrix  <- as.matrix(scale(numeric_vars))
y_factor  <- as.factor(class_vector)
n_classes <- length(levels(y_factor))

family_type <- if (n_classes == 2) "binomial" else "multinomial"

# Cross‐validated glmnet to choose lambda minimizing classification error
cv_fit <- cv.glmnet(
  x           = X_matrix,
  y           = y_factor,
  family      = family_type,
  alpha       = 1,           # LASSO penalty
  nfolds      = 5,
  type.measure = "class"
)

best_lambda <- cv_fit$lambda.min
cat("Best lambda chosen by CV:", round(best_lambda, 5), "\n")
flush.console()

# Refit glmnet at best_lambda to extract coefficients
lasso_fit <- glmnet(
  x       = X_matrix,
  y       = y_factor,
  family  = family_type,
  alpha   = 1,
  lambda  = best_lambda
)

# Extract nonzero coefficients (features selected)
coef_list <- coef(lasso_fit)
selected_features <- c()

if (family_type == "binomial") {
  nz_idx <- which(abs(coef_list[, 1]) > 0)
  selected_features <- rownames(coef_list)[nz_idx]
  selected_features <- setdiff(selected_features, "(Intercept)")
} else {
  for (cls in names(coef_list)) {
    this_coef <- coef_list[[cls]]
    nz_idx    <- which(abs(this_coef[, 1]) > 0)
    feats     <- rownames(this_coef)[nz_idx]
    feats     <- setdiff(feats, "(Intercept)")
    selected_features <- unique(c(selected_features, feats))
  }
}

cat("Number of features selected by LASSO:", length(selected_features), "\n")
flush.console()

# Save selected features to CSV
write.csv(
  x         = data.frame(Feature = selected_features),
  file      = "output/dim_reduction/lasso_selected_features.csv",
  row.names = FALSE
)

# Also save full coefficient table for reference
if (family_type == "binomial") {
  coef_df <- data.frame(
    Feature     = rownames(coef_list),
    Coefficient = as.numeric(coef_list[, 1])
  )
} else {
  coef_df <- do.call(rbind, lapply(names(coef_list), function(cls) {
    this_coef <- coef_list[[cls]]
    data.frame(
      Class       = cls,
      Feature     = rownames(this_coef),
      Coefficient = as.numeric(this_coef[, 1])
    )
  }))
}

write.csv(
  x         = coef_df,
  file      = "output/dim_reduction/lasso_coefficients.csv",
  row.names = FALSE
)

# --------------------------------------------------------------------------------
# (End) Final status message
# --------------------------------------------------------------------------------
cat("Dimensionality reduction complete. Outputs saved under output/dim_reduction/.\n")
flush.console()
