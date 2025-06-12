# 02_exploration_visualization.R

# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(pheatmap)

# Custom color palette
custom_colors <- c("#54B3AE", "#2C3A4B", "#A5D5D1", "#e8ead3", "#7FB6B0")

# Load the cleaned data
data <- read.csv("output/cleaned_data.csv")

# Create output directory if it doesn't exist
if (!dir.exists("output/graphs")) {
  dir.create("output/graphs", recursive = TRUE)
}

# ------------------------------------------------------------
# Missing values and basic info
# ------------------------------------------------------------
cat("Total missing values:", sum(is.na(data)), "\n")
cat("Columns with missing values:\n")
print(colSums(is.na(data))[colSums(is.na(data)) > 0])

# ------------------------------------------------------------
# Remove constant columns
# ------------------------------------------------------------
data_clean <- data[, sapply(data, function(x) length(unique(x)) > 1)]
cat("Original columns:", ncol(data), "\n")
cat("Columns after removing constants:", ncol(data_clean), "\n")

# ------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------
cat("Data dimensions:", dim(data_clean), "\n")
cat("Summary of gene expression:\n")
print(summary(data_clean[ , sapply(data_clean, is.numeric)]))

# ------------------------------------------------------------
# Distribution of gene variances
# ------------------------------------------------------------
# compute variance only on numeric columns, omit NAs
gene_variance <- sapply(data_clean[ , sapply(data_clean, is.numeric)], var, na.rm = TRUE)

png("output/graphs/variance_hist.png", width = 800, height = 600)
hist(gene_variance,
     main = "Distribution of Gene Variances",
     xlab = "Variance",
     col  = custom_colors[1])
dev.off()

# ------------------------------------------------------------
# Boxplot: Top 5 most variable genes
# ------------------------------------------------------------
top5_genes <- names(sort(gene_variance, decreasing = TRUE))[1:5]
plot_data <- data_clean %>%
  select(samples, all_of(top5_genes)) %>%
  pivot_longer(-samples, names_to = "gene", values_to = "expression")

p1 <- ggplot(plot_data, aes(x = gene, y = expression, fill = gene)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Expression Distribution of Top 5 Variable Genes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/graphs/top5_boxplot.png", p1, dpi = 300, width = 8, height = 5)

# ------------------------------------------------------------
# Correlation matrix of top 10 genes
# ------------------------------------------------------------
top_10_genes <- names(sort(gene_variance, decreasing = TRUE))[1:10]
cor_matrix   <- cor(data_clean[ , top_10_genes], use = "pairwise.complete.obs")

png("output/graphs/correlation_matrix.png", width = 800, height = 600)
corrplot(cor_matrix,
         method     = "color",
         tl.cex     = 0.8,
         col        = colorRampPalette(custom_colors)(100),
         na.label   = " ")
dev.off()

# ------------------------------------------------------------
# Bar plot: Top 10 most variable genes
# ------------------------------------------------------------
top10      <- sort(gene_variance, decreasing = TRUE)[1:10]
top10_df   <- data.frame(Gene = names(top10), Variance = top10)

p2 <- ggplot(top10_df, aes(x = reorder(Gene, -Variance), y = Variance, fill = Gene)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Top 10 Most Variable Genes",
       x     = "Gene",
       y     = "Variance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/graphs/top10_barplot.png", p2, dpi = 300, width = 10, height = 6)

# ------------------------------------------------------------
# Heatmap: Top 25 most variable genes
# ------------------------------------------------------------
top25        <- names(sort(gene_variance, decreasing = TRUE))[1:25]
heatmap_data <- as.matrix(data_clean[ , top25])
rownames(heatmap_data) <- data_clean$samples

pheatmap(heatmap_data,
         scale         = "row",
         show_rownames = FALSE,
         show_colnames = TRUE,
         fontsize_col  = 5,
         color         = colorRampPalette(custom_colors)(100),
         filename      = "output/graphs/heatmap_top25.png",
         width         = 10,
         height        = 8)

# ------------------------------------------------------------
# Sample-level summary plots
# ------------------------------------------------------------
numeric_data  <- data_clean[ , sapply(data_clean, is.numeric)]
sample_totals <- rowSums(numeric_data, na.rm = TRUE)

png("output/graphs/sample_totals_boxplot.png", width = 600, height = 400)
boxplot(sample_totals,
        main = "Total Expression per Sample",
        ylab = "Sum of Expression",
        col  = custom_colors[1])
dev.off()

png("output/graphs/sample_totals_density.png", width = 600, height = 400)
plot(density(sample_totals, na.rm = TRUE),
     main = "Density of Total Expression per Sample",
     col  = custom_colors[2])
dev.off()

# ------------------------------------------------------------
# Cumulative variance explained
# ------------------------------------------------------------
var_sorted <- sort(gene_variance, decreasing = TRUE)
png("output/graphs/cumulative_variance.png", width = 800, height = 600)
plot(cumsum(var_sorted) / sum(var_sorted),
     type  = "l",
     main  = "Cumulative Variance Explained by Top Genes",
     xlab  = "Top N Genes",
     ylab  = "Cumulative Proportion of Variance",
     col   = custom_colors[5])
dev.off()

# ------------------------------------------------------------
# Done
# ------------------------------------------------------------
cat("Exploration and visualization complete. Plots saved to output/graphs directory.\n")
