suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(purrr)
  library(pROC)
  library(ggplot2)
  library(readr)
  library(tibble)
  library(scales)
  library(reshape2)
})

# Ensure output folders exist
if (!dir.exists("output/final_model")) dir.create("output/final_model", recursive = TRUE)
if (!dir.exists("output/final_model/plots")) dir.create("output/final_model/plots", recursive = TRUE)

# Load data
data <- read.csv("output/dim_reduction/pca_scores.csv")
class_col <- if ("Class" %in% names(data)) "Class" else "type"
data[[class_col]] <- as.factor(data[[class_col]])
original_class_levels <- levels(data[[class_col]])

# Define your custom base colors
custom_colors <- c("#54B3AE", "#2C3A4B", "#A5D5D1", "#e8ead3", "#7FB6B0")
base_colors <- setNames(custom_colors, original_class_levels)

# Models to evaluate
models <- c("xgbTree", "rf", "nnet")

# Inner and outer CV
outer_ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final",
                           classProbs = TRUE, sampling = "smote",
                           summaryFunction = multiClassSummary)

inner_ctrl <- trainControl(method = "cv", number = 3, sampling = "smote",
                           classProbs = TRUE, summaryFunction = multiClassSummary)

# Nested evaluation function
nested_eval <- function(model_name) {
  set.seed(123)
  outer_folds <- createFolds(data[[class_col]], k = 5, returnTrain = TRUE)
  
  outer_results <- map_dfr(seq_along(outer_folds), function(i) {
    train_idx <- outer_folds[[i]]
    train_data <- data[train_idx, ]
    test_data <- data[-train_idx, ]
    
    set.seed(123)
    fit <- train(
      reformulate(setdiff(names(train_data), class_col), class_col),
      data = train_data,
      method = model_name,
      trControl = inner_ctrl,
      tuneLength = 3,
      preProcess = c("nzv", "center", "scale")
    )
    
    preds <- predict(fit, newdata = test_data)
    probs <- predict(fit, newdata = test_data, type = "prob")
    
    confusion <- confusionMatrix(preds, test_data[[class_col]])
    acc <- confusion$overall["Accuracy"]
    f1 <- confusion$byClass[, "F1"]
    aucs <- map_dbl(original_class_levels, function(lvl) {
      if (lvl %in% colnames(probs)) {
        roc(as.numeric(test_data[[class_col]] == lvl), probs[[lvl]])$auc
      } else {
        NA_real_
      }
    })
    
    tibble(
      Fold = i,
      Model = model_name,
      Accuracy = acc,
      MacroF1 = mean(f1, na.rm = TRUE),
      MacroAUC = mean(aucs, na.rm = TRUE)
    )
  })
  
  write_csv(outer_results, paste0("output/final_model/metrics_nested_", model_name, ".csv"))
  outer_results
}

# Run nested evaluation
all_results <- map_dfr(models, nested_eval)

# Save overall summary
summary_stats <- all_results %>%
  group_by(Model) %>%
  summarize(
    Accuracy = mean(Accuracy, na.rm = TRUE),
    F1 = mean(MacroF1, na.rm = TRUE),
    AUC = mean(MacroAUC, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_stats, "output/final_model/summary_nested_models.csv")

# Best model
best_model <- summary_stats %>% arrange(desc(Accuracy)) %>% slice(1) %>% pull(Model)
cat("Best model:", best_model, "\n")

# Final model on all data
set.seed(123)
final_ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                           summaryFunction = multiClassSummary,
                           savePredictions = "final", sampling = "smote")

final_model <- train(
  reformulate(setdiff(names(data), class_col), class_col),
  data = data,
  method = best_model,
  trControl = final_ctrl,
  tuneLength = 3,
  preProcess = c("nzv", "center", "scale")
)

# Save confusion matrix
conf_matrix <- confusionMatrix(final_model$pred$pred, final_model$pred$obs)
capture.output(conf_matrix, file = "output/final_model/confusion_matrix.txt")

# ----- ROC Plot -----
df_combined <- final_model$pred
pred_class_levels <- levels(df_combined$obs)
base_colors <- base_colors[pred_class_levels]  # Reorder to match prediction levels

png("output/final_model/plots/final_model_roc.png", width = 800, height = 600)
tryCatch({
  roc_named_list <- keep(pred_class_levels, ~ .x %in% colnames(df_combined)) %>%
    map(function(class_name) {
      list(
        name = class_name,
        roc = roc(
          response = factor(df_combined$obs == class_name),
          predictor = df_combined[[class_name]],
          levels = c(FALSE, TRUE),
          direction = "<",
          quiet = TRUE
        )
      )
    })
  
  plot(
    roc_named_list[[1]]$roc,
    col = base_colors[roc_named_list[[1]]$name],
    lwd = 2.5,
    main = "Final Model ROC Curves",
    legacy.axes = TRUE,
    grid = TRUE
  )
  walk(roc_named_list[-1], function(entry) {
    lines(entry$roc, col = base_colors[entry$name], lwd = 2.5)
  })
  legend("bottomright", legend = names(base_colors), col = base_colors, lwd = 2.5, bg = "white", box.lty = 0)
}, error = function(e) {
  message("❌ ROC plot error: ", e$message)
})
dev.off()

# ----- Final Corrected Confusion Matrix Plot -----
library(ggplot2)
library(scales)
library(dplyr)

# Prepare the data
conf_df <- as.data.frame(conf_matrix$table)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

# Set class order in standard top-left to bottom-right diagonal
class_order <- c("ependymoma", "glioblastoma", "medulloblastoma", "normal", "pilocytic_astrocytoma")
conf_df$Actual <- factor(conf_df$Actual, levels = class_order)        # Y-axis (rows)
conf_df$Predicted <- factor(conf_df$Predicted, levels = class_order)  # X-axis (columns)

# Calculate percentages
conf_df <- conf_df %>%
  group_by(Actual) %>%
  mutate(Pct = Freq / sum(Freq))

# Create and save the confusion matrix heatmap
p <- ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Pct)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Pct * 100)),
            color = "black", size = 5, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#54B3AE",
                      limits = c(0, 1),
                      labels = percent_format(accuracy = 1)) +
  labs(title = paste("Confusion Matrix -", best_model),
       x = "\nPredicted Class",
       y = "Actual (Reference) Class\n",
       fill = "Percentage") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right",
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  coord_fixed()

# Save it
ggsave("output/final_model/plots/final_model_confusion_matrix.png",
       plot = p, width = 10, height = 8, dpi = 300)


cat("✅ Nested CV complete. Metrics and plots saved in output/final_model/.\n")
