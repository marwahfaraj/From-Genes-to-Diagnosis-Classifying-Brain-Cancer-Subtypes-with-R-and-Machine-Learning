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
dir.create("output/final_model/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("shiny_app/model", recursive = TRUE, showWarnings = FALSE)
dir.create("shiny_app/data", recursive = TRUE, showWarnings = FALSE)

set.seed(123)
data <- read.csv("output/dim_reduction/pca_scores.csv")
class_col <- if ("Class" %in% names(data)) "Class" else "type"
data[[class_col]] <- as.factor(data[[class_col]])

# REMOVE Sample column if present
if ("Sample" %in% names(data)) {
  data <- data[, !(names(data) %in% "Sample")]
}

# Split into train/test ONCE
test_frac <- 0.2
test_idx <- sample(seq_len(nrow(data)), size = floor(test_frac * nrow(data)))
test_set <- data[test_idx, ]
train_set <- data[-test_idx, ]

# Save the test set for the app
write.csv(test_set, "shiny_app/data/test_set.csv", row.names = FALSE)

original_class_levels <- levels(data[[class_col]])

# Custom base colors
custom_colors <- c("#54B3AE", "#2C3A4B", "#A5D5D1", "#e8ead3", "#7FB6B0")
base_colors <- setNames(custom_colors, original_class_levels)

# Models
models <- c("xgbTree", "rf", "nnet")

# CV controls
outer_ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final",
                           classProbs = TRUE, sampling = "smote",
                           summaryFunction = multiClassSummary)

inner_ctrl <- trainControl(method = "cv", number = 3, sampling = "smote",
                           classProbs = TRUE, summaryFunction = multiClassSummary)

# Nested CV evaluation
nested_eval <- function(model_name) {
  set.seed(123)
  outer_folds <- createFolds(train_set[[class_col]], k = 5, returnTrain = TRUE)
  
  outer_results <- map_dfr(seq_along(outer_folds), function(i) {
    train_idx <- outer_folds[[i]]
    test_idx <- setdiff(seq_len(nrow(train_set)), train_idx)
    
    train_data <- train_set[train_idx, ]
    test_data <- train_set[test_idx, ]
    
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
      } else NA_real_
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

# Run nested CV
all_results <- map_dfr(models, nested_eval)

# Aggregate summary
summary_stats <- all_results %>%
  group_by(Model) %>%
  summarize(
    Accuracy = mean(Accuracy, na.rm = TRUE),
    F1 = mean(MacroF1, na.rm = TRUE),
    AUC = mean(MacroAUC, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_stats, "output/final_model/summary_nested_models.csv")

# Identify the best model based on Kappa
best_model_summary <- summary_stats %>%
  arrange(desc(Accuracy)) %>%
  slice(1)

best_model <- best_model_summary$Model[1]
cat("========================================\n")
cat("✅ Best model selected:", best_model, "\n")
cat("========================================\n")

# Final model training
set.seed(123)
final_ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                           summaryFunction = multiClassSummary,
                           savePredictions = "final", sampling = "smote")

final_model <- train(
  reformulate(setdiff(names(train_set), class_col), class_col),
  data = train_set,
  method = best_model,
  metric = "Kappa",
  trControl = final_ctrl,
  tuneLength = 3,
  verbose = FALSE
)

# Save final model
saveRDS(final_model, "shiny_app/model/final_model.rds")

# --- Final Evaluation on Test Set ---
# Predict on the held-out test set
predictions <- predict(final_model, newdata = test_set)
probs <- predict(final_model, newdata = test_set, type = "prob")

# Ensure factors have the same levels for confusion matrix
all_levels <- levels(data[[class_col]])
predictions <- factor(predictions, levels = all_levels)
actual_obs <- factor(test_set[[class_col]], levels = all_levels)

# Confusion matrix from the test set
conf_matrix <- confusionMatrix(predictions, actual_obs)
capture.output(conf_matrix, file = "output/final_model/confusion_matrix.txt")

# Save confusion matrix table for the app
conf_df_for_app <- as.data.frame(conf_matrix$table)
write.csv(conf_df_for_app, "shiny_app/data/confusion_matrix.csv", row.names = FALSE)

# Save metrics for the app
metrics_for_app <- data.frame(
  Accuracy = conf_matrix$overall["Accuracy"],
  Kappa = conf_matrix$overall["Kappa"],
  Precision = mean(conf_matrix$byClass[, "Precision"], na.rm = TRUE),
  Recall = mean(conf_matrix$byClass[, "Recall"], na.rm = TRUE),
  F1 = mean(conf_matrix$byClass[, "F1"], na.rm = TRUE)
)
write.csv(metrics_for_app, "shiny_app/data/metrics.csv", row.names = FALSE)

# ROC plot from the test set
df_combined <- data.frame(
  obs = actual_obs,
  probs
)
pred_class_levels <- levels(df_combined$obs)
base_colors <- base_colors[pred_class_levels]

png("output/final_model/plots/final_model_roc.png", width = 800, height = 600)
tryCatch({
  par(bg = "white")
  
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
    main = "Final Model ROC Curves (on Test Set)",
    legacy.axes = TRUE,
    grid = TRUE,
    col.grid = "gray90"
  )
  walk(roc_named_list[-1], function(entry) {
    lines(entry$roc, col = base_colors[entry$name], lwd = 2.5)
  })
  legend("bottomright", legend = names(base_colors), col = base_colors, lwd = 2.5, bg = "white", box.lty = 0)
}, error = function(e) {
  message("❌ ROC plot error: ", e$message)
})
dev.off()

# FIXED CONFUSION MATRIX PLOTTING (from test set)
conf_df <- as.data.frame(conf_matrix$table)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

# Define class order - this will control the display order
class_order <- c("ependymoma", "glioblastoma", "medulloblastoma", "normal", "pilocytic_astrocytoma")

print(getwd())
print(list.files("shiny_app/data"))


