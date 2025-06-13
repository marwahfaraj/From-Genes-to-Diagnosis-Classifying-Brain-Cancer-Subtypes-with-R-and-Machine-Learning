# 04_eval_rf.R

suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(smotefamily)
  library(ggplot2)
})

# 1. Load PCA-transformed features
pca_df <- read.csv("output/dim_reduction/pca_scores.csv", stringsAsFactors = FALSE)

# 2. Load original labels
full_df <- read.csv("output/cleaned_data.csv", stringsAsFactors = FALSE)
class_col <- if ("Class" %in% names(full_df)) "Class" else "type"
pca_df$Class <- as.factor(full_df[[class_col]])

# 3. Function to safely apply SMOTE
safe_smote <- function(df) {
  smoted <- tryCatch({
    sm <- SMOTE(X = df[ , !names(df) %in% "Class"],
                target = df$Class,
                K = 5)
    cbind(sm$data, Class = as.factor(sm$data$class))[, !names(sm$data) %in% "class"]
  }, error = function(e) {
    message("SMOTE error: ", e$message)
    return(NULL)
  })
  return(smoted)
}

# 4. Setup results container
results <- list()

# 5. Set seed and prepare 5-fold outer CV
set.seed(123)
folds <- createFolds(pca_df$Class, k = 5)

# 6. Loop through folds
for (i in seq_along(folds)) {
  train_df <- pca_df[folds[[i]], ]
  test_df  <- pca_df[-folds[[i]], ]
  
  # class weights for imbalance
  class_weights <- max(table(train_df$Class)) / table(train_df$Class)
  
  for (strategy in c("Weighted", "SMOTE")) {
    train_input <- if (strategy == "SMOTE") safe_smote(train_df) else train_df
    if (is.null(train_input)) next
    
    fit <- tryCatch({
      train(
        Class ~ .,
        data = train_input,
        method = "rf",
        trControl = trainControl(method = "cv", number = 3),
        tuneGrid = data.frame(mtry = 2),
        preProcess = c("nzv", "center", "scale"),
        weights = if (strategy == "Weighted") class_weights[train_input$Class] else NULL
      )
    }, error = function(e) {
      message("Error in RF-", strategy, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(fit)) {
      acc <- mean(predict(fit, test_df) == test_df$Class)
      results[[length(results) + 1]] <- data.frame(
        Fold = i,
        Strategy = strategy,
        Accuracy = acc
      )
    }
  }
}

# 7. Combine and save
final_results <- bind_rows(results)
write.csv(final_results, "output/results_rf.csv", row.names = FALSE)

# 8. Summary and plot
summary <- final_results %>%
  group_by(Strategy) %>%
  summarize(
    MeanAcc = mean(Accuracy),
    SDAcc   = sd(Accuracy),
    .groups = "drop"
  )

write.csv(summary, "output/summary_rf.csv", row.names = FALSE)

ggplot(summary, aes(x = Strategy, y = MeanAcc, fill = Strategy)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = MeanAcc - SDAcc, ymax = MeanAcc + SDAcc), width = 0.2) +
  labs(title = "Random Forest on PCA Features", y = "Accuracy", x = "Strategy") +
  theme_minimal()

ggsave("output/rf_pca_accuracy_plot.png", dpi = 300, width = 8, height = 5)
