# scripts/utils.R

# Custom summary function to correctly calculate key classification metrics
# This function is designed to work with caret's trainControl
custom_summary <- function(data, lev = NULL, model = NULL) {
  # Ensure observations and predictions are factors with the same levels
  levels <- union(levels(data$obs), levels(data$pred))
  obs <- factor(data$obs, levels = levels)
  pred <- factor(data$pred, levels = levels)
  
  # Use caret's confusionMatrix to get detailed statistics
  cm <- caret::confusionMatrix(pred, obs)
  
  # Extract overall statistics
  overall <- cm$overall
  
  # Calculate macro-averaged metrics from the 'byClass' component
  by_class_stats <- cm$byClass
  
  # Handle the structure of by_class_stats (matrix for >2 classes, vector for 2)
  if (is.matrix(by_class_stats)) {
    # Calculate the mean of each metric across all classes
    macro_precision <- mean(by_class_stats[, "Precision"], na.rm = TRUE)
    macro_recall <- mean(by_class_stats[, "Recall"], na.rm = TRUE)
    macro_f1 <- mean(by_class_stats[, "F1"], na.rm = TRUE)
  } else {
    # For 2 classes, the metrics are directly available
    macro_precision <- by_class_stats["Precision"]
    macro_recall <- by_class_stats["Recall"]
    macro_f1 <- by_class_stats["F1"]
  }
  
  # Combine all desired metrics into a named vector
  # This is the output that caret will use in its results
  stats <- c(
    Accuracy = overall["Accuracy"],
    Kappa = overall["Kappa"],
    F1 = macro_f1,
    Recall = macro_recall,
    Precision = macro_precision
  )
  
  # Replace any NaN values with 0 to avoid issues in model training
  stats[is.na(stats)] <- 0
  
  return(stats)
} 