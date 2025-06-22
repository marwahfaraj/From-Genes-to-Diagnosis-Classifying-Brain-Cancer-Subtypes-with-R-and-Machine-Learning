# 04_model_training_evaluation.R
# --------------------------------------------------------------------------------
# Compare modeling on different feature sets: PCA, t-SNE, LASSO vs. full data
# Nested CV (SMOTE vs. weighting × multiple algorithms), pick best, plot, then SHAP
# --------------------------------------------------------------------------------

# 0) Load libs
library(caret)
library(smotefamily)
library(randomForest)
library(xgboost)
library(e1071)      # SVM, Naive Bayes
library(nnet)       # Neural nets
library(klaR)       # Naive Bayes interface
library(dplyr)
library(ggplot2)
library(DALEX)      # SHAP explanations

# Helper: detect class column
get_class_col <- function(df) {
  if ('Class' %in% names(df)) return('Class')
  if ('type'  %in% names(df)) return('type')
  stop('No Class/type column in df')
}

# 1) Load feature sets
full_df   <- read.csv("output/cleaned_data.csv", stringsAsFactors = FALSE)
class_col <- get_class_col(full_df)
full_df$Class <- as.factor(full_df[[class_col]])

pca_df <- read.csv("output/dim_reduction/pca_scores.csv", stringsAsFactors = FALSE)
class_col_pca <- get_class_col(pca_df)
pca_df$Class <- as.factor(pca_df[[class_col_pca]])

tsne_df <- read.csv("output/dim_reduction/tsne_coordinates.csv", stringsAsFactors = FALSE)
class_col_tsne <- get_class_col(tsne_df)
tsne_df$Class <- as.factor(tsne_df[[class_col_tsne]])

lasso_feats <- read.csv("output/dim_reduction/lasso_selected_features.csv", stringsAsFactors = FALSE)
sel_feats   <- intersect(lasso_feats$Feature, names(full_df))
lasso_df    <- full_df %>% select(all_of(c(sel_feats, "Class")))

# 2) Prepare list of data frames
feature_sets <- list(
  Full  = full_df,
  PCA   = pca_df %>% select(-starts_with('Sample')),
  tSNE  = tsne_df %>% select(-starts_with('Sample')),
  LASSO = lasso_df
)

# 3) Algorithms to compare
algos <- c("rf","xgbTree","svmRadial","nnet","nb")

# 4) Nested CV and collect results
set.seed(2025)
results <- list()
for (set_name in names(feature_sets)) {
  df <- feature_sets[[set_name]]
  folds_outer <- createFolds(df$Class, k = 5)
  
  for (i in seq_along(folds_outer)) {
    train_idx <- folds_outer[[i]]
    test_idx  <- setdiff(seq_len(nrow(df)), train_idx)
    train_df  <- df[train_idx, ]
    test_df   <- df[test_idx, ]
    
    freqs <- table(train_df$Class)
    wts   <- freqs[1] / freqs
    
    for (strategy in c("SMOTE", "Weighted")) {
      ctrl <- trainControl(
        method        = "repeatedcv",
        number        = 3,
        repeats       = 1,
        sampling      = if (strategy == "SMOTE") 'smote' else NULL,
        classProbs    = TRUE,
        summaryFunction = twoClassSummary,
        verboseIter   = FALSE
      )
      
      for (alg in algos) {
        grid <- switch(alg,
                       rf       = expand.grid(mtry = c(5, 10, 20)),
                       xgbTree  = expand.grid(
                         nrounds = 100,
                         max_depth = c(3, 6),
                         eta = c(0.1, 0.3),
                         gamma = 0,
                         colsample_bytree = 0.7,
                         min_child_weight = 1,
                         subsample = 0.8
                       ),
                       svmRadial = NULL,
                       nnet      = NULL,
                       nb        = NULL
        )
        
        set.seed(100 + i)
        fit <- if (strategy == "Weighted") {
          train(
            Class ~ ., data = train_df,
            method     = alg,
            metric     = 'ROC',
            trControl  = ctrl,
            tuneGrid   = grid,
            weights    = wts[train_df$Class]
          )
        } else {
          train(
            Class ~ ., data = train_df,
            method     = alg,
            metric     = 'ROC',
            trControl  = ctrl,
            tuneGrid   = grid
          )
        }
        
        pred <- predict(fit, test_df)
        acc  <- mean(pred == test_df$Class)
        results[[length(results) + 1]] <- data.frame(
          Set       = set_name,
          Fold      = i,
          Strategy  = strategy,
          Algorithm = alg,
          Accuracy  = acc,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

# 5) Summarize and save
res_df <- bind_rows(results)
perf_summary <- res_df %>%
  group_by(Set, Strategy, Algorithm) %>%
  summarise(
    MeanAcc = mean(Accuracy),
    SD      = sd(Accuracy),
    .groups = 'drop'
  ) %>%
  arrange(desc(MeanAcc))
write.csv(perf_summary, "output/models/performance_summary.csv", row.names = FALSE)

# 6) Plot performance comparison
p_perf <- ggplot(perf_summary, aes(x = Algorithm, y = MeanAcc, fill = Set)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ Strategy) +
  theme_minimal(base_size = 14) +
  labs(
    title = 'Nested CV Mean Accuracy by Model & Feature Set',
    x     = 'Algorithm',
    y     = 'Mean Accuracy'
  )

ggsave('output/models/performance_comparison.png', plot = p_perf, dpi = 300, width = 10, height = 6)

# 7) Retrain best on full data
best <- perf_summary[1, ]
cat('Best:', best$Set, best$Strategy, best$Algorithm, '\n')
full_train <- feature_sets[[best$Set]]
if (best$Strategy == 'Weighted') {
  freqs_f <- table(full_train$Class)
  wts_f   <- freqs_f[1] / freqs_f
  final_fit <- train(
    Class ~ ., data     = full_train,
    method   = best$Algorithm,
    trControl = trainControl(method = 'none'),
    tuneGrid  = NULL,
    weights   = wts_f[full_train$Class]
  )
} else {
  sm <- SMOTE(
    X = full_train %>% select(-Class),
    target = full_train$Class
  )
  train2 <- cbind(sm$data, Class = sm$class)
  final_fit <- train(
    Class ~ ., data     = train2,
    method   = best$Algorithm,
    trControl = trainControl(method = 'none'),
    tuneGrid  = NULL
  )
}
saveRDS(final_fit, 'output/models/best_model.rds')

# 8) SHAP explanation
cat('Building SHAP explanations on final model...\n')
explainer <- explain(
  model = final_fit,
  data  = full_train %>% select(-Class),
  y     = full_train$Class,
  label = paste(best$Algorithm)
)
shap_vals <- predict_parts(
  explainer,
  type = 'shap',
  B    = 25
)
p_shap <- plot(shap_vals) +
  ggtitle('SHAP Summary for Best Model')

ggsave(
  filename = 'output/models/shap_summary.png',
  plot     = p_shap,
  dpi      = 300,
  width    = 10,
  height   = 6
)

cat('All models evaluated, comparison & SHAP saved.\n')
