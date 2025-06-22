# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Load the dataset
data_path <- "data/Brain_GSE50161.csv"
raw_data <- read.csv(data_path, stringsAsFactors = FALSE)

# Preview dataset
cat("Initial shape:\n")
print(dim(raw_data))

# Remove duplicated rows
data_clean <- raw_data %>% distinct()

# Remove columns with all NAs or constant values
data_clean <- data_clean %>% select(where(~ !(all(is.na(.)) || n_distinct(.) == 1)))

# Drop rows with excessive missing values (>30%)
row_na_threshold <- 0.3
data_clean <- data_clean[rowMeans(is.na(data_clean)) <= row_na_threshold, ]

# Fill remaining NA with column median for numeric
num_cols <- sapply(data_clean, is.numeric)
data_clean[num_cols] <- lapply(data_clean[num_cols], function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
})

# Convert character target column (if exists) to factor
if ("Class" %in% names(data_clean)) {
  data_clean$Class <- as.factor(data_clean$Class)
}

# Normalize numeric columns
data_clean[num_cols] <- scale(data_clean[num_cols])

# Save cleaned data (optional)
write.csv(data_clean, "output/cleaned_data.csv", row.names = FALSE)

# Print summary
cat("Final shape:\n")
print(dim(data_clean))
cat("Missing values:\n")
print(sum(is.na(data_clean)))
