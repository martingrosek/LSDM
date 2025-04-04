# model_original.R

# 1. Load cleaned data
source("R/load_and_clean.R")

# 2. Filter numeric variables and remove constants
numeric_cols <- sapply(combined_data_clean, is.numeric)
combined_numeric <- combined_data_clean[, numeric_cols]
sds <- apply(combined_numeric, 2, sd, na.rm = TRUE)
filtered_data <- combined_numeric[, sds > 0]

# 3. Remove rows with missing values
model_data <- filtered_data[complete.cases(filtered_data), ]

# 4. Prepare target and features
target <- model_data$log_estimated_fire_area
features <- model_data[, setdiff(names(model_data), c("Estimated_fire_area", "log_estimated_fire_area"))]

# 5. Create a data frame for modeling
df <- data.frame(target = target, features)

# 6. Train/test split (80/20)
set.seed(123)
n <- nrow(df)
train_idx <- sample(seq_len(n), size = 0.8 * n)
train_data <- df[train_idx, ]
test_data  <- df[-train_idx, ]

# 7. Train linear regression
model <- lm(target ~ ., data = train_data)

# 8. Predict on test set
predictions <- predict(model, newdata = test_data)

# 9. Evaluation metrics
rmse <- sqrt(mean((test_data$target - predictions)^2))
mae  <- mean(abs(test_data$target - predictions))
r2   <- 1 - sum((test_data$target - predictions)^2) / sum((test_data$target - mean(test_data$target))^2)

cat("Linear Regression Evaluation (Original Features):\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE: ", round(mae, 3), "\n")
cat("R²:  ", round(r2, 3), "\n")



# pca_model.R

# 1. Load PCA results and cleaned data
load("pca_output.RData")            # loads: pca_result, rows_no_na
source("R/load_and_clean.R")        # loads: combined_data_clean

# 2. Select numeric features
numeric_cols <- sapply(combined_data_clean, is.numeric)
combined_numeric <- combined_data_clean[, numeric_cols]

# 3. Keep rows used in PCA (no NA, no constant columns)
filtered_data <- combined_numeric[rows_no_na, ]

# 4. Prepare PCA scores (PC1–PC13)
pca_scores <- as.data.frame(pca_result$x[, 1:13])

# 5. Add target variable (log transformed)
pca_scores$target <- log1p(filtered_data$Estimated_fire_area)

# 6. Train/test split (80/20)
set.seed(123)
n <- nrow(pca_scores)
train_idx <- sample(seq_len(n), size = 0.8 * n)
train_data <- pca_scores[train_idx, ]
test_data  <- pca_scores[-train_idx, ]

# 7. Train linear regression on PCA components
model_pca <- lm(target ~ ., data = train_data)

# 8. Predict on test set
predictions <- predict(model_pca, newdata = test_data)

# 9. Evaluation metrics
rmse <- sqrt(mean((test_data$target - predictions)^2))
mae  <- mean(abs(test_data$target - predictions))
r2   <- 1 - sum((test_data$target - predictions)^2) / sum((test_data$target - mean(test_data$target))^2)

# 10. Results
cat("Linear Regression Evaluation (PCA Features):\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE: ", round(mae, 3), "\n")
cat("R²:  ", round(r2, 3), "\n")

# Napovedi in resnične vrednosti v originalni skali
pred_real <- expm1(predictions)
true_real <- expm1(test_data$target)

rmse_real <- sqrt(mean((true_real - pred_real)^2))
mae_real  <- mean(abs(true_real - pred_real))

cat("RMSE (real scale):", round(rmse_real, 2), "\n")
cat("MAE  (real scale):", round(mae_real, 2), "\n")

