# spark_models.R

library(sparklyr)
library(dplyr)

# 1. Connect to Spark
config <- spark_config()
config$spark.driver.memory <- "2g"
sc <- spark_connect(master = "local[2]", version = "3.4.1", config = config)

# 2. Load and prepare data
spark_df <- spark_read_csv(
  sc,
  name = "firesample",
  # path = "../sampled_data_stratified.csv",
  path = "D:/FAKS/MAG 2. letnik/LSDM/sampled_data_stratified.csv",
  infer_schema = TRUE,
  header = TRUE
)

# 3. Define relevant features
features <- setdiff(
  colnames(spark_df),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# 4. Convert all features to numeric (if needed)
for (col in features) {
  spark_df <- spark_df %>% mutate(!!col := as.numeric(!!sym(col)))
}

# 5. Prepare classification variable (make sure it's a factor)
spark_df <- spark_df %>%
  mutate(area_bin = cast(area_bin %as% "string"))

# 6. Train/test split
splits <- sdf_random_split(spark_df, train = 0.7, test = 0.3, seed = 42)
train_tbl <- splits$train
test_tbl <- splits$test

# Filter out rows with NA in selected features/targets
train_tbl <- train_tbl %>%
  filter_at(vars(one_of(c(features, "Estimated_fire_area", "area_bin"))), all_vars(!is.na(.)))

test_tbl <- test_tbl %>%
  filter_at(vars(one_of(c(features, "Estimated_fire_area", "area_bin"))), all_vars(!is.na(.)))

# ============================
# 🔷 1. Linear Regression
# ============================
lr_model <- ml_linear_regression(
  train_tbl,
  response = "Estimated_fire_area",
  features = features
)

pred_lr <- ml_predict(lr_model, test_tbl) %>% collect()

rmse_lr <- sqrt(mean((pred_lr$Estimated_fire_area - pred_lr$prediction)^2))
mae_lr  <- mean(abs(pred_lr$Estimated_fire_area - pred_lr$prediction))
r2_lr   <- 1 - sum((pred_lr$Estimated_fire_area - pred_lr$prediction)^2) /
  sum((pred_lr$Estimated_fire_area - mean(pred_lr$Estimated_fire_area))^2)

cat("\n📊 Linear Regression:\n")
cat("RMSE:", round(rmse_lr, 2), "| MAE:", round(mae_lr, 2), "| R²:", round(r2_lr, 3), "\n")

# ============================
# 🔷 2. Decision Tree Regression
# ============================
dt_model <- ml_decision_tree_regressor(
  train_tbl,
  response = "Estimated_fire_area",
  features = features
)

pred_dt <- ml_predict(dt_model, test_tbl) %>% collect()

rmse_dt <- sqrt(mean((pred_dt$Estimated_fire_area - pred_dt$prediction)^2))
mae_dt  <- mean(abs(pred_dt$Estimated_fire_area - pred_dt$prediction))
r2_dt   <- 1 - sum((pred_dt$Estimated_fire_area - pred_dt$prediction)^2) /
  sum((pred_dt$Estimated_fire_area - mean(pred_dt$Estimated_fire_area))^2)

cat("\n🌳 Decision Tree Regression:\n")
cat("RMSE:", round(rmse_dt, 2), "| MAE:", round(mae_dt, 2), "| R²:", round(r2_dt, 3), "\n")

# ============================
# 🔷 3. Random Forest Classification
# ============================
rf_model <- ml_random_forest_classifier(
  train_tbl,
  response = "area_bin",
  features = features,
  num_trees = 100
)

pred_rf <- ml_predict(rf_model, test_tbl) %>% collect()
conf_matrix_rf <- table(pred_rf$area_bin, pred_rf$prediction)
acc_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)

cat("\n🌲 Random Forest Classification (area_bin):\n")
cat("Accuracy:", round(acc_rf, 3), "\n")

# ============================
# 🔷 4. Logistic Regression (Classification)
# ============================
log_model <- ml_logistic_regression(
  train_tbl,
  response = "area_bin",
  features = features,
  max_iter = 20
)

pred_log <- ml_predict(log_model, test_tbl) %>% collect()
conf_log <- table(pred_log$area_bin, pred_log$prediction)
acc_log <- sum(diag(conf_log)) / sum(conf_log)

cat("\n📦 Logistic Regression (area_bin):\n")
cat("Accuracy:", round(acc_log, 3), "\n")

# ============================
# 📌 Disconnect
# ============================
spark_disconnect(sc)
