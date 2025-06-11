# spark_linear_regression.R

library(sparklyr)
library(dplyr)

# 1. Connect to Spark
config <- spark_config()
config$spark.driver.memory <- "2g"
sc <- spark_connect(master = "local[2]", version = "3.1.2", config = config)

# 2. Load the data
spark_df <- spark_read_csv(
  sc,
  name = "firesample",
  path = "sampled_data_stratified.csv",
  infer_schema = TRUE,
  header = TRUE
)

# 3. Ensure all feature columns are numeric
numeric_cols <- c(
  "count_km2_Precipitation_x", "count_km2_RelativeHumidity_x", "count_km2_Temperature_x", "count_km2_WindSpeed_x",
  "min_val_Precipitation_x", "min_val_RelativeHumidity_x", "min_val_Temperature_x", "min_val_WindSpeed_x",
  "max_val_Precipitation_x", "max_val_RelativeHumidity_x", "max_val_Temperature_x", "max_val_WindSpeed_x",
  "mean_val_Precipitation_x", "mean_val_RelativeHumidity_x", "mean_val_Temperature_x", "mean_val_WindSpeed_x",
  "var_val_Precipitation_x", "var_val_RelativeHumidity_x", "var_val_Temperature_x", "var_val_WindSpeed_x"
)
for (col in numeric_cols) {
  if (col %in% colnames(spark_df)) {
    spark_df <- spark_df %>% mutate(!!col := as.numeric(!!sym(col)))
  }
}

# 1. After split, define features:
features <- setdiff(
  colnames(train_tbl),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# 2. Remove rows with NA in any used feature or the target
train_tbl_clean <- train_tbl %>%
  filter_at(vars(one_of(c(features, "Estimated_fire_area"))), all_vars(!is.na(.)))

test_tbl_clean <- test_tbl %>%
  filter_at(vars(one_of(c(features, "Estimated_fire_area"))), all_vars(!is.na(.)))

# 5. Feature selection
features <- setdiff(
  colnames(train_tbl),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# 6. Linear regression (Estimated_fire_area)
lr_model <- sparklyr::ml_linear_regression(
  train_tbl,
  response = "Estimated_fire_area",
  features = features
)
predictions <- ml_predict(lr_model, test_tbl) %>% collect()
rmse <- sqrt(mean((predictions$Estimated_fire_area - predictions$prediction)^2))
mae  <- mean(abs(predictions$Estimated_fire_area - predictions$prediction))
sst  <- sum((predictions$Estimated_fire_area - mean(predictions$Estimated_fire_area))^2)
sse  <- sum((predictions$Estimated_fire_area - predictions$prediction)^2)
r2   <- 1 - sse/sst

cat("Spark Linear Regression Results (Estimated_fire_area):\n")
cat("RMSE:", round(rmse, 2), "\nMAE:", round(mae, 2), "\nR²:", round(r2, 3), "\n")

# 7. (Optional) log_estimated_fire_area
if("log_estimated_fire_area" %in% colnames(train_tbl)) {
  lr_model_log <- sparklyr::ml_linear_regression(
    train_tbl,
    response = "log_estimated_fire_area",
    features = features
  )
  pred_log <- ml_predict(lr_model_log, test_tbl) %>% collect()
  pred_log$prediction_real <- expm1(pred_log$prediction)
  pred_log$target_real     <- expm1(pred_log$log_estimated_fire_area)
  
  rmse_log <- sqrt(mean((pred_log$target_real - pred_log$prediction_real)^2))
  mae_log  <- mean(abs(pred_log$target_real - pred_log$prediction_real))
  sst_log  <- sum((pred_log$target_real - mean(pred_log$target_real))^2)
  sse_log  <- sum((pred_log$target_real - pred_log$prediction_real)^2)
  r2_log   <- 1 - sse_log/sst_log
  
  cat("\nSpark Linear Regression Results (log_estimated_fire_area, backtransformed):\n")
  cat("RMSE:", round(rmse_log, 2), "\nMAE:", round(mae_log, 2), "\nR²:", round(r2_log, 3), "\n")
}

# spark_disconnect(sc)
