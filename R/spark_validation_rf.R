
# spark_validation_rf.R
# 🔍 5-fold Cross-Validation for Random Forest Classifier (area_bin)

library(sparklyr)
library(dplyr)

# 1. Connect to Spark
config <- spark_config()
config$spark.driver.memory <- "2g"
sc <- spark_connect(master = "local[2]", version = "3.4.1", config = config)

# 2. Load dataset
spark_df <- spark_read_csv(
  sc,
  name = "firesample",
  path = "D:/FAKS/MAG 2. letnik/LSDM/sampled_data_stratified.csv",
  infer_schema = TRUE,
  header = TRUE
)

# 3. Prepare features
features <- setdiff(
  colnames(spark_df),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# 4. Cast features to numeric
for (col in features) {
  spark_df <- spark_df %>% mutate(!!col := as.numeric(!!sym(col)))
}

# 5. Convert area_bin to string for classification
spark_df <- spark_df %>%
  mutate(area_bin = cast(area_bin %as% "string"))

# 6. Remove rows with NA
spark_df <- spark_df %>%
  filter_at(vars(one_of(c(features, "area_bin"))), all_vars(!is.na(.)))

# 7. Define ML pipeline
rf_pipeline <- ml_pipeline(sc) %>%
  ft_vector_assembler(input_cols = features, output_col = "features") %>%
  ml_random_forest_classifier(
    label_col = "area_bin",
    features_col = "features",
    prediction_col = "prediction",
    num_trees = 100
  )

# 8. Perform 5-fold Cross-Validation
cv <- ml_cross_validator(
  rf_pipeline,
  estimator_param_maps = list(),
  evaluator = ml_multiclass_classification_evaluator(sc, label_col = "area_bin", prediction_col = "prediction", metric_name = "accuracy"),
  num_folds = 5,
  parallelism = 2,
  seed = 42
)

cv_model <- ml_fit(cv, spark_df)

# 9. Evaluate on full dataset
predictions <- ml_predict(cv_model, spark_df) %>% collect()
conf_matrix <- table(predictions$area_bin, predictions$prediction)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat("\n🌲 Random Forest 5-fold CV Results:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
print(conf_matrix)

# 10. Disconnect Spark
spark_disconnect(sc)
