
# spark_validation.R

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
  path = "D:/FAKS/MAG 2. letnik/LSDM/sampled_data_stratified.csv",
  infer_schema = TRUE,
  header = TRUE
)

# 3. Define features
features <- setdiff(
  colnames(spark_df),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# 4. Convert features to numeric
for (col in features) {
  spark_df <- spark_df %>% mutate(!!col := as.numeric(!!sym(col)))
}

# 5. Prepare classification variable
spark_df <- spark_df %>%
  mutate(area_bin = cast(area_bin %as% "string")) %>%
  filter(!is.na(area_bin))

# 6. Create ML pipeline
assembler <- ft_vector_assembler(sc, input_cols = features, output_col = "features_vec")

# Logistic Regression
logistic <- ml_logistic_regression(sc, features_col = "features_vec", label_col = "area_bin", max_iter = 20)

# Pipeline
pipeline <- ml_pipeline(assembler, logistic)

# Evaluator
evaluator_f1 <- ml_multiclass_classification_evaluator(sc, label_col = "area_bin", prediction_col = "prediction", metric_name = "f1")

# Cross-validation
cv_model <- ml_cross_validator(
  sc,
  estimator = pipeline,
  estimator_param_maps = list(
    logistic = list(max_iter = c(10, 20))
  ),
  evaluator = evaluator_f1,
  estimator_type = "pipeline",
  num_folds = 5,
  parallelism = 2
)

# Fit cross-validation model
cv_fitted <- ml_fit(cv_model, spark_df)

# Evaluate best model
predictions <- ml_transform(cv_fitted, spark_df)
f1_score <- ml_evaluate(evaluator_f1, predictions)

cat("\n📊 5-Fold Cross-Validation – Logistic Regression (F1 score):\n")
cat("F1 Score:", round(f1_score, 3), "\n")

# Disconnect
spark_disconnect(sc)
