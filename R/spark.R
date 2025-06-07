spark_df <- spark_read_csv(
  sc,
  name = "firesample",
  path = "sampled_data_stratified.csv",
  infer_schema = TRUE,
  header = TRUE
)

splits <- sdf_random_split(spark_df, train = 0.7, test = 0.3, seed = 42)
train_tbl <- splits$train
test_tbl <- splits$test

# Define the features you'll use for modeling (as before)
features <- setdiff(
  colnames(train_tbl),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# REMOVE ROWS WITH NA in any feature or target for train and test sets
train_tbl_clean <- train_tbl %>%
  filter_at(vars(one_of(c(features, "Estimated_fire_area"))), all_vars(!is.na(.)))

test_tbl_clean <- test_tbl %>%
  filter_at(vars(one_of(c(features, "Estimated_fire_area"))), all_vars(!is.na(.)))

# Check counts:
train_tbl %>% dplyr::count()
test_tbl %>% dplyr::count()

# (Optional: Check schema and preview data)
# sdf_schema(spark_df)
# sdf_nrow(spark_df)
train_tbl %>% head(5) %>% collect()
