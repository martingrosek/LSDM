# sampling_spark.R

library(sparklyr)
library(dplyr)


#spark_install(version = "3.4.1")




options(sparklyr.log.console = TRUE)
sc <- spark_connect(
  master  = "local[*]",
  version = "3.4.1"
)


spark_df <- spark_read_csv(
  sc,
  name         = "firesample",
  path         = "sampled_data_stratified.csv",
  infer_schema = TRUE,
  header       = TRUE
)


# Najprej določimo imena numeric stolpcev
features <- setdiff(
  colnames(spark_df),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# Pretvorba v numeric
spark_df <- spark_df %>%
  mutate(across(all_of(features), ~ as.numeric(.)))




splits     <- sdf_random_split(spark_df, train = 0.7, test = 0.3, seed = 42)
train_tbl  <- splits$train
test_tbl   <- splits$test


features <- setdiff(
  colnames(train_tbl),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)


train_tbl_clean <- train_tbl %>%
  filter(across(any_of(c(features, "Estimated_fire_area")), ~ !is.na(.)))

test_tbl_clean <- test_tbl %>%
  filter(across(any_of(c(features, "Estimated_fire_area")), ~ !is.na(.)))


cat("Training set size:\n")
train_tbl_clean %>% count() %>% print()
cat("Test set size:\n")
test_tbl_clean  %>% count() %>% print()

# 10. Preglej prvih 5 vrstic učne množice
cat("Preview of training set:\n")
train_tbl_clean %>% head(5) %>% collect() %>% print()

# Ko končaš, lahko Spark sejo zapreš:
# spark_disconnect(sc)
