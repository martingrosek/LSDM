# sampling_spark.R

# 1. Naloži potrebne knjižnice
library(sparklyr)
library(dplyr)

# 2. Namesti npr. Spark 3.4.0 (če je na voljo):
#spark_install(version = "3.4.0")

# 3. Ko se namestitev zaključi, ponovno vzpostavi povezavo na to verzijo:
options(sparklyr.log.console = TRUE)
sc <- spark_connect(
  master  = "local[*]",
  version = "3.4.0"
)

# 4. Poveži se na Spark 3.4.1
options(sparklyr.log.console = TRUE)
sc <- spark_connect(
  master  = "local[*]",
  version = "3.4.1"
)

# 5. Uvozi CSV kot Spark DataFrame
spark_df <- spark_read_csv(
  sc,
  name         = "firesample",
  path         = "sampled_data_stratified.csv",
  infer_schema = TRUE,
  header       = TRUE
)

# 6. Stratificiran split: 70% učno, 30% testno
splits     <- sdf_random_split(spark_df, train = 0.7, test = 0.3, seed = 42)
train_tbl  <- splits$train
test_tbl   <- splits$test

# 7. Definicija feature-jev (izpusti tarčo in meta-stolpce)
features <- setdiff(
  colnames(train_tbl),
  c("Estimated_fire_area", "log_estimated_fire_area", "Region", "Date", "area_bin", "Replaced")
)

# 8. Odstrani vrstice z NA vrednostmi v feature-jih ali tarči
train_tbl_clean <- train_tbl %>%
  filter(across(any_of(c(features, "Estimated_fire_area")), ~ !is.na(.)))

test_tbl_clean <- test_tbl %>%
  filter(across(any_of(c(features, "Estimated_fire_area")), ~ !is.na(.)))

# 9. Izpiši število primerov
cat("Training set size:\n")
train_tbl_clean %>% count() %>% print()
cat("Test set size:\n")
test_tbl_clean  %>% count() %>% print()

# 10. Preglej prvih 5 vrstic učne množice
cat("Preview of training set:\n")
train_tbl_clean %>% head(5) %>% collect() %>% print()

# Ko končaš, lahko Spark sejo zapreš:
# spark_disconnect(sc)
