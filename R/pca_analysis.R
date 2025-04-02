source("R/load_and_clean.R")

numeric_cols <- sapply(combined_data_clean, is.numeric)
combined_numeric <- combined_data_clean[, numeric_cols]



str(combined_numeric)
