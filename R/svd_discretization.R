library(dplyr)
library(ggplot2)
library(FSelectorRcpp)
library(FSelector)
library(arules)
library(infotheo)

source("R/load_and_clean.R")

combined_data_clean <- combined_data %>%
  select(where(is.numeric)) %>%
  filter(if_all(everything(), ~ !is.na(.) & is.finite(.)))

variance_values <- apply(combined_data_clean, 2, function(x) var(x, na.rm = TRUE))
mean_median_diff <- apply(combined_data_clean, 2, function(x) mean(x, na.rm = TRUE) - median(x, na.rm = TRUE))

variance_df <- data.frame(Feature = names(variance_values), Variance = variance_values)

fisher_scores <- information.gain(Estimated_fire_area ~ ., data = combined_data_clean)
fisher_df <- data.frame(Feature = rownames(fisher_scores), Score = fisher_scores$attr_importance)

variance_plot <- ggplot(variance_df, aes(x = reorder(Feature, Variance), y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal()

ggsave("variance_plot.png", plot = variance_plot, width = 8, height = 6)

fisher_plot <- ggplot(fisher_df, aes(x = reorder(Feature, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  theme_minimal()

ggsave("fisher_plot.png", plot = fisher_plot, width = 8, height = 6)

non_constant_cols <- combined_data_clean %>%
  select(where(~ var(., na.rm = TRUE) > 0))

scaled_data <- scale(non_constant_cols)
scaled_data_clean <- scaled_data[apply(scaled_data, 1, function(row) all(is.finite(row))), ]

svd_result <- svd(scaled_data_clean)
