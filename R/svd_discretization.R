library(dplyr)
library(ggplot2)
library(FSelectorRcpp)
library(FSelector)
library(infotheo)
library(discretization)

source("R/load_and_clean.R")

combined_data_clean <- combined_data %>%
  select(where(is.numeric)) %>%
  filter(if_all(everything(), ~ is.finite(.) & !is.na(.)))

threshold <- median(combined_data_clean$Estimated_fire_area, na.rm = TRUE)
class_column <- as.factor(ifelse(combined_data_clean$Estimated_fire_area > threshold, 1, 0))

mdlp_input <- combined_data_clean %>%
  select(-Estimated_fire_area) %>%
  select(where(~ var(., na.rm = TRUE) > 0))

mdlp_input <- as.data.frame(mdlp_input)
mdlp_input$Class <- class_column

mdlp_result <- mdlp(mdlp_input)

combined_data_discretized_supervised <- mdlp_result$Disc.data
write.csv(combined_data_discretized_supervised, "supervised_discretized_data.csv", row.names = FALSE)
