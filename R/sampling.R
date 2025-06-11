# sampling.R – Stratificirani vzorec za regresijo Estimated_fire_area


library(dplyr)
library(readr)
library(ggplot2)


combined_data <- read_csv("combined_data_clean.csv")


combined_data <- combined_data %>%
  mutate(area_bin = ntile(Estimated_fire_area, 5))


table(combined_data$area_bin)


set.seed(42)
sampled_data <- combined_data %>%
  group_by(area_bin) %>%
  sample_n(size = 100) %>%
  ungroup()


p1 <- ggplot(combined_data, aes(x = Estimated_fire_area)) +
  geom_histogram(bins = 50, fill = "lightblue") +
  ggtitle("Originalna porazdelitev")

p2 <- ggplot(sampled_data, aes(x = Estimated_fire_area)) +
  geom_histogram(bins = 50, fill = "salmon") +
  ggtitle("Stratificirani vzorec")

print(p1); print(p2)


write_csv(sampled_data, "sampled_data_stratified.csv")
