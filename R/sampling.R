# sampling.R – Stratificirani vzorec za regresijo Estimated_fire_area

# 1. Naloži knjižnice
library(dplyr)
library(readr)
library(ggplot2)

# 2. Naloži podatke
combined_data <- read_csv("combined_data_clean.csv")

# 3. Ustvari stratifikacijske razrede na podlagi kvantilov Estimated_fire_area
combined_data <- combined_data %>%
  mutate(area_bin = ntile(Estimated_fire_area, 5))

# 4. Preveri razporeditev primerov po binih
table(combined_data$area_bin)

# 5. Stratificiran sampling: 100 primerov iz vsakega kvintila
set.seed(42)
sampled_data <- combined_data %>%
  group_by(area_bin) %>%
  sample_n(size = 100) %>%
  ungroup()

# 6. Primerjava porazdelitev pred in po vzorčenju
p1 <- ggplot(combined_data, aes(x = Estimated_fire_area)) +
  geom_histogram(bins = 50, fill = "lightblue") +
  ggtitle("Originalna porazdelitev")

p2 <- ggplot(sampled_data, aes(x = Estimated_fire_area)) +
  geom_histogram(bins = 50, fill = "salmon") +
  ggtitle("Stratificirani vzorec")

print(p1); print(p2)

# 7. Shrani rezultate
write_csv(sampled_data, "sampled_data_stratified.csv")
