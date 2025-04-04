# eda_preprocessing.R

source("R/load_and_clean.R")  # naloži podatke in jih očisti

# Naloži potrebne knjižnice
library(corrplot)

# 1. Preglej strukturo in povzetek podatkov
str(combined_data_clean)
summary(combined_data_clean)

# 2. Izlušči numerične stolpce
numeric_cols <- sapply(combined_data_clean, is.numeric)
combined_numeric <- combined_data_clean[, numeric_cols]

# 3. Odstrani stolpce s standardnim odklonom 0 (konstante)
sds <- apply(combined_numeric, 2, sd, na.rm = TRUE)
combined_numeric_filtered <- combined_numeric[, sds > 0]

# 4. Korelacijska matrika (Pearson)
cor_matrix <- cor(combined_numeric_filtered, use = "complete.obs")

# 5. Prikaži korelacijsko matriko
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7)

# 6. Korelacija s ciljno spremenljivko
cor_target <- cor_matrix[, "Estimated_fire_area"]
print(sort(cor_target, decreasing = TRUE))


# Izračun korelacij
cor_matrix <- cor(combined_numeric_filtered, use = "complete.obs")

# Vzemi vrstico Estimated_fire_area
target_cor <- cor_matrix["Estimated_fire_area", ]

# Odstrani korelacijo samega s sabo
target_cor <- target_cor[names(target_cor) != "Estimated_fire_area"]

# Top 5 po absolutni korelaciji
top5_features <- sort(abs(target_cor), decreasing = TRUE)[1:5]

# Izpiši z originalnimi vrednostmi (pozitivne/negativne korelacije)
top5_original <- target_cor[names(top5_features)]

# Izpis
print(round(top5_original, 3))


# 7. Histogrami
hist(combined_data_clean$Estimated_fire_area,
     breaks = 50,
     main = "Histogram: Estimated Fire Area",
     xlab = "Area (km²)",
     col = "lightblue")

hist(log1p(combined_data_clean$Estimated_fire_area),
     breaks = 50,
     main = "Histogram (log1p): Estimated Fire Area",
     xlab = "log(1 + Area)",
     col = "lightgreen")

# 8. Boxploti
boxplot(combined_data_clean$Estimated_fire_area,
        main = "Boxplot: Estimated Fire Area",
        ylab = "Area (km²)")

boxplot(log1p(combined_data_clean$Estimated_fire_area),
        main = "Boxplot (log1p): Estimated Fire Area",
        ylab = "log(1 + Area)")

boxplot(combined_data_clean$mean_val_Temperature.x,
        main = "Boxplot: Avg Temperature",
        ylab = "°C")

boxplot(combined_data_clean$mean_val_Precipitation.x,
        main = "Boxplot: Avg Precipitation",
        ylab = "mm")

# 9. Histogrami za izbrane spremenljivke
hist(combined_data_clean$Mean_estimated_fire_brightness, breaks = 50, main = "Brightness", col = "lightblue")
hist(combined_data_clean$Mean_estimated_fire_radiative_power, breaks = 50, main = "Radiative Power", col = "lightblue")
hist(combined_data_clean$mean_val_Temperature.x, breaks = 50, main = "Avg Temperature", col = "orange")

# 10. Barplot po regijah
barplot(table(combined_data_clean$Region), main = "Požari po regijah", las = 2, col = "salmon")

# (Neobvezno) shrani slike za poročilo z ggsave() če uporabljaš ggplot2
