source("R/load_and_clean.R")



# Preglej strukturo in povzetek podatkov
str(combined_data)
summary(combined_data)

# 1. Izlušči numeric stolpce
numeric_cols <- sapply(combined_data, is.numeric)
combined_numeric <- combined_data[, numeric_cols]

# 2. Odstrani tiste s sd = 0
sds <- apply(combined_numeric, 2, sd, na.rm = TRUE)
combined_numeric_filtered <- combined_numeric[, sds > 0]

# 3. Korelacijska matrika
cor_matrix <- cor(combined_numeric_filtered, use = "complete.obs")

# 4. (Opcijsko) Prikaz korelacij
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7)




hist(combined_data$Estimated_fire_area,
     breaks = 50,
     main = "Histogram: Estimated Fire Area",
     xlab = "Area (km²)",
     col = "lightblue")


boxplot(combined_data$Estimated_fire_area,
        main = "Boxplot: Estimated Fire Area",
        ylab = "Area (km²)")



hist(combined_data_clean$Mean_estimated_fire_brightness, breaks = 50, main = "Brightness", col = "lightblue")
hist(combined_data_clean$Mean_estimated_fire_radiative_power, breaks = 50, main = "Radiative Power", col = "lightblue")
hist(combined_data_clean$mean_val_Temperature.x, breaks = 50, main = "Avg Temperature", col = "orange")

boxplot(combined_data_clean$mean_val_Temperature.x,
        main = "Boxplot: Avg Temperature",
        ylab = "°C")

boxplot(combined_data_clean$mean_val_Precipitation.x,
        main = "Boxplot: Avg Precipitation",
        ylab = "mm")

barplot(table(combined_data_clean$Region), main = "Požari po regijah", las = 2)



# Število manjkajočih vrednosti po stolpcih
na_counts <- colSums(is.na(combined_data))
na_counts[na_counts > 0]

# Odstotek manjkajočih vrednosti po stolpcih
na_percent <- colMeans(is.na(combined_data)) * 100
na_percent[na_percent > 0]



# Odstrani forecast .y stolpce in NDVI (več kot 50–90% manjkajočih)
combined_data_clean <- combined_data %>%
  select(-contains(".y"),
         -"Lead time",
         -starts_with("Vegetation_index"))



# (Neobvezno) ponovno preveri dimenzije
dim(combined_data_clean)

# Preveri NA vrednosti
colSums(is.na(combined_data_clean))


