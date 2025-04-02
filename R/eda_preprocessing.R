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


