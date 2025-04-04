source("R/load_and_clean.R")

# 2. Izlušči samo numerične spremenljivke
numeric_cols <- sapply(combined_data_clean, is.numeric)
combined_numeric <- combined_data_clean[, numeric_cols]


# 3. Odstrani stolpce s standardnim odklonom 0 (nič variacije = neuporabno za PCA)
sds <- apply(combined_numeric, 2, sd, na.rm = TRUE)
combined_numeric_filtered <- combined_numeric[, sds > 0]


# 4. Odstrani vrstice z NA in standardiziraj podatke
scaled_data <- scale(na.omit(combined_numeric_filtered))

# 5. Izvedi PCA
pca_result <- prcomp(scaled_data)


summary(pca_result)  # Pokaže koliko variance pojasni vsaka komponenta
plot(pca_result)     # Vizualni prikaz eigenvalue-jev (scree plot)

# 6. Scree plot z % variance
pca_var <- pca_result$sdev^2
pve <- pca_var / sum(pca_var)  # Proportion of variance explained

# Barplot z dodano kumulativno črto
barplot(pve, 
        main = "Scree plot - % pojasnjene variance",
        ylab = "Proportion of Variance Explained",
        xlab = "Principal Component",
        col = "lightblue")

lines(x = 1:length(pve), y = cumsum(pve), type = "b", col = "red", pch = 19)




var_explained <- summary(pca_result)$importance[2, ]
cum_var <- summary(pca_result)$importance[3, ]

barplot(cum_var[1:15], main = "Kumulativna pojasnjena varianca", 
        xlab = "Število komponent", ylab = "Kumulativna varianca", col = "steelblue")
abline(h = 0.80, col = "red", lty = 2)  # prag 80%



loadings <- pca_result$rotation  # matrika vplivov (loadingov)
abs_pc1 <- sort(abs(loadings[, 1]), decreasing = TRUE)[1:10]
abs_pc2 <- sort(abs(loadings[, 2]), decreasing = TRUE)[1:10]

print("Top 10 spremenljivk za PC1:")
print(names(abs_pc1))

print("Top 10 spremenljivk za PC2:")
print(names(abs_pc2))




library(ggplot2)

pca_df <- data.frame(pca_result$x[, 1:2])  # prvih 2 komponenti
pca_df$cluster <- kmeans(pca_df[, 1:2], centers = 3)$cluster  # izberi število skupin

ggplot(pca_df, aes(x = PC1, y = PC2, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means na PCA komponentah")


