# pca_analysis.R

# 1. Load cleaned data
source("R/load_and_clean.R")

# 2. Extract numeric columns and remove constant variables
#PCA only works on numerical data we selected only numerical variables from the data frame.
numeric_cols <- sapply(combined_data_clean, is.numeric)
combined_numeric <- combined_data_clean[, numeric_cols]


#Variables with 0 standard deviation (constants) do not contribute to the variance
#we remove them – they are irrelevant to PCA.
sds <- apply(combined_numeric, 2, sd, na.rm = TRUE)
filtered_data <- combined_numeric[, sds > 0]

#PCA cannot work with NA, so we only kept rows without missing values.
rows_no_na <- complete.cases(filtered_data)
data_for_pca <- filtered_data[rows_no_na, ]

# 3. Standardize data. 
#to convert all numerical variables to a common scale (mean 0 and standard deviation 1), 
#PCA is based on a covariance or correlation matrix - without standardization, variables with a larger range 
#( temperature in °C vs. number of pixels) would disproportionately affect the principal components results.
scaled_data <- scale(data_for_pca)

# 4. PCA
#We used the prcomp() function, which performs principal component analysis (PCA) on standardized data.
#goal of PCA is to find new independent axes (components) that best explain the variance in the data 
#while reducing dimensionality and eliminating redundancy between variables.
pca_result <- prcomp(scaled_data)

# 5. Variance calculations
#tells how much of the variance in the data is explained by each principal component.
pve <- (pca_result$sdev)^2 / sum(pca_result$sdev^2)

#the sum of pve, which shows how much of the total variance is explained if we use the first n components
cum_var <- cumsum(pve)

# 6. Top 10 variables for PC1 and PC2
loadings <- pca_result$rotation
abs_pc1 <- sort(abs(loadings[, 1]), decreasing = TRUE)[1:10]
abs_pc2 <- sort(abs(loadings[, 2]), decreasing = TRUE)[1:10]

cat("Top 10 variables for PC1:\n")
print(names(abs_pc1))

cat("\nTop 10 variables for PC2:\n")
print(names(abs_pc2))

# 7. Barplot of cumulative variance for first 15 PCs
par(mar = c(7, 4, 4, 2))  # extend bottom margin
barplot(cum_var[1:15], 
        main = "Cumulative Explained Variance", 
        ylab = "Cumulative Variance", 
        col = "steelblue",
        names.arg = paste0("PC", 1:15), las = 2)
abline(h = 0.80, col = "red", lty = 2)

# 8. Function to find number of PCs needed
#it determines how many principal components are sufficient to capture 80%, 85%, or 90% of the total variance in the data.
find_components_for_threshold <- function(cum_var, threshold) {
  which(cum_var >= threshold)[1]
}

cat("Number of components for 80% variance: ", find_components_for_threshold(cum_var, 0.80), "\n")
cat("Number of components for 85% variance: ", find_components_for_threshold(cum_var, 0.85), "\n")
cat("Number of components for 90% variance: ", find_components_for_threshold(cum_var, 0.90), "\n")


plot(cum_var,
     type = "b",
     pch = 19,
     col = "darkgreen",
     xlab = "Number of Principal Components",
     ylab = "Cumulative Explained Variance",
     main = "Cumulative Variance Explained")

abline(h = 0.80, col = "red", lty = 2)
abline(h = 0.85, col = "orange", lty = 2)
abline(h = 0.90, col = "blue", lty = 2)

abline(v = find_components_for_threshold(cum_var, 0.80), col = "red", lty = 3)
abline(v = find_components_for_threshold(cum_var, 0.85), col = "orange", lty = 3)
abline(v = find_components_for_threshold(cum_var, 0.90), col = "blue", lty = 3)

legend("bottomright",
       legend = c("80% threshold", "85% threshold", "90% threshold"),
       col = c("red", "orange", "blue"),
       lty = 2,
       bty = "n")


save(pca_result, rows_no_na, file = "pca_output.RData")
save(combined_data_clean, filtered_data, rows_no_na,
     file = "data/final_data.RData")

