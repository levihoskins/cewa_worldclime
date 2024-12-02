# PCoA
library(vegan)
library(ca)
library(ggplot2)
library(dplyr)
cewa_bio <- readRDS("Cerulean_Warbler/cewa_biovars.RDS")
cewa_bio <- cewa_bio %>%
  rename(year = YEAR, annual_mean_temp = wc2.1_10m_bio_1, mean_diurnal_range = wc2.1_10m_bio_2,
         isothermality = wc2.1_10m_bio_3, temp_seasonality = wc2.1_10m_bio_4,
         max_temp_warmest_month = wc2.1_10m_bio_5, min_temp_coldest_month = wc2.1_10m_bio_6,
         temp_annual_range = wc2.1_10m_bio_7, mean_temp_wettest_quart = wc2.1_10m_bio_8,
         mean_temp_driest_quart = wc2.1_10m_bio_9, mean_temp_warmest_quart = wc2.1_10m_bio_10,
         mean_temp_coldest_quart = wc2.1_10m_bio_11, annual_percip = wc2.1_10m_bio_12, 
         percip_wettest_month = wc2.1_10m_bio_13, percip_driest_month = wc2.1_10m_bio_14,
         percip_season = wc2.1_10m_bio_15, percip_wettest_quart = wc2.1_10m_bio_16,
         percip_driest_quart = wc2.1_10m_bio_17, percip_warmest_quart = wc2.1_10m_bio_18,
         percip_coldest_quart = wc2.1_10m_bio_19)
cewa_bio_clean <- cewa_bio %>%
  group_by(year, geometry)

data_with_presence_absence <- cewa_bio_clean %>%
  group_by(year, geometry, SAMPLING.EVENT.IDENTIFIER) %>%
  mutate(Presence = 1) %>%  # Add Presence column without dropping any rows
  ungroup()  # Remove grouping after the mutate

# Separate geometry into lat/long
data_with_presence_absence <- data_with_presence_absence %>%
  mutate(long = st_coordinates(.)[, 1],  # Extract longitude
         lat = st_coordinates(.)[, 2]) #Extract latitude

# Remove rows with missing values
cewa_bio_clean_no_na <- na.omit(data_with_presence_absence)
cewa_bio_clean_no_na$SAMPLING.EVENT.IDENTIFIER <- NULL
cewa_bio_clean_no_na$geometry <- NULL

# Shift all values by adding the absolute value of the minimum value (if any)
shift_value <- abs(min(cewa_bio_clean_no_na, na.rm = TRUE))
cewa_bio_clean_no_na_shifted <- cewa_bio_clean_no_na + shift_value

# Perform PCA on the data (excluding categorical variables if any)
pca_result <- prcomp(cewa_bio_clean_no_na_shifted, scale. = TRUE)

# Select the first few principal components (e.g., first 10 PCs)
cewa_bio_pca <- pca_result$x[, 1:10]

summary(cewa_bio_pca)

# Compute Bray-Curtis dissimilarity on the shifted data
jcewa_euclidean <- vegdist(cewa_bio_pca, method = "euclidean")

cmd <- cmdscale(jcewa_shifted, k = 5, eig = TRUE)
cmd$points

eigenvalues <- cmd$eig[1:5]
propVar <- eigenvalues/sum(eigenvalues)
cumVar <- cumsum(propVar)
PCoA_Table <- cbind(eigenvalues, propVar, cumVar)
PCoA_Table

plot(eigenvalues)
lines(lowess(eigenvalues))

x <- cmd$points[, 1]
y <- cmd$points[, 2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", xlim =
       range(x) * 1.2, ylim = range(y) *
       1.2, type = "n")
text(x, y, labels = rownames(cmd$points), cex = 0.9)




cewa_bio_clean <- cewa_bio
cewa_bio_numeric <- cewa_bio_clean[sapply(cewa_bio_clean, is.numeric)]
jcewa <- vegdist(cewa_bio_numeric, "bray")
set.seed(123)  # For reproducibility
cewa_bio_sampled <- cewa_bio_numeric[sample(1:nrow(cewa_bio_numeric), 10000), ]
jcewa <- vegdist(cewa_bio_sampled, method = "bray")

dist_matrix <- vegdist(jcewa)
pcoa_results <- cmdscale(dist_matrix, eig = TRUE)

# Plot the results
coords <- pcoa_result$vectors
plot(coords[, 1], coords[, 2], xlab = "PCoA 1", ylab = "PCoA 2", pch = 19)




