# Load pacakages
library(vegan)
library(ca)
library(ggplot2)
library(dplyr)
library(sf)

# Read in data and rename columns (biovars)
cewa_bio <- readRDS("Cerulean_Warbler/cewa_biovars.RDS")
cewa_bio <- cewa_bio %>%
  rename(
    year = YEAR, 
    annual_mean_temp = wc2.1_10m_bio_1, 
    mean_diurnal_range = wc2.1_10m_bio_2,
    isothermality = wc2.1_10m_bio_3, 
    temp_seasonality = wc2.1_10m_bio_4,
    max_temp_warmest_month = wc2.1_10m_bio_5, 
    min_temp_coldest_month = wc2.1_10m_bio_6,
    temp_annual_range = wc2.1_10m_bio_7, 
    mean_temp_wettest_quart = wc2.1_10m_bio_8,
    mean_temp_driest_quart = wc2.1_10m_bio_9, 
    mean_temp_warmest_quart = wc2.1_10m_bio_10,
    mean_temp_coldest_quart = wc2.1_10m_bio_11, 
    annual_percip = wc2.1_10m_bio_12, 
    percip_wettest_month = wc2.1_10m_bio_13, 
    percip_driest_month = wc2.1_10m_bio_14,
    percip_season = wc2.1_10m_bio_15, 
    percip_wettest_quart = wc2.1_10m_bio_16,
    percip_driest_quart = wc2.1_10m_bio_17, 
    percip_warmest_quart = wc2.1_10m_bio_18,
    percip_coldest_quart = wc2.1_10m_bio_19
  )

# Separate geometry into lat/long
cewa_bio <- cewa_bio %>%
  mutate(
    long = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

# Remove rows with missing values and omit SEI and geometry columns
cewa_bio_clean_no_na <- na.omit(cewa_bio)
cewa_bio_clean_no_na$SAMPLING.EVENT.IDENTIFIER <- NULL
cewa_bio_clean_no_na$geometry <- NULL

# Z-scaling the WorldClim variables, excluding first and last two columns 
zcewa <- cewa_bio_clean_no_na %>%
  mutate(across(
    .cols = -(c(1:2, (ncol(.) - 1):(ncol(.) - 2))),  
    .fns = ~ (.-mean(.)) / sd(.))) %>%
  dplyr::select(1:2, everything(), (ncol(.) - 1):(ncol(.) - 2))

# For reproducibility
set.seed(33)

# Set the sample size (100 rows per year)
sample_size <- 100

# Group by year and randomly sample 100 rows from each year
zcewa_sampled <- zcewa %>%
  group_by(year) %>%
  filter(n() >= sample_size) %>%
  slice_sample(n = sample_size, replace = FALSE) %>%
  ungroup()

# Exclude first two and last two columns using select
zscaled_subset <- zcewa_sampled %>%
  dplyr::select(-c(1:2, (ncol(zcewa_sampled)-1):ncol(zcewa_sampled))) 

# Perform PCA on the selected z-scaled variables
pca_result <- prcomp(zscaled_subset, scale. = FALSE)

# Select the first few principal components (e.g., first 10 PCs)
cewa_bio_pca <- pca_result$x[, 1:10]

# Display summary of PCA results
summary(cewa_bio_pca)

# Shift data to ensure no negative values
pca_shifted <- abs(min(cewa_bio_pca, na.rm = TRUE))
cewa_pca_shifted <- cewa_bio_pca + pca_shifted

# Now calculate Bray-Curtis dissimilarity
jcewa_bray <- vegdist(cewa_pca_shifted, method = "bray")

# Perform cmd
cmd <- cmdscale(jcewa_bray, k = 5, eig = TRUE)
summary(cmd)

# Extract eigenvalues and proportions
eigenvalues <- cmd$eig[1:5]
propVar <- eigenvalues / sum(eigenvalues)
cumVar <- cumsum(propVar)

# Create a table with eigenvalues, proportion of variance, and cumulative variance
PCoA_Table <- cbind(eigenvalues, propVar, cumVar)

# Print the PCoA table
PCoA_Table

# Plot the eigenvalues (scree plot)
plot(eigenvalues)
lines(lowess(eigenvalues))

# Plot the results of PCoA
ordiplot(scores(cmd)[, c(1, 2)], type = "n", cex = 1, main = "Cerulean Warbler PCoA")
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
points(scores(cmd)[, 1], scores(cmd)[, 2], pch = 8, col = "black", cex = 1)
# Add year
year <- wascores(cmd$points[, 1:2], cewa_pca_shifted)
text(year, rownames(year), cex = 0.7, col = "green")



