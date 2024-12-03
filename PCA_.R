## Run PCA  I FUCKED IT UP
library(PCAtest)
library(mvnormtest)
library(psych)
library(vegan)
library(dplyr)
library(sf)

cewa_bio <- readRDS("Cerulean_Warbler/cewa_biovars.RDS")
cewa_bio <- cewa_bio[, 4:23]
# Separate geometry into lat/long
cewa_bio$geometry <- NULL

cewa_bio <- cewa_bio %>%
  rename(annual_mean_temp = wc2.1_10m_bio_1, mean_diurnal_range = wc2.1_10m_bio_2,
         isothermality = wc2.1_10m_bio_3, temp_seasonality = wc2.1_10m_bio_4,
         max_temp_warmest_month = wc2.1_10m_bio_5, min_temp_coldest_month = wc2.1_10m_bio_6,
         temp_annual_range = wc2.1_10m_bio_7, mean_temp_wettest_quart = wc2.1_10m_bio_8,
         mean_temp_driest_quart = wc2.1_10m_bio_9, mean_temp_warmest_quart = wc2.1_10m_bio_10,
         mean_temp_coldest_quart = wc2.1_10m_bio_11, annual_percip = wc2.1_10m_bio_12, 
         percip_wettest_month = wc2.1_10m_bio_13, percip_driest_month = wc2.1_10m_bio_14,
         percip_season = wc2.1_10m_bio_15, percip_wettest_quart = wc2.1_10m_bio_16,
         percip_driest_quart = wc2.1_10m_bio_17, percip_warmest_quart = wc2.1_10m_bio_18,
         percip_coldest_quart = wc2.1_10m_bio_19)

saveRDS(cewa_bio, "Cerulean_Warbler/cewa_bio_climate.RDS")

cewa_bio <- readRDS("Cerulean_Warbler/cewa_bio_climate.RDS")
cewa_bio$sampling.event.identifier <- NULL
cewa_bio_clean <- cewa_bio[complete.cases(cewa_bio) & !is.infinite(rowSums(cewa_bio)), ]

zcewa <- scale(cewa_bio_clean)

# Run PCA on the cleaned data
cewa_bio_pca2 <- prcomp(zcewa, scale. = FALSE)
cewa_bio_pca <- princomp(zcewa, cor = F)

summary(cewa_bio_pca)
summary(cewa_bio_pca2)

# eigenvalues
eigenVal <- (cewa_bio_pca$sdev * sqrt(173765/173764))^2
eigenVal2 <- (cewa_bio_pca2$sdev)^2

propVar <- eigenVal/sum(eigenVal)
cumVar <- cumsum(propVar)
pca_Table <- t(rbind(eigenVal, propVar, cumVar))
pca_Table

propVar <- eigenVal2/sum(eigenVal2)
cumVar <- cumsum(propVar)
pca_Table2 <- t(rbind(eigenVal, propVar, cumVar))
pca_Table2

loadings(cewa_bio_pca) #min_temp_coldest_month (0.283) and percip_wettest_month (0.283)
cewa_bio_pca2$rotation #temp_seasonality (0.285) and temp_annual_range (0.283)

scores(cewa_bio_pca)
cewa_bio_pca2$x

#scree plot
plot(cewa_bio_pca, type = "lines") #keep two

set.seed(1)
perm_results <- PCAtest(zcewa, 1000, 1000, 0.05, varcorr = FALSE,
                        counter = FALSE, plot = TRUE)

plot(cewa_bio_pca$loadings, type = "n", xlab = "PC 1, 59%", ylab = "PC 2, 17%", 
     ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5))
text(cewa_bio_pca$loadings, labels = as.character(colnames(zcewa)), pos = 1, cex = 1)

plot(cewa_bio_pca$scores, type = "n", xlab = "PC 1, 59%", ylab = "PC 2, 17%", 
     ylim = c(-9, 8), xlim = c(-6, 20))
text(cewa_bio_pca$scores, labels = as.character(rownames(zcewa)), pos = 1, cex = 1)

biplot(cewa_bio_pca$scores, cewa_bio_pca$loading, xlab = "PC 1, 59%", ylab = "PC 2, 17%",
       expand = 4, ylim = c(-25, 30), xlim = c(-30, 30))
biplot(cewa_bio_pca$scores, cewa_bio_pca$loading, expand = 4, xlabs = rep("*", 173765), 
       xlab = "PC 1, 59%", ylab = "PC 2, 17%", ylim = c(-25, 30), xlim = c(-30, 30))

