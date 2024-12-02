# Levi Hoskins
# Bioclimatic Variables
# R 4.4.1

# load packages
library(dismo)
library(ggplot2)
library(dplyr)
library(readr)
library(raster)
library(sf)
library(sp)
library(geodata)

# load in data
#read in eBird data and join with modified scores for each eBird checklist
cewa_mod <- readRDS("Cerulean_Warbler/dat_raw_CEWA_2000.RDS") %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2001.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2002.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2003.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2004.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2005.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2006.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2007.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2008.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2009.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2010.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2011.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2012.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2013.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2014.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2015.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2016.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2017.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2018.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2019.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2020.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2021.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2022.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2023.RDS")) %>%
  bind_rows(readRDS("Cerulean_Warbler/dat_raw_CEWA_2024.RDS")) 

#convert points to sf object
cewa_sf <- cewa_mod %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER, LONGITUDE, LATITUDE, YEAR) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)

# draw in Bioclimatic variables with 'biovars' function in dismo
bioclim_data <- geodata::worldclim_global(var = "bio", res = 10, path = "Cerulean_Warbler")

# extract bioclimatic variables for each location in eBird data
biovars <- extract(bioclim_data, cewa_sf)

# combine extracted bioclimatic variables with eBird data
cewa_biovars <- cbind(cewa_sf, biovars)

# save RDS
saveRDS(cewa_biovars, "Cerulean_Warbler/cewa_biovars.RDS")

### Run PCA
install.packages("devtools")
devtools::install_github("arleyc/PCAtest")
install.packages("PCAtest")
library(PCAtest)
library(mvnormtest)
library(MVN)
library(MVA)
library(psych)
library(Hmisc)
library(vegan)
library(StatMatch)
library(MASS)

cewa_bio <- readRDS("Cerulean_Warbler/cewa_biovars.RDS")
cewa_bio <- cewa_bio[, 4:23]
cewa_bio$geometry <- NULL

# Remove rows with NA or infinite values
cewa_bio_clean <- cewa_bio[complete.cases(cewa_bio) & !is.infinite(rowSums(cewa_bio)), ]

zcewa_bio_clean <- scale(cewa_bio_clean)

# Run PCA on the cleaned data
cewa_bio_pca2 <- prcomp(zcewa_bio_clean, scale. = FALSE)
cewa_bio_pca <- princomp(zcewa_bio_clean, cor = F)

# Replace NA values with the column means
#cewa_bio_imputed <- cewa_bio
#for(i in seq_along(cewa_bio_imputed)) {
#  cewa_bio_imputed[[i]][is.na(cewa_bio_imputed[[i]])] <- mean(cewa_bio_imputed[[i]], na.rm = TRUE)
#}

# Run PCA on the imputed data
#cewa_bio_pca <- prcomp(cewa_bio_imputed, scale. = FALSE)
#cewa_bio_pca <- prcomp(cewa_bio_clean, scale. = TRUE)

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

loadings(cewa_bio_pca) #12
cewa_bio_pca2$rotation #12

scores(cewa_bio_pca)
cewa_bio_pca2$x

#scree plot
plot(cewa_bio_pca, type = "lines") #keep two

set.seed(1)
perm_results <- PCAtest(zcewa_bio_clean, 1000, 1000, 0.05, varcorr = FALSE,
                        counter = FALSE, plot = TRUE)

plot(cewa_bio_pca$loadings, type = "n", xlab = "PC 1, ??%", ylab = "PC 2, ??%", 
     ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5))
text(cewa_bio_pca$loadings, labels = as.character(colnames(zcewa_bio_clean)), pos = 1, cex = 1)

plot(cewa_bio_pca$scores, type = "n", xlab = "PC 1, ??%", ylab = "PC 2, ??%", 
     ylim = c(-9, 8), xlim = c(-6, 20))
text(cewa_bio_pca$scores, labels = as.character(rownames(zcewa_bio_clean)), pos = 1, cex = 1)

biplot(cewa_bio_pca$scores, cewa_bio_pca$loading, xlab = "PC 1, ??%", ylab = "PC 2, ??%",
       expand = 4, ylim = c(-25, 30), xlim = c(-30, 30))
biplot(cewa_bio_pca$scores, cewa_bio_pca$loading, expand = 4, xlabs = rep("*", 173765), 
       xlab = "PC 1, ??%", ylab = "PC 2, ??%", ylim = c(-25, 30), xlim = c(-30, 30))



# PCoA
library(vegan)
library(ca)
library(ggplot2)
library(dplyr)
cewa_bio <- readRDS("Cerulean_Warbler/cewa_biovars.RDS")
cewa_bio$geometry <- NULL
cewa_bio_summary <- cewa_bio %>%
  group_by(ID, YEAR) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

jcewa <- vegdist(cewa_bio, "bray")
cmd <- cmdscale(jcewa, k = 5, eig = TRUE)
cmd$points

cewa_bio_clean <- cewa_bio
cewa_bio_clean[is.na(cewa_bio_clean)] <- 0
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




