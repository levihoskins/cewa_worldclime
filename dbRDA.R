# Load packages
library(vegan)
library(dplyr)
library(ggplot2)

# Read in data and modify
cewa_bio <- readRDS("Cerulean_Warbler/cewa_biovars.RDS")
cewa_bio <- cewa_bio %>%
  rename(year = YEAR, 
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
         percip_coldest_quart = wc2.1_10m_bio_19)
cewa_bio_clean <- cewa_bio

# Cleaning/filtering data
cewa_bio_clean$SAMPLING.EVENT.IDENTIFIER <- NULL
cewa_bio_clean$ID <- NULL
cewa_bio_clean$geometry <- NULL
cewa_bio_clean <- cewa_bio_clean[complete.cases(cewa_bio_clean) &
                  !is.infinite(rowSums(cewa_bio_clean)), ]

# Set seed for repetition
set.seed(33)

# Scale all columns except 'year' (non-numeric variables should be excluded)
zcewa <- cewa_bio_clean 
zcewa[-1] <- scale(zcewa[-1])

# Remove empty columns
zcewa <- zcewa[, colSums(zcewa != 0, na.rm = TRUE) > 0]

# Set sample size
sample_size <- 100
zcewa_sampled <- zcewa %>%
  group_by(year) %>%
  filter(n() >= sample_size) %>% 
  slice_sample(n = sample_size, replace = FALSE) %>%
  ungroup() 

# Run the dbRDA
db.rda <- capscale(zcewa_sampled ~ min_temp_coldest_month + percip_wettest_month + 
                     mean_temp_coldest_quart + percip_wettest_quart +
                     annual_percip + isothermality, 
                   data = zcewa_sampled,
                   distance = "bray", 
                   add = TRUE)

summary(db.rda)

#R2 and adjusted R2
R2 <- RsquareAdj(db.rda)$r.squared
R2adj <- RsquareAdj(db.rda)$adj.r.squared

# Global test of the RDA result - ANOVA
anova_global <- anova.cca(db.rda, permutations = 1000)
print("Global test of dbRDA:")
print(anova_global)

# ANOVA of all canonical axes #cannot get to compute it's just too large
#anova_axes <- anova.cca(db.rda, by = "axis", permutations = 1000)
#print("Test of canonical axes:")
#print(anova_axes)

# ANOVA of individual terms #lo mismo for above
#anova_terms <- anova.cca(db.rda, by = "margin", permutations = 1000)
#print("Test of individual terms:")
#print(anova_terms)

#Plot using the F-scores:
par(mfrow=c(1,2))
plot(db.rda, scaling=1, 
     main="Triplot db-rda F scores")
spe.sc <- scores(db.rda, choices=1:2, scaling=2, display="sp")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

#Plot using the Z-scores:
plot(db.rda, scaling=1, display=c("sp", "lc", "cn"), 
     main="Triplot db-rda  Z scores")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

# Run varpart function from vegan for Variance Partitioning
resp<-vegdist(zcewa_sampled, method="bray")
cewa.part <- varpart(resp,~ percip_wettest_month,~ mean_temp_coldest_quart,
                     ~ percip_wettest_quart ,~annual_percip ,data=zcewa_sampled)
plot(cewa.part, digits=2)
