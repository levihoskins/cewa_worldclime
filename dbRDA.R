####### db-RDA pages(188-190 in Brocard et al.)
library(vegan)
library(dplyr)
library(ggplot2)
#read in data
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
cewa_bio_clean <- cewa_bio

# Cleaning/filtering data
cewa_bio_clean$SAMPLING.EVENT.IDENTIFIER <- NULL
cewa_bio_clean$ID <- NULL
cewa_bio_clean$geometry <- NULL
cewa_bio_clean <- cewa_bio_clean[complete.cases(cewa_bio_clean) &
                  !is.infinite(rowSums(cewa_bio_clean)), ]

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



# Global test of the RDA result with ANOVA/permutations in vegan package
anova(db.rda, step=1000)
# Tests of all canonical axes:
#anova(db.rda, by="axis", step=1000) #crashes running this
# Tests of all variables:
#anova(db.rda, by="margin", step=1000) #crashes running this

# Create distance matrix as the response matrix
resp<-vegdist(zcewa_sampled, method="bray")

# Run varpart function from vegan
cewa.part <- varpart(resp,~ percip_wettest_month,~ mean_temp_coldest_quart,
                     ~ percip_wettest_quart ,~annual_percip ,data=zcewa_sampled)
plot(cewa.part, digits=2)
