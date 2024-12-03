library(raster)
library(cluster)
library(mvnormtest)
library(MVN)
library(vegan)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

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
cewa_bio_clean <- cewa_bio %>%
  group_by(year, geometry)
cewa_bio_clean$SAMPLING.EVENT.IDENTIFIER <- NULL
cewa_bio_clean$ID <- NULL
cewa_bio_clean$geometry <- NULL
cewa_bio_clean <- cewa_bio_clean[complete.cases(cewa_bio_clean) & !is.infinite(rowSums(cewa_bio_clean)), ]

# Scale all columns except the first
zcewa <- cewa_bio_clean  # Create a copy of the data
zcewa[-1] <- scale(cewa_bio_clean[-1])  # Apply scaling to all columns except the first

tail(zcewa)

# Set the sample size (100 rows per year)
sample_size <- 100

# Group by year and randomly sample 100 rows from each year
zcewa_sampled <- zcewa %>%
  group_by(year) %>%
  filter(n() >= sample_size) %>%  # Ensure there are enough rows to sample
  slice_sample(n = sample_size, replace = FALSE) %>%  # Randomly sample 100 rows from each year
  ungroup()  # Remove the grouping after sampling

# Replace negative values with zero, except for the first column
zcewa_sampled_no_negatives <- zcewa_sampled %>%
  mutate(across(-1, ~ ifelse(. < 0, 0, .)))  # Exclude the first column

# Now calculate Bray-Curtis dissimilarity with the modified data
jcewa <- vegdist(zcewa_sampled_no_negatives[-1], method = "bray")

nmdscewa <- metaMDS(jcewa, k = 2, trace = T)  
stressplot(nmdscewa)

# Create a treatment matrix (treat) where years are the groups
treat <- as.matrix(zcewa_sampled_no_negatives$year)
rownames(treat) <- rownames(zcewa_sampled_no_negatives)
colnames(treat) <- "Year"

# Define color palette for the year groups (2000:2010 and 2011:2024)
year_groups <- ifelse(treat >= 2000 & treat <= 2010, "2000:2010", "2011:2024")
colors <- brewer.pal(2, "Set1")  # Two colors: one for 2000:2010, and one for 2011:2024

# Plot out the points (islands)
ordiplot(nmdscewa, type = "n", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
orditorp(nmdscewa, display = "sites", col = colors[as.factor(year_groups)], air = 0.01, cex = 1.25)

# Add legend in the top left corner
legend("topleft", legend = c("2000:2010", "2011:2024"), 
       cex = 0.8, col = colors, pch = 15)

# Add a convex hull around each group:
ordihull(nmdscewa, groups = year_groups, display = "si", lty = 1, col = "green", show.groups = "2000:2010")
ordihull(nmdscewa, groups = year_groups, display = "si", lty = 1, col = "blue", show.groups = "2011:2024")

data.scores <- as.data.frame(nmdscewa$points)
data.scores$time <- treat
head(data.scores)

ggplot(data.scores, aes(x=MDS1, y=MDS2, col=year_groups)) +
  geom_point() +
  geom_text(aes(label=rownames(data.scores)),hjust=0, vjust=0) +
  stat_ellipse() +
  theme_bw() +
  xlim(-2.2, 2)+
  ylim(-1.5,1.5)+
  labs(title = "NMDS Plot")

### Correspondence Analysis (CA)
caCEWA <- ca(zcewa_sampled_no_negatives)
plot(caTree, xlim = c(-1, 3), ylim = c(-4, 5))


