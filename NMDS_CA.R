# Load packages
library(raster)
library(cluster)
library(mvnormtest)
library(MVN)
library(vegan)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(sf)

# Read in data and modify
cewa_bio <- readRDS("Cerulean_Warbler/cewa_biovars.RDS")
cewa_bio <- cewa_bio %>%
  rename(year = YEAR, annual_mean_temp = wc2.1_10m_bio_1,
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

set.seed(33)

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

# Perform NMDS
nmdscewa <- metaMDS(jcewa, k = 2, trace = T)  
stressplot(nmdscewa)

# Create a treatment matrix (treat) where years are the groups
treat <- as.matrix(zcewa_sampled_no_negatives$year)
rownames(treat) <- rownames(zcewa_sampled_no_negatives)
colnames(treat) <- "Year"

# Define color palette for the year groups (2000:2010 and 2011:2024)
year_groups <- ifelse(treat >= 2000 & treat <= 2010, "2000:2010", "2011:2024")
colors <- brewer.pal(2, "Set1") 

# Run PERMANOVA in the adonis2 function in the vegan package
permanova <- adonis2(jcewa ~ year_groups, 
                     data = data.frame(year_groups = year_groups),
                     permutations = 1000)

# Print PERMANOVA results
print(permanova)

# Extracting scores for plotting
data.scores <- as.data.frame(nmdscewa$points)
data.scores$time <- treat
head(data.scores)

# Graph the NMDS
ggplot(data.scores, aes(x=MDS1, y=MDS2, col=year_groups)) +
  geom_point() +
  geom_text(aes(label=rownames(data.scores)),hjust=0, vjust=0) +
  stat_ellipse() +
  theme_bw() +
  xlim(-2.2, 2)+
  ylim(-1.5,1.5)+
  labs(title = "NMDS Plot")



