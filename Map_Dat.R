# Load package
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(vegan)
library(sf)

# Read in data and rename columns, then clean up for order
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

# Add in presence column
data_with_presence <- cewa_bio_clean %>%
  group_by(year, geometry) %>%
  summarize(Presence = 1, .groups = "drop")

# Separate geometry into lat/long
data_with_presence <- data_with_presence %>%
  mutate(long = st_coordinates(.)[, 1],  # Extract longitude
         lat = st_coordinates(.)[, 2]) #Extract latitude

data_with_presence <- readRDS("Cerulean_Warbler/data_with_presence.RDS")

# Load world map data at medium scale
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter the world data to get only countries in the Americas
# Select both North and South American countries
americas <- world %>%
  filter(continent %in% c("North America", "South America"))

# sf format for mapping
data_with_presence <- st_as_sf(data_with_presence)

# Plot the map with presence points (Figure 1)
ggplot() +
  geom_sf(data = americas, fill = "lightgray", color = "black") + 
  geom_point(data = data_with_presence, 
             aes(x = long, y = lat, color = as.factor(year))) +
  labs(title = "Presence by Coordinates", color = "Year") + 
  theme_minimal() +
  coord_sf(xlim = c(-180, -30), ylim = c(-60, 85))  

# Plot to analyze weird pattern (Figure 5)
ggplot() +
  geom_sf(data = americas, fill = "lightgray", color = "black") + 
  geom_point(data = data_with_presence, 
             aes(x = long, y = lat, color = as.factor(year))) +
  labs(title = "Cerulean Warblers in the United States", color = "Year") + 
  theme_minimal() +
  coord_sf(xlim = c(-100, -70), ylim = c(20, 50)) 
