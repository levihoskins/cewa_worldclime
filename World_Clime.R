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
