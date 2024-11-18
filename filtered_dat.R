# Levi Hoskins
# 1 Noviembre 2024
# Proyecto Final - Los Aves (Kentucky Warbler, Cerulean Warbler)
# R 4.4.1

library(ggplot2)
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)

#extract data with filters
cw <- read_sf("Cerulean_Warbler/CEWA.csv") 
tail(cw)

filtered_cw <- cw %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER, SCIENTIFIC.NAME, OBSERVATION.COUNT, 
                COUNTRY.CODE, STATE, STATE.CODE, LOCALITY.ID, LATITUDE, LONGITUDE, 
                OBSERVATION.DATE, PROTOCOL.TYPE, CATEGORY) %>%
  collect(n=Inf)
tail(filtered_cw)

#get rid of any checklists that have an X in them
lists_with_x_cw <- filtered_cw %>%
  dplyr::filter(OBSERVATION.COUNT=="X") %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>%
  distinct()

dat_cw <- filtered_cw %>%
  dplyr::filter(!SAMPLING.EVENT.IDENTIFIER %in% lists_with_x_cw$SAMPLING.EVENT.IDENTIFIER) %>%
  mutate(OBSERVATION.DATE = mdy(OBSERVATION.DATE))

saveRDS(dat_cw, "Cerulean_Warbler/dat_raw_CEWA.RDS")

dat_cw <- readRDS("Cerulean_Warbler/dat_raw_CEWA.RDS")

#split filtered data by month
#dat_cw_month <- dat_cw %>%
  #mutate(MONTH=month(OBSERVATION.DATE, label = TRUE, abbr = TRUE))

#split_by_month_function <- function(month){
  
 # temp <- dat_cw_month %>%
  #  dplyr::filter(MONTH==month)
  
#  saveRDS(temp, paste0("Cerulean_Warbler/dat_raw_CEWA_", month, ".RDS"))
  
#}

#lapply(unique(dat_cw_month$MONTH), split_by_month_function)

#split filtered data by year ### FIX THIS HOE
dat_cw_year <- dat_cw %>%
  mutate(YEAR=year(OBSERVATION.DATE))

split_by_year_function <- function(year){
  
  temp <- dat_cw_year %>%
    dplyr::filter(YEAR==year)
  
  saveRDS(temp, paste0("Cerulean_Warbler/dat_raw_CEWA_", year, ".RDS"))
  
}

lapply(unique(dat_cw_year$YEAR), split_by_year_function)
