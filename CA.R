library(raster)
library(cluster)
library(mvnormtest)
library(MVN)

#read in data
cewa_bio <- readRDS("Cerulean_Warbler/cewa_bio.RDS")
cewa_bio$sampling.event.identifier <- NULL
cewa_bio <- cewa_bio[complete.cases(cewa_bio) & !is.infinite(rowSums(cewa_bio)), ]

library(ggplot2)
ggplot(data.frame(value = data), aes(y = value)) +
  geom_violin() +
  theme_minimal()