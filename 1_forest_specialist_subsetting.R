###################################
## 16th June 2021             ####
## Biscuit Project            ####
## Amy Molotoks & Abbie Chapman ##
## 1.. Subsetting IUCN data to find forest specialists ###
###################################

# Here, we will take the IUCN mammal ranges and match the species names with those in the IUCN habitat preference data
# to try and identify forest specialists, as these might be put at greater risk by agriculture than generalists.

library(raster); library(maptools); library(sp); library(readbulk); library(SDMTools);library(plyr);
library(rgdal); library(rgeos); library(ggplot2); library(snow); library(dplyr); library(viridis);
library(gridExtra); library(reshape2)

setwd("C:/Users/Dr Abbie/Documents/Data/")

outDir <- "Biscuit Project/"
species.dir = "Biscuit Project/Amy copy of IUCN mammals/" 
habitat.dir = "Biscuit Project/IUCN habitat data/"

mammal_richness <- readOGR(paste0(species.dir, "MAMMALS_TERRESTRIAL_ONLY.shp"))
head(mammal_richness@data)
names(mammal_richness@data)
mammal_richness_dataframe = data.frame(mammal_richness@data) # tried fortify, which didn't work, whereas this seems to?
# write.csv(mammal_richness_dataframe, file = paste0(outDir, "mammal_richness_data.csv")) # just writing a copy (not really necessary but can skip to this stage in future if wanting to)

mammal_habitat <- readOGR(paste0(habitat.dir, "data_0.shp")) # I asked for csv as well but this didn't seem to come through from the IUCN
head(mammal_richness@data)
names(mammal_richness@data) # there's a column called binomial and this is near identical to the all mammals but is just the list for forest specialists
mammal_habitat = data.frame(mammal_habitat@data)

# Join up the data using species as the matching column

mammals_and_habitats = semi_join(mammal_richness_dataframe, mammal_habitat, by = "binomial") 
# the semi join function will just keep the data from the mammal richness with the habitats attached on, rather 
# than duplicating the species and ID bits

write.csv(paste0(outDir, "mammals_forest_habitat.csv"))
# Hopefully this is now a file we can use to filter the range polygons?





