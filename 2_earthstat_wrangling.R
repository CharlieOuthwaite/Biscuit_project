###################################
## 16th June 2021             ####
## Biscuit Project            ####
## Amy Molotoks & Abbie Chapman ##
## 2. Processing EarthStat data ###
###################################

# I tried this in Google Earth Engine but wasn't certain on the output so want to check here, for confidence.

library(raster); library(maptools); library(sp); library(readbulk); library(SDMTools);library(plyr);
library(rgdal); library(rgeos); library(ggplot2); library(snow); library(dplyr); library(viridis);
library(gridExtra); library(reshape2)

setwd("C:/Users/Dr Abbie/Documents/Data/")

outDir <- "Biscuit Project/"
cropdata.dir = "Overlay Analysis/1a_EarthStatCrops/HarvestedAreaYield175Crops_Geotiff/HarvestedAreaYield175Crops_Geotiff/GeoTiff/" 

cocoa.dir = paste0(cropdata.dir, "cocoa/")
cocoa_map = raster(paste0(cocoa.dir, ("cocoa_HarvestedAreaHectares.tif")))
# next, I will want to clip this to our focal countries of interest, so to mark the next steps:
# 1/ pull main countries off from Carole's list
# 2/ get the gadm files for these
# 3/ clip the crop to the gadm using code from 0_datasubsetting
# 4/ export as rasters and upload to google drive and earth engine to play with
# 5/ copy and repeat segments of the code as appropriate for the different crops and countries: oilpalm, wheat, and the sugars


# For sugar, there is an extra step, where we want to merge the sugar maps.

sugarbeet.dir = paste0(cropdata.dir, "sugarbeet/")
sugarbeet_map = raster(paste0(sugarbeet.dir, ("sugarbeet_HarvestedAreaHectares.tif")))
sugarcane.dir = paste0(cropdata.dir, "sugarcane/")
sugarcane_map = raster(paste0(sugarcane.dir, ("sugarcane_HarvestedAreaHectares.tif")))
sugarnes.dir = paste0(cropdata.dir, "sugarnes/")
sugarnes_map = raster(paste0(sugarnes.dir, ("sugarnes_HarvestedAreaHectares.tif")))

sugarbeet_and_cane = sum(sugarbeet_map, sugarcane_map)
plot(sugarbeet_and_cane)
plot(sugarbeet_map)
plot(sugarcane_map)

all_sugar = sum(sugarbeet_and_cane, sugarnes_map)
plot(all_sugar)

