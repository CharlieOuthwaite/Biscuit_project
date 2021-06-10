###################################
## 10th June 2021             ####
## Biscuit Project            ####
## Amy Molotoks & Abbie Chapman ##
## 0. Subsetting data for cocoa ###
###################################

# Here, we plan to clip Mark's data and data from EarthStat to the two main countries McVities source cocoa from (Ghana & Cote d'Ivoire)


library(raster); library(maptools); library(sp); library(readbulk); library(SDMTools);library(plyr);
library(rgdal); library(rgeos); library(ggplot2); library(snow); library(dplyr); library(viridis);
library(gridExtra); library(reshape2)

setwd("C:/Users/Dr Abbie/Documents/Data/")

outDir <- "Biscuit Project/"
cropdata.dir = "Biscuit Project/EarthStat_cocoa_HarvAreaYield_Geotiff/" 

#Function Arguments:
# Reference  refers to name of the layer with the desired extent: 
reference_GHA = readOGR(paste0(outDir, ("gadm36_GHA_0.shp"))) # 0 is the whole country
# OutName represents the intended prefix for output files: 
outname_GHA = "GHA_"
# OutPrj represents the desired output projection:
outprj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# and 'OutRes' represents the desired Output Resolution
# Not included here as not necessary.


# Code below not working so trying per file instead:

harvested.area.ha_cocoa = raster::raster(paste0(cropdata.dir, "cocoa_HarvestedAreaHectares.tif"))



# Here, we find all of the files in the Earthstat.org downloaded crop files that represent harvested area in hectares:
harvest.files = paste(cropdata.dir,dir(path = cropdata.dir, pattern = "*HarvestedArea", recursive = TRUE),sep="")

# Then, because some of these files come with accompanying metadata that we don't need to work with, we request only the tif files:
harvest.files = harvest.files[!grepl("ovr", harvest.files)]
harvest.files = harvest.files[!grepl("xml",harvest.files)]
harvest.files
# We don't want Data Quality or metadata files
harvest.files = harvest.files[!grepl("pdf",harvest.files)]
harvest.files = harvest.files[!grepl("mapexamples",harvest.files)]
harvest.files = harvest.files[!grepl("DataQuality",harvest.files)]

GlobalExtent <- extent(-180,180,-90,90)
GhanaExtent <- extent(reference_GHA)

# This function takes a raster, ensures it has our desired projection, crops it to match the reference shapefile (here, Ghana)
# and then the mask is needed to ensure it cuts around the shapefile, rather than forming a box of similar extent.
cropping_production_GHA <- function(x){
  r <- raster::raster(x)
  r1a = raster::projectRaster(r,crs = outprj)
  r1b = raster::extend(r1a, GlobalExtent)
  # r[is.na(r[])]<-0
  r1 <- raster::crop(r1b,GhanaExtent)
  #if (class(r) != "try-error"){
  # return(r)
  #}
  r2 <- mask(r1, reference_GHA)
}

# We then run the function by creating a stack and applying the cropping production function over the crop files individually.  
# We take a file, run the function on it, and then save it as a raster with a name
# with the country attached to the start.

GhanaCropProduction = stack(lapply(1:2,function(i){
  cropfile = harvest.files[i]
  cropfile_raster <- cropping_production_GHA(cropfile)
  outPath <- paste0(outDir, outname_GHA, basename(cropfile))
  writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
}))
plot(GhanaCropProduction)

####################################
####################################
## Repeat for Cote d'Ivoire:

reference_CIV = readOGR(paste0(outDir, ("gadm36_CIV_0.shp"))) # 0 is the whole country
# OutName represents the intended prefix for output files: 
outname_CIV = "CIV_"
CIVExtent <- extent(reference_CIV)

cropping_production_CIV <- function(x){
  r <- raster::raster(x)
  r1a = raster::projectRaster(r,crs = outprj)
  r1b = raster::extend(r1a, GlobalExtent)
  # r[is.na(r[])]<-0
  r1 <- raster::crop(r1b,CIVExtent)
  #if (class(r) != "try-error"){
  # return(r)
  #}
  r2 <- mask(r1, reference_CIV)
}

# We then run the function by creating a stack and applying the cropping production function over the crop files individually.  
# We take a file, run the function on it, and then save it as a raster with a name
# with the country attached to the start.

CIVCropProduction = stack(lapply(1:2,function(i){
  cropfile = harvest.files[i]
  cropfile_raster <- cropping_production_CIV(cropfile)
  outPath <- paste0(outDir, outname_CIV, basename(cropfile))
  writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
}))
plot(CIVCropProduction)


#############################################
#############################################
## Clipping Mark's outputs to these two initial focal countries
#############################################
## NOTE: DO NOT SHARE OUTSIDE OF BISCUIT PROJECT - Mark has put these together for his PhD thesis and publications


markdata.dir = "Biscuit Project/Mark Data DO NOT USE OUTSIDE/Outputs/" 
mark.files.total.impact = paste(markdata.dir,dir(path = markdata.dir, pattern = "*Total", recursive = TRUE),sep="")

## The function below is simply an update of the crop version so that it works for Mark's files instead. 
# It is slower to run as Mark's files are larger and comprise multiple crops per file.
# Because Mark's data have bands, we need to use stack not raster and plotRGB.
# I will split Mark's data out first, before then running the function also used for EarthStat, so the function can stay consistent.

# Multi-band raster tips: https://www.neonscience.org/resources/learning-hub/tutorials/dc-multiband-rasters-r

mark.files.total.impact
## Notes from Mark's read me file suggest that, for cocoa, we want:
# GHG - band 1 
# water - band 1
# land biodiversity impact - no cocoa for this or any N or P biodiversity impacts
mark.ghg = raster(mark.files.total.impact[1])
mark.ghg
nbands(mark.ghg) # 5 - we want band 1
mark.ghg_cocoa = raster((mark.files.total.impact[1]), band = 1)
plot(mark.ghg_cocoa)

# checking this has worked - test for another band
#mark.ghg_test = raster((mark.files.total.impact[1]), band = 2)
#plot(mark.ghg_test)

mark.water = raster(mark.files.total.impact[5])
mark.water
nbands(mark.water) # 4 bands - we want band 1
mark.water_cocoa = raster((mark.files.total.impact[5]), band = 1)
plot(mark.water_cocoa)

# using the cropping production function but breaking it down here to make sure it's appropriate for specific layers of Mark's rasters
mark.ghg_cocoa_projection1 = raster::projectRaster(mark.ghg_cocoa, crs = outprj)
mark.ghg_cocoa_projection2 = raster::extend(mark.ghg_cocoa_projection1, GlobalExtent)
mark.ghg_cocoa_ghana_crop = raster::crop(mark.ghg_cocoa_projection2, GhanaExtent) # ghana crop
mark.ghg_cocoa_CIV_crop = raster::crop(mark.ghg_cocoa_projection2, CIVExtent) # cote d'ivoire crop
mark.ghg_cocoa_ghana_mask = mask(mark.ghg_cocoa_ghana_crop, reference_GHA) # ghana mask
mark.ghg_cocoa_CIV_mask = mask(mark.ghg_cocoa_CIV_crop, reference_CIV) # cote d'ivoire mask

writeRaster(x=mark.ghg_cocoa_ghana_mask, filename = "Mark_Data_GHG_Emissions_Total_Cocoa_Ghana", format = "GTiff", overwrite = TRUE)
writeRaster(x=mark.ghg_cocoa_CIV_mask, filename = "Mark_Data_GHG_Emissions_Total_Cocoa_CotedIvoire", format = "GTiff", overwrite = TRUE)

plot(mark.ghg_cocoa_ghana_mask)
plot(mark.ghg_cocoa_CIV_mask)

## Repeating for water

mark.water_cocoa_projection1 = raster::projectRaster(mark.water_cocoa, crs = outprj)
mark.water_cocoa_projection2 = raster::extend(mark.water_cocoa_projection1, GlobalExtent)
mark.water_cocoa_ghana_crop = raster::crop(mark.water_cocoa_projection2, GhanaExtent) # ghana crop
mark.water_cocoa_CIV_crop = raster::crop(mark.water_cocoa_projection2, CIVExtent) # cote d'ivoire crop
mark.water_cocoa_ghana_mask = mask(mark.water_cocoa_ghana_crop, reference_GHA) # ghana mask
mark.water_cocoa_CIV_mask = mask(mark.water_cocoa_CIV_crop, reference_CIV) # cote d'ivoire mask

writeRaster(x=mark.water_cocoa_ghana_mask, filename = "Mark_Data_Water_Debt_Total_Cocoa_Ghana", format = "GTiff", overwrite = TRUE)
writeRaster(x=mark.water_cocoa_CIV_mask, filename = "Mark_Data_Water_Debt_Total_Cocoa_CotedIvoire", format = "GTiff", overwrite = TRUE)

plot(mark.water_cocoa_ghana_mask)
plot(mark.water_cocoa_CIV_mask)
