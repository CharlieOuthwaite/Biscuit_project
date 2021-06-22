###################################
## 22nd June 2021             ####
## Biscuit Project            ##############################################################
## Abbie Chapman              #################################################################
## 3. Reading in MapSPAM and extracting for biscuit ingredients in major producer countries ###
################################################################################################

# Here, I will read in the MapSPAM data for 2010 and get rasters for the globe as well as 
# clipping them to the major producing countries identified from Carole Dalin's list
# and for which Charlie visualised Mark's data in barcharts.


library(raster); library(maptools); library(sp); library(readbulk); library(SDMTools);library(plyr);
library(rgdal); library(rgeos); library(ggplot2); library(snow); library(dplyr); library(viridis);
library(gridExtra); library(reshape2)

setwd("C:/Users/Dr Abbie/Documents/Data/")

outDir <- "Biscuit Project/"
cropdata.dir = "MapSPAM/2010_data_downloaded22062021/spam2010v2r0_global_phys_area.geotiff/" # focusing on the physical area, rather than more than one harvest, etc.
# physical data are in this directory in a further folder called 'spam2010v2r0_global_phys_area.geotiff'

# For the biscuit project, we only need sugar, wheat, palm oil, and cocoa.
# cocoa = 'non-food crops' = coco
# oilpalm = 'non-food crops' = oilp
# wheat = 'food crops' = whea
# sugar = 'non-food crops' = sugc (sugarcane) and sugb (sugarbeet)
# we will combine the sugar layers
# As we want the physical area data, we want the files with _A_
# We also want to consider all technologies together rather than 
# irrigation-specific at this stage, so _TA, but when you look
# at the files they actually just have A, H, etc., so I think we 
# want to pull the files ending in A
# and we want to use geotiffs for spatial analysis.

cocoa_map = raster(paste0(cropdata.dir, ("spam2010V2r0_global_A_COCO_A.tif")))
plot(cocoa_map)
writeRaster(cocoa_map, filename = paste0(outDir, "cocoamap_spam"), names(cocoa_map), format = "GTiff", bylayer = FALSE)
oilpalm_map = raster(paste0(cropdata.dir, ("spam2010V2r0_global_A_OILP_A.tif")))
plot(oilpalm_map)
writeRaster(oilpalm_map, filename = paste0(outDir, "oilpalmmap_spam"), names(oilpalm_map), format = "GTiff", bylayer = FALSE)
wheat_map = raster(paste0(cropdata.dir, ("spam2010V2r0_global_A_WHEA_A.tif")))
plot(wheat_map)
writeRaster(wheat_map, filename = paste0(outDir, "wheatmap_spam"), names(wheat_map), format = "GTiff", bylayer = FALSE)
# and to combine:
sugarcane_map = raster(paste0(cropdata.dir, ("spam2010V2r0_global_A_SUGC_A.tif")))
plot(sugarcane_map)
writeRaster(sugarcane_map, filename = paste0(outDir, "sugarcanemap_spam"), names(sugarcane_map), format = "GTiff", bylayer = FALSE)
sugarbeet_map = raster(paste0(cropdata.dir, ("spam2010V2r0_global_A_SUGB_A.tif")))
plot(sugarbeet_map)
writeRaster(sugarbeet_map, filename = paste0(outDir, "sugarbeetmap_spam"), names(sugarbeet_map), format = "GTiff", bylayer = FALSE)
sugar_map = sum(sugarcane_map, sugarbeet_map)
plot(sugar_map)
writeRaster(sugar_map, filename = paste0(outDir, "sugar_combined_map_spam"), names(sugar_map), format = "GTiff", bylayer = FALSE)

###########################################################
## Charlie has kindly extracted the countries which are major producers
## from Carole's dataset on major trade partners with the UK
## for these main biscuit ingredient crops.
## The countries we will want to clip to are:
# 
# Cocoa: Cameroon, Ghana, Indonesia, Nigeria, Cote d'Ivoire (added as listed on McVities website)
# Oilpalm: Brazil, Indonesia, Malaysia, Nigeria, PNG
# Sugarbeet: France, UK
# Sugarcane: Belize, Fiji, Jamaica, Mauritius, S Africa, Swaziland, Trinidad & Tobago, Zimbabwe
# Wheat: Canada, France, Germany, UK

countryoutlines <- paste0(outDir,"Country outlines/")

cameroon_boundaries = readOGR(paste0(countryoutlines,"gadm36_CMR_0.shp"))
ghana_boundaries = readOGR(paste0(countryoutlines,"gadm36_GHA_0.shp"))
#indonesia_boundaries = readOGR(paste0(countryoutlines,"gadm36_IDN_1.shp")) # level 1 lowest for shapefile
# for indonesia, I therefore needed to read in an rds
indonesia_boundaries = readRDS(paste0(countryoutlines, "gadm36_IDN_0_sp.rds"))
nigeria_boundaries = readOGR(paste0(countryoutlines,"gadm36_NGA_0.shp"))
cotedivoire_boundaries = readOGR(paste0(countryoutlines,"gadm36_CIV_0.shp"))
brazil_boundaries = readOGR(paste0(countryoutlines,"gadm36_BRA_0.shp"))
malaysia_boundaries = readOGR(paste0(countryoutlines,"gadm36_MYS_0.shp"))
papuanewguinea_boundaries = readOGR(paste0(countryoutlines,"gadm36_PNG_0.shp"))
france_boundaries = readOGR(paste0(countryoutlines,"gadm36_FRA_0.shp"))
uk_boundaries = readOGR(paste0(countryoutlines,"gadm36_GBR_0.shp"))
belize_boundaries = readOGR(paste0(countryoutlines,"gadm36_BLZ_0.shp"))
fiji_boundaries = readOGR(paste0(countryoutlines,"gadm36_FJI_0.shp"))
jamaica_boundaries = readOGR(paste0(countryoutlines,"gadm36_JAM_0.shp"))
mauritius_boundaries = readOGR(paste0(countryoutlines,"gadm36_MUS_0.shp"))
safrica_boundaries = readOGR(paste0(countryoutlines,"gadm36_ZAF_0.shp"))
swaziland_boundaries = readOGR(paste0(countryoutlines,"gadm36_SWZ_0.shp"))
trinidadandtobago_boundaries = readOGR(paste0(countryoutlines,"gadm36_TTO_0.shp"))
zimbabwe_boundaries = readOGR(paste0(countryoutlines,"gadm36_ZWE_0.shp"))
canada_boundaries = readOGR(paste0(countryoutlines,"gadm36_CAN_0.shp"))
germany_boundaries = readOGR(paste0(countryoutlines,"gadm36_DEU_0.shp"))

plot(cameroon_boundaries)
plot(ghana_boundaries)
plot(indonesia_boundaries)
plot(nigeria_boundaries)
plot(cotedivoire_boundaries)
plot(brazil_boundaries)
plot(malaysia_boundaries)
plot(papuanewguinea_boundaries)
plot(france_boundaries)
plot(uk_boundaries)
plot(belize_boundaries)
plot(fiji_boundaries)
plot(jamaica_boundaries)
plot(mauritius_boundaries)
plot(safrica_boundaries)
plot(swaziland_boundaries)
plot(trinidadandtobago_boundaries)
plot(zimbabwe_boundaries)
plot(canada_boundaries)
plot(germany_boundaries)

# Now I have checked these boundaries, I will focus on clipping the maps to the country outlines.
# I have a function from previous work (credit to Tim Newbold & Fiona Spooner for helping me develop this) as follows:

#Function Arguments:
# Reference  refers to name of the layer with the extent (the boundary shapefiles)
# OutName represents the intended prefix for output files: 
# e.g. outname_GHA = "GHA_"
# OutPrj represents the desired output projection:
outprj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# and 'OutRes' represents the desired Output Resolution
# Not included here as not necessary.
GlobalExtent <- extent(-180,180,-90,90)

cameroon_extent = extent(cameroon_boundaries)
ghana_extent = extent(ghana_boundaries)
indonesia_extent = extent(indonesia_boundaries)
nigeria_extent = extent(nigeria_boundaries)
cotedivoire_extent = extent(cotedivoire_boundaries)
brazil_extent = extent(brazil_boundaries)
malaysia_extent = extent(malaysia_boundaries)
papuanewguinea_extent = extent(papuanewguinea_boundaries)
france_extent = extent(france_boundaries)
uk_extent = extent(uk_boundaries)
belize_extent = extent(belize_boundaries)
fiji_extent = extent(fiji_boundaries)
jamaica_extent = extent(jamaica_boundaries)
mauritius_extent = extent(mauritius_boundaries)
safrica_extent = extent(safrica_boundaries)
swaziland_extent = extent(swaziland_boundaries)
trinidadandtobago_extent = extent(trinidadandtobago_boundaries)
zimbabwe_extent = extent(zimbabwe_boundaries)
canada_extent = extent(canada_boundaries)
germany_extent = extent(germany_boundaries)

# Here is the function, but I will take the elements and run them step by step.

clipping_rasters <- function(x){
  r <- raster::raster(x)
  r1a = raster::projectRaster(r,crs = outprj)
  r1b = raster::extend(r1a, GlobalExtent)
  # r[is.na(r[])]<-0
  r1 <- raster::crop(r1b,extent)
  #if (class(r) != "try-error"){
  # return(r)
  #}
  r2 <- mask(r1, reference)
}


cocoa_project = projectRaster(cocoa_map, crs = outprj)
cocoa_extend = extend(cocoa_project, GlobalExtent)
cocoa_cameroon1 = crop(cocoa_extend, cameroon_extent)
cocoa_cameroon = mask(cocoa_cameroon1, cameroon_boundaries)
plot(cocoa_cameroon)
writeRaster(cocoa_cameroon, filename = paste0(outDir, "cocoa_cameroon"), names(cocoa_cameroon), format = "GTiff", bylayer = FALSE)

## Refresh on the countries
# Cocoa: Cameroon, Ghana, Indonesia, Nigeria, Cote d'Ivoire (added as listed on McVities website)
# Oilpalm: Brazil, Indonesia, Malaysia, Nigeria, PNG
# Sugarbeet: France, UK
# Sugarcane: Belize, Fiji, Jamaica, Mauritius, S Africa, Swaziland, Trinidad & Tobago, Zimbabwe
# Wheat: Canada, France, Germany, UK

cocoa_ghana1 = crop(cocoa_extend, ghana_extent)
cocoa_ghana = mask(cocoa_ghana1, ghana_boundaries)
plot(cocoa_ghana)
writeRaster(cocoa_ghana, filename = paste0(outDir, "cocoa_ghana"), names(cocoa_ghana), format = "GTiff", bylayer = FALSE)

cocoa_indonesia1 = crop(cocoa_extend, indonesia_extent)
cocoa_indonesia = mask(cocoa_indonesia1, indonesia_boundaries)
plot(cocoa_indonesia)
writeRaster(cocoa_indonesia, filename = paste0(outDir, "cocoa_indonesia"), names(cocoa_indonesia), format = "GTiff", bylayer = FALSE)

cocoa_nigeria1 = crop(cocoa_extend, nigeria_extent)
cocoa_nigeria = mask(cocoa_nigeria1, nigeria_boundaries)
plot(cocoa_nigeria)
writeRaster(cocoa_nigeria, filename = paste0(outDir, "cocoa_nigeria"), names(cocoa_nigeria), format = "GTiff", bylayer = FALSE)

cocoa_cotedivoire1 = crop(cocoa_extend, cotedivoire_extent)
cocoa_cotedivoire = mask(cocoa_cotedivoire1, cotedivoire_boundaries)
plot(cocoa_cotedivoire)
writeRaster(cocoa_cotedivoire, filename = paste0(outDir, "cocoa_cotedivoire"), names(cocoa_cotedivoire), format = "GTiff", bylayer = FALSE)

## Oilpalm
# Oilpalm: Brazil, Indonesia, Malaysia, Nigeria, PNG
oilpalm_project = projectRaster(oilpalm_map, crs = outprj)
oilpalm_extend = extend(oilpalm_project, GlobalExtent)

oilpalm_brazil1 = crop(oilpalm_extend, brazil_extent)
oilpalm_brazil = mask(oilpalm_brazil1, brazil_boundaries)
plot(oilpalm_brazil)
writeRaster(oilpalm_brazil, filename = paste0(outDir, "oilpalm_brazil"), names(oilpalm_brazil), format = "GTiff", bylayer = FALSE)

oilpalm_indonesia1 = crop(oilpalm_extend, indonesia_extent)
oilpalm_indonesia = mask(oilpalm_indonesia1, indonesia_boundaries)
plot(oilpalm_indonesia)
writeRaster(oilpalm_indonesia, filename = paste0(outDir, "oilpalm_indonesia"), names(oilpalm_indonesia), format = "GTiff", bylayer = FALSE)

oilpalm_malaysia1 = crop(oilpalm_extend, malaysia_extent)
oilpalm_malaysia = mask(oilpalm_malaysia1, malaysia_boundaries)
plot(oilpalm_malaysia)
writeRaster(oilpalm_malaysia, filename = paste0(outDir, "oilpalm_malaysia"), names(oilpalm_malaysia), format = "GTiff", bylayer = FALSE)

oilpalm_nigeria1 = crop(oilpalm_extend, nigeria_extent)
oilpalm_nigeria = mask(oilpalm_nigeria1, nigeria_boundaries)
plot(oilpalm_nigeria)
writeRaster(oilpalm_nigeria, filename = paste0(outDir, "oilpalm_nigeria"), names(oilpalm_nigeria), format = "GTiff", bylayer = FALSE)

oilpalm_papuanewguinea1 = crop(oilpalm_extend, papuanewguinea_extent)
oilpalm_papuanewguinea = mask(oilpalm_papuanewguinea1, papuanewguinea_boundaries)
plot(oilpalm_papuanewguinea)
writeRaster(oilpalm_papuanewguinea, filename = paste0(outDir, "oilpalm_papuanewguinea"), names(oilpalm_papuanewguinea), format = "GTiff", bylayer = FALSE)

# Sugarbeet: France, UK

sugarbeet_project = projectRaster(sugarbeet_map, crs = outprj)
sugarbeet_extend = extend(sugarbeet_project, GlobalExtent)

sugarbeet_france1 = crop(sugarbeet_extend, france_extent)
sugarbeet_france = mask(sugarbeet_france1, france_boundaries)
plot(sugarbeet_france)
writeRaster(sugarbeet_france, filename = paste0(outDir, "sugarbeet_france"), names(sugarbeet_france), format = "GTiff", bylayer = FALSE)

sugarbeet_uk1 = crop(sugarbeet_extend, uk_extent)
sugarbeet_uk = mask(sugarbeet_uk1, uk_boundaries)
plot(sugarbeet_uk)
writeRaster(sugarbeet_uk, filename = paste0(outDir, "sugarbeet_uk"), names(sugarbeet_uk), format = "GTiff", bylayer = FALSE)

# Sugarcane: Belize, Fiji, Jamaica, Mauritius, S Africa, Swaziland, Trinidad & Tobago, Zimbabwe

sugarcane_project = projectRaster(sugarcane_map, crs = outprj)
sugarcane_extend = extend(sugarcane_project, GlobalExtent)

sugarcane_belize1 = crop(sugarcane_extend, belize_extent)
sugarcane_belize = mask(sugarcane_belize1, belize_boundaries)
plot(sugarcane_belize)
writeRaster(sugarcane_belize, filename = paste0(outDir, "sugarcane_belize"), names(sugarcane_belize), format = "GTiff", bylayer = FALSE)

sugarcane_fiji1 = crop(sugarcane_extend, fiji_extent)
sugarcane_fiji = mask(sugarcane_fiji1, fiji_boundaries)
plot(sugarcane_fiji)
writeRaster(sugarcane_fiji, filename = paste0(outDir, "sugarcane_fiji"), names(sugarcane_fiji), format = "GTiff", bylayer = FALSE)

sugarcane_jamaica1 = crop(sugarcane_extend, jamaica_extent)
sugarcane_jamaica = mask(sugarcane_jamaica1, jamaica_boundaries)
plot(sugarcane_jamaica)
writeRaster(sugarcane_jamaica, filename = paste0(outDir, "sugarcane_jamaica"), names(sugarcane_jamaica), format = "GTiff", bylayer = FALSE)

sugarcane_mauritius1 = crop(sugarcane_extend, mauritius_extent)
sugarcane_mauritius = mask(sugarcane_mauritius1, mauritius_boundaries)
plot(sugarcane_mauritius)
writeRaster(sugarcane_mauritius, filename = paste0(outDir, "sugarcane_mauritius"), names(sugarcane_mauritius), format = "GTiff", bylayer = FALSE)

sugarcane_safrica1 = crop(sugarcane_extend, safrica_extent)
sugarcane_safrica = mask(sugarcane_safrica1, safrica_boundaries)
plot(sugarcane_safrica)
writeRaster(sugarcane_safrica, filename = paste0(outDir, "sugarcane_safrica"), names(sugarcane_safrica), format = "GTiff", bylayer = FALSE)

sugarcane_swaziland1 = crop(sugarcane_extend, swaziland_extent)
sugarcane_swaziland = mask(sugarcane_swaziland1, swaziland_boundaries)
plot(sugarcane_swaziland)
writeRaster(sugarcane_swaziland, filename = paste0(outDir, "sugarcane_swaziland"), names(sugarcane_swaziland), format = "GTiff", bylayer = FALSE)

sugarcane_trinidadandtobago1 = crop(sugarcane_extend, trinidadandtobago_extent)
sugarcane_trinidadandtobago = mask(sugarcane_trinidadandtobago1, trinidadandtobago_boundaries)
plot(sugarcane_trinidadandtobago)
writeRaster(sugarcane_trinidadandtobago, filename = paste0(outDir, "sugarcane_trinidadandtobago"), names(sugarcane_trinidadandtobago), format = "GTiff", bylayer = FALSE)

sugarcane_zimbabwe1 = crop(sugarcane_extend, zimbabwe_extent)
sugarcane_zimbabwe = mask(sugarcane_zimbabwe1, zimbabwe_boundaries)
plot(sugarcane_zimbabwe)
writeRaster(sugarcane_zimbabwe, filename = paste0(outDir, "sugarcane_zimbabwe"), names(sugarcane_zimbabwe), format = "GTiff", bylayer = FALSE)

# Wheat: Canada, France, Germany, UK
wheat_project = projectRaster(wheat_map, crs = outprj)
wheat_extend = extend(wheat_project, GlobalExtent)

wheat_canada1 = crop(wheat_extend, canada_extent)
wheat_canada = mask(wheat_canada1, canada_boundaries)
plot(wheat_canada)
writeRaster(wheat_canada, filename = paste0(outDir, "wheat_canada"), names(wheat_canada), format = "GTiff", bylayer = FALSE)

wheat_france1 = crop(wheat_extend, france_extent)
wheat_france = mask(wheat_france1, france_boundaries)
plot(wheat_france)
writeRaster(wheat_france, filename = paste0(outDir, "wheat_france"), names(wheat_france), format = "GTiff", bylayer = FALSE)

wheat_germany1 = crop(wheat_extend, germany_extent)
wheat_germany = mask(wheat_germany1, germany_boundaries)
plot(wheat_germany)
writeRaster(wheat_germany, filename = paste0(outDir, "wheat_germany"), names(wheat_germany), format = "GTiff", bylayer = FALSE)

wheat_uk1 = crop(wheat_extend, uk_extent)
wheat_uk = mask(wheat_uk1, uk_boundaries)
plot(wheat_uk)
writeRaster(wheat_uk, filename = paste0(outDir, "wheat_uk"), names(wheat_uk), format = "GTiff", bylayer = FALSE)

