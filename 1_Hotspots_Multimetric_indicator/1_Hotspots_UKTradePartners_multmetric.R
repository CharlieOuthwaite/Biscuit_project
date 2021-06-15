##%######################################################%##
#                                                          #
####              1. Multimetric indicator              ####
####            hotspots, UK trade partners             ####
#                                                          #
##%######################################################%##


# started by Charlie Outhwaite, 10/06/2021

# Here, I am exploring Mark's multimetric indicator rasters. 
# First, I will explore the countries that are major UK trade partners for 
# our crops and will make some summaries of the indicators for these countries

# to run this code, you will need to have downloaded Mark's rasters from the 
# Google Drive and have them in a folder "Data/Marks_Maps". You will also need 
# to have the list of coutries from Carole as a csv file in the "Data" folder.


rm(list = ls())

# load required libraries
library(raster)
library(rgdal)
library(exactextractr) # exact_extract function from here


# set directories
datadir <- "Data"
outdir <- "1_Hotspots_Multimetric_indicator"

# take a look at the raster files
list.files(paste0(datadir, "/Marks_Maps"), pattern = ".tif")

# Notes from Mark's readme doc
# Files ending with:
# a.	_CT = per tonne of crop for each grid cell
# b.	_Total = Total Impact for each grid cell


# which countries are we interested in?
# Use Carole's list of key trade partners for our crops of interest for 2003 (I think)
# this is saved in the Google drive 

# read in Carole's list
suppliers <- read.csv(paste0(datadir, "/UK_Suppliers_main_crops_Carole.csv"))

View(suppliers)

# get a list of countries (note this does not include cocoa at the moment)
countries <- unique(suppliers$partner)

length(countries) # currently 21 suppliers of interest



#### Task 1: Summary stats for each country ####

# For now, will just use the total impact raster files

files <- list.files(paste0(datadir, "/Marks_Maps"), pattern = "_Total.tif")

# 5 files
# "GHG_Emissons_Total.tif"
# "LD_BioDiv_Total.tif"
# "N_Marine_BioDiv_Total.tif"
# "P_Marine_BioDiv_Total.tif"
# "Water_Debt_Total.tif" 



## taking an initial look at Mark's maps
# each file can be opened as a raster stack which has a band per crop (See table in README from Mark)

map1_stack <- stack(paste0(datadir, "/Marks_Maps/", files[1]))

plot(map1_stack[[1]])       

res(map1_stack[[1]]) # 0.08333333 0.08333333

crs(map1_stack[[1]])# +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 


#### get country border data for the list of countries ####
View(getData('ISO3')) 

# get the country codes to extract country polygons
codes <- getData('ISO3')

# some names do not match so added in manually
cntry_codes <- codes[codes$NAME %in% countries | codes$NAME == "United States" | codes$NAME == "Swaziland" | codes$NAME == "Côte d'Ivoire", ]

# extract country shapefiles for 
codes = list()
codes$countries <- cntry_codes$ISO3

# download and combine polygons into one object
ctry_shps = do.call("bind", lapply(codes$countries, 
                                   function(x) getData('GADM', country=x, level=0)))

# this takes a little while to download all the polygons

# take a little look
plot(ctry_shps)
plot(map1_stack[[1]], add = TRUE) 

# save this polygons object for future use
shapefile(ctry_shps, filename = paste0(outdir, "/Trade_partners_polygons.shp"))



#### get some summary stat for each country/indicator/crop combo ####


# thinking... which stats will be useful
# mean across cells
# range 
# sd

# what other summaries would be good?

#### 1. GHG Total indicator map ####

# load in the raster stack
GHG <- stack(paste0(datadir, "/Marks_Maps/", files[1]))

# use exact_extract function to get some summary stats per country/band
# the function does the same for each layer automatically

GHG_sums <- exact_extract(x = GHG, y = ctry_shps, fun = c('mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(GHG_sums) <- ctry_shps$NAME_0

colnames(GHG_sums) <- sub("GHG_Emissons_Total.1", "Cocoa", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.2", "Oilpalm", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.3", "SugarBeet", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.4", "SugarCane", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.5", "Wheat", colnames(GHG_sums))

#### 2. Land biodiversity impact ####

LND <- stack(paste0(datadir, "/Marks_Maps/", files[2]))

LND_sums <- exact_extract(x = LND, y = ctry_shps, fun = c('mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(LND_sums) <- ctry_shps$NAME_0

colnames(LND_sums) <- sub("GHG_Emissons_Total.1", "Cocoa", colnames(LND_sums))
colnames(LND_sums) <- sub("GHG_Emissons_Total.2", "Oilpalm", colnames(LND_sums))
colnames(LND_sums) <- sub("GHG_Emissons_Total.3", "SugarBeet", colnames(LND_sums))
colnames(LND_sums) <- sub("GHG_Emissons_Total.4", "SugarCane", colnames(LND_sums))
colnames(LND_sums) <- sub("GHG_Emissons_Total.5", "Wheat", colnames(LND_sums))

#### 3. N biodiv impact ####

Nit <- stack(paste0(datadir, "/Marks_Maps/", files[3]))

Nit_sums <- exact_extract(x = Nit, y = ctry_shps, fun = c('mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(Nit_sums) <- ctry_shps$NAME_0

colnames(Nit_sums) <- sub("GHG_Emissons_Total.1", "Oilpalm", colnames(Nit_sums))
colnames(Nit_sums) <- sub("GHG_Emissons_Total.2", "SugarBeet", colnames(Nit_sums))
colnames(Nit_sums) <- sub("GHG_Emissons_Total.3", "SugarCane", colnames(Nit_sums))
colnames(Nit_sums) <- sub("GHG_Emissons_Total.4", "Wheat", colnames(Nit_sums))


#### 4. P biodiv impact ####

Pho <- stack(paste0(datadir, "/Marks_Maps/", files[4]))

Pho_sums <- exact_extract(x = Pho, y = ctry_shps, fun = c('mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(Pho_sums) <- ctry_shps$NAME_0

colnames(Pho_sums) <- sub("GHG_Emissons_Total.1", "Oilpalm", colnames(Pho_sums))
colnames(Pho_sums) <- sub("GHG_Emissons_Total.2", "SugarBeet", colnames(Pho_sums))
colnames(Pho_sums) <- sub("GHG_Emissons_Total.3", "SugarCane", colnames(Pho_sums))
colnames(Pho_sums) <- sub("GHG_Emissons_Total.4", "Wheat", colnames(Pho_sums))


#### 5. water debt ####

WAT <- stack(paste0(datadir, "/Marks_Maps/", files[5]))

WAT_sums <- exact_extract(x = WAT, y = ctry_shps, fun = c('mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(WAT_sums) <- ctry_shps$NAME_0

colnames(WAT_sums) <- sub("GHG_Emissons_Total.1", "Oilpalm", colnames(WAT_sums))
colnames(WAT_sums) <- sub("GHG_Emissons_Total.2", "SugarBeet", colnames(WAT_sums))
colnames(WAT_sums) <- sub("GHG_Emissons_Total.3", "SugarCane", colnames(WAT_sums))
colnames(WAT_sums) <- sub("GHG_Emissons_Total.4", "Wheat", colnames(WAT_sums))

