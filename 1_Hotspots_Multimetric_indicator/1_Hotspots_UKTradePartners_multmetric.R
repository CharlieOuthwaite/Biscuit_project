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
# this is saved in the Google drive (currently missing cocoa - to be added hopefully today)

# read in Carole's list
suppliers <- read.csv(paste0(datadir, "/UK_Suppliers_main_crops_Carole.csv"))

View(suppliers)

# get a list of countries (note this does not include cocoa at the moment)
countries <- unique(suppliers$partner)

length(countries) # currently 18 suppliers of interest



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
cntry_codes <- codes[codes$NAME %in% countries | codes$NAME == "United States" | codes$NAME == "Swaziland", ]

# extract country shapefiles for 
codes = list()
codes$countries <- cntry_codes$ISO3

# download and combine polygons into one object
ctry_shps = do.call("bind", lapply(codes$countries, 
                                   function(x) getData('GADM', country=x, level=0)))


# save this polygons object for future use





#### get some summary stat for each country/indicator/crop combo ####


# thinking... which stats will be useful
# mean across cells
# range 
# sd

# what other summaries would be good?









