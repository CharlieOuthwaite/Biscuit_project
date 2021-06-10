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












# ideas:

# 1. look specifically at the area that is covered by each crop (use Earthstat maps)
# and summarise the indicators for those crop specific areas. 

