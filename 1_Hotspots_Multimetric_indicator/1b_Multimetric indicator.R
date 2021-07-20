##%######################################################%##
#                                                          #
#                1b_Multimetric indicator               ####
#              hotspots, UK trade partners                 #
#                                                          #
##%######################################################%##
#my part creates a map for every UK partner country (GHG impact)

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
library(tiff)
library(ggplot2)
library(viridis)
library(rgeos)
library(sf)  
library(patchwork)

#API key in file (Google Earth.R) 

#theme for maps
theme_map <- function(...) {
theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "#22211d"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    ...
  )
}

# set directories
setwd("/Users/Feli/Documents/Cookie Project")
datadir <- "Data"
outdir <- "1_Hotspots_Multimetric_indicator"

# take a look at the raster files and create a list
files <- list.files(paste0(datadir, "/Marks_Maps"), pattern = ".tif", full.names = T)
files

# Notes from Mark's readme doc
# Files ending with:
# a.	_CT = per tonne of crop for each grid cell
# b.	_Total = Total Impact for each grid cell


# which countries are we interested in?
# Use Carole's list of key trade partners for our crops of interest for 2003 (I think)
# this is saved in the Google drive (currently missing cocoa - to be added hopefully today)

# read in Carole's list
suppliers <- read.csv(paste0(datadir, "/UK_Suppliers_main_crops_Carole.csv"))
colnames(suppliers)[4] <- "NAME"

# get a list of countries (note this does not include cocoa at the moment)
countries <- unique(suppliers$NAME)

length(countries) # currently 21 suppliers of interest

# Task 1: Summary stats for each country ####

# For now, will just use the total impact raster files

files <- list.files(paste0(datadir, "/Marks_Maps"), pattern = "_Total.tif", full.names = TRUE)

# 5 files
# "GHG_Emissons_Total.tif"
# "LD_BioDiv_Total.tif"
# "N_Marine_BioDiv_Total.tif"
# "P_Marine_BioDiv_Total.tif"
# "Water_Debt_Total.tif" 


## taking an initial look at Mark's maps
# each file can be opened as a raster stack which has a band per crop (See table in README from Mark)

map1_stack <- stack(paste0(files[1]))

#plot(map1_stack[[1]])       

res(map1_stack[[1]]) # 0.08333333 0.08333333

crs(map1_stack[[1]])# +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 


#get country border data for the list of countries #
#View(getData('ISO3')) 

# get the country codes to extract country polygons
codes <- getData('ISO3')

# some names do not match so added in manually
cntry_codes <- codes[codes$NAME %in% countries | codes$NAME == "United States" | codes$NAME == "Swaziland", ]

#add cntry_codes to supplier list
suppliers <- merge(suppliers,cntry_codes,by= c("NAME"))

# extract country shapefiles for 
codes = list()
codes$countries <- cntry_codes$ISO3

# download and combine polygons into one object
ctry_shps = do.call("bind", lapply(codes$countries, 
                                   function(x) getData('GADM', country=x, level=0)))


# save this polygons object for future use

#Feli's part starts here####
# Task 2 : Making Maps for each country ####

#explore each country (maps)


#import all raster files (with stack to have all bands)
mapGHG_stack <- stack(paste0(files[1])) #GHG Emissions Total (5 Bands)
mapLD_stack <- stack(paste0(files[2])) #LD BioDiv Total (5 Bands)
mapN_stack <- stack(paste0(files[3])) #N Marine BioDiv Total (4 Bands)
mapP_stack <- stack(paste0(files[4])) #P Marine BioDiv Total (4 Bands)
mapWD_stack <- stack(paste0(files[5])) #Water Debt Total (4 Bands)


## Task 2A) Wheat ####

### Germany ####

#google maps - Germany
DEU_sf <- qmap("germany", zoom = 6,maptype = "hybrid")

#extent for Germany
DEU_shps <- ctry_shps[ctry_shps$GID_0=="DEU",]

#save extent for future cropping of data
DEU_extent <- as(extent(DEU_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropW_DEU_GHG <- crop(mapGHG_stack[[5]], DEU_extent)

#Raster to Points
mapGHG_Wheat_DEU_pts <- rasterToPoints(CropW_DEU_GHG, spatial = TRUE)
mapGHG_Wheat_DEU_df  <- data.frame(mapGHG_Wheat_DEU_pts)
rm(mapGHG_Wheat_DEU_pts)

#map: 
mapGHG_DEU_Wheat <- DEU_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.5),
            data = mapGHG_Wheat_DEU_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Wheat (total)", 
       subtitle = "Germany") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_DEU_Wheat

#### Land Biodiversity Impact #####
#crop raster
CropW_DEU_LD <- crop(mapLD_stack[[5]], DEU_extent)

#Raster to Points
mapLD_Wheat_DEU_pts <- rasterToPoints(CropW_DEU_LD, spatial = TRUE)
mapLD_Wheat_DEU_df  <- data.frame(mapLD_Wheat_DEU_pts)
rm(mapLD_Wheat_DEU_pts)

#names of df
names(mapLD_Wheat_DEU_df)

#create Map
mapLD_DEU_Wheat <- DEU_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.5),
            data = mapLD_Wheat_DEU_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Wheat (total)", 
       subtitle = "Germany") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_DEU_Wheat

#### N_Marine_BioDiv #####
#crop raster
CropW_DEU_N <- crop(mapN_stack[[4]], DEU_extent)

#Raster to Points
mapN_Wheat_DEU_pts <- rasterToPoints(CropW_DEU_N, spatial = TRUE)
mapN_Wheat_DEU_df  <- data.frame(mapN_Wheat_DEU_pts)
rm(mapN_Wheat_DEU_pts)

#names of df
names(mapN_Wheat_DEU_df)

#create map
mapN_DEU_Wheat <- DEU_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.4),
            data = mapN_Wheat_DEU_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Wheat (total)", 
       subtitle = "Germany") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_DEU_Wheat

#### P_Marine_BioDiv #####
#crop raster
CropW_DEU_P <- crop(mapP_stack[[4]], DEU_extent)

#Raster to Points
mapP_Wheat_DEU_pts <- rasterToPoints(CropW_DEU_P, spatial = TRUE)
mapP_Wheat_DEU_df  <- data.frame(mapP_Wheat_DEU_pts)
rm(mapP_Wheat_DEU_pts)

#names of df
names(mapP_Wheat_DEU_df)

#create map
mapP_DEU_Wheat <- DEU_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.4),
            data = mapP_Wheat_DEU_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Wheat (total)", 
       subtitle = "Germany") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_DEU_Wheat

#### Water Depth #####
#crop raster
CropW_DEU_WD <- crop(mapWD_stack[[4]], DEU_extent)

#Raster to Points
mapWD_Wheat_DEU_pts <- rasterToPoints(CropW_DEU_WD, spatial = TRUE)
mapWD_Wheat_DEU_df  <- data.frame(mapWD_Wheat_DEU_pts)
rm(mapWD_Wheat_DEU_pts)

#names of df
names(mapWD_Wheat_DEU_df)

#create map
mapWD_DEU_Wheat <- DEU_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.4),
            data = mapWD_Wheat_DEU_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Wheat (total)", 
       subtitle = "Germany") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_DEU_Wheat


#extent for UK
GBR_shps <- ctry_shps[ctry_shps$GID_0=="GBR",]

#example map for UK
GBR_sf <- qmap("uk", zoom = 5,maptype = "hybrid")


### UK ####

#google maps - GBR
GBR_sf <- qmap("uk", zoom = 5,maptype = "hybrid")

#extent for GBR
GBR_shps <- ctry_shps[ctry_shps$GID_0=="GBR",]

#save extent for future cropping of data
GBR_extent <- as(extent(GBR_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropW_GBR_GHG <- crop(mapGHG_stack[[5]], GBR_extent)

#Raster to Points
mapGHG_Wheat_GBR_pts <- rasterToPoints(CropW_GBR_GHG, spatial = TRUE)
mapGHG_Wheat_GBR_df  <- data.frame(mapGHG_Wheat_GBR_pts)
rm(mapGHG_Wheat_GBR_pts)

#map: 
mapGHG_GBR_Wheat <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.5),
            data = mapGHG_Wheat_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Wheat (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_GBR_Wheat

#### Land Biodiversity Impact #####
#crop raster
CropW_GBR_LD <- crop(mapLD_stack[[5]], GBR_extent)

#Raster to Points
mapLD_Wheat_GBR_pts <- rasterToPoints(CropW_GBR_LD, spatial = TRUE)
mapLD_Wheat_GBR_df  <- data.frame(mapLD_Wheat_GBR_pts)
rm(mapLD_Wheat_GBR_pts)

#names of df
names(mapLD_Wheat_GBR_df)

#create Map
mapLD_GBR_Wheat <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.5),
            data = mapLD_Wheat_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Wheat (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_GBR_Wheat

#### N_Marine_BioDiv #####
#crop raster
CropW_GBR_N <- crop(mapN_stack[[4]], GBR_extent)

#Raster to Points
mapN_Wheat_GBR_pts <- rasterToPoints(CropW_GBR_N, spatial = TRUE)
mapN_Wheat_GBR_df  <- data.frame(mapN_Wheat_GBR_pts)
rm(mapN_Wheat_GBR_pts)

#names of df
names(mapN_Wheat_GBR_df)

#create map
mapN_GBR_Wheat <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.4),
            data = mapN_Wheat_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Wheat (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_GBR_Wheat

#### P_Marine_BioDiv #####
#crop raster
CropW_GBR_P <- crop(mapP_stack[[4]], GBR_extent)

#Raster to Points
mapP_Wheat_GBR_pts <- rasterToPoints(CropW_GBR_P, spatial = TRUE)
mapP_Wheat_GBR_df  <- data.frame(mapP_Wheat_GBR_pts)
rm(mapP_Wheat_GBR_pts)

#names of df
names(mapP_Wheat_GBR_df)

#create map
mapP_GBR_Wheat <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.4),
            data = mapP_Wheat_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Wheat (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_GBR_Wheat

#### Water Depth #####
#crop raster
CropW_GBR_WD <- crop(mapWD_stack[[4]], GBR_extent)

#Raster to Points
mapWD_Wheat_GBR_pts <- rasterToPoints(CropW_GBR_WD, spatial = TRUE)
mapWD_Wheat_GBR_df  <- data.frame(mapWD_Wheat_GBR_pts)
rm(mapWD_Wheat_GBR_pts)

#names of df
names(mapWD_Wheat_GBR_df)

#create map
mapWD_GBR_Wheat <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.4),
            data = mapWD_Wheat_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Wheat (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_GBR_Wheat

####print all maps for GBR:####

mapGHG_GBR_Wheat
mapLD_GBR_Wheat
mapN_GBR_Wheat
mapP_GBR_Wheat
mapWD_GBR_Wheat




### FRA - France ####

#google maps - FRA
FRA_sf <- qmap("france", zoom = 6,maptype = "hybrid")

#extent for FRA
FRA_shps <- ctry_shps[ctry_shps$GID_0=="FRA",]

#save extent for future cropping of data
FRA_extent <- as(extent(FRA_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropW_FRA_GHG <- crop(mapGHG_stack[[5]], FRA_extent)

#Raster to Points
mapGHG_Wheat_FRA_pts <- rasterToPoints(CropW_FRA_GHG, spatial = TRUE)
mapGHG_Wheat_FRA_df  <- data.frame(mapGHG_Wheat_FRA_pts)
rm(mapGHG_Wheat_FRA_pts)

#map: 
mapGHG_FRA_Wheat <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.5),
            data = mapGHG_Wheat_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Wheat (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_FRA_Wheat

#### Land Biodiversity Impact #####
#crop raster
CropW_FRA_LD <- crop(mapLD_stack[[5]], FRA_extent)

#Raster to Points
mapLD_Wheat_FRA_pts <- rasterToPoints(CropW_FRA_LD, spatial = TRUE)
mapLD_Wheat_FRA_df  <- data.frame(mapLD_Wheat_FRA_pts)
rm(mapLD_Wheat_FRA_pts)

#names of df
names(mapLD_Wheat_FRA_df)

#create Map
mapLD_FRA_Wheat <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.5),
            data = mapLD_Wheat_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Wheat (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_FRA_Wheat

#### N_Marine_BioDiv #####
#crop raster
CropW_FRA_N <- crop(mapN_stack[[4]], FRA_extent)

#Raster to Points
mapN_Wheat_FRA_pts <- rasterToPoints(CropW_FRA_N, spatial = TRUE)
mapN_Wheat_FRA_df  <- data.frame(mapN_Wheat_FRA_pts)
rm(mapN_Wheat_FRA_pts)

#names of df
names(mapN_Wheat_FRA_df)

#create map
mapN_FRA_Wheat <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.4),
            data = mapN_Wheat_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Wheat (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_FRA_Wheat

#### P_Marine_BioDiv #####
#crop raster
CropW_FRA_P <- crop(mapP_stack[[4]], FRA_extent)

#Raster to Points
mapP_Wheat_FRA_pts <- rasterToPoints(CropW_FRA_P, spatial = TRUE)
mapP_Wheat_FRA_df  <- data.frame(mapP_Wheat_FRA_pts)
rm(mapP_Wheat_FRA_pts)

#names of df
names(mapP_Wheat_FRA_df)

#create map
mapP_FRA_Wheat <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.4),
            data = mapP_Wheat_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Wheat (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_FRA_Wheat

#### Water Depth #####
#crop raster
CropW_FRA_WD <- crop(mapWD_stack[[4]], FRA_extent)

#Raster to Points
mapWD_Wheat_FRA_pts <- rasterToPoints(CropW_FRA_WD, spatial = TRUE)
mapWD_Wheat_FRA_df  <- data.frame(mapWD_Wheat_FRA_pts)
rm(mapWD_Wheat_FRA_pts)

#names of df
names(mapWD_Wheat_FRA_df)

#create map
mapWD_FRA_Wheat <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.4),
            data = mapWD_Wheat_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Wheat (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_FRA_Wheat

####print all maps for FRA:####

mapGHG_FRA_Wheat
mapLD_FRA_Wheat
mapN_FRA_Wheat
mapP_FRA_Wheat
mapWD_FRA_Wheat


### USA ####

#google maps - USA
USA_sf <- qmap("usa", zoom = 4,maptype = "hybrid")

#extent for USA
USA_shps <- ctry_shps[ctry_shps$GID_0=="USA",]

#save extent for future cropping of data
USA_extent <- as(extent(USA_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropW_USA_GHG <- crop(mapGHG_stack[[5]], USA_extent)

#Raster to Points
mapGHG_Wheat_USA_pts <- rasterToPoints(CropW_USA_GHG, spatial = TRUE)
mapGHG_Wheat_USA_df  <- data.frame(mapGHG_Wheat_USA_pts)
rm(mapGHG_Wheat_USA_pts)

#map: 
mapGHG_USA_Wheat <- USA_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.5),
            data = mapGHG_Wheat_USA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Wheat (total)", 
       subtitle = "USA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_USA_Wheat

#### Land Biodiversity Impact #####
#crop raster
CropW_USA_LD <- crop(mapLD_stack[[5]], USA_extent)

#Raster to Points
mapLD_Wheat_USA_pts <- rasterToPoints(CropW_USA_LD, spatial = TRUE)
mapLD_Wheat_USA_df  <- data.frame(mapLD_Wheat_USA_pts)
rm(mapLD_Wheat_USA_pts)

#names of df
names(mapLD_Wheat_USA_df)

#create Map
mapLD_USA_Wheat <- USA_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.5),
            data = mapLD_Wheat_USA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Wheat (total)", 
       subtitle = "USA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_USA_Wheat

#### N_Marine_BioDiv #####
#crop raster
CropW_USA_N <- crop(mapN_stack[[4]], USA_extent)

#Raster to Points
mapN_Wheat_USA_pts <- rasterToPoints(CropW_USA_N, spatial = TRUE)
mapN_Wheat_USA_df  <- data.frame(mapN_Wheat_USA_pts)
rm(mapN_Wheat_USA_pts)

#names of df
names(mapN_Wheat_USA_df)

#create map
mapN_USA_Wheat <- USA_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.4),
            data = mapN_Wheat_USA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Wheat (total)", 
       subtitle = "USA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_USA_Wheat

#### P_Marine_BioDiv #####
#crop raster
CropW_USA_P <- crop(mapP_stack[[4]], USA_extent)

#Raster to Points
mapP_Wheat_USA_pts <- rasterToPoints(CropW_USA_P, spatial = TRUE)
mapP_Wheat_USA_df  <- data.frame(mapP_Wheat_USA_pts)
rm(mapP_Wheat_USA_pts)

#names of df
names(mapP_Wheat_USA_df)

#create map
mapP_USA_Wheat <- USA_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.4),
            data = mapP_Wheat_USA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Wheat (total)", 
       subtitle = "USA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_USA_Wheat

#### Water Depth #####
#crop raster
CropW_USA_WD <- crop(mapWD_stack[[4]], USA_extent)

#Raster to Points
mapWD_Wheat_USA_pts <- rasterToPoints(CropW_USA_WD, spatial = TRUE)
mapWD_Wheat_USA_df  <- data.frame(mapWD_Wheat_USA_pts)
rm(mapWD_Wheat_USA_pts)

#names of df
names(mapWD_Wheat_USA_df)

#create map
mapWD_USA_Wheat <- USA_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.4),
            data = mapWD_Wheat_USA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Wheat (total)", 
       subtitle = "USA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_USA_Wheat

####print all maps for USA:####

mapGHG_USA_Wheat
mapLD_USA_Wheat
mapN_USA_Wheat
mapP_USA_Wheat
mapWD_USA_Wheat


### CAN ####

#google maps - CAN
CAN_sf <- qmap("Canada", zoom = 5,maptype = "hybrid")

#extent for CAN
CAN_shps <- ctry_shps[ctry_shps$GID_0=="CAN",]

#save extent for future cropping of data
CAN_extent <- as(extent(CAN_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropW_CAN_GHG <- crop(mapGHG_stack[[5]], CAN_extent)

#Raster to Points
mapGHG_Wheat_CAN_pts <- rasterToPoints(CropW_CAN_GHG, spatial = TRUE)
mapGHG_Wheat_CAN_df  <- data.frame(mapGHG_Wheat_CAN_pts)
rm(mapGHG_Wheat_CAN_pts)

#map: 
mapGHG_CAN_Wheat <- CAN_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.5),
            data = mapGHG_Wheat_CAN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Wheat (total)", 
       subtitle = "CAN") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_CAN_Wheat

#### Land Biodiversity Impact #####
#crop raster
CropW_CAN_LD <- crop(mapLD_stack[[5]], CAN_extent)

#Raster to Points
mapLD_Wheat_CAN_pts <- rasterToPoints(CropW_CAN_LD, spatial = TRUE)
mapLD_Wheat_CAN_df  <- data.frame(mapLD_Wheat_CAN_pts)
rm(mapLD_Wheat_CAN_pts)

#names of df
names(mapLD_Wheat_CAN_df)

#create Map
mapLD_CAN_Wheat <- CAN_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.5),
            data = mapLD_Wheat_CAN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Wheat (total)", 
       subtitle = "CAN") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_CAN_Wheat

#### N_Marine_BioDiv #####
#crop raster
CropW_CAN_N <- crop(mapN_stack[[4]], CAN_extent)

#Raster to Points
mapN_Wheat_CAN_pts <- rasterToPoints(CropW_CAN_N, spatial = TRUE)
mapN_Wheat_CAN_df  <- data.frame(mapN_Wheat_CAN_pts)
rm(mapN_Wheat_CAN_pts)

#names of df
names(mapN_Wheat_CAN_df)

#create map
mapN_CAN_Wheat <- CAN_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.4),
            data = mapN_Wheat_CAN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Wheat (total)", 
       subtitle = "CAN") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_CAN_Wheat

#### P_Marine_BioDiv #####
#crop raster
CropW_CAN_P <- crop(mapP_stack[[4]], CAN_extent)

#Raster to Points
mapP_Wheat_CAN_pts <- rasterToPoints(CropW_CAN_P, spatial = TRUE)
mapP_Wheat_CAN_df  <- data.frame(mapP_Wheat_CAN_pts)
rm(mapP_Wheat_CAN_pts)

#names of df
names(mapP_Wheat_CAN_df)

#create map
mapP_CAN_Wheat <- CAN_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.4),
            data = mapP_Wheat_CAN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Wheat (total)", 
       subtitle = "CAN") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_CAN_Wheat

#### Water Depth #####
#crop raster
CropW_CAN_WD <- crop(mapWD_stack[[4]], CAN_extent)

#Raster to Points
mapWD_Wheat_CAN_pts <- rasterToPoints(CropW_CAN_WD, spatial = TRUE)
mapWD_Wheat_CAN_df  <- data.frame(mapWD_Wheat_CAN_pts)
rm(mapWD_Wheat_CAN_pts)

#names of df
names(mapWD_Wheat_CAN_df)

#create map
mapWD_CAN_Wheat <- CAN_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.4),
            data = mapWD_Wheat_CAN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Wheat (total)", 
       subtitle = "CAN") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_CAN_Wheat

####print all maps for CAN:####

mapGHG_CAN_Wheat
mapLD_CAN_Wheat
mapN_CAN_Wheat
mapP_CAN_Wheat
mapWD_CAN_Wheat


#all maps for wheat####
mapGHG_DEU_Wheat + mapGHG_GBR_Wheat + mapGHG_FRA_Wheat + mapGHG_USA_Wheat + mapGHG_CAN_Wheat
mapLD_DEU_Wheat + mapLD_GBR_Wheat + mapLD_FRA_Wheat + mapLD_USA_Wheat + mapLD_CAN_Wheat
mapN_DEU_Wheat + mapN_GBR_Wheat + mapN_FRA_Wheat + mapN_USA_Wheat + mapN_CAN_Wheat
mapP_DEU_Wheat + mapP_GBR_Wheat + mapP_FRA_Wheat + mapP_USA_Wheat + mapP_CAN_Wheat
mapWD_DEU_Wheat + mapWD_GBR_Wheat + mapWD_FRA_Wheat + mapWD_USA_Wheat + mapWD_CAN_Wheat



## Task 2B) Sugar ####


### UK ####

#google maps - GBR
GBR_sf <- qmap("uk", zoom = 5,maptype = "hybrid")

#extent for GBR
GBR_shps <- ctry_shps[ctry_shps$GID_0=="GBR",]

#save extent for future cropping of data
GBR_extent <- as(extent(GBR_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropS_GBR_GHG <- crop(mapGHG_stack[[3]], GBR_extent)

#Raster to Points
mapGHG_Sugar_GBR_pts <- rasterToPoints(CropS_GBR_GHG, spatial = TRUE)
mapGHG_Sugar_GBR_df  <- data.frame(mapGHG_Sugar_GBR_pts)
rm(mapGHG_Sugar_GBR_pts)

#names of df
names(mapGHG_Sugar_GBR_df)

#map: 
mapGHG_GBR_Sugar <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.3),
            data = mapGHG_Sugar_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Sugar (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_GBR_Sugar

#### Land Biodiversity Impact #####
#crop raster
CropS_GBR_LD <- crop(mapLD_stack[[3]], GBR_extent)

#Raster to Points
mapLD_Sugar_GBR_pts <- rasterToPoints(CropS_GBR_LD, spatial = TRUE)
mapLD_Sugar_GBR_df  <- data.frame(mapLD_Sugar_GBR_pts)
rm(mapLD_Sugar_GBR_pts)

#names of df
names(mapLD_Sugar_GBR_df)

#create Map
mapLD_GBR_Sugar <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.3),
            data = mapLD_Sugar_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Sugar (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_GBR_Sugar

#### N_Marine_BioDiv #####
#crop raster
CropS_GBR_N <- crop(mapN_stack[[2]], GBR_extent)

#Raster to Points
mapN_Sugar_GBR_pts <- rasterToPoints(CropS_GBR_N, spatial = TRUE)
mapN_Sugar_GBR_df  <- data.frame(mapN_Sugar_GBR_pts)
rm(mapN_Sugar_GBR_pts)

#names of df
names(mapN_Sugar_GBR_df)

#create map
mapN_GBR_Sugar <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.2),
            data = mapN_Sugar_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_GBR_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_GBR_P <- crop(mapP_stack[[2]], GBR_extent)

#Raster to Points
mapP_Sugar_GBR_pts <- rasterToPoints(CropS_GBR_P, spatial = TRUE)
mapP_Sugar_GBR_df  <- data.frame(mapP_Sugar_GBR_pts)
rm(mapP_Sugar_GBR_pts)

#names of df
names(mapP_Sugar_GBR_df)

#create map
mapP_GBR_Sugar <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.2),
            data = mapP_Sugar_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_GBR_Sugar

#### Water Depth #####
#crop raster
CropS_GBR_WD <- crop(mapWD_stack[[2]], GBR_extent)

#Raster to Points
mapWD_Sugar_GBR_pts <- rasterToPoints(CropS_GBR_WD, spatial = TRUE)
mapWD_Sugar_GBR_df  <- data.frame(mapWD_Sugar_GBR_pts)
rm(mapWD_Sugar_GBR_pts)

#names of df
names(mapWD_Sugar_GBR_df)

#create map
mapWD_GBR_Sugar <- GBR_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.2),
            data = mapWD_Sugar_GBR_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "GBR") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_GBR_Sugar

####print all maps for GBR:####

mapGHG_GBR_Sugar
mapLD_GBR_Sugar
mapN_GBR_Sugar
mapP_GBR_Sugar
mapWD_GBR_Sugar




### FRA - France ####

#google maps - FRA
FRA_sf <- qmap("france", zoom = 6,maptype = "hybrid")

#extent for FRA
FRA_shps <- ctry_shps[ctry_shps$GID_0=="FRA",]

#save extent for future cropping of data
FRA_extent <- as(extent(FRA_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropS_FRA_GHG <- crop(mapGHG_stack[[3]], FRA_extent)

#Raster to Points
mapGHG_Sugar_FRA_pts <- rasterToPoints(CropS_FRA_GHG, spatial = TRUE)
mapGHG_Sugar_FRA_df  <- data.frame(mapGHG_Sugar_FRA_pts)
rm(mapGHG_Sugar_FRA_pts)

#names of df
names(mapGHG_Sugar_FRA_df)

#map: 
mapGHG_FRA_Sugar <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.3),
            data = mapGHG_Sugar_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Sugar (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_FRA_Sugar

#### Land Biodiversity Impact #####
#crop raster
CropS_FRA_LD <- crop(mapLD_stack[[3]], FRA_extent)

#Raster to Points
mapLD_Sugar_FRA_pts <- rasterToPoints(CropS_FRA_LD, spatial = TRUE)
mapLD_Sugar_FRA_df  <- data.frame(mapLD_Sugar_FRA_pts)
rm(mapLD_Sugar_FRA_pts)

#names of df
names(mapLD_Sugar_FRA_df)

#create Map
mapLD_FRA_Sugar <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.3),
            data = mapLD_Sugar_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Sugar (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_FRA_Sugar

#### N_Marine_BioDiv #####
#crop raster
CropS_FRA_N <- crop(mapN_stack[[2]], FRA_extent)

#Raster to Points
mapN_Sugar_FRA_pts <- rasterToPoints(CropS_FRA_N, spatial = TRUE)
mapN_Sugar_FRA_df  <- data.frame(mapN_Sugar_FRA_pts)
rm(mapN_Sugar_FRA_pts)

#names of df
names(mapN_Sugar_FRA_df)

#create map
mapN_FRA_Sugar <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.2),
            data = mapN_Sugar_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_FRA_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_FRA_P <- crop(mapP_stack[[2]], FRA_extent)

#Raster to Points
mapP_Sugar_FRA_pts <- rasterToPoints(CropS_FRA_P, spatial = TRUE)
mapP_Sugar_FRA_df  <- data.frame(mapP_Sugar_FRA_pts)
rm(mapP_Sugar_FRA_pts)

#names of df
names(mapP_Sugar_FRA_df)

#create map
mapP_FRA_Sugar <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.2),
            data = mapP_Sugar_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_FRA_Sugar

#### Water Depth #####
#crop raster
CropS_FRA_WD <- crop(mapWD_stack[[2]], FRA_extent)

#Raster to Points
mapWD_Sugar_FRA_pts <- rasterToPoints(CropS_FRA_WD, spatial = TRUE)
mapWD_Sugar_FRA_df  <- data.frame(mapWD_Sugar_FRA_pts)
rm(mapWD_Sugar_FRA_pts)

#names of df
names(mapWD_Sugar_FRA_df)

#create map
mapWD_FRA_Sugar <- FRA_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.2),
            data = mapWD_Sugar_FRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "FRA") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_FRA_Sugar

####print all maps for FRA:####

mapGHG_FRA_Sugar
mapLD_FRA_Sugar
mapN_FRA_Sugar
mapP_FRA_Sugar
mapWD_FRA_Sugar


### Belize - BLZ ####

#google maps - BLZ
BLZ_sf <- qmap("Belize", zoom = 7,maptype = "hybrid")

#extent for BLZ
BLZ_shps <- ctry_shps[ctry_shps$GID_0=="BLZ",]

#save extent for future cropping of data
BLZ_extent <- as(extent(BLZ_shps), 'SpatialPolygons')


#### GHG #####
#no data

#### Land Biodiversity Impact #####
#no data

#### N_Marine_BioDiv #####
#crop raster
CropS_BLZ_N <- crop(mapN_stack[[3]], BLZ_extent)

#Raster to Points
mapN_Sugar_BLZ_pts <- rasterToPoints(CropS_BLZ_N, spatial = TRUE)
mapN_Sugar_BLZ_df  <- data.frame(mapN_Sugar_BLZ_pts)
rm(mapN_Sugar_BLZ_pts)

#names of df
names(mapN_Sugar_BLZ_df)

#create map
mapN_BLZ_Sugar <- BLZ_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.3),
            data = mapN_Sugar_BLZ_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "Belize") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_BLZ_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_BLZ_P <- crop(mapP_stack[[3]], BLZ_extent)

#Raster to Points
mapP_Sugar_BLZ_pts <- rasterToPoints(CropS_BLZ_P, spatial = TRUE)
mapP_Sugar_BLZ_df  <- data.frame(mapP_Sugar_BLZ_pts)
rm(mapP_Sugar_BLZ_pts)

#names of df
names(mapP_Sugar_BLZ_df)

#create map
mapP_BLZ_Sugar <- BLZ_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.3),
            data = mapP_Sugar_BLZ_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "Belize") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_BLZ_Sugar

#### Water Depth #####
#crop raster
CropS_BLZ_WD <- crop(mapWD_stack[[3]], BLZ_extent)

#Raster to Points
mapWD_Sugar_BLZ_pts <- rasterToPoints(CropS_BLZ_WD, spatial = TRUE)
mapWD_Sugar_BLZ_df  <- data.frame(mapWD_Sugar_BLZ_pts)
rm(mapWD_Sugar_BLZ_pts)

#names of df
names(mapWD_Sugar_BLZ_df)

#create map
mapWD_BLZ_Sugar <- BLZ_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.3),
            data = mapWD_Sugar_BLZ_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "Belize") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_BLZ_Sugar

####print all maps for BLZ:####

mapGHG_BLZ_Sugar
mapLD_BLZ_Sugar
mapN_BLZ_Sugar
mapP_BLZ_Sugar
mapWD_BLZ_Sugar


### FJI ####

#google maps - FJI
FJI_sf <- qmap("Fiji", zoom = 8,maptype = "hybrid")
FJI_sf

#extent for FJI
FJI_shps <- ctry_shps[ctry_shps$GID_0=="FJI",]

#save extent for future cropping of data
FJI_extent <- as(extent(FJI_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropS_FJI_GHG <- crop(mapGHG_stack[[4]], FJI_extent)

#Raster to Points
mapGHG_Sugar_FJI_pts <- rasterToPoints(CropS_FJI_GHG, spatial = TRUE)
mapGHG_Sugar_FJI_df  <- data.frame(mapGHG_Sugar_FJI_pts)
rm(mapGHG_Sugar_FJI_pts)

#names of df
names(mapGHG_Sugar_FJI_df)

#map: 
mapGHG_FJI_Sugar <- FJI_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.4),
            data = mapGHG_Sugar_FJI_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Sugar (total)", 
       subtitle = "FJI") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_FJI_Sugar

#### Land Biodiversity Impact #####
#crop raster
CropS_FJI_LD <- crop(mapLD_stack[[4]], FJI_extent)

#Raster to Points
mapLD_Sugar_FJI_pts <- rasterToPoints(CropS_FJI_LD, spatial = TRUE)
mapLD_Sugar_FJI_df  <- data.frame(mapLD_Sugar_FJI_pts)
rm(mapLD_Sugar_FJI_pts)

#names of df
names(mapLD_Sugar_FJI_df)

#create Map
mapLD_FJI_Sugar <- FJI_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.4),
            data = mapLD_Sugar_FJI_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Sugar (total)", 
       subtitle = "FJI") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_FJI_Sugar

#### N_Marine_BioDiv #####
#crop raster
CropS_FJI_N <- crop(mapN_stack[[3]], FJI_extent)

#Raster to Points
mapN_Sugar_FJI_pts <- rasterToPoints(CropS_FJI_N, spatial = TRUE)
mapN_Sugar_FJI_df  <- data.frame(mapN_Sugar_FJI_pts)
rm(mapN_Sugar_FJI_pts)

#names of df
names(mapN_Sugar_FJI_df)

#create map
mapN_FJI_Sugar <- FJI_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.3),
            data = mapN_Sugar_FJI_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "FJI") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_FJI_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_FJI_P <- crop(mapP_stack[[3]], FJI_extent)

#Raster to Points
mapP_Sugar_FJI_pts <- rasterToPoints(CropS_FJI_P, spatial = TRUE)
mapP_Sugar_FJI_df  <- data.frame(mapP_Sugar_FJI_pts)
rm(mapP_Sugar_FJI_pts)

#names of df
names(mapP_Sugar_FJI_df)

#create map
mapP_FJI_Sugar <- FJI_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.3),
            data = mapP_Sugar_FJI_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "FJI") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_FJI_Sugar

#### Water Depth #####
#crop raster
CropS_FJI_WD <- crop(mapWD_stack[[3]], FJI_extent)

#Raster to Points
mapWD_Sugar_FJI_pts <- rasterToPoints(CropS_FJI_WD, spatial = TRUE)
mapWD_Sugar_FJI_df  <- data.frame(mapWD_Sugar_FJI_pts)
rm(mapWD_Sugar_FJI_pts)

#names of df
names(mapWD_Sugar_FJI_df)

#create map
mapWD_FJI_Sugar <- FJI_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.3),
            data = mapWD_Sugar_FJI_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "FJI") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_FJI_Sugar

####print all maps for FJI:####

mapGHG_FJI_Sugar
mapLD_FJI_Sugar
mapN_FJI_Sugar
mapP_FJI_Sugar
mapWD_FJI_Sugar

### Jamaica ####

#google maps - Jamaica
JAM_sf <- qmap("Jamaica", zoom = 8,maptype = "hybrid")
JAM_sf
#extent for Jamaica
JAM_shps <- ctry_shps[ctry_shps$GID_0=="JAM",]

#save extent for future cropping of data
JAM_extent <- as(extent(JAM_shps), 'SpatialPolygons')


#### GHG #####
#no data

#### Land Biodiversity Impact #####
#no data

#### N_Marine_BioDiv #####
#crop raster
CropS_JAM_N <- crop(mapN_stack[[3]], JAM_extent)

#Raster to Points
mapN_Sugar_JAM_pts <- rasterToPoints(CropS_JAM_N, spatial = TRUE)
mapN_Sugar_JAM_df  <- data.frame(mapN_Sugar_JAM_pts)
rm(mapN_Sugar_JAM_pts)

#names of df
names(mapN_Sugar_JAM_df)

#create map
mapN_JAM_Sugar <- JAM_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.3),
            data = mapN_Sugar_JAM_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "Jamaica") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_JAM_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_JAM_P <- crop(mapP_stack[[3]], JAM_extent)

#Raster to Points
mapP_Sugar_JAM_pts <- rasterToPoints(CropS_JAM_P, spatial = TRUE)
mapP_Sugar_JAM_df  <- data.frame(mapP_Sugar_JAM_pts)
rm(mapP_Sugar_JAM_pts)

#names of df
names(mapP_Sugar_JAM_df)

#create map
mapP_JAM_Sugar <- JAM_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.3),
            data = mapP_Sugar_JAM_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "Jamaica") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_JAM_Sugar

#### Water Depth #####
#crop raster
CropS_JAM_WD <- crop(mapWD_stack[[3]], JAM_extent)

#Raster to Points
mapWD_Sugar_JAM_pts <- rasterToPoints(CropS_JAM_WD, spatial = TRUE)
mapWD_Sugar_JAM_df  <- data.frame(mapWD_Sugar_JAM_pts)
rm(mapWD_Sugar_JAM_pts)

#names of df
names(mapWD_Sugar_JAM_df)

#create map
mapWD_JAM_Sugar <- JAM_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.3),
            data = mapWD_Sugar_JAM_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "Jamaica") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_JAM_Sugar

####print all maps for JAM:####


mapN_JAM_Sugar
mapP_JAM_Sugar
mapWD_JAM_Sugar



### Mauritius ####

#google maps - Mauritius
MUS_sf <- qmap("Mauritius", zoom = 9,maptype = "hybrid")
MUS_sf
#extent for Mauritius
MUS_shps <- ctry_shps[ctry_shps$GID_0=="MUS",]

#save extent for future cropping of data
MUS_extent <- as(extent(MUS_shps), 'SpatialPolygons')

#### GHG #no data #####
#### Land Biodiversity Impact #no data #####
#### N_Marine_BioDiv #no data #####
#### P_Marine_BioDiv #no data #####
#### Water Depth #no data #####

### Trinidad and Tobago ####

#google maps - Trinidad and Tobago
TTO_sf <- qmap("Trinidad and Tobago", zoom = 9,maptype = "hybrid")
TTO_sf
#extent for Trinidad and Tobago
TTO_shps <- ctry_shps[ctry_shps$GID_0=="TTO",]

#save extent for future cropping of data
TTO_extent <- as(extent(TTO_shps), 'SpatialPolygons')


#### GHG #no data #####
#### Land Biodiversity Impact #no data #####
#### N_Marine_BioDiv #####
#crop raster
CropS_TTO_N <- crop(mapN_stack[[3]], TTO_extent)

#Raster to Points
mapN_Sugar_TTO_pts <- rasterToPoints(CropS_TTO_N, spatial = TRUE)
mapN_Sugar_TTO_df  <- data.frame(mapN_Sugar_TTO_pts)
rm(mapN_Sugar_TTO_pts)

#names of df
names(mapN_Sugar_TTO_df)

#create map
mapN_TTO_Sugar <- TTO_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.3),
            data = mapN_Sugar_TTO_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "Trinidad and Tobago") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_TTO_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_TTO_P <- crop(mapP_stack[[3]], TTO_extent)

#Raster to Points
mapP_Sugar_TTO_pts <- rasterToPoints(CropS_TTO_P, spatial = TRUE)
mapP_Sugar_TTO_df  <- data.frame(mapP_Sugar_TTO_pts)
rm(mapP_Sugar_TTO_pts)

#names of df
names(mapP_Sugar_TTO_df)

#create map
mapP_TTO_Sugar <- TTO_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.3),
            data = mapP_Sugar_TTO_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "Trinidad and Tobago") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_TTO_Sugar

#### Water Depth #####
#crop raster
CropS_TTO_WD <- crop(mapWD_stack[[3]], TTO_extent)

#Raster to Points
mapWD_Sugar_TTO_pts <- rasterToPoints(CropS_TTO_WD, spatial = TRUE)
mapWD_Sugar_TTO_df  <- data.frame(mapWD_Sugar_TTO_pts)
rm(mapWD_Sugar_TTO_pts)

#names of df
names(mapWD_Sugar_TTO_df)

#create map
mapWD_TTO_Sugar <- TTO_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.3),
            data = mapWD_Sugar_TTO_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "Trinidad and Tobago") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_TTO_Sugar

####print all maps for Trinidad and Tobago:####

mapN_TTO_Sugar
mapP_TTO_Sugar
mapWD_TTO_Sugar

### South Africa ####

#google maps - South Africa
ZAF_sf <- qmap("South Africa", zoom = 5,maptype = "hybrid")
ZAF_sf
#extent for South Africa
ZAF_shps <- ctry_shps[ctry_shps$GID_0=="ZAF",]

#save extent for future cropping of data
ZAF_extent <- as(extent(ZAF_shps), 'SpatialPolygons')


#### GHG #no data #####


#### Land Biodiversity Impact #no data #####
#### N_Marine_BioDiv #####
#crop raster
CropS_ZAF_N <- crop(mapN_stack[[3]], ZAF_extent)

#Raster to Points
mapN_Sugar_ZAF_pts <- rasterToPoints(CropS_ZAF_N, spatial = TRUE)
mapN_Sugar_ZAF_df  <- data.frame(mapN_Sugar_ZAF_pts)
rm(mapN_Sugar_ZAF_pts)

#names of df
names(mapN_Sugar_ZAF_df)

#create map
mapN_ZAF_Sugar <- ZAF_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.3),
            data = mapN_Sugar_ZAF_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "South Africa") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_ZAF_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_ZAF_P <- crop(mapP_stack[[3]], ZAF_extent)

#Raster to Points
mapP_Sugar_ZAF_pts <- rasterToPoints(CropS_ZAF_P, spatial = TRUE)
mapP_Sugar_ZAF_df  <- data.frame(mapP_Sugar_ZAF_pts)
rm(mapP_Sugar_ZAF_pts)

#names of df
names(mapP_Sugar_ZAF_df)

#create map
mapP_ZAF_Sugar <- ZAF_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.3),
            data = mapP_Sugar_ZAF_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "South Africa") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_ZAF_Sugar

#### Water Depth #####
#crop raster
CropS_ZAF_WD <- crop(mapWD_stack[[3]], ZAF_extent)

#Raster to Points
mapWD_Sugar_ZAF_pts <- rasterToPoints(CropS_ZAF_WD, spatial = TRUE)
mapWD_Sugar_ZAF_df  <- data.frame(mapWD_Sugar_ZAF_pts)
rm(mapWD_Sugar_ZAF_pts)

#names of df
names(mapWD_Sugar_ZAF_df)

#create map
mapWD_ZAF_Sugar <- ZAF_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.3),
            data = mapWD_Sugar_ZAF_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "South Africa") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_ZAF_Sugar

####print all maps for South Africa:####

mapGHG_ZAF_Sugar
mapLD_ZAF_Sugar
mapN_ZAF_Sugar
mapP_ZAF_Sugar
mapWD_ZAF_Sugar

### Zimbabwe ####

#google maps - Zimbabwe
ZWE_sf <- qmap("Zimbabwe", zoom = 6,maptype = "hybrid")
ZWE_sf
#extent for Zimbabwe
ZWE_shps <- ctry_shps[ctry_shps$GID_0=="ZWE",]

#save extent for future cropping of data
ZWE_extent <- as(extent(ZWE_shps), 'SpatialPolygons')


#### GHG #no data #####
#### Land Biodiversity Impact #no data  #####
#### N_Marine_BioDiv #####
#crop raster
CropS_ZWE_N <- crop(mapN_stack[[3]], ZWE_extent)

#Raster to Points
mapN_Sugar_ZWE_pts <- rasterToPoints(CropS_ZWE_N, spatial = TRUE)
mapN_Sugar_ZWE_df  <- data.frame(mapN_Sugar_ZWE_pts)
rm(mapN_Sugar_ZWE_pts)

#names of df
names(mapN_Sugar_ZWE_df)

#create map
mapN_ZWE_Sugar <- ZWE_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.3),
            data = mapN_Sugar_ZWE_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Sugar (total)", 
       subtitle = "Zimbabwe") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_ZWE_Sugar

#### P_Marine_BioDiv #####
#crop raster
CropS_ZWE_P <- crop(mapP_stack[[3]], ZWE_extent)

#Raster to Points
mapP_Sugar_ZWE_pts <- rasterToPoints(CropS_ZWE_P, spatial = TRUE)
mapP_Sugar_ZWE_df  <- data.frame(mapP_Sugar_ZWE_pts)
rm(mapP_Sugar_ZWE_pts)

#names of df
names(mapP_Sugar_ZWE_df)

#create map
mapP_ZWE_Sugar <- ZWE_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.3),
            data = mapP_Sugar_ZWE_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Sugar (total)", 
       subtitle = "Zimbabwe") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_ZWE_Sugar

#### Water Depth #####
#crop raster
CropS_ZWE_WD <- crop(mapWD_stack[[3]], ZWE_extent)

#Raster to Points
mapWD_Sugar_ZWE_pts <- rasterToPoints(CropS_ZWE_WD, spatial = TRUE)
mapWD_Sugar_ZWE_df  <- data.frame(mapWD_Sugar_ZWE_pts)
rm(mapWD_Sugar_ZWE_pts)

#names of df
names(mapWD_Sugar_ZWE_df)

#create map
mapWD_ZWE_Sugar <- ZWE_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.3),
            data = mapWD_Sugar_ZWE_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Sugar (total)", 
       subtitle = "Zimbabwe") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_ZWE_Sugar

####print all maps for Zimbabwe:####

mapN_ZWE_Sugar
mapP_ZWE_Sugar
mapWD_ZWE_Sugar



## Task 2C) Palm ####


### Malaysia ####

#google maps - MYS
MYS_sf <- qmap("Malaysia", zoom = 7,maptype = "hybrid")
MYS_sf

#extent for MYS
MYS_shps <- ctry_shps[ctry_shps$GID_0=="MYS",]

#save extent for future cropping of data
MYS_extent <- as(extent(MYS_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropOP_MYS_GHG <- crop(mapGHG_stack[[2]], MYS_extent)

#Raster to Points
mapGHG_Palm_MYS_pts <- rasterToPoints(CropOP_MYS_GHG, spatial = TRUE)
mapGHG_Palm_MYS_df  <- data.frame(mapGHG_Palm_MYS_pts)
rm(mapGHG_Palm_MYS_pts)

#names of df
names(mapGHG_Palm_MYS_df)

#map: 
mapGHG_MYS_Palm <- MYS_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.2),
            data = mapGHG_Palm_MYS_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Malaysia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_MYS_Palm

#### Land Biodiversity Impact #####
#crop raster
CropOP_MYS_LD <- crop(mapLD_stack[[2]], MYS_extent)

#Raster to Points
mapLD_Palm_MYS_pts <- rasterToPoints(CropOP_MYS_LD, spatial = TRUE)
mapLD_Palm_MYS_df  <- data.frame(mapLD_Palm_MYS_pts)
rm(mapLD_Palm_MYS_pts)

#names of df
names(mapLD_Palm_MYS_df)

#create Map
mapLD_MYS_Palm <- MYS_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.2),
            data = mapLD_Palm_MYS_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Malaysia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_MYS_Palm

#### N_Marine_BioDiv #####
#crop raster
CropOP_MYS_N <- crop(mapN_stack[[1]], MYS_extent)

#Raster to Points
mapN_Palm_MYS_pts <- rasterToPoints(CropOP_MYS_N, spatial = TRUE)
mapN_Palm_MYS_df  <- data.frame(mapN_Palm_MYS_pts)
rm(mapN_Palm_MYS_pts)

#names of df
names(mapN_Palm_MYS_df)

#create map
mapN_MYS_Palm <- MYS_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.1),
            data = mapN_Palm_MYS_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Palm (total)", 
       subtitle = "Malaysia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_MYS_Palm

#### P_Marine_BioDiv #####
#crop raster
CropOP_MYS_P <- crop(mapP_stack[[1]], MYS_extent)

#Raster to Points
mapP_Palm_MYS_pts <- rasterToPoints(CropOP_MYS_P, spatial = TRUE)
mapP_Palm_MYS_df  <- data.frame(mapP_Palm_MYS_pts)
rm(mapP_Palm_MYS_pts)

#names of df
names(mapP_Palm_MYS_df)

#create map
mapP_MYS_Palm <- MYS_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.1),
            data = mapP_Palm_MYS_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Palm (total)", 
       subtitle = "Malaysia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_MYS_Palm

#### Water Depth #####
#crop raster
CropOP_MYS_WD <- crop(mapWD_stack[[1]], MYS_extent)

#Raster to Points
mapWD_Palm_MYS_pts <- rasterToPoints(CropOP_MYS_WD, spatial = TRUE)
mapWD_Palm_MYS_df  <- data.frame(mapWD_Palm_MYS_pts)
rm(mapWD_Palm_MYS_pts)

#names of df
names(mapWD_Palm_MYS_df)

#create map
mapWD_MYS_Palm <- MYS_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.1),
            data = mapWD_Palm_MYS_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Palm (total)", 
       subtitle = "Malaysia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_MYS_Palm

####print all maps for MYS:####

mapGHG_MYS_Palm
mapLD_MYS_Palm
mapN_MYS_Palm
mapP_MYS_Palm
mapWD_MYS_Palm


### Indonesia ####

#google maps - IDN
IDN_sf <- qmap("Indonesia", zoom = 4,maptype = "hybrid")
#IDN_sf
#extent for IDN
IDN_shps <- ctry_shps[ctry_shps$GID_0=="IDN",]

#save extent for future cropping of data
IDN_extent <- as(extent(IDN_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropOP_IDN_GHG <- crop(mapGHG_stack[[2]], IDN_extent)

#Raster to Points
mapGHG_Palm_IDN_pts <- rasterToPoints(CropOP_IDN_GHG, spatial = TRUE)
mapGHG_Palm_IDN_df  <- data.frame(mapGHG_Palm_IDN_pts)
rm(mapGHG_Palm_IDN_pts)

#names of df
names(mapGHG_Palm_IDN_df)

#map: 
mapGHG_IDN_Palm <- IDN_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.2),
            data = mapGHG_Palm_IDN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Indonesia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_IDN_Palm

#### Land Biodiversity Impact #####
#crop raster
CropOP_IDN_LD <- crop(mapLD_stack[[2]], IDN_extent)

#Raster to Points
mapLD_Palm_IDN_pts <- rasterToPoints(CropOP_IDN_LD, spatial = TRUE)
mapLD_Palm_IDN_df  <- data.frame(mapLD_Palm_IDN_pts)
rm(mapLD_Palm_IDN_pts)

#names of df
names(mapLD_Palm_IDN_df)

#create Map
mapLD_IDN_Palm <- IDN_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.2),
            data = mapLD_Palm_IDN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Indonesia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_IDN_Palm

#### N_Marine_BioDiv #####
#crop raster
CropOP_IDN_N <- crop(mapN_stack[[1]], IDN_extent)

#Raster to Points
mapN_Palm_IDN_pts <- rasterToPoints(CropOP_IDN_N, spatial = TRUE)
mapN_Palm_IDN_df  <- data.frame(mapN_Palm_IDN_pts)
rm(mapN_Palm_IDN_pts)

#names of df
names(mapN_Palm_IDN_df)

#create map
mapN_IDN_Palm <- IDN_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.1),
            data = mapN_Palm_IDN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Palm (total)", 
       subtitle = "Indonesia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_IDN_Palm

#### P_Marine_BioDiv #####
#crop raster
CropOP_IDN_P <- crop(mapP_stack[[1]], IDN_extent)

#Raster to Points
mapP_Palm_IDN_pts <- rasterToPoints(CropOP_IDN_P, spatial = TRUE)
mapP_Palm_IDN_df  <- data.frame(mapP_Palm_IDN_pts)
rm(mapP_Palm_IDN_pts)

#names of df
names(mapP_Palm_IDN_df)

#create map
mapP_IDN_Palm <- IDN_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.1),
            data = mapP_Palm_IDN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Palm (total)", 
       subtitle = "Indonesia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_IDN_Palm

#### Water Depth #####
#crop raster
CropOP_IDN_WD <- crop(mapWD_stack[[1]], IDN_extent)

#Raster to Points
mapWD_Palm_IDN_pts <- rasterToPoints(CropOP_IDN_WD, spatial = TRUE)
mapWD_Palm_IDN_df  <- data.frame(mapWD_Palm_IDN_pts)
rm(mapWD_Palm_IDN_pts)

#names of df
names(mapWD_Palm_IDN_df)

#create map
mapWD_IDN_Palm <- IDN_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.1),
            data = mapWD_Palm_IDN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Palm (total)", 
       subtitle = "Indonesia") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_IDN_Palm

####print all maps for IDN:####

mapGHG_IDN_Palm
mapLD_IDN_Palm
mapN_IDN_Palm
mapP_IDN_Palm
mapWD_IDN_Palm

### Papua New Guinea ####

#google maps - PNG
PNG_sf <- qmap("Papua New Guinea", zoom = 6,maptype = "hybrid")
PNG_sf
#extent for PNG
PNG_shps <- ctry_shps[ctry_shps$GID_0=="PNG",]

#save extent for future cropping of data
PNG_extent <- as(extent(PNG_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropOP_PNG_GHG <- crop(mapGHG_stack[[2]], PNG_extent)

#Raster to Points
mapGHG_Palm_PNG_pts <- rasterToPoints(CropOP_PNG_GHG, spatial = TRUE)
mapGHG_Palm_PNG_df  <- data.frame(mapGHG_Palm_PNG_pts)
rm(mapGHG_Palm_PNG_pts)

#names of df
names(mapGHG_Palm_PNG_df)

#map: 
mapGHG_PNG_Palm <- PNG_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.2),
            data = mapGHG_Palm_PNG_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Papua New Guinea") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_PNG_Palm

#### Land Biodiversity Impact #####
#crop raster
CropOP_PNG_LD <- crop(mapLD_stack[[2]], PNG_extent)

#Raster to Points
mapLD_Palm_PNG_pts <- rasterToPoints(CropOP_PNG_LD, spatial = TRUE)
mapLD_Palm_PNG_df  <- data.frame(mapLD_Palm_PNG_pts)
rm(mapLD_Palm_PNG_pts)

#names of df
names(mapLD_Palm_PNG_df)

#create Map
mapLD_PNG_Palm <- PNG_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.2),
            data = mapLD_Palm_PNG_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Papua New Guinea") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_PNG_Palm

#### N_Marine_BioDiv #####
#crop raster
CropOP_PNG_N <- crop(mapN_stack[[1]], PNG_extent)

#Raster to Points
mapN_Palm_PNG_pts <- rasterToPoints(CropOP_PNG_N, spatial = TRUE)
mapN_Palm_PNG_df  <- data.frame(mapN_Palm_PNG_pts)
rm(mapN_Palm_PNG_pts)

#names of df
names(mapN_Palm_PNG_df)

#create map
mapN_PNG_Palm <- PNG_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.1),
            data = mapN_Palm_PNG_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Palm (total)", 
       subtitle = "Papua New Guinea") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_PNG_Palm

#### P_Marine_BioDiv #####
#crop raster
CropOP_PNG_P <- crop(mapP_stack[[1]], PNG_extent)

#Raster to Points
mapP_Palm_PNG_pts <- rasterToPoints(CropOP_PNG_P, spatial = TRUE)
mapP_Palm_PNG_df  <- data.frame(mapP_Palm_PNG_pts)
rm(mapP_Palm_PNG_pts)

#names of df
names(mapP_Palm_PNG_df)

#create map
mapP_PNG_Palm <- PNG_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.1),
            data = mapP_Palm_PNG_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Palm (total)", 
       subtitle = "Papua New Guinea") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_PNG_Palm

#### Water Depth #####
#crop raster
CropOP_PNG_WD <- crop(mapWD_stack[[1]], PNG_extent)

#Raster to Points
mapWD_Palm_PNG_pts <- rasterToPoints(CropOP_PNG_WD, spatial = TRUE)
mapWD_Palm_PNG_df  <- data.frame(mapWD_Palm_PNG_pts)
rm(mapWD_Palm_PNG_pts)

#names of df
names(mapWD_Palm_PNG_df)

#create map
mapWD_PNG_Palm <- PNG_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.1),
            data = mapWD_Palm_PNG_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Palm (total)", 
       subtitle = "Papua New Guinea") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_PNG_Palm

####print all maps for PNG:####

mapGHG_PNG_Palm
mapLD_PNG_Palm
mapN_PNG_Palm
mapP_PNG_Palm
mapWD_PNG_Palm


### Nigeria ####

#google maps - NIN
NIN_sf <- qmap("Nigeria", zoom = 6,maptype = "hybrid")
#NIN_sf
#extent for NIN
NIN_shps <- ctry_shps[ctry_shps$GID_0=="NIN",]

#save extent for future cropping of data
NIN_extent <- as(extent(NIN_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropOP_NIN_GHG <- crop(mapGHG_stack[[2]], NIN_extent)

#Raster to Points
mapGHG_Palm_NIN_pts <- rasterToPoints(CropOP_NIN_GHG, spatial = TRUE)
mapGHG_Palm_NIN_df  <- data.frame(mapGHG_Palm_NIN_pts)
rm(mapGHG_Palm_NIN_pts)

#names of df
names(mapGHG_Palm_NIN_df)

#map: 
mapGHG_NIN_Palm <- NIN_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.2),
            data = mapGHG_Palm_NIN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Nigeria") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_NIN_Palm

#### Land Biodiversity Impact #####
#crop raster
CropOP_NIN_LD <- crop(mapLD_stack[[2]], NIN_extent)

#Raster to Points
mapLD_Palm_NIN_pts <- rasterToPoints(CropOP_NIN_LD, spatial = TRUE)
mapLD_Palm_NIN_df  <- data.frame(mapLD_Palm_NIN_pts)
rm(mapLD_Palm_NIN_pts)

#names of df
names(mapLD_Palm_NIN_df)

#create Map
mapLD_NIN_Palm <- NIN_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.2),
            data = mapLD_Palm_NIN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Nigeria") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_NIN_Palm

#### N_Marine_BioDiv #####
#crop raster
CropOP_NIN_N <- crop(mapN_stack[[1]], NIN_extent)

#Raster to Points
mapN_Palm_NIN_pts <- rasterToPoints(CropOP_NIN_N, spatial = TRUE)
mapN_Palm_NIN_df  <- data.frame(mapN_Palm_NIN_pts)
rm(mapN_Palm_NIN_pts)

#names of df
names(mapN_Palm_NIN_df)

#create map
mapN_NIN_Palm <- NIN_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.1),
            data = mapN_Palm_NIN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Palm (total)", 
       subtitle = "Nigeria") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_NIN_Palm

#### P_Marine_BioDiv #####
#crop raster
CropOP_NIN_P <- crop(mapP_stack[[1]], NIN_extent)

#Raster to Points
mapP_Palm_NIN_pts <- rasterToPoints(CropOP_NIN_P, spatial = TRUE)
mapP_Palm_NIN_df  <- data.frame(mapP_Palm_NIN_pts)
rm(mapP_Palm_NIN_pts)

#names of df
names(mapP_Palm_NIN_df)

#create map
mapP_NIN_Palm <- NIN_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.1),
            data = mapP_Palm_NIN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Palm (total)", 
       subtitle = "Nigeria") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_NIN_Palm

#### Water Depth #####
#crop raster
CropOP_NIN_WD <- crop(mapWD_stack[[1]], NIN_extent)

#Raster to Points
mapWD_Palm_NIN_pts <- rasterToPoints(CropOP_NIN_WD, spatial = TRUE)
mapWD_Palm_NIN_df  <- data.frame(mapWD_Palm_NIN_pts)
rm(mapWD_Palm_NIN_pts)

#names of df
names(mapWD_Palm_NIN_df)

#create map
mapWD_NIN_Palm <- NIN_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.1),
            data = mapWD_Palm_NIN_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Palm (total)", 
       subtitle = "Nigeria") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_NIN_Palm

####print all maps for NIN:####

mapGHG_NIN_Palm
mapLD_NIN_Palm
mapN_NIN_Palm
mapP_NIN_Palm
mapWD_NIN_Palm


### Brazil ####

#google maps - BRA
BRA_sf <- qmap("Brazil", zoom = 4,maptype = "hybrid")
#BRA_sf
#extent for BRA
BRA_shps <- ctry_shps[ctry_shps$GID_0=="BRA",]

#save extent for future cropping of data
BRA_extent <- as(extent(BRA_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropOP_BRA_GHG <- crop(mapGHG_stack[[2]], BRA_extent)

#Raster to Points
mapGHG_Palm_BRA_pts <- rasterToPoints(CropOP_BRA_GHG, spatial = TRUE)
mapGHG_Palm_BRA_df  <- data.frame(mapGHG_Palm_BRA_pts)
rm(mapGHG_Palm_BRA_pts)

#names of df
names(mapGHG_Palm_BRA_df)

#map: 
mapGHG_BRA_Palm <- BRA_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.2),
            data = mapGHG_Palm_BRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Brazil") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_BRA_Palm

#### Land Biodiversity Impact #####
#crop raster
CropOP_BRA_LD <- crop(mapLD_stack[[2]], BRA_extent)

#Raster to Points
mapLD_Palm_BRA_pts <- rasterToPoints(CropOP_BRA_LD, spatial = TRUE)
mapLD_Palm_BRA_df  <- data.frame(mapLD_Palm_BRA_pts)
rm(mapLD_Palm_BRA_pts)

#names of df
names(mapLD_Palm_BRA_df)

#create Map
mapLD_BRA_Palm <- BRA_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.2),
            data = mapLD_Palm_BRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Brazil") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_BRA_Palm

#### N_Marine_BioDiv #####
#crop raster
CropOP_BRA_N <- crop(mapN_stack[[1]], BRA_extent)

#Raster to Points
mapN_Palm_BRA_pts <- rasterToPoints(CropOP_BRA_N, spatial = TRUE)
mapN_Palm_BRA_df  <- data.frame(mapN_Palm_BRA_pts)
rm(mapN_Palm_BRA_pts)

#names of df
names(mapN_Palm_BRA_df)

#create map
mapN_BRA_Palm <- BRA_sf +
  geom_tile(aes(x = x, y = y, fill = N_Marine_BioDiv_Total.1),
            data = mapN_Palm_BRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "N Biodiversity Impact of Palm (total)", 
       subtitle = "Brazil") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "N Biodiversity Impact")+
  theme_map()

mapN_BRA_Palm

#### P_Marine_BioDiv #####
#crop raster
CropOP_BRA_P <- crop(mapP_stack[[1]], BRA_extent)

#Raster to Points
mapP_Palm_BRA_pts <- rasterToPoints(CropOP_BRA_P, spatial = TRUE)
mapP_Palm_BRA_df  <- data.frame(mapP_Palm_BRA_pts)
rm(mapP_Palm_BRA_pts)

#names of df
names(mapP_Palm_BRA_df)

#create map
mapP_BRA_Palm <- BRA_sf +
  geom_tile(aes(x = x, y = y, fill = P_Marine_BioDiv_Total.1),
            data = mapP_Palm_BRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "P Biodiversity Impact of Palm (total)", 
       subtitle = "Brazil") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "P Biodiversity Impact")+
  theme_map()

mapP_BRA_Palm

#### Water Depth #####
#crop raster
CropOP_BRA_WD <- crop(mapWD_stack[[1]], BRA_extent)

#Raster to Points
mapWD_Palm_BRA_pts <- rasterToPoints(CropOP_BRA_WD, spatial = TRUE)
mapWD_Palm_BRA_df  <- data.frame(mapWD_Palm_BRA_pts)
rm(mapWD_Palm_BRA_pts)

#names of df
names(mapWD_Palm_BRA_df)

#create map
mapWD_BRA_Palm <- BRA_sf +
  geom_tile(aes(x = x, y = y, fill = Water_Debt_Total.1),
            data = mapWD_Palm_BRA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Water Depth of Palm (total)", 
       subtitle = "Brazil") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Water Depth")+
  theme_map()

mapWD_BRA_Palm

####print all maps for BRA:####

mapGHG_BRA_Palm
mapLD_BRA_Palm
mapN_BRA_Palm
mapP_BRA_Palm
mapWD_BRA_Palm




## Task 2D) Cocoa ####

### Ghana ####

#google maps - GHA
GHA_sf <- qmap("Ghana", zoom = 7,maptype = "hybrid")
#GHA_sf
#extent for GHA
GHA_shps <- ctry_shps[ctry_shps$GID_0=="GHA",]

#save extent for future cropping of data
GHA_extent <- as(extent(GHA_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropC_GHA_GHG <- crop(mapGHG_stack[[1]], GHA_extent)

#Raster to Points
mapGHG_Palm_GHA_pts <- rasterToPoints(CropC_GHA_GHG, spatial = TRUE)
mapGHG_Palm_GHA_df  <- data.frame(mapGHG_Palm_GHA_pts)
rm(mapGHG_Palm_GHA_pts)

#names of df
names(mapGHG_Palm_GHA_df)

#map: 
mapGHG_GHA_Palm <- GHA_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.1),
            data = mapGHG_Palm_GHA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Ghana") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_GHA_Palm

#### Land Biodiversity Impact #####
#crop raster
CropC_GHA_LD <- crop(mapLD_stack[[1]], GHA_extent)

#Raster to Points
mapLD_Palm_GHA_pts <- rasterToPoints(CropC_GHA_LD, spatial = TRUE)
mapLD_Palm_GHA_df  <- data.frame(mapLD_Palm_GHA_pts)
rm(mapLD_Palm_GHA_pts)

#names of df
names(mapLD_Palm_GHA_df)

#create Map
mapLD_GHA_Palm <- GHA_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.1),
            data = mapLD_Palm_GHA_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Ghana") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_GHA_Palm


### Nigeria ####

#google maps - NIG
NIG_sf <- qmap("Nigeria", zoom = 6,maptype = "hybrid")
#NIG_sf
#extent for NIG
NIG_shps <- ctry_shps[ctry_shps$GID_0=="NIG",]

#save extent for future cropping of data
NIG_extent <- as(extent(NIG_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropC_NIG_GHG <- crop(mapGHG_stack[[1]], NIG_extent)

#Raster to Points
mapGHG_Palm_NIG_pts <- rasterToPoints(CropC_NIG_GHG, spatial = TRUE)
mapGHG_Palm_NIG_df  <- data.frame(mapGHG_Palm_NIG_pts)
rm(mapGHG_Palm_NIG_pts)

#names of df
names(mapGHG_Palm_NIG_df)

#map: 
mapGHG_NIG_Palm <- NIG_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.1),
            data = mapGHG_Palm_NIG_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Nigeria") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_NIG_Palm

#### Land Biodiversity Impact #####
#crop raster
CropC_NIG_LD <- crop(mapLD_stack[[1]], NIG_extent)

#Raster to Points
mapLD_Palm_NIG_pts <- rasterToPoints(CropC_NIG_LD, spatial = TRUE)
mapLD_Palm_NIG_df  <- data.frame(mapLD_Palm_NIG_pts)
rm(mapLD_Palm_NIG_pts)

#names of df
names(mapLD_Palm_NIG_df)

#create Map
mapLD_NIG_Palm <- NIG_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.1),
            data = mapLD_Palm_NIG_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Nigeria") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_NIG_Palm

### Cote d'Ivorie ####

#google maps - CIV
CIV_sf <- qmap("Cote d'Ivorie", zoom = 7,maptype = "hybrid")
#CIV_sf
#extent for CIV
CIV_shps <- ctry_shps[ctry_shps$GID_0=="CIV",]

#save extent for future cropping of data
CIV_extent <- as(extent(CIV_shps), 'SpatialPolygons')


#### GHG #####
#crop raster
CropC_CIV_GHG <- crop(mapGHG_stack[[1]], CIV_extent)

#Raster to Points
mapGHG_Palm_CIV_pts <- rasterToPoints(CropC_CIV_GHG, spatial = TRUE)
mapGHG_Palm_CIV_df  <- data.frame(mapGHG_Palm_CIV_pts)
rm(mapGHG_Palm_CIV_pts)

#names of df
names(mapGHG_Palm_CIV_df)

#map: 
mapGHG_CIV_Palm <- CIV_sf +
  geom_tile(aes(x = x, y = y, fill = GHG_Emissons_Total.1),
            data = mapGHG_Palm_CIV_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "GHG Emission of Palm (total)", 
       subtitle = "Cote d'Ivorie") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "GHG Emission")+
  theme_map()

mapGHG_CIV_Palm

#### Land Biodiversity Impact #####
#crop raster
CropC_CIV_LD <- crop(mapLD_stack[[1]], CIV_extent)

#Raster to Points
mapLD_Palm_CIV_pts <- rasterToPoints(CropC_CIV_LD, spatial = TRUE)
mapLD_Palm_CIV_df  <- data.frame(mapLD_Palm_CIV_pts)
rm(mapLD_Palm_CIV_pts)

#names of df
names(mapLD_Palm_CIV_df)

#create Map
mapLD_CIV_Palm <- CIV_sf +
  geom_tile(aes(x = x, y = y, fill = LD_BioDiv_Total.1),
            data = mapLD_Palm_CIV_df)+
  labs(x="Longitude",
       y="Latitude",
       title = "Land Biodiversity Impact of Palm (total)", 
       subtitle = "Cote d'Ivorie") +
  scale_fill_viridis(option = "magma", direction = -1, alpha=0.3, name = "Land Biodiversity Impact")+
  theme_map()

mapLD_CIV_Palm




