##%######################################################%##
#                                                          #
#                  4. Summary Statistics                ####
#                  GHG, WD, LND, Nit, Pho                  #
#                                                          #
##%######################################################%##
# started by Feli Pamatat, 08/07/2021

#Prepare R for analysis####
remove(list = ls(all.names = TRUE))

detachAllPackages <- function() {
  basic.packages.blank <-  c("stats", 
                             "graphics", 
                             "grDevices", 
                             "utils", 
                             "datasets", 
                             "methods", 
                             "base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, 
                                  TRUE, 
                                  FALSE)]
  
  package.list <- setdiff(package.list, basic.packages)
  
  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

detachAllPackages()


if (!require(raster)) {
  install.packages("raster")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}


remove(list = ls(all.names = TRUE))
Sys.setenv(LANG = "en")


#Task 1 - set dir ####

setwd("/Users/Feli/Documents/Cookie Project")

#Mark's data in different dir (see Mark's data google drive)
MMdir <- "Data"

#Summary Table dir
SumTabdir <- "Summary_Table"


# load in the raster stack
files <- list.files(paste0(MMdir, "/Marks_Maps"), pattern = "_Total.tif", full.names = TRUE)

#import all raster files (with stack to have all bands)
mapGHG_stack <- stack(paste0(files[1])) #GHG Emissions Total (5 Bands)
mapLD_stack <- stack(paste0(files[2])) #LD BioDiv Total (5 Bands)
mapN_stack <- stack(paste0(files[3])) #N Marine BioDiv Total (4 Bands)
mapP_stack <- stack(paste0(files[4])) #P Marine BioDiv Total (4 Bands)
mapWD_stack <- stack(paste0(files[5])) #Water Debt Total (4 Bands)

#merge the sugars
mapGHG_stack[[6]] <- mosaic(mapGHG_stack[[3]], mapGHG_stack[[4]],fun = sum , overlap=TRUE, tolerance=0.05)
mapLD_stack[[6]]  <- mosaic(mapLD_stack[[3]], mapLD_stack[[4]],fun = sum , overlap=TRUE, tolerance=0.05)
mapWD_stack[[5]]  <- mosaic(mapWD_stack[[3]], mapWD_stack[[4]],fun = sum , overlap=TRUE, tolerance=0.05)
mapN_stack[[5]]   <- mosaic(mapN_stack[[3]], mapN_stack[[4]],fun = sum , overlap=TRUE, tolerance=0.05)
mapP_stack[[5]]   <- mosaic(mapP_stack[[3]], mapP_stack[[4]],fun = sum , overlap=TRUE, tolerance=0.05)

#GHG
GHG_Cocoa <- mapGHG_stack[[1]]
GHG_Oilpalm <- mapGHG_stack[[2]]
GHG_Sugarbeet <- mapGHG_stack[[3]]
GHG_Sugarcane <- mapGHG_stack[[4]]
GHG_Wheat <- mapGHG_stack[[5]]
GHG_Sugar <- mapGHG_stack[[6]]

#WD
WD_Oilpalm <- mapWD_stack[[1]]
WD_Sugarbeet <- mapWD_stack[[2]]
WD_Sugarcane <- mapWD_stack[[3]]
WD_Wheat <- mapWD_stack[[4]]
WD_Sugar <- mapWD_stack[[5]]

#LD
LD_Cocoa <- mapLD_stack[[1]]
LD_Oilpalm <- mapLD_stack[[2]]
LD_Sugarbeet <- mapLD_stack[[3]]
LD_Sugarcane <- mapLD_stack[[4]]
LD_Wheat <- mapLD_stack[[5]]
LD_Sugar <- mapLD_stack[[6]]

#N
N_Oilpalm <- mapN_stack[[1]]
N_Sugarbeet <- mapN_stack[[2]]
N_Sugarcane <- mapN_stack[[3]]
N_Wheat <- mapN_stack[[4]]
N_Sugar <- mapN_stack[[5]]

#P
P_Oilpalm <- mapP_stack[[1]]
P_Sugarbeet <- mapP_stack[[2]]
P_Sugarcane <- mapP_stack[[3]]
P_Wheat <- mapP_stack[[4]]
P_Sugar <- mapP_stack[[5]]


# Task 2 - Summary stat: global Impact####

GHG_sum             <- matrix(nrow = 6, ncol = 7)
colnames(GHG_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer", "#Na")
GHG_sum             <- as.data.frame(GHG_sum)
GHG_sum$RasterLayer <- c("Cocoa","OilPalm", "Sugarbeet","Sugarcane", "Wheat", "Sugar")
GHG_sum$Sum         <- cellStats(mapGHG_stack, stat='sum', na.rm=TRUE)
GHG_sum$Mean        <- cellStats(mapGHG_stack, stat='mean', na.rm=TRUE)
GHG_sum$Sd          <- cellStats(mapGHG_stack, stat='sd', na.rm=TRUE)
GHG_sum$Max         <- cellStats(mapGHG_stack, stat='max', na.rm=TRUE)
GHG_sum$Min         <- cellStats(mapGHG_stack, stat='min', na.rm=TRUE)
GHG_sum[1,7]        <- length(is.na(mapGHG_stack[[1]]))
GHG_sum[2,7]        <- length(is.na(mapGHG_stack[[2]]))
GHG_sum[3,7]        <- length(is.na(mapGHG_stack[[3]]))
GHG_sum[4,7]        <- length(is.na(mapGHG_stack[[4]]))
GHG_sum[5,7]        <- length(is.na(mapGHG_stack[[5]]))
GHG_sum[6,7]        <- length(is.na(mapGHG_stack[[6]]))

GHG_sum    <- write_csv(GHG_sum, file.path(SumTabdir, "GHG_sum.csv"))


#LD
LD_sum             <- matrix(nrow = 6, ncol = 7)
colnames(LD_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer", "#Na")
LD_sum             <- as.data.frame(LD_sum)
LD_sum$RasterLayer <- c("Cocoa","OilPalm", "Sugarbeet","Sugarcane", "Wheat", "Sugar")
LD_sum$Sum         <- cellStats(mapLD_stack, stat='sum', na.rm=TRUE)
LD_sum$Mean        <- cellStats(mapLD_stack, stat='mean', na.rm=TRUE)
LD_sum$Sd          <- cellStats(mapLD_stack, stat='sd', na.rm=TRUE)
LD_sum$Max         <- cellStats(mapLD_stack, stat='max', na.rm=TRUE)
LD_sum$Min         <- cellStats(mapLD_stack, stat='min', na.rm=TRUE)
LD_sum[1,7]        <- length(is.na(mapLD_stack[[1]]))
LD_sum[2,7]        <- length(is.na(mapLD_stack[[2]]))
LD_sum[3,7]        <- length(is.na(mapLD_stack[[3]]))
LD_sum[4,7]        <- length(is.na(mapLD_stack[[4]]))
LD_sum[5,7]        <- length(is.na(mapLD_stack[[5]]))
LD_sum[6,7]        <- length(is.na(mapLD_stack[[6]]))

LD_sum    <- write_csv(LD_sum, file.path(SumTabdir, "LD_sum.csv"))


#WD
WD_sum             <- matrix(nrow = 5, ncol = 7)
colnames(WD_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer", "#Na")
WD_sum             <- as.data.frame(WD_sum)
WD_sum$RasterLayer <- c("OilPalm", "Sugarbeet","Sugarcane", "Wheat", "Sugar")
WD_sum$Sum         <- cellStats(mapWD_stack, stat='sum', na.rm=TRUE)
WD_sum$Mean        <- cellStats(mapWD_stack, stat='mean', na.rm=TRUE)
WD_sum$Sd          <- cellStats(mapWD_stack, stat='sd', na.rm=TRUE)
WD_sum$Max         <- cellStats(mapWD_stack, stat='max', na.rm=TRUE)
WD_sum$Min         <- cellStats(mapWD_stack, stat='min', na.rm=TRUE)
WD_sum[1,7]        <- length(is.na(mapWD_stack[[1]]))
WD_sum[2,7]        <- length(is.na(mapWD_stack[[2]]))
WD_sum[3,7]        <- length(is.na(mapWD_stack[[3]]))
WD_sum[4,7]        <- length(is.na(mapWD_stack[[4]]))
WD_sum[5,7]        <- length(is.na(mapWD_stack[[5]]))

WD_sum    <- write_csv(WD_sum, file.path(SumTabdir, "WD_sum.csv"))


#N
N_sum             <- matrix(nrow = 5, ncol = 7)
colnames(N_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer", "#Na")
N_sum             <- as.data.frame(N_sum)
N_sum$RasterLayer <- c("OilPalm", "Sugarbeet","Sugarcane", "Wheat", "Sugar")
N_sum$Sum         <- cellStats(mapN_stack, stat='sum', na.rm=TRUE)
N_sum$Mean        <- cellStats(mapN_stack, stat='mean', na.rm=TRUE)
N_sum$Sd          <- cellStats(mapN_stack, stat='sd', na.rm=TRUE)
N_sum$Max         <- cellStats(mapN_stack, stat='max', na.rm=TRUE)
N_sum$Min         <- cellStats(mapN_stack, stat='min', na.rm=TRUE)
N_sum[1,7]        <- length(is.na(mapN_stack[[1]]))
N_sum[2,7]        <- length(is.na(mapN_stack[[2]]))
N_sum[3,7]        <- length(is.na(mapN_stack[[3]]))
N_sum[4,7]        <- length(is.na(mapN_stack[[4]]))
N_sum[5,7]        <- length(is.na(mapN_stack[[5]]))

N_sum    <- write_csv(N_sum, file.path(SumTabdir, "N_sum.csv"))


#P
P_sum             <- matrix(nrow = 5, ncol = 7)
colnames(P_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer", "#Na")
P_sum             <- as.data.frame(P_sum)
P_sum$RasterLayer <- c("OilPalm", "Sugarbeet","Sugarcane", "Wheat", "Sugar")
P_sum$Sum         <- cellStats(mapP_stack, stat='sum', na.rm=TRUE)
P_sum$Mean        <- cellStats(mapP_stack, stat='mean', na.rm=TRUE)
P_sum$Sd          <- cellStats(mapP_stack, stat='sd', na.rm=TRUE)
P_sum$Max         <- cellStats(mapP_stack, stat='max', na.rm=TRUE)
P_sum$Min         <- cellStats(mapP_stack, stat='min', na.rm=TRUE)
P_sum[1,7]        <- length(is.na(mapP_stack[[1]]))
P_sum[2,7]        <- length(is.na(mapP_stack[[2]]))
P_sum[3,7]        <- length(is.na(mapP_stack[[3]]))
P_sum[4,7]        <- length(is.na(mapP_stack[[4]]))
P_sum[5,7]        <- length(is.na(mapP_stack[[5]]))

P_sum    <- write_csv(P_sum, file.path(SumTabdir, "P_sum.csv"))

#Task 3 - consult the mean per country####

## Charlie Data ####
if(!require(exactextractr)) {
  install.packages("exactextractr")
  require(exactextractr)
} # exact_extract function from here

datadir <- "Data"
outdir <- "1_Hotspots_Multimetric_indicator"

# take a look at the raster files
#list.files(paste0(datadir, "/Marks_Maps"), pattern = ".tif")

# Notes from Mark's readme doc
# Files ending with:
# a.	_CT = per tonne of crop for each grid cell
# b.	_Total = Total Impact for each grid cell

#for later - read and prepare Carole's data
#read
suppliers <- read.csv(paste0(datadir, "/UK_Suppliers_main_crops_Carole.csv"))
#cut
suppliers <- suppliers[,-c(2,5)]
#rename columns 
names(suppliers)[names(suppliers) =="item"] <- "Item"
names(suppliers)[names(suppliers) =="percent_of_UK_supply"] <- "ImportToUK"
names(suppliers)[names(suppliers) =="partner"] <- "Area"
#rename Items
suppliers$Item[suppliers$Item %in% "Oil_palm_fruit"] <- "OilPalm"
suppliers$Item[suppliers$Item %in% "Sugarcane"] <- "Sugar"
suppliers$Item[suppliers$Item %in% "Cocoa_beans"] <- "Cocoa"
#change ImportToUK from %age to proportion
suppliers$ImportToUK <- suppliers$ImportToUK/100

#### Task 1: Summary stats for each country ####

# For now, will just use the total impact raster files

files <- list.files(paste0(datadir, "/Marks_Maps"), pattern = "_Total.tif")

# 5 files
# "GHG_Emissons_Total.tif"
# "LD_BioDiv_Total.tif"
# "N_Marine_BioDiv_Total.tif"
# "P_Marine_BioDiv_Total.tif"
# "Water_Debt_Total.tif" 


### get country border data for the list of countries ####

# get the country codes to extract country polygons
codes <- getData('ISO3')

###For complete Impact list for all Top 50 countries######
#FAOSTAT and Mark's data have different country names
#next step will correct for that
#
#for top 50 countries (plus 5 sugar countries)
countries <- unique(TopProductionTradeUK$Area)
#as data frame to be able to merge with code df
countries <- as.data.frame(countries)
#change column names so it matches
names(countries)[names(countries) =="countries"] <- "Area"
names(codes)[names(codes) =="NAME"] <- "Area"

#merge
cntry_codes<-merge(codes, countries, by= "Area")

# extract country shapefiles for 
codes = list() #creates empty list; ready to be filled with the ISO3 of the "Top 50" crops
codes$countries <- cntry_codes$ISO3

names(codes)[names(codes) =="ISO3"] <- "countries"

# download and combine polygons into one object
ctry_shps = do.call("bind", lapply(codes$countries, 
                                   function(x) getData('GADM', country=x, level=0)))

#### 1. GHG Total indicator map ####

# load in the raster stack
GHG <- stack(paste0(datadir, "/Marks_Maps/", files[1]))

# use exact_extract function to get some summary stats per country/band
# the function does the same for each layer automatically

GHG_sums <- exact_extract(x = GHG, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(GHG_sums) <- ctry_shps$NAME_0

colnames(GHG_sums) <- sub("GHG_Emissons_Total.1", "Cocoa", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.2", "Oilpalm", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.3", "SugarBeet", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.4", "SugarCane", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.5", "Wheat", colnames(GHG_sums))


#### 2. Land biodiversity impact ####

LND <- stack(paste0(datadir, "/Marks_Maps/", files[2]))

LND_sums <- exact_extract(x = LND, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(LND_sums) <- ctry_shps$NAME_0

colnames(LND_sums) <- sub("LD_BioDiv_Total.1", "Cocoa", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.2", "Oilpalm", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.3", "SugarBeet", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.4", "SugarCane", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.5", "Wheat", colnames(LND_sums))

#### 3. N biodiv impact ####

Nit <- stack(paste0(datadir, "/Marks_Maps/", files[3]))

Nit_sums <- exact_extract(x = Nit, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(Nit_sums) <- ctry_shps$NAME_0

colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.1", "Oilpalm", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.2", "SugarBeet", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.3", "SugarCane", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.4", "Wheat", colnames(Nit_sums))

Nit_sums$sum.Cocoa <- NA
Nit_sums$mean.Cocoa <- NA

#### 4. P biodiv impact ####

Pho <- stack(paste0(datadir, "/Marks_Maps/", files[4]))

Pho_sums <- exact_extract(x = Pho, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(Pho_sums) <- ctry_shps$NAME_0

colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.1", "Oilpalm", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.2", "SugarBeet", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.3", "SugarCane", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.4", "Wheat", colnames(Pho_sums))

Pho_sums$sum.Cocoa <- NA
Pho_sums$mean.Cocoa <- NA

#### 5. water debt ####

WAT <- stack(paste0(datadir, "/Marks_Maps/", files[5]))

WAT_sums <- exact_extract(x = WAT, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(WAT_sums) <- ctry_shps$NAME_0

colnames(WAT_sums) <- sub("Water_Debt_Total.1", "Oilpalm", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_Total.2", "SugarBeet", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_Total.3", "SugarCane", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_Total.4", "Wheat", colnames(WAT_sums))

WAT_sums$sum.Cocoa <- NA
WAT_sums$mean.Cocoa <- NA

# next: create visualisations of the total impacts as detailed in the sum columns in the tables above for each vountry/crop/metric

# possibly some kind of bar chart, colour for each crop

all_sums <- rbind(GHG_sums[, c(grep("sum", colnames(GHG_sums)))],
                  LND_sums[, c(grep("sum", colnames(LND_sums)))],
                  Nit_sums[, c(grep("sum", colnames(Nit_sums)))],
                  Pho_sums[, c(grep("sum", colnames(Pho_sums)))],
                  WAT_sums[, c(grep("sum", colnames(WAT_sums)))])


#2000:
all_sums$metric <-c(rep("GHG", 112), rep("LND", 112), rep("Nit", 112), rep("Pho", 112), rep("WAT", 112))

#2010:
#all_sums$metric <-c(rep("GHG", 115), rep("LND", 115), rep("Nit", 115), rep("Pho", 115), rep("WAT", 115))

#View(all_sums)

all_sums$Area <- sub("[0-9]+", "", rownames(all_sums))

#all Crops combined into long format
all_data1 <- all_sums[all_sums$metric=="GHG",] %>% gather(Item, GHG_metric, sum.Cocoa:sum.Wheat)
all_data2 <- all_sums[all_sums$metric=="LND",] %>% gather(Item, LND_metric, sum.Cocoa:sum.Wheat)
all_data3 <- all_sums[all_sums$metric=="Nit",] %>% gather(Item, Nit_metric, sum.Cocoa:sum.Wheat)
all_data4 <- all_sums[all_sums$metric=="Pho",] %>% gather(Item, Pho_metric, sum.Cocoa:sum.Wheat)
all_data5 <- all_sums[all_sums$metric=="WAT",] %>% gather(Item, WAT_metric, sum.Cocoa:sum.Wheat)


all_data1 <- all_data1[,-1]
all_data2 <- all_data2[,-1]
all_data3 <- all_data3[,-1]
all_data4 <- all_data4[,-1]
all_data5 <- all_data5[,-1]

all_data <- merge(all_data1,all_data2,by=c("Area", "Item"))
all_data <- merge(all_data,all_data3,by=c("Area", "Item"))
all_data <- merge(all_data,all_data4,by=c("Area", "Item"))
all_data <- merge(all_data,all_data5,by=c("Area", "Item"))


all_data$Item[all_data$Item %in% "sum.Cocoa"] <- "Cocoa"
all_data$Item[all_data$Item %in% "sum.SugarBeet"] <- "SugarBeet"
all_data$Item[all_data$Item %in% "sum.SugarCane"] <- "SugarCane"
all_data$Item[all_data$Item %in% "sum.Oilpalm"] <- "OilPalm"
all_data$Item[all_data$Item %in% "sum.Wheat"] <- "Wheat"


#create a data frame which includes the mean
all_mean <- rbind(GHG_sums[, c(grep("mean", colnames(GHG_sums)))],
                  LND_sums[, c(grep("mean", colnames(LND_sums)))],
                  Nit_sums[, c(grep("mean", colnames(Nit_sums)))],
                  Pho_sums[, c(grep("mean", colnames(Pho_sums)))],
                  WAT_sums[, c(grep("mean", colnames(WAT_sums)))])

all_mean$metric <-c(rep("GHG", 112), rep("LND", 112), rep("Nit", 112), rep("Pho", 112), rep("WAT", 112))

all_mean$Area <- sub("[0-9]+", "", rownames(all_mean))

all_dataMean1 <- all_mean[all_mean$metric=="GHG",] %>% gather(Item, GHG_metricMean, mean.Cocoa:mean.Wheat)
all_dataMean2 <- all_mean[all_mean$metric=="LND",] %>% gather(Item, LND_metricMean, mean.Cocoa:mean.Wheat)
all_dataMean3 <- all_mean[all_mean$metric=="Nit",] %>% gather(Item, Nit_metricMean, mean.Cocoa:mean.Wheat)
all_dataMean4 <- all_mean[all_mean$metric=="Pho",] %>% gather(Item, Pho_metricMean, mean.Cocoa:mean.Wheat)
all_dataMean5 <- all_mean[all_mean$metric=="WAT",] %>% gather(Item, WAT_metricMean, mean.Cocoa:mean.Wheat)


all_dataMean1 <- all_dataMean1[,-1]
all_dataMean2 <- all_dataMean2[,-1]
all_dataMean3 <- all_dataMean3[,-1]
all_dataMean4 <- all_dataMean4[,-1]
all_dataMean5 <- all_dataMean5[,-1]

all_dataMean <- merge(all_dataMean1,all_dataMean2,by=c("Area", "Item"))
all_dataMean <- merge(all_dataMean,all_dataMean3,by=c("Area", "Item"))
all_dataMean <- merge(all_dataMean,all_dataMean4,by=c("Area", "Item"))
all_dataMean <- merge(all_dataMean,all_dataMean5,by=c("Area", "Item"))


all_dataMean$Item[all_dataMean$Item %in% "mean.Cocoa"] <- "Cocoa"
all_dataMean$Item[all_dataMean$Item %in% "mean.SugarBeet"] <- "SugarBeet"
all_dataMean$Item[all_dataMean$Item %in% "mean.SugarCane"] <- "SugarCane"
all_dataMean$Item[all_dataMean$Item %in% "mean.Oilpalm"] <- "OilPalm"
all_dataMean$Item[all_dataMean$Item %in% "mean.Wheat"] <- "Wheat"


#merge the mean data frames together with sum df (but into a new dataframe)
all_datainclMean <- merge(all_data,all_dataMean,by=c("Area", "Item"))

#change all NaN to 0
all_datainclMean$GHG_metricMean[is.nan(all_datainclMean$GHG_metricMean)]<-0
all_datainclMean$LND_metricMean[is.nan(all_datainclMean$LND_metricMean)]<-0
all_datainclMean$Nit_metricMean[is.nan(all_datainclMean$Nit_metricMean)]<-0
all_datainclMean$Pho_metricMean[is.nan(all_datainclMean$Pho_metricMean)]<-0
all_datainclMean$WAT_metricMean[is.nan(all_datainclMean$WAT_metricMean)]<-0

#save per Crop

MeanSugarcaneImpact00    <- write_csv(all_datainclMean[all_datainclMean$Item=="SugarCane",], file.path(SumTabdir, "MeanSugarcane_2000.csv"))
MeanSugarbeetImpact00    <- write_csv(all_datainclMean[all_datainclMean$Item=="SugarBeet",], file.path(SumTabdir, "MeanSugarbeet_2000.csv"))
MeanWheatImpact00        <- write_csv(all_datainclMean[all_datainclMean$Item=="Wheat",], file.path(SumTabdir, "MeanWheat_2000.csv"))
MeanCocoaImpact00        <- write_csv(all_datainclMean[all_datainclMean$Item=="Cocoa",], file.path(SumTabdir, "MeanCocoa_2000.csv"))
MeanOilPalmImpact00      <- write_csv(all_datainclMean[all_datainclMean$Item=="OilPalm",], file.path(SumTabdir, "MeanOilPalm_2000.csv"))


#Task 3b - Which countries have an unsustainable water consumption####
Watover1           <- all_datainclMean[,-c(3:11)]
Watover1           <- Watover1[Watover1$WAT_metricMean>=1,]

#save: countries with an on average unsustainable water practice
WATSugarcane    <- write_csv(Watover1[Watover1$Item=="SugarCane"& !is.na(Watover1$Item),], file.path(SumTabdir, "WATSugarcane_2000.csv"))
WATSugarbeet    <- write_csv(Watover1[Watover1$Item=="SugarBeet"& !is.na(Watover1$Item),], file.path(SumTabdir, "WATSugarbeet_2000.csv"))
WATWheat        <- write_csv(Watover1[Watover1$Item=="Wheat"& !is.na(Watover1$Item),], file.path(SumTabdir, "WATWheat_2000.csv"))
WATOilPalm      <- write_csv(Watover1[Watover1$Item=="OilPalm"& !is.na(Watover1$Item),], file.path(SumTabdir, "WATOilPalm_2000.csv"))



