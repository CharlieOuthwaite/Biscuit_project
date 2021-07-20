##%######################################################%##
#                                                          #
#                    3. FAOSTAT Table                   ####
#      Produce a table incl. the top 50 crop producing     #
#        countries & the average Biodiversity Impact       #
#                                                          #
##%######################################################%##
#this script produces:
#1 table with the Production (in tonnes) for the Top50 countries (plus important Sugar trade partners; aka Top50+)
#--> Top50Production
#2 table Top50+ countries: Production plus Export (combined "Oil, palm" & "Oil, palm kernel" to "Oil palm fruit"; and "Sugar beet", "Sugar Raw Centrifugal"& "Sugar non-centrifugal" to "Sugar"
#--> TopProductionTrade
#3 table Top50+ countries: Production plus Export + Proportion of Export to UK
#--> TopProductionTradeUK
#4 table Top50+ countries:  Production plus Export + Proportion of Export to UK
#                           + Sum of Impact (per Country) + Impact per Tonne + Impact of the Exported Crop to the Uk 
#--> ImpactandTradeUK

#FAOSTAT selection:
#   FAOTableProduction  -     COUNTRIES - Select all; ELEMENTS - Production Quantity, Yield, Area harvested; ITEMS - Cocoa, beans, Oil palm fruit, Wheat, Sugar cane, Sugar beet
#   FAOTableTrade       -     COUNTRIES - Select all; ELEMENTS - Export Quantity; ITEMS - Cocoa, beans/ Oil, palm/ Oil, palm kernel/ Wheat/ Sugar Raw centrifugal/Sugar non-centrifugal/ Sugar beet
#   FAOTableTradeMatrix -     Reporter COUNTRIES - Select all; Partner COUNTRIES - United Kingdom of Great Britain and Northern Ireland; ELEMENTS - Export Quantity; ITEMS - Cocoa, beans/ Oil, palm/ Oil, palm kernel/ Wheat/ Sugar Raw centrifugal/Sugar non-centrifugal/ Sugar beet

#Sugar Raw centrifugal/Sugar non-centrifugal - appear to be Sugar cane

# started by Feli Pamatat, 05/07/2021

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


if (!require(rgeos)) {
  install.packages("rgeos")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal")
  require(rgdal)
}
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

#dir
FAOdir <- "FAOSTAT_Table"

#save in separate folder
FAOsavedir <- "FAOSTAT_Table/FAOTable_2000"
#FAOsavedir <- "FAOSTAT_Table/FAOTable_2010"


#Task 2 - Load FAOSTAT table####
#all data is from 2010 to match MapSPAM data

#!!!!edit: 19/07/21: changed to 2000 
#changed because we are now using EARTHSTAT (instead of MapSPAM) as Mark used that for his analysis 


#Crops and livestock products - Trade data
#2000:
FAOTableTrade <- read_csv(paste0(FAOdir,"/FAOSTAT_data_7-20-2021_Trade_Crops_and_livestock_products_2000.csv"))
#2010:
#FAOTableTrade <- read_csv(paste0(FAOdir,"/FAOSTAT_data_7-5-2021_Trade_Crops_and_livestock_products_2010.csv"))

#Production Quantity
#2000:
FAOTableProduction <- read_csv(paste0(FAOdir,"/FAOSTAT_data_7-20-2021_Production_Crops_2000.csv"))
#2010:
#FAOTableProduction <- read_csv(paste0(FAOdir,"/FAOSTAT_data_7-5-2021_Production_Crops_2010.csv"))

#TradeMatrix
#2000:
FAOTableTradeMatrix <- read_csv(paste0(FAOdir,"/FAOSTAT_data_7-20-2021_TradeMatrix_2000.csv"))
#2010:
#FAOTableTradeMatrix <- read_csv(paste0(FAOdir,"/FAOSTAT_data_7-5-2021_TradeMatrix_2010.csv"))


# Task 3a - select top 50 crop producing countries####
#since focus is on top 50 I decided to omit Area harvested and Yield for this step and proceed with Production value

#top 50 producing countries
#1 remove area harv. and yield
FAOTableProduction <-FAOTableProduction[FAOTableProduction$Element=="Production",]

#combine sugar cane and sugar beet
FAOTableProduction$Item[FAOTableProduction$Item %in% c("Sugar beet", "Sugar cane")] <- "Sugar"

#add sugar values
FAOTableProduction<-FAOTableProduction %>% 
  group_by(Area, Item) %>% 
  summarise(Value = sum(Value))

#top 50 countries for each crop
Top50Production<-FAOTableProduction %>% group_by(Item) %>% top_n(50) #this includes China and China, mainland

#check that only 4 crops are in the df
unique(Top50Production$Item)

#add Zimbabwe, Fiji, Jamaica, Belize, Trinidad and Tobago (as they are not in the top 50 but impotant UK trade partners) 
#create seperate data frame
ImportantSugarUKTrade <- FAOTableProduction[FAOTableProduction$Area  %in% c("Zimbabwe", "Fiji", "Jamaica",  "Belize", "Trinidad and Tobago")& FAOTableProduction$Item %in% "Sugar",]

#add ImportantSugarUKTrade to Top50 producers
Top50Production <- rbind(Top50Production, ImportantSugarUKTrade)


# Task 3b - add trade data to the top 50 crop producing countries (plus Zimbabwe, Fiji, Jamaica, Belize, Trinidad and Tobago as they are important UK trade partners)####

#Think about Trade data and if it's necessary to group them 

#check names of crops
unique(FAOTableTrade$Item)
#"Cocoa, beans"
#"Wheat"
#"Oil, palm"
#"Oil, palm kernel"
#"Sugar beet"
#"Sugar Raw Centrifugal" 
#"Sugar non-centrifugal"

#so original does not get lost
FAOTableTradeOrig <- FAOTableTrade

#group crops (e.g. Oil, palm & Oil, palm kernel --> "Oil palm fruit")
FAOTableTrade$Item[FAOTableTrade$Item %in% c("Oil, palm", "Oil, palm kernel")] <- "Oil palm fruit"
FAOTableTrade$Item[FAOTableTrade$Item %in% c("Sugar beet", "Sugar Raw Centrifugal", "Sugar non-centrifugal")] <- "Sugar"

#summarize/merge grouped items

FAOTableTrade<-FAOTableTrade %>% 
  group_by(Area, Item) %>% 
  summarise(Value = sum(Value))

#merge FAOTableTrade with Top50Production 
#rename Values so that they are distinct
names(Top50Production)[names(Top50Production) =="Value"] <- "ProductionValue"
names(FAOTableTrade)[names(FAOTableTrade) =="Value"] <- "TradeValue"

#merge FAOTableTrade with Top50Production 
TopProductionTrade <- left_join(Top50Production, FAOTableTrade, by = c("Area", "Item"))

#Task 3c - add proportion of export to Uk into df####
# with Area being the country of origin

#check names of crops
unique(FAOTableTradeMatrix$Item)
#"Cocoa, beans"
#"Wheat"
#"Oil, palm"
#"Oil, palm kernel"
#"Sugar beet"
#"Sugar Raw Centrifugal" 
#does not have data for "Sugar non-centrifugal"

#so original does not get lost
FAOTableTradeMatrixOrig <- FAOTableTradeMatrix

#group crops (e.g. Oil, palm & Oil, palm kernel --> "Oil palm fruit")
FAOTableTradeMatrix$Item[FAOTableTradeMatrix$Item %in% c("Oil, palm", "Oil, palm kernel")] <- "Oil palm fruit"
FAOTableTradeMatrix$Item[FAOTableTradeMatrix$Item %in% c("Sugar beet", "Sugar Raw Centrifugal")] <- "Sugar"

#rename Reporter Countries to Reporter_Countries and Value to TradeMatrixValue
names(FAOTableTradeMatrix)[names(FAOTableTradeMatrix) =="Reporter Countries"] <- "Area"
names(FAOTableTradeMatrix)[names(FAOTableTradeMatrix) =="Value"] <- "TradeUKValue"

#summarize/merge grouped items
FAOTableTradeMatrix<-FAOTableTradeMatrix %>% 
  group_by(Area, Item) %>% 
  summarise(TradeUKValue = sum(TradeUKValue))

#merge with TopProductionTrade to include Total Production of area  (aka country X; ProductionValue), total amount of exported products (TradeValue), and amount traded to UK (TradeUKValue) 
#all "Values" in tonnes
TopProductionTradeUK <- left_join(TopProductionTrade, FAOTableTradeMatrix, by = c("Area", "Item"), )


# Task 4 - save as csv####
#TopUKTrade   <- write_csv(TopProductionTradeUK, file.path(FAOsavedir, "TopUKTrade.csv"))
#TopUKSugar   <- write_csv(TopProductionTradeUK[TopProductionTradeUK$Item=="Sugar",], file.path(FAOsavedir, "TopUKSugar.csv"))
#TopUKWheat   <- write_csv(TopProductionTradeUK[TopProductionTradeUK$Item=="Wheat",], file.path(FAOsavedir, "TopUKWheat.csv"))
#TopUKCocoa   <- write_csv(TopProductionTradeUK[TopProductionTradeUK$Item=="Cocoa, beans",], file.path(FAOsavedir, "TopUKCocoa.csv"))
#TopUKOilPalm <- write_csv(TopProductionTradeUK[TopProductionTradeUK$Item=="Oil palm fruit",], file.path(FAOsavedir, "TopUKOilPalm.csv"))


# Task 5 - add Charlie's Summary stats
#countries without ISO (FAOSTAT --> ISO3):
#Bolivia (Plurinational State of)  --> Bolivia
#China, mainland --> will ignore as China exists
#Congo --> Republic of Congo #Democratic Republic of the Congo already included
#Czechia --> Czech Republic
#Eswatini --> Swaziland
#Iran (Islamic Republic of) --> Iran
#Russian Federation --> Russia
#Sudan (former) --> not clear if South Sudan or Sudan (probably before split)
#Syrian Arab Republic --> Syria
#Timor-Leste --> East Timor
#United Kingdom of Great Britain and Northern Ireland --> United Kingdom
#United Republic of Tanzania --> Tanzania
#United States of America --> United States
#Venezuela (Bolivarian Republic of) --> Venezuela
#Viet Nam --> Vietnam

#rename country names 
#2000:
#Timor-Leste is missing
TopProductionTradeUK$Area[TopProductionTradeUK$Area %in% 
                            c("Bolivia (Plurinational State of)","Congo","Czechia","Eswatini",
                              "Iran (Islamic Republic of)","Russian Federation","Syrian Arab Republic",
                              "United Kingdom of Great Britain and Northern Ireland",
                              "United Republic of Tanzania","United States of America",
                              "Venezuela (Bolivarian Republic of)","Viet Nam")] <- c("Bolivia","Republic of Congo","Czech Republic",
                                                                                     "Swaziland","Iran","Russia","Syria",
                                                                                     "United Kingdom","Tanzania","United States","Venezuela",
                                                                                    "Vietnam")
#2010: 
TopProductionTradeUK$Area[TopProductionTradeUK$Area %in% 
                            c("Bolivia (Plurinational State of)","Congo","Czechia","Eswatini",
                              "Iran (Islamic Republic of)","Russian Federation","Syrian Arab Republic",
                              "Timor-Leste","United Kingdom of Great Britain and Northern Ireland",
                              "United Republic of Tanzania","United States of America",
                              "Venezuela (Bolivarian Republic of)","Viet Nam")] <- c("Bolivia","Republic of Congo","Czech Republic",
                                                                                     "Swaziland","Iran","Russia","Syria","East Timor",
                                                                                     "United Kingdom","Tanzania","United States","Venezuela",
                                                                                     "Vietnam")
#first need to rename Crops in TopProductionTradeUK
TopProductionTradeUK$Item[TopProductionTradeUK$Item %in% "Cocoa, beans"] <- "Cocoa"
TopProductionTradeUK$Item[TopProductionTradeUK$Item %in% "Oil palm fruit"] <- "OilPalm"


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

## Continue with FAO Table prep####

#and plot_data
all_data$Item[all_data$Item %in% c("SugarCane", "SugarBeet")] <- "Sugar"
all_data<-all_data %>% 
  group_by(Area, Item) %>% 
  summarise_at(vars(GHG_metric:WAT_metric), sum)

###
#TopProductionTradeUK: add proportion of Porportion of total Crop Production which goes to the UK (CropProUK), Percentage of UK Import (from Carole data)
#calculate: CropProUK
TopProductionTradeUK$CropProUK <- TopProductionTradeUK$TradeUKValue/TopProductionTradeUK$ProductionValue
#UKImport
TopProductionTradeUK <- left_join(TopProductionTradeUK, suppliers, by = c("Area","Item"))

#merge
ImpactandTradeUK <- merge(TopProductionTradeUK, all_data, by = c("Area","Item"))

#add Impact per tonne, Impact Uk related

#calculate Impact per tonne of crop for each country
#(Impact / Crop Production in Tonnes)
PerTonne <-ImpactandTradeUK %>% 
  group_by(Area, Item) %>% 
  mutate_at(vars(GHG_metric:WAT_metric), funs(./ProductionValue))
#rename
names(PerTonne)[names(PerTonne) =="GHG_metric"] <- "GHG_PerTonne"
names(PerTonne)[names(PerTonne) =="LND_metric"] <- "LND_PerTonne"
names(PerTonne)[names(PerTonne) =="Nit_metric"] <- "Nit_PerTonne"
names(PerTonne)[names(PerTonne) =="Pho_metric"] <- "Pho_PerTonne"
names(PerTonne)[names(PerTonne) =="WAT_metric"] <- "WAT_PerTonne"
#add Impact per tonne to df: ImpactandTradeUK
ImpactandTradeUK <- merge(ImpactandTradeUK, PerTonne, by = c("Area","Item","ProductionValue", "TradeValue","TradeUKValue","CropProUK","ImportToUK"))

#calculate the Impact of the Exported Crop to the Uk 
#(Impact per Tonne * Exported Tonnes to the UK)
ImpactUK <-ImpactandTradeUK %>% 
  group_by(Area, Item) %>% 
  mutate_at(vars(GHG_PerTonne:WAT_PerTonne), funs(.*TradeUKValue))
#rename columns
names(ImpactUK)[names(ImpactUK) =="GHG_PerTonne"] <- "GHG_UKImpact"
names(ImpactUK)[names(ImpactUK) =="LND_PerTonne"] <- "LND_UKImpact"
names(ImpactUK)[names(ImpactUK) =="Nit_PerTonne"] <- "Nit_UKImpact"
names(ImpactUK)[names(ImpactUK) =="Pho_PerTonne"] <- "Pho_UKImpact"
names(ImpactUK)[names(ImpactUK) =="WAT_PerTonne"] <- "WAT_UKImpact"
#reduce to smaller df (for easy merging)
ImpactUK<-ImpactUK[,-c(3:12)]
#add Impact per tonne to df: ImpactandTradeUK
ImpactandTradeUK <- merge(ImpactandTradeUK, ImpactUK, by = c("Area","Item"))


## End - Save as CSV####

#save as csv
#2000:
TopUKTradeImpact00    <- write_csv(ImpactandTradeUK, file.path(FAOsavedir, "ImpactUKTrade_2000.csv"))
TopUKSugarImpact00    <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="Sugar",], file.path(FAOsavedir, "ImpactUKSugar_2000.csv"))
TopUKWheatImpact00    <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="Wheat",], file.path(FAOsavedir, "ImpactUKWheat_2000.csv"))
TopUKCocoaImpact00    <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="Cocoa",], file.path(FAOsavedir, "ImpactUKCocoa_2000.csv"))
TopUKOilPalmImpact00  <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="OilPalm",], file.path(FAOsavedir, "ImpactUKOilPalm_2000.csv"))

#2010:
#TopUKTradeImpact2010    <- write_csv(ImpactandTradeUK, file.path(FAOsavedir, "ImpactUKTrade_2010.csv"))
#TopUKSugarImpact2010    <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="Sugar",], file.path(FAOsavedir, "ImpactUKSugar_2010.csv"))
#TopUKWheatImpact2010    <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="Wheat",], file.path(FAOsavedir, "ImpactUKWheat_2010.csv"))
#TopUKCocoaImpact2010    <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="Cocoa",], file.path(FAOsavedir, "ImpactUKCocoa_2010.csv"))
#TopUKOilPalmImpact2010  <- write_csv(ImpactandTradeUK[ImpactandTradeUK$Item=="OilPalm",], file.path(FAOsavedir, "ImpactUKOilPalm_2010.csv"))





