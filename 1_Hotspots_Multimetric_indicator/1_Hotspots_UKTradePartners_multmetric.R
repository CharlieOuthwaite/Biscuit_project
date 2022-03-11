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
library(ggplot2)


# set directories
datadir <- "Data"
outdir <- "1_Hotspots_Multimetric_indicator/"

# take a look at the raster files
list.files(paste0(datadir, "/Marks_Maps"), pattern = ".tif")

# Notes from Mark's readme doc
# Files ending with:
# a.	_CT = per tonne of crop for each grid cell
# b.	_Total = Total Impact for each grid cell


# which countries are we interested in?
# Use Carole's list of key trade partners for our crops of interest for 2003 (I think)
# this is saved in the Google drive 

# **think this will be updated in the future**

# read in Carole's list
suppliers <- read.csv(paste0(datadir, "/UK_Suppliers_main_crops_Carole.csv"))

#View(suppliers)

# get a list of countries
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

#map1_stack <- stack(paste0(datadir, "/Marks_Maps/", files[1]))
#plot(map1_stack[[1]])       
#res(map1_stack[[1]]) # 0.08333333 0.08333333
#crs(map1_stack[[1]])# +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 


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

# need to standardise to between 0 and 1, but across all values, not just per crop???

cellStats(GHG, stat = "min")
# GHG_Emissons_Total.1 GHG_Emissons_Total.2 GHG_Emissons_Total.3 GHG_Emissons_Total.4 GHG_Emissons_Total.5 
# 0                    0                    0                    0                    0 

cellStats(GHG, stat = "max")
# GHG_Emissons_Total.1 GHG_Emissons_Total.2 GHG_Emissons_Total.3 GHG_Emissons_Total.4 GHG_Emissons_Total.5 
# 21319.51            106966.16             12279.86            332213.59             58436.73

max1 <- max(cellStats(GHG, stat = "max"))
min1 <- 0


GHG_RS <- ((GHG-min1)/(max1-min1))


# use exact_extract function to get some summary stats per country/band
# the function does the same for each layer automatically

GHG_sums <- exact_extract(x = GHG_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(GHG_sums) <- ctry_shps$NAME_0

colnames(GHG_sums) <- sub("GHG_Emissons_Total.1", "Cocoa", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.2", "Oilpalm", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.3", "SugarBeet", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.4", "SugarCane", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.5", "Wheat", colnames(GHG_sums))


#### 2. Land biodiversity impact ####

LND <- stack(paste0(datadir, "/Marks_Maps/", files[2]))

# need to standardise to between 0 and 1, but across all values, not just per crop???

cellStats(LND, stat = "min")
# LD_BioDiv_Total.1 LD_BioDiv_Total.2 LD_BioDiv_Total.3 LD_BioDiv_Total.4 LD_BioDiv_Total.5 
# 0                 0                 0                 0                 0

cellStats(LND, stat = "max")
# LD_BioDiv_Total.1 LD_BioDiv_Total.2 LD_BioDiv_Total.3 LD_BioDiv_Total.4 LD_BioDiv_Total.5 
# 2.854524e-08      2.386932e-05      1.473781e-06      1.182444e-04      5.721687e-05

max2 <- max(cellStats(LND, stat = "max"))
min2 <- 0


LND_RS <- ((LND-min2)/(max2-min2))


LND_sums <- exact_extract(x = LND_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(LND_sums) <- ctry_shps$NAME_0

colnames(LND_sums) <- sub("LD_BioDiv_Total.1", "Cocoa", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.2", "Oilpalm", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.3", "SugarBeet", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.4", "SugarCane", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.5", "Wheat", colnames(LND_sums))

#### 3. N biodiv impact ####

Nit <- stack(paste0(datadir, "/Marks_Maps/", files[3]))

# need to standardise to between 0 and 1, but across all values, not just per crop???

cellStats(Nit, stat = "min")
# N_Marine_BioDiv_Total.1 N_Marine_BioDiv_Total.2 N_Marine_BioDiv_Total.3 N_Marine_BioDiv_Total.4 
# 0                       0                       0                       0                 0                    0                    0                    0 

cellStats(Nit, stat = "max")
# N_Marine_BioDiv_Total.1 N_Marine_BioDiv_Total.2 N_Marine_BioDiv_Total.3 N_Marine_BioDiv_Total.4 
# 9.312263e-07            2.872260e-07            2.222444e-07            8.720011e-07 

max3 <- max(cellStats(Nit, stat = "max"))
min3 <- 0


Nit_RS <- ((Nit-min3)/(max3-min3))


Nit_sums <- exact_extract(x = Nit_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(Nit_sums) <- ctry_shps$NAME_0

colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.1", "Oilpalm", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.2", "SugarBeet", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.3", "SugarCane", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.4", "Wheat", colnames(Nit_sums))

Nit_sums$sum.Cocoa <- NA

#### 4. P biodiv impact ####

Pho <- stack(paste0(datadir, "/Marks_Maps/", files[4]))

# need to standardise to between 0 and 1, but across all values, not just per crop???

cellStats(Pho, stat = "min")
# P_Marine_BioDiv_Total.1 P_Marine_BioDiv_Total.2 P_Marine_BioDiv_Total.3 P_Marine_BioDiv_Total.4 
# 0                       0                       0                       0                     0                    0 

cellStats(Pho, stat = "max")
# P_Marine_BioDiv_Total.1 P_Marine_BioDiv_Total.2 P_Marine_BioDiv_Total.3 P_Marine_BioDiv_Total.4 
# 8.588686e-07            3.745093e-05            2.162281e-03            2.375582e-04

max4 <- max(cellStats(Pho, stat = "max"))
min4 <- 0


Pho_RS <- ((Pho-min4)/(max4-min4))

Pho_sums <- exact_extract(x = Pho_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
rownames(Pho_sums) <- ctry_shps$NAME_0

colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.1", "Oilpalm", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.2", "SugarBeet", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.3", "SugarCane", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.4", "Wheat", colnames(Pho_sums))

Pho_sums$sum.Cocoa <- NA

#### 5. water debt ####

WAT <- stack(paste0(datadir, "/Marks_Maps/", files[5]))

# need to standardise to between 0 and 1, but across all values, not just per crop???

# Water_Debt_Total.1 Water_Debt_Total.2 Water_Debt_Total.3 Water_Debt_Total.4 
# 2.779843e-11      -1.316293e+02      -4.723896e+03       0.000000e+00 

cellStats(WAT, stat = "max")
# Water_Debt_Total.1 Water_Debt_Total.2 Water_Debt_Total.3 Water_Debt_Total.4 
# 11523.34          176487.58         1072299.38        80222504.00

max5 <- max(cellStats(WAT, stat = "max"))
min5 <- min(cellStats(WAT, stat = "min"))


WAT_RS <- ((WAT-min5)/(max5-min5))



WAT_sums <- exact_extract(x = WAT_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

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


all_sums$metric <- c(rep("GHG", 21), rep("LND", 21), rep("Nit", 21), rep("Pho", 21), rep("WAT", 21))

View(all_sums)

all_sums$Country <- sub("[0-9]+", "", rownames(all_sums))

# save data
write.csv(all_sums, file = paste0(outdir, "Indicator_summaries_perCountry_tradepartners_RS.csv"))



##%######################################################%##
#                                                          #
####                       plots                        ####
#                                                          #
##%######################################################%##


#1. facet per metric, bar per country, bar split by crop


# need to organise data into long format

plot_data <- matrix(nrow = 525, ncol = 4)

plot_data[, 1] <- c(all_sums[, 1], all_sums[, 2], all_sums[, 3], all_sums[, 4], all_sums[, 5])

plot_data[, 2] <- rep(all_sums$metric, 5)

plot_data[, 3] <- rep(all_sums$Country, 5)

plot_data[, 4] <- c(rep("Cocoa", 105), rep("OilPalm", 105), rep("SugarBeet", 105), rep("SugarCane", 105), rep("Wheat", 105))

colnames(plot_data) <- c("Value", "Metric", "Country", "Crop")

plot_data <- as.data.frame(plot_data)

plot_data$Value <- as.numeric(as.character(plot_data$Value))

# create a plot, panel for each

ggplot(data = plot_data) + 
  geom_col(aes(x = Country, y = Value, fill = Crop)) +
  facet_wrap(~ Metric, scales = "free_y") + 
  scale_fill_manual(values = c("#8B4500", "#CD9B1D", "#458B00", "#9ACD32", "#EEB422")) +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust=0.5))
  

ggsave(filename = paste0(outdir, "/Plot_indicators_UK_partners.pdf"), width = 9, height = 6)



# create similar plots but just for the specific countries of interest per crop


cocoa_dat <- na.omit(plot_data[plot_data$Crop == "Cocoa" & plot_data$Country %in% c("Cote d'Ivoire", "Ghana", "Nigeria", "Cameroon", "Indonesia"),  ])

ggplot(data = cocoa_dat) + 
  geom_col(aes(x = Country, y = Value, fill = Metric)) +
  facet_wrap(~ Metric, scales = "free_y") + 
  scale_fill_manual(values = c("#00008B", "#006400")) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  ggtitle("Cocoa")

ggsave(filename = paste0(outdir, "/Cocoa_indicators_major_partners.pdf"))


palm_dat <- na.omit(plot_data[plot_data$Crop == "OilPalm" & plot_data$Country %in% c("Malaysia", "Indonesia", "Papua New Guinea", "Nigeria", "Brazil"),  ])

ggplot(data = palm_dat) + 
  geom_col(aes(x = Country, y = Value, fill = Metric)) +
  facet_wrap(~ Metric, scales = "free_y") + 
  scale_fill_manual(values = c("#00008B", "#006400", "#00CD00", "#FF7F00", "#104E8B")) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  ggtitle("Oil Palm")

ggsave(filename = paste0(outdir, "/OilPalm_indicators_major_partners.pdf"))


beet_dat <- na.omit(plot_data[plot_data$Crop == "SugarBeet" & plot_data$Country %in% c("United Kingdom", "France"),  ])

ggplot(data = beet_dat) + 
  geom_col(aes(x = Country, y = Value, fill = Metric)) +
  facet_wrap(~ Metric, scales = "free_y") + 
  scale_fill_manual(values = c("#00008B", "#006400", "#00CD00", "#FF7F00", "#104E8B")) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  ggtitle("Sugar Beet")

ggsave(filename = paste0(outdir, "/SugarBeet_indicators_major_partners.pdf"))


cane_dat <- na.omit(plot_data[plot_data$Crop == "SugarCane" & plot_data$Country %in% c("Mauritius", "Fiji", "Jamaica", "Swaziland", "Belize", "Trinidad and Tobago", "Zimbabwe", "South Africa"),  ])

ggplot(data = cane_dat) + 
  geom_col(aes(x = Country, y = Value, fill = Metric)) +
  facet_wrap(~ Metric, scales = "free_y") + 
  scale_fill_manual(values = c("#00008B", "#006400", "#00CD00", "#FF7F00", "#104E8B")) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  ggtitle("Sugar Cane")

ggsave(filename = paste0(outdir, "/SugarCane_indicators_major_partners.pdf"))


wheat_dat <- na.omit(plot_data[plot_data$Crop == "Wheat" & plot_data$Country %in% c("United Kingdom","France", "Canada", "Germany", "United States of America"),  ])

ggplot(data = wheat_dat) + 
  geom_col(aes(x = Country, y = Value, fill = Metric)) +
  facet_wrap(~ Metric, scales = "free_y") + 
  scale_fill_manual(values = c("#00008B", "#006400", "#00CD00", "#FF7F00", "#104E8B")) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  ggtitle("Wheat")

ggsave(filename = paste0(outdir, "/Wheat_indicators_major_partners.pdf"))




##%######################################################%##
#                                                          #
####            bar charts for each country             ####
#                                                          #
##%######################################################%##

# creating bar charts which show the values for each metric 
# for each country. These are just for trade partners of the UK currently.
# bar chart per country to be put on a map highlighting the areas harvested. 

# still to do:
# 1. need a way to standardise the metrics so they are comparable (set all
# values at the global level to between 0 and 1?)
# 2. try spliting the bars by crop if there is some crossover?
# each country extracted metric data but not a top supplier

# loop through each country and create a plot

for(i in countries){
  
  # select the crop/crops of interest for this country
  crop  <- suppliers[suppliers$partner == i, "item"]
  
  # subset the plotting data
  plot_data2 <- plot_data[grep(i, plot_data$Country ), ]
  

  
  
plot_data2 <- plot_data[plot_data$Country == "United States" & plot_data$Crop == "Wheat", ]


ggplot(plot_data2) +
  geom_col(aes(x = Metric, y = Value), fill = c("#EEC900")) +  # wheat only for the US so selected a colour
  xlab("") + 
  ylab("Standardised indicator value") + 
  theme_bw()


}

