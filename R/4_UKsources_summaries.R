##%######################################################%##
#                                                          #
####               Multimetric indicator                ####
####            hotspots, UK trade partners             ####
#                                                          #
##%######################################################%##


# Charlie Outhwaite, started 10/06/2021

# This script uses information on the countries that the UK imports biscuit crops from
# and global maps on environmental impact indicators of the same crops to assess the level
# of impact on biodiversity in those source regions.

# clear environment
rm(list = ls())

# load required libraries
library(raster)
library(exactextractr) # exact_extract function from here
library(ggplot2)
library(plyr)
library(sf)
library(dplyr)


# set directories
datadir <- "Data"
outdir <- "Output_part2/final_run/"
if(dir.exists(outdir) == F) dir.create(outdir)

# take a look at the raster files
list.files(paste0(datadir, "/Indicator_Maps"), pattern = ".tif")

# Notes from readme doc related to indicator metrics
# Files ending with:
# a.	_CT = per tonne of crop for each grid cell
# b.	_Total = Total Impact for each grid cell


# which countries are we interested in?
# Suppliers list, processed by Abbie, producers of 5% or more of UK imports
# Sept 2023, list appended _NEW
suppliers <- read.csv(paste0(datadir, "/top_countries_5_percent_NEW.csv"))

View(suppliers)

# get a list of countries
countries <- unique(suppliers$partner)

length(countries) # currently 7 suppliers of interest for 2019 at 5%


#### Task 1: Summary stats for each source country ####

# We will use the total impact raster files
#files <- list.files(paste0(datadir, "/Indicator_Maps"), pattern = "_Total.tif")

# do the same but with the per tonne of crop data
files <- list.files(paste0(datadir, "/Indicator_Maps"), pattern = "_CT.tif")

# 5 files
# "GHG_Emissons_Total.tif"
# "LD_BioDiv_Total.tif"
# "N_Marine_BioDiv_Total.tif"
# "P_Marine_BioDiv_Total.tif"
# "Water_Debt_Total.tif" 

# 5 files
# "GHG_Emissons_CT.tif"    
# "LD_BioDiv_CT.tif"       
# "N_Marine_BioDiv_CT.tif" 
# "P_Marine_BioDiv_CT.tif"
# "Water_Debt_CT.tif"

# each file can be opened as a raster stack which has a band per crop (See table in README)

#map1_stack <- stack(paste0(datadir, "/Marks_Maps/", files[1]))
#plot(map1_stack[[1]])       
#res(map1_stack[[1]]) # 0.08333333 0.08333333
#crs(map1_stack[[1]])# +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 


#### get country border data for the list of countries ####
View(getData('ISO3')) 

# get the country codes to extract country polygons
codes <- getData('ISO3')

# some names do not match so added in manually
countries <- sub("Cote d'Ivoire", "Côte d'Ivoire", countries)

# get matches
cntry_codes <- codes[codes$NAME %in% countries, ]

# extract country shapefiles for 
codes = list()
codes$countries <- cntry_codes$ISO3

# download and combine polygons into one object
ctry_shps = do.call("bind", lapply(codes$countries, 
                                   function(x) getData('GADM', country=x, level=0)))

# take a little look
plot(ctry_shps)
#plot(map1_stack[[1]], add = TRUE) 

# save this polygons object for future use
shapefile(ctry_shps, filename = paste0(outdir, "/Trade_partners_polygons_5perc.shp"), overwrite = T)


## simplify information on crops for polygon fill in figure 2
crps <- suppliers

# add the UK twice as it is representing 2 crops
ctry_shps <- rbind(ctry_shps, ctry_shps[7,]) # 7 is the UK

# add into to polygons, note replicated country added onto the end
ctry_shps$Crops <- crps$crop[c(5, 2, 3, 7, 8, 4, 1, 6)]
# check data correctly matched
ctry_shps@data


##%######################################################%##
#                                                          #
####               Get some summary stats               ####
####       for each country/indicator/crop combo        ####
#                                                          #
##%######################################################%##

#### 1. GHG Total indicator map ####

# load in the raster stack
GHG <- stack(paste0(datadir, "/Indicator_Maps/", files[1]))

# need to standardise to between 0 and 1, but across all values, not just per crop

cellStats(GHG, stat = "min")
# GHG_Emissons_Total.1 GHG_Emissons_Total.2 GHG_Emissons_Total.3 GHG_Emissons_Total.4 GHG_Emissons_Total.5 
# 0                    0                    0                    0                    0 

cellStats(GHG, stat = "max")
# GHG_Emissons_Total.1 GHG_Emissons_Total.2 GHG_Emissons_Total.3 GHG_Emissons_Total.4 GHG_Emissons_Total.5 
# 21319.51            106966.16             12279.86            332213.59             58436.73

max1 <- max(cellStats(GHG, stat = "max"))
min1 <- 0

# carry out the rescaling (min-max normalisation)
GHG_RS <- ((GHG-min1)/(max1-min1))


# use exact_extract function to get some summary stats per country/band
# the function does the same for each layer automatically

GHG_sums <- exact_extract(x = GHG_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
GHG_sums$partner <- ctry_shps$NAME_0

# organise col names
# colnames(GHG_sums) <- sub("GHG_Emissons_Total.1", "Cocoa", colnames(GHG_sums))
# colnames(GHG_sums) <- sub("GHG_Emissons_Total.2", "Oilpalm", colnames(GHG_sums))
# colnames(GHG_sums) <- sub("GHG_Emissons_Total.3", "SugarBeet", colnames(GHG_sums))
# colnames(GHG_sums) <- sub("GHG_Emissons_Total.4", "SugarCane", colnames(GHG_sums))
# colnames(GHG_sums) <- sub("GHG_Emissons_Total.5", "Wheat", colnames(GHG_sums))

# organise col names
colnames(GHG_sums) <- sub("GHG_Emissons_CT_1", "Cocoa", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_CT_2", "Oilpalm", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_CT_3", "SugarBeet", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_CT_4", "SugarCane", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_CT_5", "Wheat", colnames(GHG_sums))

# save
#write.csv(GHG_sums, paste0(outdir, "GHG_summaries_allcrops.csv"), row.names = F)
write.csv(GHG_sums, paste0(outdir, "GHG_summaries_allcrops_CT.csv"), row.names = F)


#### 2. Land biodiversity impact ####

# read in the files
LND <- stack(paste0(datadir, "/Indicator_Maps/", files[2]))

# need to standardise to between 0 and 1, but across all values, not just per crop???

cellStats(LND, stat = "min")
# LD_BioDiv_Total.1 LD_BioDiv_Total.2 LD_BioDiv_Total.3 LD_BioDiv_Total.4 LD_BioDiv_Total.5 
# 0                 0                 0                 0                 0

cellStats(LND, stat = "max")
# LD_BioDiv_Total.1 LD_BioDiv_Total.2 LD_BioDiv_Total.3 LD_BioDiv_Total.4 LD_BioDiv_Total.5 
# 2.854524e-08      2.386932e-05      1.473781e-06      1.182444e-04      5.721687e-05

max2 <- max(cellStats(LND, stat = "max"))
min2 <- 0

# rescale
LND_RS <- ((LND-min2)/(max2-min2))

# extract country summaries
LND_sums <- exact_extract(x = LND_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
LND_sums$partner <- ctry_shps$NAME_0

# colnames(LND_sums) <- sub("LD_BioDiv_Total.1", "Cocoa", colnames(LND_sums))
# colnames(LND_sums) <- sub("LD_BioDiv_Total.2", "Oilpalm", colnames(LND_sums))
# colnames(LND_sums) <- sub("LD_BioDiv_Total.3", "SugarBeet", colnames(LND_sums))
# colnames(LND_sums) <- sub("LD_BioDiv_Total.4", "SugarCane", colnames(LND_sums))
# colnames(LND_sums) <- sub("LD_BioDiv_Total.5", "Wheat", colnames(LND_sums))


colnames(LND_sums) <- sub("LD_BioDiv_CT_1", "Cocoa", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_CT_2", "Oilpalm", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_CT_3", "SugarBeet", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_CT_4", "SugarCane", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_CT_5", "Wheat", colnames(LND_sums))


#write.csv(LND_sums, paste0(outdir, "LND_summaries_allcrops.csv"), row.names = F)
write.csv(LND_sums, paste0(outdir, "LND_summaries_allcrops_CT.csv"), row.names = F)

#### 3. N biodiv impact ####

# load file
Nit <- stack(paste0(datadir, "/Indicator_Maps/", files[3]))

# need to standardise to between 0 and 1, but across all values, not just per crop???

cellStats(Nit, stat = "min")
# N_Marine_BioDiv_Total.1 N_Marine_BioDiv_Total.2 N_Marine_BioDiv_Total.3 N_Marine_BioDiv_Total.4 
# 0                       0                       0                       0                 0                    0                    0                    0 

cellStats(Nit, stat = "max")
# N_Marine_BioDiv_Total.1 N_Marine_BioDiv_Total.2 N_Marine_BioDiv_Total.3 N_Marine_BioDiv_Total.4 
# 9.312263e-07            2.872260e-07            2.222444e-07            8.720011e-07 

max3 <- max(cellStats(Nit, stat = "max"))
min3 <- 0

# rescale
Nit_RS <- ((Nit-min3)/(max3-min3))

# extract country summaries
Nit_sums <- exact_extract(x = Nit_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
Nit_sums$partner <- ctry_shps$NAME_0

# colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.1", "Oilpalm", colnames(Nit_sums))
# colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.2", "SugarBeet", colnames(Nit_sums))
# colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.3", "SugarCane", colnames(Nit_sums))
# colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.4", "Wheat", colnames(Nit_sums))

colnames(Nit_sums) <- sub("N_Marine_BioDiv_CT_1", "Oilpalm", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_CT_2", "SugarBeet", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_CT_3", "SugarCane", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_CT_4", "Wheat", colnames(Nit_sums))

Nit_sums$sum.Cocoa <- NA

# save
#write.csv(Nit_sums, paste0(outdir, "Nit_summaries_allcrops.csv"), row.names = F)
write.csv(Nit_sums, paste0(outdir, "Nit_summaries_allcrops_CT.csv"), row.names = F)


#### 4. P biodiv impact ####

Pho <- stack(paste0(datadir, "/Indicator_Maps/", files[4]))

# need to standardise to between 0 and 1, but across all values, not just per crop???

cellStats(Pho, stat = "min")
# P_Marine_BioDiv_Total.1 P_Marine_BioDiv_Total.2 P_Marine_BioDiv_Total.3 P_Marine_BioDiv_Total.4 
# 0                       0                       0                       0                     0                    0 

cellStats(Pho, stat = "max")
# P_Marine_BioDiv_Total.1 P_Marine_BioDiv_Total.2 P_Marine_BioDiv_Total.3 P_Marine_BioDiv_Total.4 
# 8.588686e-07            3.745093e-05            2.162281e-03            2.375582e-04

max4 <- max(cellStats(Pho, stat = "max"))
min4 <- 0

# rescale
Pho_RS <- ((Pho-min4)/(max4-min4))

# extract country summaries
Pho_sums <- exact_extract(x = Pho_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
Pho_sums$partner <- ctry_shps$NAME_0

# colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.1", "Oilpalm", colnames(Pho_sums))
# colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.2", "SugarBeet", colnames(Pho_sums))
# colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.3", "SugarCane", colnames(Pho_sums))
# colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.4", "Wheat", colnames(Pho_sums))

colnames(Pho_sums) <- sub("P_Marine_BioDiv_CT_1", "Oilpalm", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_CT_2", "SugarBeet", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_CT_3", "SugarCane", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_CT_4", "Wheat", colnames(Pho_sums))

Pho_sums$sum.Cocoa <- NA

# save
#write.csv(Pho_sums, paste0(outdir, "Pho_summaries_allcrops.csv"), row.names = F)
write.csv(Pho_sums, paste0(outdir, "Pho_summaries_allcrops_CT.csv"), row.names = F)



#### 5. water debt ####

# read in file
WAT <- stack(paste0(datadir, "/Indicator_Maps/", files[5]))

cellStats(WAT, stat = "min")
# Water_Debt_Total.1 Water_Debt_Total.2 Water_Debt_Total.3 Water_Debt_Total.4 
# 2.779843e-11      -1.316293e+02      -4.723896e+03       0.000000e+00 

cellStats(WAT, stat = "max")
# Water_Debt_Total.1 Water_Debt_Total.2 Water_Debt_Total.3 Water_Debt_Total.4 
# 11523.34          176487.58         1072299.38        80222504.00

# we only want to consider values that represent unsustainable use of water
# therefore set values that are below 1 to NA
WAT_sub <- reclassify(WAT, cbind(-Inf, 1, NA), right=FALSE)

# check
cellStats(WAT_sub, stat = "min")
cellStats(WAT_sub, stat = "max")

#get max and mins
max5 <- max(cellStats(WAT_sub, stat = "max"))
min5 <- min(cellStats(WAT_sub, stat = "min"))

# rescale
WAT_RS <- ((WAT_sub-min5)/(max5-min5))

# extract country summaries
WAT_sums <- exact_extract(x = WAT_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
WAT_sums$partner <- ctry_shps$NAME_0

# colnames(WAT_sums) <- sub("Water_Debt_Total.1", "Oilpalm", colnames(WAT_sums))
# colnames(WAT_sums) <- sub("Water_Debt_Total.2", "SugarBeet", colnames(WAT_sums))
# colnames(WAT_sums) <- sub("Water_Debt_Total.3", "SugarCane", colnames(WAT_sums))
# colnames(WAT_sums) <- sub("Water_Debt_Total.4", "Wheat", colnames(WAT_sums))

colnames(WAT_sums) <- sub("Water_Debt_CT_1", "Oilpalm", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_CT_2", "SugarBeet", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_CT_3", "SugarCane", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_CT_4", "Wheat", colnames(WAT_sums))

WAT_sums$sum.Cocoa <- NA


#write.csv(WAT_sums, paste0(outdir, "WAT_summaries_allcrops.csv"), row.names = F)
write.csv(WAT_sums, paste0(outdir, "WAT_summaries_allcrops_CT.csv"), row.names = F)


# organise all the summed values
all_sums <- rbind(GHG_sums[, c(grep("sum", colnames(GHG_sums)))],
                  LND_sums[, c(grep("sum", colnames(LND_sums)))],
                  Nit_sums[, c(grep("sum", colnames(Nit_sums)))],
                  Pho_sums[, c(grep("sum", colnames(Pho_sums)))],
                  WAT_sums[, c(grep("sum", colnames(WAT_sums)))])


# add in metric info
all_sums$metric <- c(rep("GHG", 8), rep("LND", 8), rep("Nit", 8), rep("Pho", 8), rep("WAT", 8))

# add in country names
all_sums$Country <- rep(GHG_sums$partner, 5)

# take a look
View(all_sums)

# save data
write.csv(all_sums, file = paste0(outdir, "Indicator_summaries_perCountry_tradepartners_5perc_CT.csv"))


##%######################################################%##
#                                                          #
####                       plots                        ####
#                                                          #
##%######################################################%##

#all_sums <- read.csv(file = paste0(outdir, "Indicator_summaries_perCountry_tradepartners_5perc.csv"))

# need to organise data into long format

# create an empty matrix
plot_data <- matrix(nrow = 200, ncol = 4)

# fill in each column of data
plot_data[, 1] <- c(all_sums[, 1], all_sums[, 2], all_sums[, 3], all_sums[, 4], all_sums[, 5])

plot_data[, 2] <- rep(all_sums$metric, 5)

plot_data[, 3] <- rep(all_sums$Country, 5)

# add in variable names
plot_data[, 4] <- c(rep("Cocoa", nrow(all_sums)), rep("OilPalm", nrow(all_sums)), rep("SugarBeet", nrow(all_sums)), rep("SugarCane", nrow(all_sums)), rep("Wheat", nrow(all_sums)))

# edit column names
colnames(plot_data) <- c("Value", "Metric", "Country", "Crop")

# convert to dataframe
plot_data <- as.data.frame(plot_data)

# conver to numerical values
plot_data$Value <- as.numeric(as.character(plot_data$Value))

# save
#write.csv(plot_data, paste0(outdir, "Plot_data_country_barplots_FIG2.csv"), row.names = F)
write.csv(plot_data, paste0(outdir, "Plot_data_country_barplots_FIG2_CT.csv"), row.names = F)




##%######################################################%##
#                                                          #
####                 Stacked barchart                   ####
#                                                          #
##%######################################################%##

# load in data if not already in environment
#plot_data <- read.csv(paste0(outdir, "Plot_data_country_barplots_FIG2.csv"))

plot_data$Crop <- tolower(plot_data$Crop)

# combine sugar data
plot_data$Crop[plot_data$Crop == "sugarbeet"] <- "sugar"
plot_data$Crop[plot_data$Crop == "sugarcane"] <- "sugar"

suppliers$crop[suppliers$crop == "sugarbeet"] <- "sugar"
suppliers$crop[suppliers$crop == "sugarcane"] <- "sugar"

suppliers$partner[suppliers$partne == "Cote d'Ivoire"] <- "Côte d'Ivoire"

# subset to just the information for the crop/country combo required
plot_data2 <- left_join(suppliers, plot_data, by=c('partner'='Country', 'crop'='Crop'))

# organise labels
plot_data2$Country_crop <- paste(plot_data2$partner, "-\n",  plot_data2$crop)

# to order the bars by height
plot_data2$Country_crop <- factor(plot_data2$Country_crop, levels = rev(c("Malaysia -\n oilpalm", "Indonesia -\n oilpalm", "United Kingdom -\n wheat", "United Kingdom -\n sugar", "Côte d'Ivoire -\n cocoa",  "Ghana -\n cocoa", "Nigeria -\n cocoa", "Cameroon -\n cocoa")))

# remove duplicated rows due to rep of UK
plot_data2 <- distinct(plot_data2)

# save
# write.csv(plot_data2, paste0(outdir, "Plot_data_country_barplots_FIG2_DATA.csv"), row.names = F)
write.csv(plot_data2, paste0(outdir, "Plot_data_country_barplots_FIG2_DATA_CT.csv"), row.names = F)


# create plot
ggplot(plot_data2, aes(fill=Metric, y=Country_crop, x=Value)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#d73027",   "#fdae61", "#abd9e9", "#006d2c", "#54278f")) + 
  scale_x_continuous(trans = 'sqrt') + 
  xlab("Total standardised impact across indicators") + 
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        text = element_text(size = 12))

# save
# ggsave(filename = paste0(outdir, "Figure2_stackedbarplot_suppliers_impact.pdf"))
ggsave(filename = paste0(outdir, "Figure2_stackedbarplot_suppliers_impact_CT.pdf"))
