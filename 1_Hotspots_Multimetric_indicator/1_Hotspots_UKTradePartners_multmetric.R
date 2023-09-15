##%######################################################%##
#                                                          #
####              1. Multimetric indicator              ####
####            hotspots, UK trade partners             ####
#                                                          #
##%######################################################%##


# Charlie Outhwaite, 10/06/2021

# Here, I am exploring Mark's multimetric indicator rasters. 
# First, I will explore the countries that are major UK trade partners for 
# our crops and will make some summaries of the indicators for these countries

# to run this code, you will need to have downloaded Mark's rasters from the 
# Google Drive and have them in a folder "Data/Marks_Maps". You will also need 
# to have the list of top producing countries as a csv file in the "Data" folder.

# Country lists updated using new data, processed by Abbie.


rm(list = ls())

# load required libraries
library(raster)
library(rgdal)
library(exactextractr) # exact_extract function from here
library(ggplot2)
library(plyr)
library(sf)
library(dplyr)


# set directories
datadir <- "Data"
outdir <- "1_Hotspots_Multimetric_indicator/"

# take a look at the raster files
list.files(paste0(datadir, "/Indicator_Maps"), pattern = ".tif")

# Notes from Mark's readme doc
# Files ending with:
# a.	_CT = per tonne of crop for each grid cell
# b.	_Total = Total Impact for each grid cell


# which countries are we interested in?
## updated suppliers list, processed by Abbie, producers of 5% or more of UK imports
# Sept 2023, list appended _NEW
suppliers <- read.csv(paste0(datadir, "/top_countries_5_percent_NEW.csv"))

View(suppliers)

# get a list of countries
countries <- unique(suppliers$partner)

length(countries) # currently 7 suppliers of interest for 2019 at 5%


#### Task 1: Summary stats for each country ####

# For now, will use the total impact raster files

files <- list.files(paste0(datadir, "/Indicator_Maps"), pattern = "_Total.tif")

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

# reorganise country names
#countries <- gsub("\\.", " ", countries)
# countries <- sub(" of America", "", countries)
# countries <- sub("Viet Nam", "Vietnam", countries)
#countries <- sub("Cote d Ivoire", "Côte d'Ivoire", countries)


# some names do not match so added in manually
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
shapefile(ctry_shps, filename = paste0(outdir, "/Trade_partners_polygons_Kaster_5perc_2019.shp"), overwrite = T)


## simplify information on crops for polygon fill in figure 2
#crps <- suppliers[, c(2,4)]
crps <- suppliers

# UK and Indonesia include more than one crop of interest, create new columns?
# crps$Crops <- crps$Crop
# #crps[crps$Trade_Partner == "United Kingdom", "Crops"] <- "wheat_sugar"
# #crps[crps$Trade_Partner == "Indonesia", "Crops"] <- "oilpalm_sugar_cocoa"
# 
# # relabel the two types of sugar
# crps$Crops[crps$Crops =="sugarcane"] <- "sugar"
# crps$Crops[crps$Crops =="sugarbeet"] <- "sugar"

# crps <- crps[, c(1,3)]
# crps <- unique(crps)

# testing alternative to relabelling based on multiple crops, try overlaying
ctry_shps <- rbind(ctry_shps, ctry_shps[7,]) # 7 is the UK

# add into to polygons, note replicated country added onto the end
ctry_shps$Crops <- crps$crop[c(5, 2, 3, 7, 8, 4, 1, 6)]
# check data correctly matched
ctry_shps@data

# #### create a base map for Figure 2 ####
# # This needs to be a world outline with the countries of interest coloured by crop
# 
# # think I found these lines of code via google
# ctry_shps@data$id = rownames(ctry_shps@data)
# ctry_shps.points = fortify(ctry_shps, region="id") # this takes a while
# ctry_shps.df = join(ctry_shps.points, ctry_shps@data, by="id")
# 
# # save dataframe for ggplot
# save(ctry_shps.df, file = paste0(outdir, "Trade_partners_DF_Kaster_5perc_pluscrop_2019.rdata"))
# 
# # save this polygons object for future use
# #shapefile(ctry_shps, filename = paste0(outdir, "/Trade_partners_polygons_Kaster_5perc_pluscrop_2019.shp"))
# 
# # load in shapefile
# #ctry_shps <- shapefile(paste0(outdir, "/Trade_partners_polygons_Kaster_5perc_pluscrop_2019.shp"))
# #load(file = paste0(outdir, "Trade_partners_DF_Kaster_5perc_pluscrop_2019.rdata"))
# 
# # base map - country outlines with coloured polygons for those countries of interest
# world <- map_data("world")
# 
# # replace sugarcane and sugarbeet with sugar
# ctry_shps.df$Crops[ctry_shps.df$Crops %in% c("sugarbeet", "sugarcane")] <- "sugar"
# 
# ctry_shps.df$Crops <- as.factor(ctry_shps.df$Crops)
# levels(ctry_shps.df$Crops)
# ctry_shps.df$Crops <- factor(ctry_shps.df$Crops, levels = c("oilpalm", "sugar", "wheat", "cocoa"))
# levels(ctry_shps.df$Crops)
# 
# p1 <- ggplot() +
#   geom_map(
#     data = world, map = world,
#     aes(long, lat, map_id = region), fill = "transparent", col = "darkgrey") +
#   geom_polygon(data = ctry_shps.df, aes(fill = Crops, x = long, y = lat, group = group), alpha = 0.6) +
#   scale_fill_manual(values = c("#A9C975", "#8B8878", "#EEB422","#8B3E2F")) + # green, grey, yellow, brown
#   scale_y_continuous(limits=c(-55,90)) +
#   theme_bw() + 
#   theme(panel.grid = element_blank(), 
#         panel.border = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_rect(fill = "transparent",
#                                         colour = NA_character_),
#         plot.background = element_rect(fill = "transparent",
#                                        colour = NA_character_), 
#         legend.background = element_rect(fill = "transparent",
#                                          colour = NA_character_), 
#         legend.position = "bottom")
# 
# ggsave(p1, filename = paste0(outdir, "FIGURE_2_Basemap_inc_country_polygons_5perc2.png"), height = 4, width = 8, unit = "in")

##%######################################################%##
#                                                          #
####               get some summary stat                ####
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


GHG_RS <- ((GHG-min1)/(max1-min1))


# use exact_extract function to get some summary stats per country/band
# the function does the same for each layer automatically

GHG_sums <- exact_extract(x = GHG_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
GHG_sums$partner <- ctry_shps$NAME_0

colnames(GHG_sums) <- sub("GHG_Emissons_Total.1", "Cocoa", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.2", "Oilpalm", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.3", "SugarBeet", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.4", "SugarCane", colnames(GHG_sums))
colnames(GHG_sums) <- sub("GHG_Emissons_Total.5", "Wheat", colnames(GHG_sums))

write.csv(GHG_sums, paste0(outdir, "GHG_summaries_allcrops.csv"), row.names = F)


#### 2. Land biodiversity impact ####

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


LND_RS <- ((LND-min2)/(max2-min2))


LND_sums <- exact_extract(x = LND_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
LND_sums$partner <- ctry_shps$NAME_0

colnames(LND_sums) <- sub("LD_BioDiv_Total.1", "Cocoa", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.2", "Oilpalm", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.3", "SugarBeet", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.4", "SugarCane", colnames(LND_sums))
colnames(LND_sums) <- sub("LD_BioDiv_Total.5", "Wheat", colnames(LND_sums))

write.csv(LND_sums, paste0(outdir, "LND_summaries_allcrops.csv"), row.names = F)

#### 3. N biodiv impact ####

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


Nit_RS <- ((Nit-min3)/(max3-min3))


Nit_sums <- exact_extract(x = Nit_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
Nit_sums$partner <- ctry_shps$NAME_0

colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.1", "Oilpalm", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.2", "SugarBeet", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.3", "SugarCane", colnames(Nit_sums))
colnames(Nit_sums) <- sub("N_Marine_BioDiv_Total.4", "Wheat", colnames(Nit_sums))

Nit_sums$sum.Cocoa <- NA

write.csv(Nit_sums, paste0(outdir, "Nit_summaries_allcrops.csv"), row.names = F)


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


Pho_RS <- ((Pho-min4)/(max4-min4))

Pho_sums <- exact_extract(x = Pho_RS, y = ctry_shps, fun = c('sum', 'mean', 'min', 'max', 'median', 'stdev'))

# need to organise the outputted info
Pho_sums$partner <- ctry_shps$NAME_0

colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.1", "Oilpalm", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.2", "SugarBeet", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.3", "SugarCane", colnames(Pho_sums))
colnames(Pho_sums) <- sub("P_Marine_BioDiv_Total.4", "Wheat", colnames(Pho_sums))

Pho_sums$sum.Cocoa <- NA

write.csv(Pho_sums, paste0(outdir, "Pho_summaries_allcrops.csv"), row.names = F)

#### 5. water debt ####

WAT <- stack(paste0(datadir, "/Indicator_Maps/", files[5]))

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
WAT_sums$partner <- ctry_shps$NAME_0

colnames(WAT_sums) <- sub("Water_Debt_Total.1", "Oilpalm", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_Total.2", "SugarBeet", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_Total.3", "SugarCane", colnames(WAT_sums))
colnames(WAT_sums) <- sub("Water_Debt_Total.4", "Wheat", colnames(WAT_sums))

WAT_sums$sum.Cocoa <- NA


write.csv(WAT_sums, paste0(outdir, "WAT_summaries_allcrops.csv"), row.names = F)


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
write.csv(all_sums, file = paste0(outdir, "Indicator_summaries_perCountry_tradepartners_5perc_2019.csv"))


# ##%######################################################%##
# #                                                          #
# ####                       plots                        ####
# #                                                          #
# ##%######################################################%##
# 
# #all_sums <- read.csv(file = paste0(outdir, "Indicator_summaries_perCountry_tradepartners_5perc_2019.csv"))
# 
# 
#1. facet per metric, bar per country, bar split by crop

# need to organise data into long format

plot_data <- matrix(nrow = 200, ncol = 4)

plot_data[, 1] <- c(all_sums[, 1], all_sums[, 2], all_sums[, 3], all_sums[, 4], all_sums[, 5])

plot_data[, 2] <- rep(all_sums$metric, 5)

plot_data[, 3] <- rep(all_sums$Country, 5)

plot_data[, 4] <- c(rep("Cocoa", nrow(all_sums)), rep("OilPalm", nrow(all_sums)), rep("SugarBeet", nrow(all_sums)), rep("SugarCane", nrow(all_sums)), rep("Wheat", nrow(all_sums)))

colnames(plot_data) <- c("Value", "Metric", "Country", "Crop")

plot_data <- as.data.frame(plot_data)

plot_data$Value <- as.numeric(as.character(plot_data$Value))

write.csv(plot_data, paste0(outdir, "Plot_data_country_barplots_FIG2.csv"), row.names = F)


##%######################################################%##
#                                                          #
####            bar charts for each country             ####
#                                                          #
##%######################################################%##

# creating bar charts which show the values for each metric 
# for each country. These are just for trade partners of the UK currently.
# bar chart per country to be put on a map highlighting the areas harvested. 

# loop through each country and create a plot
# 
# 
# plot_data <- read.csv( paste0(outdir, "Plot_data_country_barplots_FIG2.csv"))
# 
# plot_data$Crop <- tolower(plot_data$Crop)
# 
# plot_data$Crop[plot_data$Crop == "sugarbeet"] <- "sugar"
# plot_data$Crop[plot_data$Crop == "sugarcane"] <- "sugar"
# 
# suppliers$crop[suppliers$crop == "sugarbeet"] <- "sugar"
# suppliers$crop[suppliers$crop == "sugarcane"] <- "sugar"
# 
# suppliers$partner[suppliers$partner == "United.States.of.America"] <- "United States"
# suppliers$partner[suppliers$partner == "Cote.d.Ivoire"] <- "Côte d'Ivoire"
# suppliers$partner[suppliers$partner == "Viet.Nam"] <- "Vietnam"
# 
# countries <- suppliers$partner
#   
# # i <- countries[8]
# 
# 
# for(i in countries){
# 
#   # select the crop/crops of interest for this country
#   crops  <- suppliers[suppliers$partner == i, "crop"]
#   
#   # subset the plotting data
#   plot_data2 <- plot_data[grep(gsub("\\.", " ", i), plot_data$Country ), ]
#   plot_data2 <- plot_data2[plot_data2$Crop %in% tolower(crops), ]
#   
#   if(length(grep("GHG", plot_data2$Metric)) > 1){
#     
#     plot_data2 <- plot_data2 %>% group_by(Metric) %>%
#       summarise(Value=sum(Value), Country = unique(Country), Crop = unique(Crop))
#   }
#   
#   # use a certain colour depending on the crop of interest
#   # colour selections used above look quite nice.
#   if(length(crops) == 1){
#     
#     if(crops == "sugar") cols <- c("#8B8878")
#     if(crops == "cocoa") cols <- c("#8B4500")
#     if(crops == "oilpalm") cols <- c("#6E8B3D")
#     if(crops == "wheat") cols <- c("#EEB422")
#     
#   ggplot(plot_data2) +
#     geom_col(aes(x = Metric, y = Value), fill = cols) +  
#     xlab("") + 
#     ylab("Index") + 
#     ggtitle(gsub("\\.", " ", i)) +
#     theme_bw() + 
#     #ylim(0, 300) + 
#     scale_y_sqrt(limits = c(0,220), breaks = c(1, 10, 50, 100, 200, 300)) +
#     theme(legend.position = "none",
#           #panel.background = element_blank(), 
#           text = element_text(size = 10), 
#           line = element_line(size = 0.5),
#           legend.key.size = unit(0.6, 'cm'),
#           # to add: legend background to white
#           panel.grid = element_blank(),
#           panel.border = element_rect(size = 0.2),
#           axis.ticks = element_line(size = 0.2), 
#           aspect.ratio = 1, 
#           axis.text.x = element_text(angle = 90, vjust = 0.5),
#           panel.background = element_rect(fill = "transparent",
#                                           colour = NA_character_),
#           plot.background = element_rect(fill = "transparent",
#                                          colour = NA_character_)) 
#     # + 
#     #guides(fill=guide_legend(nrow=2,byrow=TRUE))
#   
#   
#   # save the plots in a subfolder for use in Inkscape 
#   ggsave(filename = paste0(outdir, "/barplots/Barplot_Indicators_forMap_", i, ".png"), width = 2, height = 2, units = "in")
#   
#   }else{
#     
# 
#     cols_tab <- data.frame(crop = c("sugar", "cocoa", "oilpalm", "wheat"),
#                            cols = c("#8B8878", "#8B4500", "#6E8B3D", "#EEB422"))
#     
#     
#    cols <- cols_tab[cols_tab$crop %in% crops, "cols"]
#    
#   # plot_data2$Crop <- factor(plot_data2$Crop, levels = c("sugarcane", "sugarbeet", "cocoa", "oilpalm", "wheat"))
#    plot_data2$Crop <- factor(plot_data2$Crop, levels = c("sugar", "cocoa", "oilpalm", "wheat"))
#    
#     ggplot(plot_data2) +
#       geom_col(aes(x = Metric, y = Value, fill = Crop), position = "stack") + 
#       scale_fill_manual(values = cols) +
#       xlab("") + 
#       ylab("Index") + 
#       ggtitle(gsub("\\.", " ", i)) +
#       theme_bw() + 
#       scale_y_sqrt(limits = c(0,220), breaks = c(1, 10, 50, 100, 200, 300)) +
#       theme(legend.position = "none",
#             #panel.background = element_blank(), 
#             text = element_text(size = 10), 
#             line = element_line(size = 0.5),
#             legend.key.size = unit(0.6, 'cm'),
#             panel.grid = element_blank(),
#             panel.border = element_rect(size = 0.2),
#             axis.ticks = element_line(size = 0.2), 
#             aspect.ratio = 1, 
#             axis.text.x = element_text(angle = 90, vjust = 0.5),
#             panel.background = element_rect(fill = "transparent",
#                                             colour = NA_character_),
#             plot.background = element_rect(fill = "transparent",
#                                            colour = NA_character_)) 
# 
#     
#     
#     # save the plots in a subfolder for use in Inkscape 
#     ggsave(filename = paste0(outdir, "/barplots/Barplot_Indicators_forMap_", i, ".png"), width = 2, height = 2, units = "in")
#     
#     
#   }
# 
# }
# 

# notes:


##%######################################################%##
#                                                          #
####                 stacked barchart                   ####
#                                                          #
##%######################################################%##


plot_data <- read.csv( paste0(outdir, "Plot_data_country_barplots_FIG2.csv"))

plot_data$Crop <- tolower(plot_data$Crop)

plot_data$Crop[plot_data$Crop == "sugarbeet"] <- "sugar"
plot_data$Crop[plot_data$Crop == "sugarcane"] <- "sugar"

suppliers$crop[suppliers$crop == "sugarbeet"] <- "sugar"
suppliers$crop[suppliers$crop == "sugarcane"] <- "sugar"

#suppliers$partner[suppliers$partner == "United.States.of.America"] <- "United States"
#suppliers$partner[suppliers$partner == "Cote.d.Ivoire"] <- "Côte d'Ivoire"
#suppliers$partner[suppliers$partner == "Viet.Nam"] <- "Vietnam"
#suppliers$partner[suppliers$partner == "United.Kingdom"] <- "United Kingdom"
#suppliers$partner[suppliers$partner == "El.Salvador"] <- "El Salvador"

plot_data2 <- left_join(suppliers, plot_data, by=c('partner'='Country', 'crop'='Crop'))

plot_data2$Country_crop <- paste(plot_data2$partner, "-\n",  plot_data2$crop)

# to order the bars by height
plot_data2$Country_crop <- factor(plot_data2$Country_crop, levels = rev(c("United Kingdom -\n wheat", "Malaysia -\n oilpalm", "Indonesia -\n oilpalm", "United Kingdom -\n sugar", "Côte d'Ivoire -\n cocoa",  "Ghana -\n cocoa", "Nigeria -\n cocoa", "Cameroon -\n cocoa")))

ggplot(plot_data2, aes(fill=Metric, y=Country_crop, x=Value)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#8B668B",   "#838B8B", "#CD8C95", "#4F94CD", "#8B0000")) + 
  xlab("Total standardised impact across indicators") + 
  theme_bw() +
  theme(axis.title.y = element_blank())

ggsave(filename = paste0(outdir, "Figure2_stackedbarplot_suppliers_impact.pdf"))
