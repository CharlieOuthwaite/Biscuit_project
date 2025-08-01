##%######################################################%##
#                                                          #
####                Crop Impact Mapping                 ####
####             Highest Impact Areas by Crop           ####
#                                                          #
##%######################################################%##

# Author: Feli Pamatat, Last edited: 29.07.24

# This script generates maps that outline the highest impact areas 
# created by different crops on global species richness. Follow the 
# instructions below to prepare the necessary data and run the analysis.

# Steps to follow if the BiMap Folder is empty
# 1. Execute the script "2_Global_Map_Species_Richness.R" to create 
#    the required functions for the overview graphic and make sure it is saved
#    under "BiMap".



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
if(!require(cowplot)) {
  install.packages("cowplot")
  require(cowplot)
}
if(!require(ragg)) {
  install.packages("ragg")
  require(ragg)
}
if (!requireNamespace("fs", quietly = TRUE)) {
  install.packages("fs")
}
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}



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
if(!require(viridis)) {
  install.packages("viridis")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if(!require(classInt)) {
  install.packages("classInt")
  require(classInt)
}
if(!require(dismo)) {
  install.packages("dismo")
  require(dismo)
}
if(!require(XML)) {
  install.packages("XML")
  require(XML)
}
if(!require(maps)) {
  install.packages("maps")
  require(maps)
}
if(!require(sp)) {
  install.packages("sp")
  require(sp)
}
if(!require(magrittr)) {
  install.packages("magrittr")
  require(magrittr)
}
if(!require(classInt)) {
  install.packages("classInt")
  require(classInt)
}
if(!require(cowplot)) {
  install.packages("cowplot")
  require(cowplot)
}
if(!require(patchwork)) {
  install.packages("patchwork")
  require(patchwork)
}
if(!require(ggpubr)) {
  install.packages("ggpubr")
  require(ggpubr)
}
if(!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if(!require(sf)) {
  install.packages("sf")
  require(sf)
}

remove(list = ls(all.names = TRUE))
Sys.setenv(LANG = "en")

#Task 1 - set dir and load data ####

setwd("/GitHub")

#DataMaps in BaseMaps Folder
MMdir <- "Data"

#0.1 Get the Bivariate Map ready####
#read in Bivariate Map functions to get a BivariateMap indepenently from the other script
#Function for handling color matrices in bivariate maps.
colmat <- readRDS("Data/BiMap/colmat.rds")

#Function for generating bivariate maps.
bivariate.map <- readRDS("Data/BiMap/bivariate.map.rds")

#Function for creating the final bivariate map output needed for the overview document
makeBivmap <- readRDS("Data/BiMap/makeBivmap.rds")

#for species richness maps | download Adriennes rds files Google Drive
SRdir <- "Species_Richness_Maps"

#Load all data required for making the Bivariate Map
load("Data/BiMap/BiMaps.RData")

#0.2 Load in the raster stack####
files <- list.files(paste0(MMdir, "/BaseMaps"), pattern = "_Total.tif", full.names = TRUE)

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

## Task 1aa - normalize raster#### 
#function for every impact apart from WD 
raster01 <- function(r){
  values(r)[values(r) == 0] <- NA
  #get min and max value
  minmax_r <- range(values(r), na.rm = T)
  
  #rescale
  return((r-minmax_r[1])/(diff(minmax_r)))
}


#but for WD (since everything over 1 is unsustainable remove values below 1 and norm the rest)
rasterWD <- function(r){
  values(r)[values(r) <=1 ] <- NA
  #get min and max value
  minmax_r <- range(values(r), na.rm = T)
  
  #rescale
  return((r-minmax_r[1])/(diff(minmax_r)))
}


#transform GHG rasters to a dist between 0 and 1
GHG_Cocoa_norm <- raster01(GHG_Cocoa)
GHG_Wheat_norm <- raster01(GHG_Wheat)
GHG_Sugar_norm <- raster01(GHG_Sugar)
GHG_Oilpalm_norm <- raster01(GHG_Oilpalm)

#transform WD rasters to a dist between 0 and 1 but only for areas with a WD of over 1
WD_Wheat_norm <- rasterWD(WD_Wheat)
WD_Sugar_norm <- rasterWD(WD_Sugar)
WD_Oilpalm_norm <- rasterWD(WD_Oilpalm)

#transform LD rasters to a dist between 0 and 1
LD_Cocoa_norm <- raster01(LD_Cocoa)
LD_Wheat_norm <- raster01(LD_Wheat)
LD_Sugar_norm <- raster01(LD_Sugar)
LD_Oilpalm_norm <- raster01(LD_Oilpalm)


#transform N rasters to a dist between 0 and 1
N_Wheat_norm <- raster01(N_Wheat)
N_Sugar_norm <- raster01(N_Sugar)
N_Oilpalm_norm <- raster01(N_Oilpalm)

#transform P rasters to a dist between 0 and 1
P_Wheat_norm <- raster01(P_Wheat)
P_Sugar_norm <- raster01(P_Sugar)
P_Oilpalm_norm <- raster01(P_Oilpalm)

###### Function####
#Wheat
raster1 <- GHG_Wheat_norm
raster2 <- LD_Wheat_norm
raster3 <- N_Wheat_norm
raster4 <- P_Wheat_norm
raster5 <- WD_Wheat_norm

#Sugar
raster1 <- GHG_Sugar_norm
raster2 <- LD_Sugar_norm
raster3 <- N_Sugar_norm
raster4 <- P_Sugar_norm
raster5 <- WD_Sugar_norm

#Oil
raster1 <- GHG_Oilpalm_norm
raster2 <- LD_Oilpalm_norm
raster3 <- N_Oilpalm_norm
raster4 <- P_Oilpalm_norm
raster5 <- WD_Oilpalm_norm

#Cocoa
raster1 <- GHG_Cocoa_norm
raster2 <- LD_Cocoa_norm

allImpacts <- function(raster1, raster2, raster3, raster4, raster5, CropName, clipExt, percentage, PercentDes, fig){
  #raster to data frame for easier handling
  raster1_pts <- rasterToPoints(raster1, spatial = TRUE)
  raster1_df  <- data.frame(raster1_pts)
  rm(raster1_pts)
  
  #remove all columns from df 
  raster1b <- raster1_df
  raster1b <- raster1b[,-(2:4)]
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
  quant <- quantile(raster1b, c(0, percentage, 1)) 
  g <- cut(raster1b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
  splitraster1<-split(raster1_df, g)
  
  #only select areas with the (5%) highest GHG Impact value
  splitraster1 <- as.data.frame(splitraster1$hi)
  
  #do the same for the second raster
  raster2_pts <- rasterToPoints(raster2, spatial = TRUE)
  raster2_df  <- data.frame(raster2_pts)
  rm(raster2_pts)
  
  #remove all columns from df 
  raster2b <- raster2_df
  raster2b <- raster2b[,-(2:4)]
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.95 - mid, 0.95-1 - high
  quant <- quantile(raster2b, c(0, percentage, 1)) 
  g <- cut(raster2b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
  splitraster2<-split(raster2_df, g)
  
  #only select areas with the (5%) highest GHG Impact value
  splitraster2 <- as.data.frame(splitraster2$hi)
  
  #do the same for the 3rd raster
  raster3_pts <- rasterToPoints(raster3, spatial = TRUE)
  raster3_df  <- data.frame(raster3_pts)
  rm(raster3_pts)
  
  #remove all columns from df 
  raster3b <- raster3_df
  raster3b <- raster3b[,-(2:4)]
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
  quant <- quantile(raster3b, c(0, percentage, 1)) 
  g <- cut(raster3b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
  splitraster3<-split(raster3_df, g)
  
  #only select areas with the (5%) highest GHG Impact value
  splitraster3 <- as.data.frame(splitraster3$hi)
  
  
  #do the same for the 4th raster
  raster4_pts <- rasterToPoints(raster4, spatial = TRUE)
  raster4_df  <- data.frame(raster4_pts)
  rm(raster4_pts)
  
  #remove all columns from df 
  raster4b <- raster4_df
  raster4b <- raster4b[,-(2:4)]
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
  quant <- quantile(raster4b, c(0, percentage, 1)) 
  g <- cut(raster4b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
  splitraster4<-split(raster4_df, g)
  
  #only select areas with the (5%) highest GHG Impact value
  splitraster4 <- as.data.frame(splitraster4$hi)
  
  
  #WD
  raster5_pts <- rasterToPoints(raster5, spatial = TRUE)
  raster5_df  <- data.frame(raster5_pts)
  rm(raster5_pts)
  
  #remove all columns from df 
  raster5b <- raster5_df
  raster5b <- raster5b[,-(2:4)]
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
  quant <- quantile(raster5b, c(0, percentage, 1)) 
  g <- cut(raster5b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
  splitraster5<-split(raster5_df, g)
  
  #only select areas with the (5%) highest GHG Impact value
  splitraster5 <- as.data.frame(splitraster5$hi)
  
  

  
  # areas of overlap
  colnames(splitraster1)[1] <- "colnameraster1"
  colnames(splitraster2)[1] <- "colnameraster2"
  colnames(splitraster3)[1] <- "colnameraster3"
  colnames(splitraster4)[1] <- "colnameraster4"
  colnames(splitraster5)[1] <- "colnameRaster5"
  
  #remove optional
  splitraster1 <- splitraster1[,-4]
  splitraster2 <- splitraster2[,-4]
  splitraster3 <- splitraster3[,-4]
  splitraster4 <- splitraster4[,-4]
  splitraster5   <- splitraster5[,-4]
  
  #take all impacts not just the top 5%
  # areas of overlap
  colnames(raster1_df)[1] <- "colnameraster1"
  colnames(raster2_df)[1] <- "colnameraster2"
  colnames(raster3_df)[1] <- "colnameraster3"
  colnames(raster4_df)[1] <- "colnameraster4"
  colnames(raster5_df)[1] <- "colnameraster5"
  
  #remove optional
  raster1_df <- raster1_df[,-4]
  raster2_df <- raster2_df[,-4]
  raster3_df <- raster3_df[,-4]
  raster4_df <- raster4_df[,-4]
  raster5_df <- raster5_df[,-4]
  
  # merge all rasters -> for all layers with value of 0 --> NA 
  #--> make another column with the highest normalized values 
  newRaster <- merge(raster1_df, raster2_df, by = c("x", "y"), all = TRUE)
  newRaster <- merge(newRaster, raster3_df, by = c("x", "y"), all = TRUE)
  newRaster <- merge(newRaster, raster4_df, by = c("x", "y"), all = TRUE)
  newRaster <- merge(newRaster, raster5_df, by = c("x", "y"), all = TRUE)
  
  #find max of each square
  newRaster$max       <-apply(X=newRaster[,3:7],1, FUN=max, na.rm = T )
  
  #for Cocoa only
  #newRaster$max       <-apply(X=newRaster[,3:4],1, FUN=max, na.rm = T )
  
  #give impact names to max values 
  newRaster$max_names <- colnames(newRaster[,3:7])[apply(newRaster[,3:7],1,which.max)]
  
  #for Cocoa only
  #newRaster$max_names <- colnames(newRaster[,3:4])[apply(newRaster[,3:4],1,which.max)]
  
  #add total 
  newRaster$total       <-apply(X=newRaster[,3:7],1, FUN=sum, na.rm = T )
  
  #for cocoa only
  #newRaster$total       <-apply(X=newRaster[,3:4],1, FUN=sum, na.rm = T )
  
    #take top ...% 
  #change big raster to smaller subset
  HighImpactRaster <- newRaster[, c(1:2,10)]
  
  #for cocoa only
  #HighImpactRaster <- newRaster[, c(1:2,7)]
  
  #anything below the impact of 0.01 gets removed
  HighImpactRaster$total[HighImpactRaster$total<0.01]=NA
  
  #remove all columns from df 
  rasterb <- HighImpactRaster
  rasterb <- rasterb[, -(1:2)]
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
  quant <- quantile(rasterb, c(0, percentage, 1), na.rm = TRUE) 
  g <- cut(rasterb, quant, include.lowest = TRUE, lab = c("lower", "hi"))
  totalHigh<-split(HighImpactRaster, g)
  
  #only select areas with the (5%) highest GHG Impact value
  totalHigh <- as.data.frame(totalHigh$hi)
  
  # PercentDes <- "Top 5%"
  # CropName <- "Wheat"
  #make map with all impact factors
  Allimpactmap<-ggplot()+
    geom_tile(data= splitraster1, aes(x=x, y=y, fill = colnameraster1), fill= "blue", alpha=0.7)+
    geom_tile(data= splitraster2, aes(x=x, y=y, fill = colnameraster2),fill= "red", alpha=0.6)+ 
    geom_tile(data= splitraster3, aes(x=x, y=y, fill = colnameraster3),fill= "darkturquoise", alpha=0.5)+ 
    geom_tile(data= splitraster4, aes(x=x, y=y, fill = colnameraster4),fill= "yellow", alpha=0.4)+ 
    geom_tile(data= raster5_df[raster5_df$colnameRaster5>1,], aes(x = x, y = y,fill = colnameRaster5),fill= "forestgreen", alpha=0.3)+
    # scale_y_continuous(breaks = seq(-90, 90, by = 30),
    #                  labels = paste0(seq(-90, 90, 30), "°")) +
    # scale_x_continuous(breaks = seq(-180,182,60), 
    #                    labels = paste0(seq(-180,182,60), "°")) +
    theme(text = element_text(size = 20, colour = "#444444"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          rect = element_blank())+
    borders(colour = "#444444", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4])  #draws world map (using country boarders) on top of data
    #labs(title = PercentDes,
    #     subtitle = CropName)
  
  
  print(Allimpactmap)
  
  
  
  #map with highest impacts for each cell
  OnlyMaxMap<- ggplot()+
    geom_tile(data= newRaster, aes(x=x, y=y, fill = max_names)) +
    scale_fill_manual(values = c("#d73027", "#fdae61", "#abd9e9", '#006d2c',"#54278f"), labels=c('GHG', 'LND', 'Nit','Pho', 'WAT'))+
    #scale_fill_brewer(palette = "Dark2", labels=c('GHG', 'LD', 'P','N', 'WD'))+
    # scale_y_continuous(breaks = seq(-90, 90, by = 30), 
    #                    labels = paste0(seq(-90, 90, 30), "?")) +
    # scale_x_continuous(breaks = seq(-180,182,60), 
    #                    labels = paste0(seq(-180,182,60), "?")) +
    guides(fill= guide_legend(title = ""))+
    theme(text = element_text(size = 17, colour = "#444444"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          legend.background = element_rect(fill = "white"),
          legend.position = c(0.12,  0.35),
          rect = element_blank())+
    borders(colour = "#444444", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) #draws world map (using country boarders) on top of data
    
  
  OnlyMaxMap<- ggplot()+
    geom_tile(data= newRaster, aes(x=x, y=y, fill = max_names)) +
    scale_fill_manual(values = c("#d73027", "#fdae61"), labels=c('GHG', 'LND'))+
    #scale_fill_brewer(palette = "Dark2", labels=c('GHG', 'LD', 'P','N', 'WD'))+
    # scale_y_continuous(breaks = seq(-90, 90, by = 30), 
    #                    labels = paste0(seq(-90, 90, 30), "?")) +
    # scale_x_continuous(breaks = seq(-180,182,60), 
    #                    labels = paste0(seq(-180,182,60), "?")) +
    guides(fill= guide_legend(title = ""))+
    theme(text = element_text(size = 17, colour = "#444444"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          legend.background = element_rect(fill = "white"),
          legend.position = c(0.12,  0.35),
          rect = element_blank())+
    borders(colour = "#444444", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) #draws world map (using country boarders) on top of data
  
  
  print(OnlyMaxMap)
  
  
  #made a map of top ...% of added impacts
  TopImpactMap<-ggplot()+
    geom_tile(data=HighImpactRaster, aes(x=x, y=y, fill = total))+
    scale_fill_gradient(low= '#F64A8A', high = '#DE3163', na.value = "transparent", name = "Total Impact")+
    #scale_fill_distiller(palette = "Spectral", na.value = "transparent", name = "Total Impact")+
    scale_y_continuous(breaks = seq(-90, 90, by = 30),
                       labels = paste0(seq(-90, 90, 30), "?")) +
    scale_x_continuous(breaks = seq(-180,182,60),
                       labels = paste0(seq(-180,182,60), "?")) +
    # theme_classic() +
    theme(text = element_text(size = 30, colour = "#444444"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          legend.position = "none",
          rect = element_blank())+
    borders(colour = "#444444", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4])  #draws world map (using country boarders) on top of data
  
  
  print(TopImpactMap)
  
  
  #maybe change this map to 100% and a gradient
  
  #harvested area
  
 #  
 #  #emmissions
 #  GHG_pts <- rasterToPoints(raster1, spatial = TRUE)
 #  GHG_df  <- data.frame(GHG_pts)
 #  rm(GHG_pts)
 #  
 #  #remove all columns from df 
 #  GHG_b <- GHG_df
 #  GHG_b <- GHG_b[,-(2:4)]
 #  
 #  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
 #  quant <- quantile(GHG_b, c(0, percentage, 1)) 
 #  g <- cut(GHG_b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
 #  GHG_df_split<-split(GHG_df, g)
 #  
 #  #only select areas with the (5%) highest GHG Impact value
 #  GHG_df_split <- as.data.frame(GHG_df_split$hi)
 #  
 #  #plot to show areas
 #  clipExt <- extent(-190, 190, -95, 95)
 #  
 #  colnames(GHG_df_split)[1] <- "colnameRaster"
 #  
 #  
 #    GHG_map<- ggplot(GHG_df_split, aes(x = x, y = y))+
 #    geom_tile(aes(fill = colnameRaster))+
 #    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
 #                       labels = paste0(seq(-90, 90, 30), "?")) +
 #    scale_x_continuous(breaks = seq(-180,182,60), 
 #                       labels = paste0(seq(-180,182,60), "?")) +
 #    scale_fill_gradient(low= '#fc9272', high = '#d7191c', na.value = "transparent", name = "GHG Emissions")+
 #    #theme_classic() +
 #    theme(text = element_text(size = 20, colour = "black"),
 #          axis.title.x=element_blank(),
 #          axis.text.x=element_blank(),
 #          axis.ticks=element_blank(),
 #          axis.title.y=element_blank(),
 #          axis.text.y=element_blank(),
 #          rect = element_blank())+
 #    borders(colour = "black", size = 0.5) +
 #    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
 #    labs(subtitle = "GHG Emissions")
 #    
 #  #print(GHG_map)
 #  
 #  
 #  
 #  #LD
 #  LD_pts <- rasterToPoints(raster2, spatial = TRUE)
 #  LD_df  <- data.frame(LD_pts)
 #  rm(LD_pts)
 #  
 #  #remove all columns from df 
 #  LD_b <- LD_df
 #  LD_b <- LD_b[,-(2:4)]
 #  
 #  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
 #  quant <- quantile(LD_b, c(0, percentage, 1)) 
 #  g <- cut(LD_b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
 #  LD_df_split<-split(LD_df, g)
 #  
 #  #only select areas with the (5%) highest LD Impact value
 #  LD_df_split <- as.data.frame(LD_df_split$hi)
 #  
 #  #plot to show areas
 #  clipExt <- extent(-190, 190, -95, 95)
 #  
 #  colnames(LD_df_split)[1] <- "colnameRaster"
 #  
 #  
 #  LD_map<- ggplot(LD_df_split, aes(x = x, y = y))+
 #    geom_tile(aes(fill = colnameRaster))+
 #    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
 #                       labels = paste0(seq(-90, 90, 30), "?")) +
 #    scale_x_continuous(breaks = seq(-180,182,60), 
 #                       labels = paste0(seq(-180,182,60), "?")) +
 #    scale_fill_gradient(low= '#fec44f', high = '#cc4c02', na.value = "transparent", name = "LD Impact")+
 #    #theme_classic() +
 #    theme(text = element_text(size = 20, colour = "black"),
 #          axis.title.x=element_blank(),
 #          axis.text.x=element_blank(),
 #          axis.ticks=element_blank(),
 #          axis.title.y=element_blank(),
 #          axis.text.y=element_blank(),
 #          rect = element_blank())+
 #    borders(colour = "black", size = 0.5) +
 #    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
 #    labs(subtitle = "LD Impact")
 #  
 #  
 #  #print(LD_map)
 #  
 #  #P
 #  P_pts <- rasterToPoints(raster3, spatial = TRUE)
 #  P_df  <- data.frame(P_pts)
 #  rm(P_pts)
 #  
 #  #remove all columns from df 
 #  P_b <- P_df
 #  P_b <- P_b[,-(2:4)]
 #  
 #  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
 #  quant <- quantile(P_b, c(0, percentage, 1)) 
 #  g <- cut(P_b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
 #  P_df_split<-split(P_df, g)
 #  
 #  #only select areas with the (5%) highest P Impact value
 #  P_df_split <- as.data.frame(P_df_split$hi)
 #  
 #  #plot to show areas
 #  clipExt <- extent(-190, 190, -95, 95)
 #  
 #  colnames(P_df_split)[1] <- "colnameRaster"
 #  
 #  P_map<- ggplot(P_df_split, aes(x = x, y = y))+
 #    geom_tile(aes(fill = colnameRaster))+
 #    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
 #                       labels = paste0(seq(-90, 90, 30), "?")) +
 #    scale_x_continuous(breaks = seq(-180,182,60), 
 #                       labels = paste0(seq(-180,182,60), "?")) +
 #    scale_fill_gradient(low= '#41ab5d', high = '#006d2c', na.value = "transparent", name = "P Impact")+
 #    #theme_classic() +
 #    theme(text = element_text(size = 20, colour = "black"),
 #          axis.title.x=element_blank(),
 #          axis.text.x=element_blank(),
 #          axis.ticks=element_blank(),
 #          axis.title.y=element_blank(),
 #          axis.text.y=element_blank(),
 #          rect = element_blank())+
 #    borders(colour = "black", size = 0.5) +
 #    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
 #    labs(subtitle = "P Impact")
 #    
 #  
 # # print(P_map)
 #  
 #  #N
 #  N_pts <- rasterToPoints(raster4, spatial = TRUE)
 #  N_df  <- data.frame(N_pts)
 #  rm(N_pts)
 #  
 #  #remove all columns from df 
 #  N_b <- N_df
 #  N_b <- N_b[,-(2:4)]
 #  
 #  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
 #  quant <- quantile(N_b, c(0, percentage, 1)) 
 #  g <- cut(N_b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
 #  N_df_split<-split(N_df, g)
 #  
 #  #only select areas with the (5%) highest N Impact value
 #  N_df_split <- as.data.frame(N_df_split$hi)
 #  
 #  #plot to show areas
 #  clipExt <- extent(-190, 190, -95, 95)
 #  
 #  colnames(N_df_split)[1] <- "colnameRaster"
 #  
 #  
 #  
 #  N_map<- ggplot(N_df_split, aes(x = x, y = y))+
 #    geom_tile(aes(fill = colnameRaster))+
 #    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
 #                       labels = paste0(seq(-90, 90, 30), "?")) +
 #    scale_x_continuous(breaks = seq(-180,182,60), 
 #                       labels = paste0(seq(-180,182,60), "?")) +
 #    scale_fill_gradient(low= '#4292c6', high = '#08306b', na.value = "transparent", name = "N Impact")+
 #    #theme_classic() +
 #    theme(text = element_text(size = 20, colour = "black"),
 #          axis.title.x=element_blank(),
 #          axis.text.x=element_blank(),
 #          axis.ticks=element_blank(),
 #          axis.title.y=element_blank(),
 #          axis.text.y=element_blank(),
 #          rect = element_blank())+
 #    borders(colour = "black", size = 0.5) +
 #    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
 #    labs(subtitle = "N Impact")
 #  #
 # # print(N_map)
 #  
 #  #WD
 #  WD_pts <- rasterToPoints(raster5, spatial = TRUE)
 #  WD_df  <- data.frame(WD_pts)
 #  rm(WD_pts)
 #  
 #  #remove all columns from df 
 #  WD_b <- WD_df
 #  WD_b <- WD_b[,-(2:4)]
 #  
 #  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
 #  quant <- quantile(WD_b, c(0, percentage, 1)) 
 #  g <- cut(WD_b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
 #  WD_df_split<-split(WD_df, g)
 #  
 #  #only select areas with the (5%) highest WD Impact value
 #  WD_df_split <- as.data.frame(WD_df_split$hi)
 #  
 #  #plot to show areas
 #  clipExt <- extent(-190, 190, -95, 95)
 #  
 #  colnames(WD_df_split)[1] <- "colnameRaster"
 #  
 #  
 #  WD_map<- ggplot(WD_df_split, aes(x = x, y = y))+
 #    geom_tile(aes(fill = colnameRaster))+
 #    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
 #                       labels = paste0(seq(-90, 90, 30), "?")) +
 #    scale_x_continuous(breaks = seq(-180,182,60), 
 #                       labels = paste0(seq(-180,182,60), "?")) +
 #    scale_fill_gradient(low= '#807dba', high = '#54278f', na.value = "transparent", name = "WD Impact")+
 #    #theme_classic() +
 #    theme(text = element_text(size = 20, colour = "black"),
 #          axis.title.x=element_blank(),
 #          axis.text.x=element_blank(),
 #          axis.ticks=element_blank(),
 #          axis.title.y=element_blank(),
 #          axis.text.y=element_blank(),
 #          rect = element_blank())+
 #    borders(colour = "black", size = 0.5) +
 #    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
 #    labs(subtitle = "WD Impact")
  
  
  #print(WD_map)
  
  # (maprasterY2+ GHG_map)| #harvested map + GHG map
  #   (P_map + N_map) |
  #   (LD_map + WD_map)|
  #   (fig + OnlyMaxMap)|
  #   TopImpactMap
  # 
  # wrap_plots(maprasterY2, GHG_map, P_map, N_map, LD_map , WD_map, fig,  OnlyMaxMap, TopImpactMap)
  # 
  # fig / (OnlyMaxMap | TopImpactMap)
    
layout <- "
AB
CD
EF
GH
II
"
  # maprasterY2 + GHG_map + P_map + N_map + LD_map + WD_map +  OnlyMaxMap + TopImpactMap + fig +
  # plot_layout(design = layout)
  
  #for cocoa only
#   layoutcocoa <- "
# A#
# BC
# GH
# II
# "
#  maprasterY2 + GHG_map + LD_map +  OnlyMaxMap + TopImpactMap + fig +
#    plot_layout(design = layoutcocoa)
  
  
  #for simple graph
  layoutsimple <- "
AB
CC
"
  SimpleMap<-
    OnlyMaxMap + TopImpactMap + fig +
    plot_layout(design = layoutsimple)
  
  library(ragg)
  library(palmerpenguins)
  
  #view single plots
  maprasterY2
  
  
  #saving prep####
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  folder_name <- paste0("Hotspot_", date_str)
  
  # Create the full path for the new directory
  figures_path <- fs::path("Figures", folder_name)
  
  # Create the directory if it doesn't exist
  fs::dir_create(figures_path)

  ggsave(filename = paste0("Test",
                           format(Sys.time(), "%d-%m-%Y_%H-%M"),
                           ".png"),SimpleMap, width = 20, height = 11, dpi = 150, units = "in", device='png')
  
  
  return(SimpleMap)
}


#Prepare & Select for Function####

clipExt     <- extent(-190, 190, -95, 95)

#Extent can be changed to only show certain regions
#clipExt     <- extent(-120, 0, -10, 60)
#clipExt <- c(-13.69139, 1.764168, 49.86542, 61.52708)


percentage  <- .9

#Cocoa
BiCoco <- makeBivmap(SR_all,SPAM_Coco,3, "Species richness", "Harvested Area", "Combined", "Cocoa", "Species Richness and Total Harvested Area of Cocoa",mapdir, AllArea = 0, SaveMap = 1)
allImpacts(raster1C, raster2C, CropName = "Cocoa",  clipExt = clipExt, percentage = percentage,PercentDes = "Top 10%", BiCoco)


#Oilpalm
BiOil <- makeBivmap(SR_all,SPAM_Oilp,3, "Species richness", "Harvested Area", "Combined", "Oilpalm", "Species Richness and Total Harvested Area of Oilpalm",mapdir, AllArea = 0, SaveMap = 1)
allImpacts(raster1O, raster2O, raster3O, raster4O, raster5O, "Oilpalm", clipExt, percentage, "Top 10%", BiOil)

#Wheat
BiWheat <- makeBivmap(SR_all,SPAM_Whea,3, "Species richness", "Harvested Area", "Combined", "Wheat", "Species Richness and Total Harvested Area of Wheat",mapdir, AllArea = 0, SaveMap = 1)
allImpacts(raster1W, raster2W, raster3W, raster4W, raster5W, "Wheat", clipExt, percentage, "Top 10%", BiWheat)

#Sugar
BiSugar <- makeBivmap(SR_all,SPAM_Suga,3, "Species richness", "Harvested Area", "Combined", "Sugar", "Species Richness and Total Harvested Area of Sugarcane",mapdir, AllArea = 0, SaveMap = 1)
allImpacts(raster1S, raster2S, raster3S, raster4S, raster5S, "Sugar", clipExt, percentage, "Top 10%", BiSugar)




