##%######################################################%##
#                                                          #
#                     5. Hotspot maps                   ####
#                     using Marks Data                     #
#                                                          #
#                                                          #
##%######################################################%##
# started by Feli Pamatat, 15/07/2021

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



remove(list = ls(all.names = TRUE))
Sys.setenv(LANG = "en")


#Task 1 - set dir ####

setwd("/Users/Feli/Documents/Cookie Project")

#Mark's data in different dir (see Mark's data google drive)
MMdir <- "Data"

# Task 1a - Hotspot map - top 1% of grids####

hotspotopt1 <- function(raster, nameImpact){
  #raster to dataframe for easier handling
  raster_pts <- rasterToPoints(raster, spatial = TRUE)
  raster_df  <- data.frame(raster_pts)
  rm(raster_pts)
  
  #remove all columns from df 
  rasterb <- raster_df
  rasterb <- rasterb[,-(2:4)]
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
  quant <- quantile(rasterb, c(0, .99, 1)) 
  g <- cut(rasterb, quant, include.lowest = TRUE, lab = c("lower", "hi"))
  splitraster<-split(raster_df, g)
  
  #only select areas with the (5%) highest GHG Impact value
  splitraster <- as.data.frame(splitraster$hi)
  
  #plot to show areas
  clipExt <- extent(-190, 190, -95, 95)
  
  colnames(splitraster)[1] <- "colnameRaster"
  
  plotoption1<- ggplot(splitraster, aes(x = x, y = y))+
    geom_tile(aes(fill = colnameRaster)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,60), 
                       labels = paste0(seq(-180,182,60), "°")) +
    scale_fill_gradientn(colours = "red",
                         na.value = "transparent", name = nameImpact) +
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="",
         y="",
         title = "Top 1%",
         subtitle = nameImpact)+
    theme(plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
  print(plotoption1)
}

#draw maps for all Impact factors and avail. crops
hotspotopt1(GHG_Wheat, "GHG Emissions - Wheat")
hotspotopt1(LD_Wheat, "Land diversity - Wheat")
hotspotopt1(N_Wheat, "N fertilizer - Wheat")
hotspotopt1(P_Wheat, "P fertilizer - Wheat")


hotspotopt1(GHG_Sugar, "GHG Emissions - Sugar")
hotspotopt1(LD_Sugar, "Land diversity - Sugar")
hotspotopt1(N_Sugar, "N fertilizer - Sugar")
hotspotopt1(P_Sugar, "P fertilizer - Sugar")

hotspotopt1(GHG_Oilpalm, "GHG Emissions - Oilpalm")
hotspotopt1(LD_Oilpalm, "Land diversity - Oilpalm")
hotspotopt1(N_Oilpalm, "N fertilizer - Oilpalm")
hotspotopt1(P_Oilpalm, "P fertilizer - Oilpalm")

hotspotopt1(GHG_Cocoa, "GHG Emissions - Cocoa")
hotspotopt1(LD_Cocoa, "Land diversity - Cocoa")



#Task 1b - Hotspot map Water Dept ####
#everything higher than 1 is consider unsustainable 


waterhotspot <- function(raster, nameCrop){
  raster_pts <- rasterToPoints(raster, spatial = TRUE)
  raster_df  <- data.frame(raster_pts)
  rm(raster_pts)
  
  colnames(raster_df)[1] <- "colnameRaster"
  
  
  WaterDeptmap<-ggplot(raster_df[raster_df$colnameRaster>1,], aes(x = x, y = y))+
    geom_tile(aes(fill = colnameRaster)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,60), 
                       labels = paste0(seq(-180,182,60), "°")) +
    scale_fill_gradientn(colours = "red",
                         na.value = "transparent", name = "Water Dept") +
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="",
         y="",
         title = "Areas with an unsustainable Water use",
         subtitle = nameCrop)+
    theme(plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
  print(WaterDeptmap)
}

#WD available for 3 of the 4 crops:
waterhotspot(WD_Wheat, "Wheat")
waterhotspot(WD_Sugar, "Sugar")
waterhotspot(WD_Oilpalm, "OilPalm")





