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


#Task 1 - set dir and load data ####

setwd("/Users/Feli/Documents/Cookie Project")

#Mark's data in different dir (see Mark's data google drive)
MMdir <- "Data"

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
  quant <- quantile(rasterb, c(0, .95, 1)) 
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
                         na.value = "transparent", name = nameImpact, guide = FALSE) +
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="Longitude",
         y="Latitude",
         title = "Top 5%",
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


#Task 2 - Overlay Impact factors####
#raster1    - blue
#raster2    - red
#raster3    - darkturquoise
#raster4    - yellow
#raster5    - forestgreen (Water dept)
#CropName   - "Name of Crop"
#clipExt    - extent of map 
#percentage - .95 - upper 5% of data; .99 - upper 1%
#PercentDes - "Top 5%" or "Top 1%"

allImpacts <- function(raster1, raster2, raster3, raster4, raster5, CropName, clipExt, percentage, PercentDes){
  #raster to dataframe for easier handling
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
  
  #devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
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
  
  
  
  raster5_pts <- rasterToPoints(raster5, spatial = TRUE)
  raster5_df  <- data.frame(raster5_pts)
  rm(raster5_pts)
  

  
  
  
  colnames(splitraster1)[1] <- "colnameraster1"
  colnames(splitraster2)[1] <- "colnameraster2"
  colnames(splitraster3)[1] <- "colnameraster3"
  colnames(splitraster4)[1] <- "colnameraster4"
  colnames(raster5_df)[1] <- "colnameRaster5"
  
  
#make map with all impact factors
Allimpactmap<-ggplot()+
  geom_tile(data= splitraster1, aes(x=x, y=y, fill = colnameraster1), fill= "blue", alpha=0.7)+
  geom_tile(data= splitraster2, aes(x=x, y=y, fill = colnameraster2),fill= "red", alpha=0.6)+ 
  geom_tile(data= splitraster3, aes(x=x, y=y, fill = colnameraster3),fill= "darkturquoise", alpha=0.5)+ 
  geom_tile(data= splitraster4, aes(x=x, y=y, fill = colnameraster4),fill= "yellow", alpha=0.4)+ 
  geom_tile(data= raster5_df[raster5_df$colnameRaster5>1,], aes(x = x, y = y,fill = colnameRaster5),fill= "forestgreen", alpha=0.3)+
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,60), 
                       labels = paste0(seq(-180,182,60), "°")) +
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="Longitude",
         y="Latitude",
         title = PercentDes,
         subtitle = CropName)+
    theme(plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
print(Allimpactmap)
}

#plot to show areas | can be changed to zoom in on certain areas
#Europe extent(-30, 60, 0, 90)
#Philippines etc. extent(60, 180, -30, 30)
#Africa| South Am. equator extent(-90, 30, -30, 30)
clipExt     <- extent(-190, 190, -95, 95)
clipExt     <- extent(-120, 0, -10, 60)

percentage  <- .99

allImpacts(GHG_Sugar,LD_Sugar, N_Sugar,P_Sugar, WD_Sugar, "Sugar", clipExt, percentage, "Top 1%")
allImpacts(GHG_Sugarbeet,LD_Sugarbeet, N_Sugarbeet,P_Sugarbeet, WD_Sugarbeet, "Sugarbeet", clipExt, percentage, "Top 1%")
allImpacts(GHG_Sugarcane,LD_Sugarcane, N_Sugarcane,P_Sugarcane, WD_Sugarcane, "Sugarcane", clipExt, percentage, "Top 1%")

allImpacts(GHG_Wheat,LD_Wheat, N_Wheat,P_Wheat, WD_Wheat, "Wheat", clipExt, percentage, "Top 5%")
allImpacts(GHG_Oilpalm,LD_Oilpalm, N_Oilpalm,P_Oilpalm, WD_Oilpalm, "Oilpalm", clipExt, percentage, "Top 5%")

#create separate Legend
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('GHG emissions', 'Land diversity', 'N fertilizer',
                            'P fertilizer', 'Water dept'), pch=16, pt.cex=3, cex=1.5, bty='n',
       col = c('blue', 'red', 'darkturquoise', 'yellow', 'forestgreen'))
mtext("Impact", at=0.2, cex=2)

#
###for cocoa:####

#raster to dataframe for easier handling
raster1_pts <- rasterToPoints(GHG_Cocoa, spatial = TRUE)
raster1_df  <- data.frame(raster1_pts)
rm(raster1_pts)

#remove all columns from df 
raster1b <- raster1_df
raster1b <- raster1b[,-(2:4)]

#devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
quant <- quantile(raster1b, c(0, .95, 1)) 
g <- cut(raster1b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
splitraster1<-split(raster1_df, g)

#only select areas with the (5%) highest GHG Impact value
splitraster1 <- as.data.frame(splitraster1$hi)

#do the same for the second raster
raster2_pts <- rasterToPoints(LD_Cocoa, spatial = TRUE)
raster2_df  <- data.frame(raster2_pts)
rm(raster2_pts)

#remove all columns from df 
raster2b <- raster2_df
raster2b <- raster2b[,-(2:4)]

#devide in three blocks; 0-0.1 - low; 0.1-0.9 - mid, 0.9-1 - high
quant <- quantile(raster2b, c(0, .95, 1)) 
g <- cut(raster2b, quant, include.lowest = TRUE, lab = c("lower", "hi"))
splitraster2<-split(raster2_df, g)

#only select areas with the (5%) highest GHG Impact value
splitraster2 <- as.data.frame(splitraster2$hi)


colnames(splitraster1)[1] <- "colnameraster1"
colnames(splitraster2)[1] <- "colnameraster2"

#plot to show areas | can be changed to zoom in on certain areas
clipExt <- extent(-190, 190, -95, 95)

#make map with all impact factors
ggplot()+
  geom_tile(data= splitraster1, aes(x=x, y=y, fill = colnameraster1), fill= "blue", alpha=0.7)+
  geom_tile(data= splitraster2, aes(x=x, y=y, fill = colnameraster2),fill= "red", alpha=0.6)+ 
  scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                     labels = paste0(seq(-90, 90, 30), "°")) +
  scale_x_continuous(breaks = seq(-180,182,60), 
                     labels = paste0(seq(-180,182,60), "°")) +
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.5) +
  coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
  labs(x="Longitude",
       y="Latitude",
       title = "Top 5%",
       subtitle = "Cocoa")+
  theme(plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black"))


