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


