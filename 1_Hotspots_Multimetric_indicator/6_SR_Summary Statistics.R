##%######################################################%##
#                                                          #
#                  6. Summary Statistics                ####
#       Physical Area overlap with species richness        #
#                                                          #
##%######################################################%##
# started by Feli Pamatat, 11/08/2021

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
if(!require(rgeos)) {
  install.packages("rgeos")
  require(rgeos)
}


remove(list = ls(all.names = TRUE))
Sys.setenv(LANG = "en")


#Task 1 - set dir ####

setwd("/Users/Feli/Documents/Cookie Project")

#MapSpam2010 PhysicalArea
PhysAreadir <- "MapSPAMPhysArea_Biscuit"

#for species richness maps | download Adriennes rds files Google Drive
SRdir <- "Species_Richness_Maps"

#save summary
SummDir<- "Summary_Table"

#Task 2 - import Species Richness files####
#create list of Species Richness files
filesSR <- list.files(paste0(SRdir), pattern = ".rds", full.names = TRUE)

#load in species richness data (credits: Adrienne)
SR_Amphibians_50k<-readRDS(paste0(filesSR[1]))
SR_Mammals_50k<-readRDS(paste0(filesSR[2]))
SR_Birds_50k<-readRDS(paste0(filesSR[3]))
SR_Reptiles_50k<-readRDS(paste0(filesSR[4]))

##Task 2a - add original projection to SR rasters####
#data does not come with proj; Berhman proj added (original proj)

#Berhman projection
crs(SR_Amphibians_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
crs(SR_Mammals_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
crs(SR_Birds_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
crs(SR_Reptiles_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

#merge to one SpeciesRichness raster
listSR <- list(SR_Amphibians_50k, SR_Birds_50k,SR_Mammals_50k, SR_Reptiles_50k)
names(listSR) <- c("x", "y")
listSR$filename <- 'SpeciesRichnessAll.tif'
listSR$overwrite <- TRUE
SR_all <- do.call(merge, listSR)


#Task 3 - load physical area for crops

#create list with all MapSPAM file names
filesMapSPAM <- list.files(paste0(PhysAreadir), pattern = "_A.tif", full.names = TRUE)

#check names:
filesMapSPAM
#[1] "MapSPAMPhysArea_Biscuit/spam2010V2r0_global_A_COCO_A.tif"
#[2] "MapSPAMPhysArea_Biscuit/spam2010V2r0_global_A_OILP_A.tif"
#[3] "MapSPAMPhysArea_Biscuit/spam2010V2r0_global_A_SUGB_A.tif"
#[4] "MapSPAMPhysArea_Biscuit/spam2010V2r0_global_A_SUGC_A.tif"
#[5] "MapSPAMPhysArea_Biscuit/spam2010V2r0_global_A_WHEA_A.tif"

#load in species richness data (credits: Adrienne)
SPAM_Coco<-raster(paste0(filesMapSPAM[1]))
SPAM_Oilp<-raster(paste0(filesMapSPAM[2]))
SPAM_Sugb<-raster(paste0(filesMapSPAM[3]))
SPAM_Sugc<-raster(paste0(filesMapSPAM[4]))
SPAM_Whea<-raster(paste0(filesMapSPAM[5]))


#Task 4 - adjust species raster to MapSpam raster####

#crop Species richness raster to match extend of Crop raster (tried with SPAM_Coco)
b                 <- extent(-17372530, 17372470,  0.99*(-6357770), 0.99*(7347230))
SR_all_crp        <- crop(SR_all, b)
  
#change with Species richness raster to have the same projection as Crop raster
projectionY       <- projection(SPAM_Coco)
reprojectedSR_all <- projectRaster(SR_all_crp, crs = projectionY)
  
#convert Crop raster to coarsest (aka Species richness raster's) resolution:
resampledCoco     <-resample(SPAM_Coco, reprojectedSR_all, method='bilinear')
  
#this step needed so bivariate map function can work
reprojectedSR_all[is.na(reprojectedSR_all[])] <- 0 
reprojectedSR_all[reprojectedSR_all<1] <- 0 
reprojectedSR_all[reprojectedSR_all==0] <- NA

resampledCoco[is.na(resampledCoco[])] <- 0 
resampledCoco[resampledCoco<1] <- 0 
resampledCoco[resampledCoco==0] <- NA

#Task 5 - overlap physical area with species richness map ####

# create your blank raster to populate. This is only the area both inputs occupy.
r_intersec <- raster(raster::intersect(reprojectedSR_all, resampledCoco))


# The other 2 rasters should be cropped to the extent of your target raster, to avoid extent errors as mentioned in the comments
croped_SR <- crop(reprojectedSR_all,extent(r_intersec))
croped_Coco <- crop(resampledCoco,extent(r_intersec))

#mask the two cropped rasters to only get the SR values where the Crop raster is over 1
overlapCoco <- mask(croped_SR, croped_Coco)

#plot
par(mfrow=c(1,3))
plot(croped_SR)
plot(croped_Coco)
plot(overlapCoco)

#Task 6 - Create Summary statistic 

Coco_sum             <- matrix(nrow = 1, ncol = 6)
colnames(Coco_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer")
Coco_sum             <- as.data.frame(Coco_sum)
Coco_sum$RasterLayer <- "Cocoa"
Coco_sum$Sum         <- cellStats(overlapCoco, stat='sum', na.rm=TRUE)
Coco_sum$Mean        <- cellStats(overlapCoco, stat='mean', na.rm=TRUE)
Coco_sum$Sd          <- cellStats(overlapCoco, stat='sd', na.rm=TRUE)
Coco_sum$Max         <- cellStats(overlapCoco, stat='max', na.rm=TRUE)
Coco_sum$Min         <- cellStats(overlapCoco, stat='min', na.rm=TRUE)


#Repeat with Wheat, Oilpalm, Sugarbeet and Sugarcane####

##Wheat####
#convert Crop raster to coarsest (aka Species richness raster's) resolution:
resampledWhea     <-resample(SPAM_Whea, reprojectedSR_all, method='bilinear')

#this step needed so mask can work
resampledWhea[is.na(resampledWhea[])] <- 0 
resampledWhea[resampledWhea<1] <- 0 
resampledWhea[resampledWhea==0] <- NA



#overlap physical area with species richness map

# create your blank raster to populate. This is only the area both inputs occupy.
r_intersec <- raster(raster::intersect(reprojectedSR_all, resampledWhea))


# The other 2 rasters should be cropped to the extent of your target raster, to avoid extent errors as mentioned in the comments
croped_SR <- crop(reprojectedSR_all,extent(r_intersec))
croped_Whea <- crop(resampledWhea,extent(r_intersec))

#mask the two cropped rasters to only get the SR values where the Crop raster is over 1
overlapWhea <- mask(croped_SR, croped_Whea)

#Create Summary statistic 

Whea_sum             <- matrix(nrow = 1, ncol = 6)
colnames(Whea_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer")
Whea_sum             <- as.data.frame(Whea_sum)
Whea_sum$RasterLayer <- "Wheat"
Whea_sum$Sum         <- cellStats(overlapWhea, stat='sum', na.rm=TRUE)
Whea_sum$Mean        <- cellStats(overlapWhea, stat='mean', na.rm=TRUE)
Whea_sum$Sd          <- cellStats(overlapWhea, stat='sd', na.rm=TRUE)
Whea_sum$Max         <- cellStats(overlapWhea, stat='max', na.rm=TRUE)
Whea_sum$Min         <- cellStats(overlapWhea, stat='min', na.rm=TRUE)

plot(overlapWhea)

##Oilpalm####
#convert Crop raster to coarsest (aka Species richness raster's) resolution:
resampledOilp     <-resample(SPAM_Oilp, reprojectedSR_all, method='bilinear')

#this step needed so mask can work
resampledOilp[is.na(resampledOilp[])] <- 0 
resampledOilp[resampledOilp<1] <- 0 
resampledOilp[resampledOilp==0] <- NA



#overlap physical area with species richness map

# create your blank raster to populate. This is only the area both inputs occupy.
r_intersec <- raster(raster::intersect(reprojectedSR_all, resampledOilp))


# The other 2 rasters should be cropped to the extent of your target raster, to avoid extent errors as mentioned in the comments
croped_SR <- crop(reprojectedSR_all,extent(r_intersec))
croped_Oilp <- crop(resampledOilp,extent(r_intersec))

#mask the two cropped rasters to only get the SR values where the Crop raster is over 1
overlapOilp <- mask(croped_SR, croped_Oilp)

#Create Summary statistic 

Oilp_sum             <- matrix(nrow = 1, ncol = 6)
colnames(Oilp_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer")
Oilp_sum             <- as.data.frame(Oilp_sum)
Oilp_sum$RasterLayer <- "OilPalm"
Oilp_sum$Sum         <- cellStats(overlapOilp, stat='sum', na.rm=TRUE)
Oilp_sum$Mean        <- cellStats(overlapOilp, stat='mean', na.rm=TRUE)
Oilp_sum$Sd          <- cellStats(overlapOilp, stat='sd', na.rm=TRUE)
Oilp_sum$Max         <- cellStats(overlapOilp, stat='max', na.rm=TRUE)
Oilp_sum$Min         <- cellStats(overlapOilp, stat='min', na.rm=TRUE)

plot(overlapOilp)

##Sugarbeet####
#convert Crop raster to coarsest (aka Species richness raster's) resolution:
resampledSugb     <-resample(SPAM_Sugb, reprojectedSR_all, method='bilinear')

#this step needed so mask can work
resampledSugb[is.na(resampledSugb[])] <- 0 
resampledSugb[resampledSugb<1] <- 0 
resampledSugb[resampledSugb==0] <- NA



#overlap physical area with species richness map

# create your blank raster to populate. This is only the area both inputs occupy.
r_intersec <- raster(raster::intersect(reprojectedSR_all, resampledSugb))


# The other 2 rasters should be cropped to the extent of your target raster, to avoid extent errors as mentioned in the comments
croped_SR <- crop(reprojectedSR_all,extent(r_intersec))
croped_Sugb <- crop(resampledSugb,extent(r_intersec))

#mask the two cropped rasters to only get the SR values where the Crop raster is over 1
overlapSugb <- mask(croped_SR, croped_Sugb)

#Create Summary statistic 

Sugb_sum             <- matrix(nrow = 1, ncol = 6)
colnames(Sugb_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer")
Sugb_sum             <- as.data.frame(Sugb_sum)
Sugb_sum$RasterLayer <- "Sugarbeet"
Sugb_sum$Sum         <- cellStats(overlapSugb, stat='sum', na.rm=TRUE)
Sugb_sum$Mean        <- cellStats(overlapSugb, stat='mean', na.rm=TRUE)
Sugb_sum$Sd          <- cellStats(overlapSugb, stat='sd', na.rm=TRUE)
Sugb_sum$Max         <- cellStats(overlapSugb, stat='max', na.rm=TRUE)
Sugb_sum$Min         <- cellStats(overlapSugb, stat='min', na.rm=TRUE)

plot(overlapSugb)


##Sugarcane####
#convert Crop raster to coarsest (aka Species richness raster's) resolution:
resampledSugc     <-resample(SPAM_Sugc, reprojectedSR_all, method='bilinear')

#this step needed so mask can work
resampledSugc[is.na(resampledSugc[])] <- 0 
resampledSugc[resampledSugc<1] <- 0 
resampledSugc[resampledSugc==0] <- NA



#overlap physical area with species richness map

# create your blank raster to populate. This is only the area both inputs occupy.
r_intersec <- raster(raster::intersect(reprojectedSR_all, resampledSugc))


# The other 2 rasters should be cropped to the extent of your target raster, to avoid extent errors as mentioned in the comments
croped_SR <- crop(reprojectedSR_all,extent(r_intersec))
croped_Sugc <- crop(resampledSugc,extent(r_intersec))

#mask the two cropped rasters to only get the SR values where the Crop raster is over 1
overlapSugc <- mask(croped_SR, croped_Sugc)

#Create Summary statistic 

Sugc_sum             <- matrix(nrow = 1, ncol = 6)
colnames(Sugc_sum)   <- c("Sum", "Mean", "Sd", "Max","Min", "RasterLayer")
Sugc_sum             <- as.data.frame(Sugc_sum)
Sugc_sum$RasterLayer <- "Sugarcane"
Sugc_sum$Sum         <- cellStats(overlapSugc, stat='sum', na.rm=TRUE)
Sugc_sum$Mean        <- cellStats(overlapSugc, stat='mean', na.rm=TRUE)
Sugc_sum$Sd          <- cellStats(overlapSugc, stat='sd', na.rm=TRUE)
Sugc_sum$Max         <- cellStats(overlapSugc, stat='max', na.rm=TRUE)
Sugc_sum$Min         <- cellStats(overlapSugc, stat='min', na.rm=TRUE)

plot(overlapSugc)


#Task 6 - rbind all Summary tables & save####
summarySR <- rbind(Coco_sum, Whea_sum, Oilp_sum, Sugb_sum, Sugc_sum)

#save
write_csv(summarySR, file.path(SummDir, "SummarySR.csv"))


