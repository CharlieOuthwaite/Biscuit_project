##%######################################################%##
#                                                          #
#                      2. Global map                    ####
#         overlay of crop area and species richness        #
#                                                          #
##%######################################################%##

#the final function can create bivariate maps (species richness and any other Impact factor/Production Map)
# started by Feli Pamatat, 16/06/2021

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


remove(list = ls(all.names = TRUE))
Sys.setenv(LANG = "en")


#Task 1 - set dir ####

setwd("/Users/Feli/Documents/Cookie Project")

#SR - species richness
#MM - Mark's maps

#for species richness maps | download Adriennes rds files Google Drive
SRdir <- "Species_Richness_Maps"

#Mark's data in different dir
MMdir <- "Data"

#set dir for saving maps
mapdir <- "Bivariate Map/RawMapsReadytobesorted"


#Task 2 - import Species Richness files####
#create list of Species Richness files
filesSR <- list.files(paste0(SRdir), pattern = ".rds", full.names = TRUE)

#load in species richness data (credits: Adrienne)
SR_Amphibians_50k<-readRDS(paste0(filesSR[1]))
SR_Mammals_50k<-readRDS(paste0(filesSR[2]))
SR_Birds_50k<-readRDS(paste0(filesSR[3]))
SR_Reptiles_50k<-readRDS(paste0(filesSR[4]))

#Task 3a - add original projection to SR rasters####
#data does not come with proj; Berhman proj added (original proj)

#Berhman projection
crs(SR_Amphibians_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
crs(SR_Mammals_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
crs(SR_Birds_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
crs(SR_Reptiles_50k) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

#export Raster as tif (for Google Earth Engine)
Amph <- writeRaster(SR_Amphibians_50k, filename=file.path(SRdir, "amp50k.tif"), format="GTiff", overwrite=TRUE)
Mamm <- writeRaster(SR_Mammals_50k, filename=file.path(SRdir, "man50k.tif"), format="GTiff", overwrite=TRUE)
Bird <- writeRaster(SR_Birds_50k, filename=file.path(SRdir, "bid50k.tif"), format="GTiff", overwrite=TRUE)
Rept <- writeRaster(SR_Reptiles_50k, filename=file.path(SRdir, "rep50k.tif"), format="GTiff", overwrite=TRUE)

#Task 3b - import Mark's data####
#for each crop and indicator

files <- list.files(paste0(MMdir, "/Marks_Maps"), pattern = "_Total.tif", full.names = TRUE)

#import all raster files (with stack to have all bands)
mapGHG_stack <- stack(paste0(files[1])) #GHG Emissions Total (5 Bands)
mapLD_stack <- stack(paste0(files[2])) #LD BioDiv Total (5 Bands)
mapN_stack <- stack(paste0(files[3])) #N Marine BioDiv Total (4 Bands)
mapP_stack <- stack(paste0(files[4])) #P Marine BioDiv Total (4 Bands)
mapWD_stack <- stack(paste0(files[5])) #Water Debt Total (4 Bands)


#GHG
GHG_cocoa <- mapGHG_stack[[1]]
GHG_oilpalm <- mapGHG_stack[[2]]
GHG_sugarbeet <- mapGHG_stack[[3]]
GHG_sugarcane <- mapGHG_stack[[4]]
GHG_wheat <- mapGHG_stack[[5]]

#WD
WD_oilpalm <- mapWD_stack[[1]]
WD_sugarbeet <- mapWD_stack[[2]]
WD_sugarcane <- mapWD_stack[[3]]
WD_wheat <- mapWD_stack[[4]]

# Task 3c - load MapSPAM ####

#create list with all MapSPAM file names
filesMapSPAM <- list.files(paste0(MapSPAM), pattern = "_A.tif", full.names = TRUE)

#check names:
filesMapSPAM
#[1] "MapSPAM_Cookie/spam2010V2r0_global_H_COCO_A.tif"
#[2] "MapSPAM_Cookie/spam2010V2r0_global_H_OILP_A.tif"
#[3] "MapSPAM_Cookie/spam2010V2r0_global_H_SUGB_A.tif"
#[4] "MapSPAM_Cookie/spam2010V2r0_global_H_SUGC_A.tif"
#[5] "MapSPAM_Cookie/spam2010V2r0_global_H_WHEA_A.tif"

#load in species richness data (credits: Adrienne)
SPAM_Coco<-raster(paste0(filesMapSPAM[1]))
SPAM_Oilp<-raster(paste0(filesMapSPAM[2]))
SPAM_Sugb<-raster(paste0(filesMapSPAM[3]))
SPAM_Sugc<-raster(paste0(filesMapSPAM[4]))
SPAM_Whea<-raster(paste0(filesMapSPAM[5]))

##### Task 4 - create Bivariate Map function (with help from https://gist.github.com/scbrown86/2779137a9378df7b60afd23e0c45c188)#####
#function to create colour matrix
colmat <- function(nquantiles = 3, upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = TRUE) {
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(magrittr)
  require(classInt)
  # The colours can be changed by changing the HEX codes for:
  # upperleft, upperright, bottomleft, bottomright
  # From http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
  # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
  # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5", 
  # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
  # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
  # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
  # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
  # Viridis style
  # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
  # Similar to Fjeldsa, Bowie, Rahbek 2012
  # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
  # Default from original source
  # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F",
  my.data <- seq(0, 1, .01)
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nquantiles,
                                       style = "quantile" )
  my.pal.1 <- findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- findColours(my.class, my.col)
  }
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>% 
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>% 
    dplyr::select(-c(4)) %>%
    mutate("Y" = rep(seq(from = 1, to = nquantiles, by = 1), each = nquantiles),
           "X" = rep(seq(from = 1, to = nquantiles, by = 1), times = nquantiles)) %>%
    mutate("UID" = row_number())
  # Use plotLeg if you want a preview of the legend
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_raster() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 12, colour = "black",hjust = 0.5, 
                                      vjust = 1),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    ggsave(filename = file.path(SRdir, "bivLegend.pdf"), plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  seqs <- seq(0, 100, (100 / nquantiles))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}


# Function to assign colour-codes to raster data
bivariate.map <- function(rasterx, rastery, colormatrix = col.matrix,
                          nquantiles = 3, export.colour.matrix = TRUE,
                          outname = paste0("colMatrix_rasValues", names(rasterx))) {
  # export.colour.matrix will export a data.frame of rastervalues and RGB codes 
  # to the global environment outname defines the name of the data.frame
  quanmean <- getValues(rasterx)
  temp1 <- data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  brks1 <- with(temp1,  quantile(temp1,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))
  ))
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  brks1[-1] <- brks1[-1] + seq_along(brks1[-1]) * .Machine$double.eps
  r1 <- within(temp1, quantile <- cut(quanmean,
                                     breaks = brks1,
                                     labels = 2:length(brks1),
                                     include.lowest = TRUE
  ))
  quantr <- data.frame(r1[, 2])
  #
  quanvar <- getValues(rastery)
  temp2 <- data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  brks2 <- with(temp2,  quantile(temp2,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))
  ))
  #brks2[-1] <- brks2[-1] + seq_along(brks2[-1]) * .Machine$double.eps
  r2 <- within(temp2, quantile <- cut(quanvar,
                                     breaks = brks2,
                                     labels = 2:length(brks2),
                                     include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }
  # Export the colour.matrix to data.frame() in the global env
  # Can then save with write.table() and use in ArcMap/QGIS
  # Need to save the output raster as integer data-type
  if (export.colour.matrix) {
    # create a dataframe of colours corresponding to raster values
    exportCols <- as.data.frame(cbind(
      as.vector(col.matrix2), as.vector(colormatrix),
      t(col2rgb(as.vector(colormatrix)))
    ))
    # rename columns of data.frame()
    colnames(exportCols)[1:2] <- c("rasValue", "HEX")
    # Export to the global environment
    assign(
      x = outname,
      value = exportCols,
      pos = .GlobalEnv
    )
  }
  cols <- numeric(length(quantr[, 1]))
  for (i in 1:length(quantr[, 1])) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
  }
  r <- rasterx
  r[1:length(r)] <- cols
  return(r)
}

# Task 5 - Automate the making of maps####
#since this is made for SR maps from Adrienne; it is assumed that rasterX is always has the coarser and a Berhman projection
#rasterX          - raster with coarser resolution
#rasterY          - raster with finer resolution
#rasternameX      - e.g. "Species Richness" (needs "" to work)
#rasternameY      - e.g. "GHG Emission" (needs "" to work)
#rasterXSUBtitle  - e.g. "Birds" (needs "" to work)
#rasterYSUBtitle  - e.g. "Wheat" (needs "" to work)
#subtitle         - e.g. "Species Richness Birds and GHG Emission of Cocoa" (needs "" to work)
#PATH             - e.g. SRdir (SRdir being a dir assigned to a object) or "/Users/Feli/Documents/Cookie Project" or which ever dir you want
#outlierY         - only added in case the rasterY data has an outlier (e.g. GHG_wheat has outliers above 2000 --> outlierY=2000)
#AllArea = 1      - bivariate map fills all areas - including areas where data is only available from a single raster  
#  "     ≠ 1      - bivariate map shows only areas where both datasets overlap
#SaveMap = 1      - Maps will be saved as png
#SaveMap ≠ 1      - Maps will not be saved and instead printed in plot window
#save and print for SR map disabled (to avoid junk maps)

makeBivmap<- function(rasterX, rasterY, nBreaks, rasternameX, rasternameY, rasterXSUBtitle, 
                      rasterYSUBtitle, subtitle, PATH, outlierY=0, AllArea = 1, SaveMap = 1){
  #crop rasterX to match extend of rasterY
  b <- extent(-17372530, 17372470,  0.99*(-6357770), 0.99*(7347230))
  rasterX_crp <- crop(rasterX, b)
  
  #change with rasterX to have the same projection as rasterY
  projectionY <- projection(rasterY)
  reprojectedX <- projectRaster(rasterX_crp, crs = projectionY)
  
  #convert raster Y to coarsest (aka rasterX's) resolution:
  resampledY<-resample(rasterY, reprojectedX, method='bilinear')
  
  #this step needed so bivariate map function can work
  reprojectedX[is.na(reprojectedX[])] <- 0 
  reprojectedX[reprojectedX<1] <- 0 
  reprojectedX[reprojectedX==0] <- NA
  
  #for now: default cut off is 0; will find a way to exclude outlayers later
  if (outlierY > 0) {
    resampledY[resampledY>outlierY] <- 0 
  }
  #for use in single map assign different name (if all na is 0 the mapping of a rasterY only map does not work)
  RY <- resampledY
  
  #continue preparing for bivariate.map function:
  if (AllArea == 1) {  
    resampledY[is.na(resampledY[])] <- 0
  }
  
  #Bivariate Map
  col.matrix <- colmat(nquantiles = nBreaks, xlab = rasternameY, ylab =rasternameX, 
                       ## non default colours
                       upperleft = "#64acbe", upperright = "#574249", 
                       bottomleft = "#e8e8e8", bottomright = "#c85a5a")
  
  bivmap <- bivariate.map(rasterx = resampledY, rastery = reprojectedX,
                          export.colour.matrix = TRUE, outname = "bivMapCols",
                          colormatrix = col.matrix, nquantiles = nBreaks)
  
  #change all 1 to NA
  bivmap[bivmap==1] <- NA
  
  #bivmap raster to data frame
  bivMapDF <- as.data.frame(bivmap, xy = TRUE) %>%
    tbl_df() %>%
    dplyr::rename("BivValue" = 3) %>%
    pivot_longer(., names_to = "Variable", values_to = "bivVal", cols = BivValue)
  
  #raw map extend; extent(-190, 190, -95, 95) give out a map which is a bit bigger than normal world map
  clipExt <- extent(-190, 190, -95, 95)
  
  # Bivariate Map 
  map <- ggplot(bivMapDF, aes(x = x, y = y))+
    geom_raster(aes(fill = bivVal)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,20), 
                       labels = paste0(seq(-180,182,20), "°")) +
    scale_fill_gradientn(colours = col.matrix, na.value = "transparent") + 
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="Longitude",
         y="Latitude",
         title = "Bivariate Map", 
         subtitle = subtitle)+
    theme(legend.position = "none",
          plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
  
  #add legend to bivariate map
  fig <- ggdraw(map) + 
    draw_plot(BivLegend +
                theme(plot.background = element_rect(fill = "white", colour = NA)),
              width = 0.25, height = 0.25, x = 0, y = 0.3)
  
  #fig2 is for combining maps all three maps (bivariate, rasterx, rastery) in one patched map
  fig2 <- ggdraw(map) + 
    draw_plot(BivLegend +
                theme(plot.background = element_rect(fill = "white", colour = NA)),
              width = 0.4, height = 0.4, x = 0.8, y = 0.25)
  
  #Make Seperate Maps and Maps to be added to the patched map for the two indicators
  
  #rasterY:
  
  #rasterY into df
  rasterY_pts <- rasterToPoints(RY, spatial = TRUE)
  rasterY_df  <- data.frame(rasterY_pts)
  rm(rasterY_pts)
  
  names(rasterY_df)[1] <- "Y_value"
  
  #stand alone map for rasterY
  maprasterY1 <- ggplot(rasterY_df, aes(x = x, y = y))+
    geom_tile(aes(fill = Y_value)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,20), 
                       labels = paste0(seq(-180,182,20), "°")) +
    scale_fill_gradientn(colours = c("#E8E8E8","#E4D8D8","#E0C8C8", "#DDB8B8","#D9A8A8", "#D69999", "#D28989","#CF7979","#CB6969","#C85A5A"),
                         na.value = "transparent", name = "GHG Emissions") + #add "transparent" at the front so all 
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="Longitude",
         y="Latitude",
         title = rasternameY, 
         subtitle = rasterYSUBtitle)+
    theme(plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))

  
  #for combined/patched map (rasterY)
  maprasterY2 <- ggplot(rasterY_df, aes(x = x, y = y))+
    geom_tile(aes(fill = Y_value)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,60), 
                       labels = paste0(seq(-180,182,60), "°")) +
    scale_fill_gradientn(colours = c("#E8E8E8","#E4D8D8","#E0C8C8", "#DDB8B8","#D9A8A8", "#D69999", "#D28989","#CF7979","#CB6969","#C85A5A"),
                         na.value = "transparent", name = "GHG Emissions") + #add "transparent" at the front so all 
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="",
         y="",
         title = rasternameY, 
         subtitle = rasterYSUBtitle)+
    theme(plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
  
  #rasterX:
  
  #rasterX into df
  rasterX_pts <- rasterToPoints(reprojectedX, spatial = TRUE)
  rasterX_df  <- data.frame(rasterX_pts)
  rm(rasterX_pts)
  
  names(rasterX_df)[1] <- "X_value"
  
  #stand alone map for rasterX
  maprasterX1 <- ggplot(rasterX_df, aes(x = x, y = y))+
    geom_tile(aes(fill = X_value)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,20), 
                       labels = paste0(seq(-180,182,20), "°")) +
    scale_fill_gradientn(colours = c("#E8E8E8","#D9E1E3", "#CADADE","#BBD3DA", "#ADCDD5", "#9EC6D0" 
                                     ,"#90C0CC", "#81B9C7", "#72B2C2", "#64ACBE"),
                         na.value = "transparent", name = "Species Richness") +
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="Longitude",
         y="Latitude",
         title = rasternameX, 
         subtitle = rasterXSUBtitle)+
    theme(plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))

  #for combined/patched map (rasterX)
  maprasterX2 <- ggplot(rasterX_df, aes(x = x, y = y))+
    geom_tile(aes(fill = X_value)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), 
                       labels = paste0(seq(-90, 90, 30), "°")) +
    scale_x_continuous(breaks = seq(-180,182,60), 
                       labels = paste0(seq(-180,182,60), "°")) +
    scale_fill_gradientn(colours = c("#E8E8E8","#D9E1E3", "#CADADE","#BBD3DA", "#ADCDD5", "#9EC6D0" 
                                     ,"#90C0CC", "#81B9C7", "#72B2C2", "#64ACBE"),
                         na.value = "transparent", name = "Species Richness") +
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) + #draws world map (using country boarders) on top of data
    labs(x="",
         y="",
         title = rasternameX, 
         subtitle = rasterXSUBtitle)+
    theme(plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"))
  #maprasterX2
  
  #Combined|Patched Map (Bivariate, RasterX and RasterY)####
  combined <- maprasterX2 + maprasterY2 - fig2 +  plot_layout(ncol = 1,widths = c(2, 1), heights = unit(c(1, 13), c('null','cm')))
  
  
  #save all plots | change if different label is preferred####
  if (SaveMap == 1){
    if (AllArea == 1) {
      ggsave(fig,filename=paste("Biv",nBreaks,"Breaks","SR", rasterXSUBtitle, "_", deparse(substitute(rasterY)),
                              "_PO.png",sep=""),width = 50, height = 37.7038895859, units = "cm", path = PATH)
      ggsave(combined,filename=paste("BivCombined",nBreaks,"Breaks","SR", rasterXSUBtitle, "_", deparse(substitute(rasterY)),
                                   "_PO.png",sep=""),width = 50, height = 37.7038895859, units = "cm", path = PATH) 
    } else {
      ggsave(fig,filename=paste("Biv",nBreaks,"Breaks","SR", rasterXSUBtitle, "_", deparse(substitute(rasterY)),
                              "_FO.png",sep=""),width = 50, height = 37.7038895859, units = "cm", path = PATH) 
      ggsave(combined,filename=paste("BivCombined",nBreaks,"Breaks","SR", rasterXSUBtitle, "_", deparse(substitute(rasterY)),
                                   "_FO.png",sep=""),width = 50, height = 37.7038895859, units = "cm", path = PATH) 
    }
    #enable ggsave for SR if needed
    #ggsave(maprasterX1,filename=paste(rasterXSUBtitle, rasternameX, ".png",sep=""),width = 50, height = 37.7038895859, units = "cm", path = PATH)
    ggsave(maprasterY1,filename=paste(rasterYSUBtitle, rasternameY, ".png",sep=""),width = 50, height = 37.7038895859, units = "cm", path = PATH)
  } else {
    #enable print for SR if needed
    #print(maprasterX1)
    print(maprasterY1)
    print(fig)
    print(combined)
  }
  
  
}


#

#Task 6 - Make maps####
#path/dir for saving maps: mapdir

#example:
makeBivmap(SR_all,GHG_wheat,10, "Species richness", "GHG emission", "Combined", "Wheat","Species Richness and GHG emmision of Wheat",mapdir, AllArea = 0, SaveMap = 1)


