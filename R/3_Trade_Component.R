## 1st August 2025
## Code to accompany 'the biscuit project', edited on 1st August 2025 to ensure clear for dissemination
## This script was used to compute the values in Table 1 in the manuscript. 
## In it, we open and process the trade data before using it to compute relevant measures of trade
## of the focal crops with the UK.
## Note: we have these trade data courtesy of Carole Dalin, Harry Kennard, and Thomas Kastner, among others, so we 
## recommend contacting Dr Carole Dalin at c.dalin@ucl.ac.uk for any data access needs.

rm(list = ls())
library(dplyr)
options(scipen = 999)

setwd("~/Data/") # set the working directory
biscuitdir = "Biscuit Project/" # set the output directory
cropdir = "SHEFS/6_Trade_Matrices/Corrected_trade_long/" # identify the directory with the trade data inside

cropdata = list.files(path = cropdir, pattern="*.csv", all.files = TRUE, full.names = TRUE)
cropdata # listing all the files in the trade data directory
for (i in 1:length(cropdata)) assign(cropdata[i], read.csv(cropdata[i])) # opening each of these files

## We found issues in the data for 2018 (e.g., Ghana is missing for cocoa), so we used data
## for 2003-2013 to marry up with EarthStat crop data (representing c.2000) and
## the SPAM data used in the final stage of our analysis.
## Below, we filter the trade data for these years:
yearslist = c("2003", "2004", "2005",
              "2006", "2007", "2008",
              "2009", "2010", "2011",
              "2012", "2013")
cropdata_sub = lapply(yearslist, FUN = function(x) {
  cropdata[grepl(x, cropdata)]})
cropdata_sub
cropdata_sub1 = unlist(cropdata_sub)

## We then further filter the data so it shows only our crops of interest:
croplist = c("cocoa", "oilpalm", "wheat", "sugarbeet", "sugarcane")
croplist
cropdata_sub2 = lapply(croplist, FUN = function(x) {
  cropdata_sub1[grepl(x, cropdata_sub1)]
})
cropdata_sub2
cropdata_sub3 = unlist(cropdata_sub2)
cropdata = cropdata_sub3
str(cropdata)

## Remove buckwheat, which is extracted as it contains the word 'wheat'.
cropdata1 = cropdata[!grepl("buckwheat", cropdata)]
str(cropdata1)

## Now, we are pulling out the countries which are trading with the UK:
df = NULL
for (o in 1:55) {
  file1 = read.csv(cropdata1[o])
  file2 = file1[complete.cases(file1),]
  cropname = stringr::str_remove(cropdata1[o], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
  cropname1 = stringr::str_remove(cropname, ".csv")
  countrydata = subset(file2, reporter == "United Kingdom") 
  print(countrydata)
  df = rbind(df, data.frame(cropname1, countrydata))
}

# df should now contain everything that partners with the UK
df

# We now just want to see which the top partners are according to imports with the UK for the biscuit crops.
outdir = "Biscuit Project/Trade_out_July2023/"
# write.csv(df, paste0(outdir, "all_crops_2003to2013_years_sep_07Sep2023.csv"))# - writing to save running each time
df= read.csv(paste0(outdir, "all_crops_2003to2013_years_sep_07Sep2023.csv"))

# Separating out the crops and years into different columns for easier analysis:
head(df, n = 5)
library(tidyverse)
cropyear = df %>%
  tidyr::separate (cropname1, into = c("crop", "year"), sep = ("_"))
head(cropyear)

# Checking we have the years we want:
unique(cropyear$year)

# Get an average:
average_percrop_years = cropyear %>% 
  group_by(crop, partner) %>% 
  summarise(average_tonnes_2003to2013 = mean(dmi_origin_corrected, na.rm = TRUE))

# Oil palm - what is the total domestic production and imports (UK consumption)?
(total_consumption_oilpalm = average_percrop_years %>% 
  filter(crop == "oilpalm") %>% 
  summarise(sum(average_tonnes_2003to2013, na.rm = TRUE))) 

# Which countries are supplying at least 5% of this?
(top5_oilpalm = 0.05*total_consumption_oilpalm$`sum(average_tonnes_2003to2013, na.rm = TRUE)`) 
(oilpalm = average_percrop_years %>% 
    filter(crop == "oilpalm") %>% 
  filter(average_tonnes_2003to2013>top5_oilpalm))
(oilpalm$average_tonnes_2003to2013/total_consumption_oilpalm$`sum(average_tonnes_2003to2013, na.rm = TRUE)`)*100

# Cocoa - what is the total domestic production and imports (UK consumption)?
(total_consumption_cocoa = average_percrop_years %>% 
    filter(crop == "cocoa") %>% 
    summarise(sum(average_tonnes_2003to2013, na.rm = TRUE))) 

# Which countries are supplying at least 5% of this?
(top5_cocoa = 0.05*total_consumption_cocoa$`sum(average_tonnes_2003to2013, na.rm = TRUE)`) 
(cocoa = average_percrop_years %>% 
    filter(crop == "cocoa") %>% 
    filter(average_tonnes_2003to2013>top5_cocoa)) 
cocoa$average_tonnes_2003to2013/total_consumption_cocoa$`sum(average_tonnes_2003to2013, na.rm = TRUE)`*100
# This is the average tonnes from that country as a proportion of total UK consumption

# Wheat - what is the total domestic production and imports (UK consumption)?
(total_consumption_wheat = average_percrop_years %>% 
    filter(crop == "wheat") %>% 
    summarise(sum(average_tonnes_2003to2013, na.rm = TRUE))) 

# Which countries are supplying at least 5% of this?
(top5_wheat = 0.05*total_consumption_wheat$`sum(average_tonnes_2003to2013, na.rm = TRUE)`) 
(wheat = average_percrop_years %>% 
    filter(crop == "wheat") %>% 
    filter(average_tonnes_2003to2013>top5_wheat)) 

# Sugarcane and sugarbeet - can be grouped as 'sugar' for our analysis but were separate in the original trade data.
# Sugar - what is the total domestic production and imports (UK consumption)?
(sugardata = average_percrop_years %>% 
    filter(crop == "sugarcane" | crop == "sugarbeet"))
(total_consumption_sugar = (sum(sugardata$average_tonnes_2003to2013, na.rm = TRUE))) 

# Which countries are supplying at least 5% of this?
(top5_sugar = 0.05*total_consumption_sugar) 
(sugar = sugardata %>% 
    filter(average_tonnes_2003to2013>top5_sugar)) 
sugar$average_tonnes_2003to2013/total_consumption_sugar*100

## Now, to get the percentage of total in country production, we need to not filter the csvs by UK as the reporter
# Reporter = importer, with partner
# Partner = sending the crops to the importer

df1 = NULL
for (o in 1:55) {
  file1 = read.csv(cropdata1[o])
  file2 = file1[complete.cases(file1),]
  cropname = stringr::str_remove(cropdata1[o], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
  cropname1 = stringr::str_remove(cropname, ".csv")
  countrydata = file2
  print(countrydata)
  df1 = rbind(df1, data.frame(cropname1, countrydata))
}
df1
head(df1)
cropyear_allreporters = df1 %>%
  tidyr::separate (cropname1, into = c("crop", "year"), sep = ("_"))
head(cropyear_allreporters)
unique(cropyear_allreporters$year)
# Get an average:
average_percrop_years_allreporters = cropyear_allreporters %>% 
  group_by(crop, partner) %>% 
  summarise(average_tonnes_2003to2013 = mean(dmi_origin_corrected, na.rm = TRUE))

# Wheat
allreporters_wheat = filter(average_percrop_years_allreporters, crop == "wheat")
head(allreporters_wheat)
wheat_uk = filter(allreporters_wheat, partner == "United Kingdom")
84649/total_consumption_wheat$`sum(average_tonnes_2003to2013, na.rm = TRUE)`

## Computing the percentage of the country's production exported to the UK:

cropyear_allreporters = read.csv(paste0(outdir, "cropyear_allreporters.csv"))
head(cropyear_allreporters, n=6)

wheat_uk_to_uk = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "United Kingdom"
         & crop == "wheat") %>% 
  group_by(year) %>% 
  summarise(sum_wheat_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
wheat_uk_to_uk1 = mean(wheat_uk_to_uk$sum_wheat_peryear, na.rm = TRUE)

wheat_uk_to_all =  cropyear_allreporters %>% 
  filter(partner == "United Kingdom"
         & crop == "wheat") %>% 
  group_by(year) %>% 
  summarise(sum_wheat_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
wheat_uk_to_all1 = mean(wheat_uk_to_all$sum_wheat_peryear, na.rm = TRUE)

(wheat_proportion_ofexports_to_uk = (wheat_uk_to_uk1/wheat_uk_to_all1)*100)

cocoa_ghana_to_uk = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "Ghana"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_ghana_to_uk1 = mean(cocoa_ghana_to_uk$sum_cocoa_peryear, na.rm = TRUE)

cocoa_ghana_to_all =  cropyear_allreporters %>% 
  filter(partner == "Ghana"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_ghana_to_all1 = mean(cocoa_ghana_to_all$sum_cocoa_peryear, na.rm = TRUE)

cocoacheck = cocoa_ghana_to_all =  cropyear_allreporters %>% 
  filter(partner == "Ghana"
         & crop == "cocoa")

(cocoa_proportion_ofexports_to_uk = (cocoa_ghana_to_uk1/cocoa_ghana_to_all1)*100)

cocoa_Nigeria_to_uk = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "Nigeria"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_Nigeria_to_uk1 = mean(cocoa_Nigeria_to_uk$sum_cocoa_peryear, na.rm = TRUE)

cocoa_Nigeria_to_all =  cropyear_allreporters %>% 
  filter(partner == "Nigeria"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_Nigeria_to_all1 = mean(cocoa_Nigeria_to_all$sum_cocoa_peryear, na.rm = TRUE)

(cocoa_proportion_ofexports_to_uk = (cocoa_Nigeria_to_uk1/cocoa_Nigeria_to_all1)*100)

cocoa_Cameroon_to_uk = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "Cameroon"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_Cameroon_to_uk1 = mean(cocoa_Cameroon_to_uk$sum_cocoa_peryear, na.rm = TRUE)

cocoa_Cameroon_to_all =  cropyear_allreporters %>% 
  filter(partner == "Cameroon"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_Cameroon_to_all1 = mean(cocoa_Cameroon_to_all$sum_cocoa_peryear, na.rm = TRUE)

(cocoa_proportion_ofexports_to_Cameroon = (cocoa_Cameroon_to_uk1/cocoa_Cameroon_to_all1)*100)

# Cote d'Ivoire doesn't display properly because of special characters, so we have to manage this as shown below
unique(cropyear_allreporters$partner)
cropyear_allreporters_1 = cropyear_allreporters
test = subset(cropyear_allreporters, partner == "C\xf4te d'Ivoire")
test

cocoa_CIV_to_uk = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "C\xf4te d'Ivoire"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_CIV_to_uk1 = mean(cocoa_CIV_to_uk$sum_cocoa_peryear, na.rm = TRUE)

cocoa_CIV_to_all =  cropyear_allreporters %>% 
  filter(partner == "C\xf4te d'Ivoire"
         & crop == "cocoa") %>% 
  group_by(year) %>% 
  summarise(sum_cocoa_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
cocoa_CIV_to_all1 = mean(cocoa_CIV_to_all$sum_cocoa_peryear, na.rm = TRUE)

(cocoa_proportion_ofexports_to_uk = (cocoa_CIV_to_uk1/cocoa_CIV_to_all1)*100)

sugarbeet_UnitedKingdom_to_UnitedKingdom = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "United Kingdom"
         & crop == "sugarbeet") %>% 
  group_by(year) %>% 
  summarise(sum_sugarbeet_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
sugarbeet_UnitedKingdom_to_UnitedKingdom1 = mean(sugarbeet_UnitedKingdom_to_UnitedKingdom$sum_sugarbeet_peryear, na.rm = TRUE)

sugarbeet_UnitedKingdom_to_all =  cropyear_allreporters %>% 
  filter(partner == "United Kingdom"
         & crop == "sugarbeet") %>% 
  group_by(year) %>% 
  summarise(sum_sugarbeet_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
sugarbeet_UnitedKingdom_to_all1 = mean(sugarbeet_UnitedKingdom_to_all$sum_sugarbeet_peryear, na.rm = TRUE)

(sugarbeet_proportion_ofexports_to_UnitedKingdom = (sugarbeet_UnitedKingdom_to_UnitedKingdom1/sugarbeet_UnitedKingdom_to_all1)*100)

oilpalm_Indonesia_to_uk = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "Indonesia"
         & crop == "oilpalm") %>% 
  group_by(year) %>% 
  summarise(sum_oilpalm_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
oilpalm_Indonesia_to_uk1 = mean(oilpalm_Indonesia_to_uk$sum_oilpalm_peryear, na.rm = TRUE)

oilpalm_Indonesia_to_all =  cropyear_allreporters %>% 
  filter(partner == "Indonesia"
         & crop == "oilpalm") %>% 
  group_by(year) %>% 
  summarise(sum_oilpalm_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
oilpalm_Indonesia_to_all1 = mean(oilpalm_Indonesia_to_all$sum_oilpalm_peryear, na.rm = TRUE)

(oilpalm_proportion_ofexports_to_Indonesia = (oilpalm_Indonesia_to_uk1/oilpalm_Indonesia_to_all1)*100)

oilpalm_Malaysia_to_uk = cropyear_allreporters %>% 
  filter(reporter == "United Kingdom" & partner == "Malaysia"
         & crop == "oilpalm") %>% 
  group_by(year) %>% 
  summarise(sum_oilpalm_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
oilpalm_Malaysia_to_uk1 = mean(oilpalm_Malaysia_to_uk$sum_oilpalm_peryear, na.rm = TRUE)

oilpalm_Malaysia_to_all =  cropyear_allreporters %>% 
  filter(partner == "Malaysia"
         & crop == "oilpalm") %>% 
  group_by(year) %>% 
  summarise(sum_oilpalm_peryear = sum(dmi_origin_corrected, na.rm = TRUE))
oilpalm_Malaysia_to_all1 = mean(oilpalm_Malaysia_to_all$sum_oilpalm_peryear, na.rm = TRUE)

(oilpalm_proportion_ofexports_to_Malaysia = (oilpalm_Malaysia_to_uk1/oilpalm_Malaysia_to_all1)*100)

View(cropyear_allreporters)

# For Supporting Information:
cropyear_allreporters1 = cropyear_allreporters
cropyear_allreporters2 = cropyear_allreporters1 %>% 
  filter(reporter == "United Kingdom") %>% 
  filter(dmi_origin_corrected != 0) %>% 
  distinct(partner, .keep_all = TRUE)
cropyear_allreporters2

getwd()
write.csv(cropyear_allreporters2, paste0(biscuitdir, "crop_year_UK_allpartners.csv"))
