# library(rgdal) #for reading/writing geo files
# library(rgeos) #for simplification
# library(sp)
# 
# # Three data sources
# 
# # 1. shapefile
# map <- readOGR(dsn = "/home/aurelius/data/shapefiles/russia/gadm/RUS_adm_simple/", layer = "RUS_adm1")
# # ----- Transform to EPSG 4326 - WGS84 (required)
# map <- spTransform(map, CRS("+init=epsg:4326"))
# 
# save(map, file="data/map_adm1.rda")
# 
# 
# # 2. region key to match Rosstat data with shape
# library(RCurl)
# GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/data/master/russia/regionkey.csv")
# dat <- read.csv(text = GHurl)
# # dat <-  dat[dat$ID > 0,]  # remove moscow city 
# dat <- dat[!is.na(dat$ID),] # Russian Federation
# 
# save(dat, file="data/key_adm1.rda")
# 
# 
# 
# ###### 
# 
# # attribute_Data
# 
# source("data/rosstat_data.R") # if needed
# source("data/rustfare_data.R") # if needed
# source("data/qualitative_data.R") # if needed
# source("data/election_data.R") # if needed

load("data/df_rosstat.rda")
load("data/df_rustfare.rda")
load("data/df_qualitative.rda")
load("data/df_election.rda")
load("data/df_socpol.rda")
load("data/df_rlms.rda")
load("data/df_obdx.rda")
load("data/df_census.rda")

dfA <- rbind(df_rosstat,df_rustfare,df_qualitative,df_election,df_socpol,df_rlms,df_obdx,df_census)

rm(df_rosstat)
rm(df_rustfare)
rm(df_qualitative)
rm(df_election)
rm(df_socpol)
rm(df_rlms)

## --------------------------- ##
# Harmonize the region names
library(regioncoderus)
source("~/workspace/ropengov/regioncoderus/R/trim_region_names.R")
dfA$region <- trim_region_names_rus(dfA$region)
dfA$region_en <- regioncoderus(dfA$region, "region.name.russian","region.name.finnish")

# remove duplicates
dfA <- dfA[!duplicated(dfA[c("region","variable","indicator_en")]),]

# remove useless stuff

dfA <- dfA[dfA$region != "Сумма",]
dfA <- dfA[dfA$region != "Более аграрные регионы",]
dfA <- dfA[dfA$region != "социальное страхование",]

## --------------------------- ##
# Write the file
## --------------------------- ##
save(dfA, file="data/attribute_data.rda")
# # 
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# # below subsistence minimum
# df <- read.csv("~/workspace/russia/aspalter/data/below_subsistence_minimum.csv", skip=1)
# library(reshape2)
# names(df)[1] <- "region"
# names(df)[2] <- "unit"
# names(df)[3:24] <- 1992:2013
# df <- df[-1,]
# df.long <- melt(df, id.vars=c("region","unit"))
# library(stringr)
# df.long$value <- str_replace_all(df.long$value, ",",".")
# df.long$value <- factor(df.long$value)
# df.long$value <- as.numeric(levels(df.long$value))[df.long$value]
# df.long$variable <- as.numeric(levels(df.long$variable))[df.long$variable]
# df.long <- df.long[!is.na(df.long$value),]
# df.long$region <- str_trim(df.long$region)
# # add indicator
# df.long$indicator <- "below_subsistence_minimum"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## --------------------------- ##
# # internet_access
# df <- read.csv("~/workspace/russia/aspalter/data/internet_access.csv", skip=1)
# library(reshape2)
# names(df)[1] <- "region"
# names(df)[2] <- "unit"
# names(df)[3:11] <- 2005:2013
# df <- df[-1,]
# df.long <- melt(df, id.vars=c("region","unit"))
# library(stringr)
# df.long$value <- str_replace_all(df.long$value, ",",".")
# df.long$value <- factor(df.long$value)
# df.long$value <- as.numeric(levels(df.long$value))[df.long$value]
# df.long$variable <- as.numeric(levels(df.long$variable))[df.long$variable]
# df.long <- df.long[!is.na(df.long$value),]
# df.long$region <- str_trim(df.long$region)
# # add indicator
# df.long$indicator <- "internet_access"
# 
# dfA <- rbind(dfA,df.long)
# 
# 
# 
# 
# ## --------------------------- ##
# # Write the file
# ## --------------------------- ##
# 
# 
