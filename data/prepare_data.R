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
# dat <-  dat[dat$ID > 0,]  # remove moscow city 
# dat <- dat[!is.na(dat$ID),] # Russian Federation
# 
# save(dat, file="data/key_adm1.rda")
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
# ## --------------------------- ##
# # From rustfare
# 
# ## ----------------------------
# # Infant mortality
# library(rustfare)
# df <- GetRosstat("infant_mortality_rate", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# df.long$unit <- "year"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # average_percapita_income
# df <- GetRosstat("average_percapita_income", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$region <- str_replace_all(df.long$region, "2)","")
# df.long$unit <- "roubles"
# 
# dfA <- rbind(dfA,df.long)
# 
# 
# ## ----------------------------
# # gross_regional_product
# df <- GetRosstat("gross_regional_product", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "millions roubles"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # population_total
# df <- GetRosstat("population_total", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "thousands"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # population_urban
# df <- GetRosstat("population_urban", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "thousands"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # population_rural
# df <- GetRosstat("population_rural", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "thousands"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # life_expectancy_total
# df <- GetRosstat("life_expectancy_total", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "years"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # life_expectancy_women
# df <- GetRosstat("life_expectancy_women", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "years"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # life_expectancy_men
# df <- GetRosstat("life_expectancy_men", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "years"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # crude_birth_rate
# df <- GetRosstat("crude_birth_rate", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "rate"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # crude_birth_rate
# df <- GetRosstat("crude_birth_rate", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "rate"
# 
# dfA <- rbind(dfA,df.long)
# 
# 
# ## ----------------------------
# # mortality_rate
# df <- GetRosstat("mortality_rate", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "rate"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # infant_mortality_rate
# df <- GetRosstat("infant_mortality_rate", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "rate"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # average_nominal_monthly_salary
# df <- GetRosstat("average_nominal_monthly_salary", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "roubles"
# 
# dfA <- rbind(dfA,df.long)
# 
# ## ----------------------------
# # average_size_of_pensions
# df <- GetRosstat("average_size_of_pensions", "region")
# df.long <- df[c(1,3,4,7)]
# names(df.long)[2] <- "variable"
# library(stringr)
# df.long$unit <- "roubles"
# 
# dfA <- rbind(dfA,df.long)
# 
# # crude_birth_rate
# # 
# # ## --------------------------- ##
# # # Average pension
# # df <- read.csv("~/workspace/russia/aspalter/data/pension_number.csv", skip=1, stringsAsFactors=F)
# # library(reshape2)
# # names(df)[1] <- "region"
# # names(df)[2] <- "unit"
# # names(df)[3:8] <- c(2008,2009,2011:2014) # 2010 missing?!
# # df <- df[-1:-2,]
# # df.long <- melt(df, id.vars=c("region","unit"))
# # library(stringr)
# # as.numeric(gsub(' ', '', df.long$value))
# # 
# # df.long$value <- str_replace_all(df.long$value, " ","")
# # df.long$value <- str_replace_all(df.long$value, ",",".")
# # 
# # df.long$value <- factor(df.long$value)
# # df.long$value <- as.numeric(levels(df.long$value))[df.long$value]
# # df.long$variable <- as.numeric(levels(df.long$variable))[df.long$variable]
# # df.long <- df.long[!is.na(df.long$value),]
# # df.long$region <- str_trim(df.long$region)
# # # add indicator
# # df.long$indicator <- "average_pension"
# # 
# # dfA <- rbind(dfA,df.long)
# 
# 
# ## --------------------------- ##
# # Write the file
# ## --------------------------- ##
# dfA$indicator_en <- factor(dfA$indicator_en)
# dfA$indicator_ru <- factor(dfA$indicator_ru)
# save(dfA, file="data/attribute_data.rda")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 3. Rosstat regional datas
# ## subsistence minimum
# # df <- read.csv("data/subsistence_minimum.csv", skip=3)
# # df <- df[-2,]
# # names(df) <- c("region","who",
# #                "kv1_1992","kv2_1992","kv3_1992","kv4_1992",
# #                "kv1_1993","kv2_1993","kv3_1993","kv4_1993",
# #                "kv1_1994","kv2_1994","kv3_1994","kv4_1994",
# #                "kv1_1995","kv2_1995","kv3_1995","kv4_1995",
# #                "kv1_1996","kv2_1996","kv3_1996","kv4_1996",
# #                "kv1_1997","kv2_1997","kv3_1997","kv4_1997",
# #                "kv1_1998","kv2_1998","kv3_1998","kv4_1998",
# #                "kv1_1999","kv2_1999","kv3_1999","kv4_1999",
# #                "kv1_2000","kv2_2000","kv3_2000","kv4_2000",
# #                "kv1_2001","kv2_2001","kv3_2001","kv4_2001",
# #                "kv1_2002","kv2_2002","kv3_2002","kv4_2002",
# #                "kv1_2003","kv2_2003","kv3_2003","kv4_2003",
# #                "kv1_2004","kv2_2004","kv3_2004","kv4_2004",
# #                "kv1_2005","kv2_2005","kv3_2005","kv4_2005",
# #                "kv1_2006","kv2_2006","kv3_2006","kv4_2006",
# #                "kv1_2007","kv2_2007","kv3_2007","kv4_2007",
# #                "kv1_2008","kv2_2008","kv3_2008","kv4_2008",
# #                "kv1_2009","kv2_2009","kv3_2009","kv4_2009",
# #                "kv1_2010","kv2_2010","kv3_2010","kv4_2010",
# #                "kv1_2011","kv2_2011","kv3_2011","kv4_2011",
# #                "kv1_2012","kv2_2012","kv3_2012","kv4_2012",
# #                "kv1_2013","kv2_2013","kv3_2003","kv4_2013",
# #                "kv1_2014")