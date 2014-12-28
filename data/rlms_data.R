
load_rlms <- function(wave,sample,level) {
  # source http://stackoverflow.com/questions/2520780/determining-name-of-object-loaded-in-r
  temp.space <- new.env()
  if (level =="individual" & sample == "rep") string2 <- "i_os23"
  if (level =="individual" & sample == "all") string2 <- "iall23"
  if (level =="household"  & sample == "rep") string2 <- "h_os23"
  if (level =="household"  & sample == "all") string2 <- "hall23"
  # year
  if (nchar(wave) == 1) wave <- paste0("0",wave)
  string1 <- paste0("r",wave)
  filename <- paste0(string1,string2)
  bar <- load(paste0(filename,".rda"), envir = temp.space)
  bar <- get(bar, envir = temp.space)
  library(stringr)
  names(bar) <- str_replace_all(names(bar), "^.", "")
  assign(filename, bar, envir = globalenv()) 
  rm(temp.space)
  rm(bar)
}

# for (i in 5:22) {
#   load_rlms(wave=i,sample="rep",level="household")
# }

# for (i in 5:22) {
#   load_rlms(wave=i,sample="rep",level="individual")
# }


# lets determine the identical variable names across waves

### ------------- ALL HOUSELDS INTO SINGLE DATA ------------------- ###

setwd("~/data/rlms/rda")

for (i in 5:22) {
  load_rlms(wave=i,sample="all",level="household")
}

## all household files with all observations 
obj.ls.hall <- grep(x= ls(pos=1),
                    pattern="hall",
                    value=TRUE)
## create character vectors out of variable names of each datasets
vec <- vector()
lis <- list()
for (i in 1:length(obj.ls.hall)) {
  dat <- get(obj.ls.hall[i], envir = globalenv())
  nimet <- names(dat)
  assign(paste0("nimi_",obj.ls.hall[i]), nimet, envir = globalenv())
}

obj.ls.nimi <- grep(x= ls(pos=1),
                    pattern="nimi_",
                    value=TRUE)


common_vars_hh <- Reduce(intersect, list(nimi_r05hall23,nimi_r06hall23,
                                      nimi_r07hall23,nimi_r08hall23,
                                      nimi_r09hall23,nimi_r10hall23,
                                      nimi_r11hall23,nimi_r12hall23,
                                      nimi_r13hall23,nimi_r14hall23,
                                      nimi_r15hall23,nimi_r16hall23,
                                      nimi_r17hall23,nimi_r18hall23,
                                      nimi_r19hall23,nimi_r20hall23,
                                      nimi_r21hall23,nimi_r22hall23))

datalst <- str_replace_all(obj.ls.nimi, "nimi_", "")

df <- data.frame()
for (i in 1:length(datalst)) {
  dat <- get(datalst[i], envir = globalenv())
  dat <- dat[c(common_vars_hh)]
  dat$wave <- as.character(datalst[i])
  df <- rbind(df,dat)
}

hall <- df
### ----------------------------------------------------- ###


### ------------- ALL INDIVIDUALS INTO SINGLE DATA ------------------- ###

#rm(list=setdiff(ls(), c("load_rlms","hall","common_vars_hh")))

for (i in 5:22) {
  load_rlms(wave=i,sample="all",level="individual")
}

## all household files with all observations 
obj.ls.iall <- grep(x= ls(pos=1),
                    pattern="iall",
                    value=TRUE)
## create character vectors out of variable names of each datasets
vec <- vector()
lis <- list()
for (i in 1:length(obj.ls.iall)) {
  dat <- get(obj.ls.iall[i], envir = globalenv())
  nimet <- names(dat)
  assign(paste0("nimi_",obj.ls.iall[i]), nimet, envir = globalenv())
}

obj.ls.nimi <- grep(x= ls(pos=1),
                    pattern="nimi_",
                    value=TRUE)


common_vars_in <- Reduce(intersect, list(nimi_r05iall23,nimi_r06iall23,
                                      nimi_r07iall23,nimi_r08iall23,
                                      nimi_r09iall23,nimi_r10iall23,
                                      nimi_r11iall23,nimi_r12iall23,
                                      nimi_r13iall23,nimi_r14iall23,
                                      nimi_r15iall23,nimi_r16iall23,
                                      nimi_r17iall23,nimi_r18iall23,
                                      nimi_r19iall23,nimi_r20iall23,
                                      nimi_r21iall23,nimi_r22iall23))

datalst <- str_replace_all(obj.ls.nimi, "nimi_", "")

df <- data.frame()
for (i in 1:length(datalst)) {
  dat <- get(datalst[i], envir = globalenv())
  dat <- dat[c(common_vars_in)]
  dat$wave <- as.character(datalst[i])
  df <- rbind(df,dat)
}

iall <- df
rm(list=setdiff(ls(), c("load_rlms","hall","iall","common_vars_hh","common_vars_in")))

### ----------------------------------------------------- ###


# create a new region variable to match with statistical data

source("~/workspace/russia/regionapp-rus/data/rlms_functions.R")


# recode the regions for region-app
hall <- recode_region(data=hall)
iall <- recode_region(data=iall)

# recode the wave for region-app
hall <- recode_wave(data=hall, level="household", sample="all")
iall <- recode_wave(data=iall, level="individual", sample="all")


### --------

load("/home/aurelius/data/rlms/rda/r22hall23.rda")
load("/home/aurelius/data/rlms/rda/r22iall23.rda")
hall <- r22hall25a
iall <- r22iall25a
names(hall) <- str_replace_all(names(hall), "^.", "")

hall <- recode_region(data=hall)
#hall <- recode_wave(data=hall, level="household", sample="all")

tbl_iall <- function(i) {
  print(table(iall[i], useNA = "ifany"))
  print(names(iall)[i])
}

tbl_hall <- function(i) {
  print(table(hall[i], useNA = "ifany"))
  print(names(hall)[i])
}

for (i in ncol(hall[1:10])) {
  table(hall[1])
}


key_file <- read.csv("/home/aurelius/data/csv-codebook/rlms/rlms-household-22.csv")
library(stringr)
key_file$varcode <- str_replace_all(key_file$varcode, "^.", "")

df <- data.frame()

hall1 <- hall[c("e1.54a","e1.54b","e1.54c")]

var <- as.character(unlist(hall[i]))
var <- as.numeric(unlist(hall[i]))

for (i in 1:ncol(hall1)) {
  
  
  #if (is.atomic(var)) {
  library(plyr)
  hall$year <- 2013
  dw <- ddply(hall,~russian + year,summarise,median=median(var, na.rm = TRUE))
  names(dw)[names(dw)=="median"] <- "value"
  names(dw)[names(dw)=="russian"] <- "region"
  names(dw)[names(dw)=="year"] <- "variable"
  
  dw$indicator_en <- as.character(unlist(key_file[key_file$varcode == names(hall)[i],][3]))[1]
  dw$indicator_ru <- as.character(unlist(key_file[key_file$varcode == names(hall)[i],][2]))[1]
  dw$unit <- "median"
  dw$class <- "rlms"

  df <- rbind(df,dw)
}

df_rlms <- df
#names(df_rlms)[names(df_rlms)=="Var1"] <- "variable"
#df_rlms$variable <- as.numeric(levels(df_rlms$variable))[df_rlms$variable]
df_rlms <- df_rlms[!is.na(df_rlms$value),]

setwd("~/workspace/russia/regionapp-rus")
save(df_rlms, file="data/df_rlms.rda")

#### -------------------------------------------------



head(redid_h)


df <- data.frame()

## ---- central heating   -------- ##
hall$var <- rec_yesno(hall$c7.1)
hall$var[hall$var == "no opinion"] <- NA
hall$var[hall$var == "missing"] <- NA

tbl <- as.data.frame(table(hall$year,hall$russian,hall$var))
library(reshape2)
dw <- dcast(tbl, Var1 + Var2 ~ Var3, value.var="Freq")
dw$yes <- (dw$yes / (dw$yes + dw$no))  * 100
dw$no <- NULL
names(dw)[names(dw)=="yes"] <- "value"
names(dw)[names(dw)=="Var2"] <- "region"
dw$indicator_en <- "in your house has central heating from CHP boiler?"
dw$indicator_ru <- NA
dw$unit <- "yes share"
dw$class <- "rlms"

df <- rbind(df,dw)
## ------------------------------- ##

## ---- HOT water supply -------- ##
hall$var <- rec_yesno(hall$c7.3)
hall$var[hall$var == "no opinion"] <- NA
hall$var[hall$var == "missing"] <- NA

tbl <- as.data.frame(table(hall$year,hall$russian,hall$var))
library(reshape2)
dw <- dcast(tbl, Var1 + Var2 ~ Var3, value.var="Freq")
dw$yes <- (dw$yes / (dw$yes + dw$no))  * 100
dw$no <- NULL
names(dw)[names(dw)=="yes"] <- "value"
names(dw)[names(dw)=="Var2"] <- "region"
dw$indicator_en <- "in your home has hot water?"
dw$indicator_ru <- NA
dw$unit <- "yes share"
dw$class <- "rlms"

df <- rbind(df,dw)
## ------------------------------- ##

## ---- gas grid -------- ##
# hall$var <- rec_yesno(hall$c7.7)
# hall$var[hall$var == "no opinion"] <- NA
# hall$var[hall$var == "missing"] <- NA
# 
# tbl <- as.data.frame(table(hall$year,hall$russian,hall$var))
# library(reshape2)
# dw <- dcast(tbl, Var1 + Var2 ~ Var3, value.var="Freq")
# dw$yes <- (dw$yes / (dw$yes + dw$no))  * 100
# dw$no <- NULL
# names(dw)[names(dw)=="yes"] <- "value"
# names(dw)[names(dw)=="Var2"] <- "region"
# dw$indicator_en <- "Do you have a main gas grid house, not bottled gas?"
# dw$indicator_ru <- NA
# dw$unit <- "yes share"
# dw$class <- "rlms"
# 
# df <- rbind(df,dw)
## ------------------------------- ##

## ----  centralized sewerage -------- ##
hall$var <- rec_yesno(hall$c7.5)
hall$var[hall$var == "no opinion"] <- NA
hall$var[hall$var == "missing"] <- NA

tbl <- as.data.frame(table(hall$year,hall$russian,hall$var))
library(reshape2)
dw <- dcast(tbl, Var1 + Var2 ~ Var3, value.var="Freq")
dw$yes <- (dw$yes / (dw$yes + dw$no))  * 100
dw$no <- NULL
names(dw)[names(dw)=="yes"] <- "value"
names(dw)[names(dw)=="Var2"] <- "region"
dw$indicator_en <- "in your house in a centralized sewerage system?"
dw$indicator_ru <- NA
dw$unit <- "yes share"
dw$class <- "rlms"

df <- rbind(df,dw)
## ------------------------------- ##

## ----  centralized sewerage -------- ##
hall$var <- rec_yesno(hall$c9.3.2a)
hall$var[hall$var == "no opinion"] <- NA
hall$var[hall$var == "missing"] <- NA

tbl <- as.data.frame(table(hall$year,hall$russian,hall$var))
library(reshape2)
dw <- dcast(tbl, Var1 + Var2 ~ Var3, value.var="Freq")
dw$yes <- (dw$yes / (dw$yes + dw$no))  * 100
dw$no <- NULL
names(dw)[names(dw)=="yes"] <- "value"
names(dw)[names(dw)=="Var2"] <- "region"
dw$indicator_en <- "in your house in a centralized sewerage system?"
dw$indicator_ru <- NA
dw$unit <- "yes share"
dw$class <- "rlms"

df <- rbind(df,dw)
## ------------------------------- ##



df_rlms <- df
names(df_rlms)[names(df_rlms)=="Var1"] <- "variable"
df_rlms$variable <- as.numeric(levels(df_rlms$variable))[df_rlms$variable]
df_rlms <- df_rlms[!is.na(df_rlms$value),]
save(df_rlms, file="data/df_rlms.rda")