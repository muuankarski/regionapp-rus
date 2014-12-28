
library(dplyr)
library(stringr)
library(RCurl)

filelist <- list.files("~/data/obdx/rda_string", full.names = TRUE)

# Ladataan kaikki datat 
for (i in filelist) {
  load(i) 
}


library(RCurl)
GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/csv-codebook/master/obdx/all.csv")
varnames <- read.csv(text = GHurl)
varnames <- varnames[-1]
GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/csv-codebook/master/obdx/year_data.csv")
varnames2 <- read.csv(text = GHurl)
varnames <- rbind(varnames,varnames2)
varnames <- varnames[!duplicated(varnames[c("year","file","var")]),]

# yhdistetään vuosittaiset KVARTAALIDATAT

# funktiot

create_data <- function(idvars=c("region_en","year"),
                        valuevars=c("DOXODSN","RASSQ")) {
  where <- varnames[varnames$var %in% valuevars[1],c("year","file")]
  where$df <- paste0("df",where$year,"_",where$file,"_kv1")  
  # Koska vuosidatoissa eri objektinnimeämisidea
  where$df <- ifelse(where$file %in% c("FZ","FO","FG"), 
                     str_replace_all(where$df, "_kv1", ""),
                     where$df)
  where$df <- ifelse(where$file %in% c("FZ","FO","FG"), 
                     str_replace_all(where$df, "_", ""),
                     where$df)
  where$df <- ifelse(where$file %in% c("FZ","FO","FG"), 
                     str_replace_all(where$df, "df", "dfyear"),
                     where$df)
  
  # remove duplicates
  where <- where[!duplicated(where[c("year")]),]
  
  
  
  olist <- as.character(where$df)
  
  vars <- c(idvars,valuevars)
  
  f <- data.frame()
  for (i in 1:length(olist)) {
    dat <- get(olist[i])
    nimet <- names(dat)
    name.exists <- vars %in% nimet
    if (!(FALSE %in% name.exists)) {
      dat <- dat[c(vars)]
    } 
    if (FALSE %in% name.exists) {
      dat <- read.table(text = "", col.names = vars)
    }
    f <- rbind(f,dat)
  }
  f
}



library(ineq)
create_app_data_num <- function(data=dat,
                                measure="median",
                                varname="RASRESS") {
  
  FC <- data
  t <- data.frame()
  for (i in unique(FC$year)) {
    
    dat <- FC[FC$year == i ,]
    
    for (ii in unique(dat$region)) {
      datdat <- dat[dat$region == ii ,]
      if (measure == "gini") ie <- ineq(datdat$value)
      if (measure == "mean") ie <- mean(datdat$value)
      if (measure == "median") ie <- median(datdat$value)
      row <- data.frame(i,ii,ie)
      t <- rbind(t,row)
    }
  }
  names(t)[names(t)=="i"] <- "variable"  
  names(t)[names(t)=="ii"] <- "region"  
  names(t)[names(t)=="ie"] <- "value"  
  t$indicator_ru <- NA
  t$class <- "OBDX"
  t$unit <- "roubles"
  t$indicator_en <- paste0(measure," of ",unique(as.character(varnames[varnames$var == varname,"english"]))[1])
  t$indicator_ru <- unique(as.character(varnames[varnames$var == varname,"russian"]))[1]
  t 
}



create_app_data_cat <- function(data=dat,
                                varname="DOLGB") {
  
  FC <- data
  t <- data.frame()
  for (i in unique(FC$year)) {
    
    dat <- FC[FC$year == i ,]
    
    datdat <- as.data.frame(prop.table(table(dat$region,dat$value),1)*100)
    if (!("да" %in% as.character(unique(datdat$Var2)))) yes_value <- 1
    if ("да" %in% as.character(unique(datdat$Var2))) yes_value <- "да"
    row <- datdat[datdat$Var2 == yes_value, c("Var1","Freq")]
    row$Freq <- round(row$Freq,1)
    row$year <- i
    t <- rbind(t,row)
  }
  names(t)[names(t)=="year"] <- "variable"
  names(t)[names(t)=="Var1"] <- "region"  
  names(t)[names(t)=="Freq"] <- "value"  
  t$class <- "OBDX"
  t$unit <- "share of yes answers"
  t$indicator_en <- paste0("Share of Yes answer in ",unique(as.character(varnames[varnames$var == varname,"english"]))[1])
  t$indicator_ru <- unique(as.character(varnames[varnames$var == varname,"russian"]))[1]
  t 
}


## Muuttujien nimet github:sta


## Tulot

raw <- create_data(idvars=c("year","region","CHLICN","ID"),
                   valuevars=c("RASRESS", # Disposable resource
                                     "RASSQ", # Final consumption expenditure
                                     "POTRAS", # consumer spending
                                     "PROD", # Expenditure on food
                                     "PITRES", # Spending on food away from home
                                     "ALK", # Expenditures for the purchase of alcoholic beverages
                                     "NEPROD" # The cost of purchasing non-food products
                                     )) 

# RASRESS - Disposable resource
raw$value <- raw$RASRESS / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat1 <- create_app_data_num(data=FC,measure="mean",varname="RASRESS")
# RASRESS - Disposable resource GINI
raw$value <- raw$RASRESS / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat2 <- create_app_data_num(data=FC,measure="gini",varname="RASRESS")
# RASSQ - Final consumption expenditure
raw$value <- raw$RASSQ / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat3 <- create_app_data_num(data=FC,measure="mean",varname="RASSQ")
# RASSQ - Final consumption expenditure GINI
raw$value <- raw$RASSQ / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat4 <- create_app_data_num(data=FC,measure="gini",varname="RASSQ")

# "POTRAS", # consumer spending
raw$value <- raw$POTRAS / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat5 <- create_app_data_num(data=FC,measure="median",varname="POTRAS")

# "PROD", # Expenditure on food
raw$value <- raw$PROD / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat6 <- create_app_data_num(data=FC,measure="median",varname="PROD")

# "PITRES", # Spending on food away from home
raw$value <- raw$PITRES / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat7 <- create_app_data_num(data=FC,measure="median",varname="PITRES")

# "ALK", # Expenditures for the purchase of alcoholic beverages
raw$value <- raw$ALK / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat8 <- create_app_data_num(data=FC,measure="median",varname="ALK")

# "NEPROD")) # The cost of purchasing non-food products
raw$value <- raw$NEPROD / sqrt(raw$CHLICN)
FC <- raw[c("year","region","value","ID")]
dat9 <- create_app_data_num(data=FC,measure="median",varname="NEPROD")

raw <- create_data(idvars=c("year","region","ID"),
                   valuevars=c("OCHXOR", # The amount of money necessary for your household to live: very good
                               "XOROSHO", # The amount of money necessary for your household to live: good
                               "UDOVL"  #  The amount of money necessary for your household to live: Satisfactory 
                   )) 

# "OCHXOR", # The amount of money necessary for your household to live: very good
raw$value <- raw$OCHXOR
FC <- raw[c("year","region","value","ID")]
dat10 <- create_app_data_num(data=FC,measure="median",varname="OCHXOR")

# "XOROSHO", # The amount of money necessary for your household to live: good
raw$value <- raw$XOROSHO
FC <- raw[c("year","region","value","ID")]
dat11 <- create_app_data_num(data=FC,measure="median",varname="XOROSHO")

# "UDOVL"  #  The amount of money necessary for your household to live: Satisfactory 
raw$value <- raw$UDOVL
FC <- raw[c("year","region","value","ID")]
dat12 <- create_app_data_num(data=FC,measure="median",varname="UDOVL")

raw <- create_data(idvars=c("year","region","ID"),
                   valuevars=c("FINPOL", # Determination of the financial situation of the household 
                               "DOLGB", #The presence of the household balance of the loan, a loan or other debt obligations to: banks
                               "DOLGP",  #The presence of the household balance of the loan, a loan or other debt obligations before: now that is running
                               "DOLGO",  # The presence of the household balance of the loan, a loan or other debt obligations to: other organizations for services or goods (without bank)
                               "DOLGL",  #The presence of the household balance of the loan, a loan or other debt obligations to: individuals
                               "DOLGDR")) # The presence of the household balance of the loan, a loan or other debt obligations to: a different kind of debt

# "DOLGB", #The presence of the household balance of the loan, a loan or other debt obligations to: banks
raw$value <- raw$DOLGB
FC <- raw[c("year","region","value","ID")]
dat13 <- create_app_data_cat(data=FC,varname="DOLGB")

# "DOLGP",  #The presence of the household balance of the loan, a loan or other debt obligations before: now that is running
raw$value <- raw$DOLGP
FC <- raw[c("year","region","value","ID")]
dat14 <- create_app_data_cat(data=FC,varname="DOLGP")

# "DOLGO",  # The presence of the household balance of the loan, a loan or other debt obligations to: other organizations for services or goods (without bank)
raw$value <- raw$DOLGO
FC <- raw[c("year","region","value","ID")]
dat15 <- create_app_data_cat(data=FC,varname="DOLGO")

# "DOLGL",  #The presence of the household balance of the loan, a loan or other debt obligations to: individuals
raw$value <- raw$DOLGL
FC <- raw[c("year","region","value","ID")]
dat16 <- create_app_data_cat(data=FC,varname="DOLGL")

# "DOLGDR")) # The presence of the household balance of the loan, a loan or other debt obligations to: a different kind of debt
raw$value <- raw$DOLGDR
FC <- raw[c("year","region","value","ID")]
dat17 <- create_app_data_cat(data=FC,varname="DOLGDR")


# Erilaisia

raw <- create_data(idvars=c("year","region","ID"),
                   valuevars=c("PHOLVOD",
                               "PGORVOD",
                               "PELEKTR",
                               "PTEPL",
                               "PPROGR",
                               "XOLOD",
                               "NSANTEH",
                               "GRYZ",
                               "PLVODA",
                               "PLOBTR",
                               "SHUMS"))
                               #"DOSINT"))

# PHOLVOD Share of Yes answer in The presence of disruptions: cold water
raw$value <- raw$PHOLVOD
FC <- raw[c("year","region","value","ID")]
dat18 <- create_app_data_cat(data=FC,varname="PHOLVOD")

# PGORVOD Share of Yes answer in The presence of disruptions: hot water
raw$value <- raw$PGORVOD
FC <- raw[c("year","region","value","ID")]
dat19 <- create_app_data_cat(data=FC,varname="PGORVOD")

# PELEKTR Share of Yes answer in The presence of disruptions: electricity
raw$value <- raw$PELEKTR
FC <- raw[c("year","region","value","ID")]
dat20 <- create_app_data_cat(data=FC,varname="PELEKTR")

# PTEPL Share of Yes answer in The presence of disruptions: warm
raw$value <- raw$PTEPL
FC <- raw[c("year","region","value","ID")]
dat21 <- create_app_data_cat(data=FC,varname="PTEPL")

# PPROGR Share of Yes answer in The presence of disruptions: the reception of television programs
raw$value <- raw$PPROGR
FC <- raw[c("year","region","value","ID")]
dat22 <- create_app_data_cat(data=FC,varname="PPROGR")

# XOLOD Share of Yes answer in Inconvenience to stay, lack of heat
raw$value <- raw$XOLOD
FC <- raw[c("year","region","value","ID")]
dat23 <- create_app_data_cat(data=FC,varname="XOLOD")

# NSANTEH  Share of Yes answer in Inconvenience to stay, problems with sanitary equipment
raw$value <- raw$NSANTEH
FC <- raw[c("year","region","value","ID")]
dat24 <- create_app_data_cat(data=FC,varname="NSANTEH")

# GRYZ  Share of Yes answer in Inconvenience to stay, air pollution
raw$value <- raw$GRYZ
FC <- raw[c("year","region","value","ID")]
dat25 <- create_app_data_cat(data=FC,varname="GRYZ")

# PLVODA  Share of Yes answer in Inconvenience to stay poor water quality
raw$value <- raw$PLVODA
FC <- raw[c("year","region","value","ID")]
dat26 <- create_app_data_cat(data=FC,varname="PLVODA")

# PLOBTR  Share of Yes answer in Inconvenience to stay poor performance of public transport
raw$value <- raw$PLOBTR
FC <- raw[c("year","region","value","ID")]
dat27 <- create_app_data_cat(data=FC,varname="PLOBTR")

# SHUMS  Share of Yes answer in Inconvenience to stay restless neighbors
raw$value <- raw$SHUMS
FC <- raw[c("year","region","value","ID")]
dat28 <- create_app_data_cat(data=FC,varname="SHUMS")


df_obdx <- rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,
                 dat10,dat11,dat12,dat13,dat14,dat15,dat16,dat17,dat18,dat19,
                 dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28)

save(df_obdx, file="data/df_obdx.rda")




