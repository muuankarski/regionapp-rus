## fixed over time

## Presidential election 2012
df <- read.csv("data/election_presidential_2012.csv")
library(reshape2)

dl <- melt(df, id.vars="item")
library(stringr)
dl$variable <- str_replace_all(dl$variable, "\\.", " ") # dots into spaces
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_trim(dl$variable, side = "both") # whitespace from either end

names(dl)[names(dl)=="variable"] <- "region"
names(dl)[names(dl)=="item"] <- "indicator_en"

dl$indicator_ru <- NA
dl$unit <- "percent"
dl$class <- "presidential election 2012"

dlx <- dl

dl <- data.frame()
for (i in 1992:2014) {
  dd <- dlx
  dd$variable <- i
  dl <- rbind(dl,dd)
}

# remove other items but voting shares
table(dl$indicator_en)
dl <- dl[ with(dl, grepl("Share of", indicator_en)),]
dl$indicator_en <- factor(dl$indicator_en)


dfA <- data.frame()
dfA <- rbind(dfA,dl)

## ---------
## ---------

## Presidential election 2012
df <- read.csv("data/election_presidential_2008.csv")
library(reshape2)

dl <- melt(df, id.vars="item")
library(stringr)
dl$variable <- str_replace_all(dl$variable, "\\.", " ") # dots into spaces
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_trim(dl$variable, side = "both") # whitespace from either end

names(dl)[names(dl)=="variable"] <- "region"
names(dl)[names(dl)=="item"] <- "indicator_en"

dl$indicator_ru <- NA
dl$unit <- "percent"
dl$class <- "presidential election 2008"

dlx <- dl

dl <- data.frame()
for (i in 1992:2014) {
  dd <- dlx
  dd$variable <- i
  dl <- rbind(dl,dd)
}

# remove other items but voting shares
table(dl$indicator_en)
dl <- dl[ with(dl, grepl("Share of", indicator_en)),]
dl$indicator_en <- factor(dl$indicator_en)

dfA <- rbind(dfA,dl)

## ---------
## ---------

## Duma election 2011
df <- read.csv("data/election_duma_2011.csv")
library(reshape2)

dl <- melt(df, id.vars="item")
library(stringr)
dl$variable <- str_replace_all(dl$variable, "\\.", " ") # dots into spaces
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_trim(dl$variable, side = "both") # whitespace from either end

dl$item <- str_replace_all(dl$item, '"', '') # quotation marks off

names(dl)[names(dl)=="variable"] <- "region"
names(dl)[names(dl)=="item"] <- "indicator_en"

dl$indicator_ru <- NA
dl$unit <- "percent"
dl$class <- "duma election 2011"

dlx <- dl

dl <- data.frame()
for (i in 1992:2014) {
  dd <- dlx
  dd$variable <- i
  dl <- rbind(dl,dd)
}

# remove other items but voting shares
table(dl$indicator_en)
dl <- dl[ with(dl, grepl("Share of", indicator_en)),]
dl$indicator_en <- factor(dl$indicator_en)

dfA <- rbind(dfA,dl)

## ---------
## ---------

## Duma election 2007
df <- read.csv("data/election_duma_2007.csv")
library(reshape2)

dl <- melt(df, id.vars="item")
library(stringr)
dl$variable <- str_replace_all(dl$variable, "\\.", " ") # dots into spaces
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_replace_all(dl$variable, "  ", " ") # double space into single
dl$variable <- str_trim(dl$variable, side = "both") # whitespace from either end

dl$item <- str_replace_all(dl$item, '"', '') # quotation marks off

names(dl)[names(dl)=="variable"] <- "region"
names(dl)[names(dl)=="item"] <- "indicator_en"

dl$indicator_ru <- NA
dl$unit <- "percent"
dl$class <- "duma election 2007"

dlx <- dl

dl <- data.frame()
for (i in 1992:2014) {
  dd <- dlx
  dd$variable <- i
  dl <- rbind(dl,dd)
}

# remove other items but voting shares
table(dl$indicator_en)
dl <- dl[ with(dl, grepl("Share of", indicator_en)),]
dl$indicator_en <- factor(dl$indicator_en)

dfA <- rbind(dfA,dl)

## List of current heads of federal subjects of Russia
# http://en.wikipedia.org/wiki/List_of_current_heads_of_federal_subjects_of_Russia

library(XML)
theurl <- "http://en.wikipedia.org/wiki/Regional_parliaments_of_Russia"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

t <- tables[[which.max(n.rows)]]
t <- t[c(-1,-23,-33,-80.-82,-87,-91,-92),] # remove uselees rows

names(t)[1] <- "region"
df <- t[c(1,2,4:11)]
dl <- melt(df, id.vars="region")

dl$value <- as.factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]

dw <- dcast(dl, region  ~ variable, value.var="value")
dw[3] <- NULL

dw[3] <- dw[3] / dw[2] * 100
dw[4] <- dw[4] / dw[2] * 100
dw[5] <- dw[5] / dw[2] * 100
dw[6] <- dw[6] / dw[2] * 100
dw[7] <- dw[7] / dw[2] * 100
dw[8] <- dw[8] / dw[2] * 100
dw[9] <- dw[9] / dw[2] * 100

names(dw) <- c("region_en","Seats","united_russia","communist","just_russia","liberal_democrat","patriots","right_cause","yabloko")
dw[2] <- NULL

dl <- melt(dw, id.vars="region_en")

# some region names are badly formatted, need to fix a bit
dl$region_en <- as.character(dl$region_en)

dl$region_en[dl$region_en == "Buryatia"] <- "Buryat Republic"
dl$region_en[dl$region_en == "Chechnya"] <- "Chechen Republic"
dl$region_en[dl$region_en == "Сhukotka Autonomous Okrug"] <- "Chukotka Autonomous Okrug"
dl$region_en[dl$region_en == "Chuvashia"] <- "Chuvash Republic"
dl$region_en[dl$region_en == "Kabardino-Balkaria"] <- "Kabardino-Balkaria Republic"
dl$region_en[dl$region_en == "Karachay-Cherkessia"] <- "Karachay-Cherkess Republic"
dl$region_en[dl$region_en == "Khanty–Mansi Autonomous Okrug – Yugra"] <- "Khanty-Mansiysk Autonomous Okrug"
dl$region_en[dl$region_en == "Mari El"] <- "Mari El Republic"
dl$region_en[dl$region_en == "Nenets\nAutonomous Okrug"] <- "Nenets Autonomous District"
dl$region_en[dl$region_en == "Adygea"] <- "Republic of Adygea"
dl$region_en[dl$region_en == "Bashkortostan"] <- "Republic of Bashkortostan"
dl$region_en[dl$region_en == "Dagestan"] <- "Republic of Dagestan"
dl$region_en[dl$region_en == "Ingushetia"] <- "Republic of Ingushetia"
dl$region_en[dl$region_en == "Kalmykia"] <- "Republic of Kalmykia"
dl$region_en[dl$region_en == "Karelia"] <- "Republic of Karelia"
dl$region_en[dl$region_en == "Khakassia"] <- "Republic of Khakassia"
dl$region_en[dl$region_en == "Mordovia"] <- "Republic of Mordovia"
dl$region_en[dl$region_en == "North Ossetia–Alania"] <- "Republic of North Ossetia - Alania"
dl$region_en[dl$region_en == "Tatarstan"] <- "Republic of Tatarstan"
dl$region_en[dl$region_en == "Tuva"] <- "Republic of Tyva"
dl$region_en[dl$region_en == "Sakha"] <- "Sakha Republic"
dl$region_en[dl$region_en == "Saint Petersburg"] <- "St. Petersburg"
dl$region_en[dl$region_en == "Udmurtia"] <- "Udmurt Republic"



library(RCurl)
GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/data/master/russia/regionkey.csv")
dat <- read.csv(text = GHurl)

df <- merge(dl,dat,by="region_en")
dl <- df[c(2,3,5)]

dl$indicator_ru <- NA
dl$unit <- "percent"
dl$class <- "share of seats in regional parliaments"

names(dl)[names(dl)=="variable"] <- "indicator_en"
names(dl)[names(dl)=="russian"] <- "region"

dlx <- dl

dl <- data.frame()
for (i in 1992:2014) {
  dd <- dlx
  dd$variable <- i
  dl <- rbind(dl,dd)
}


dfA <- rbind(dfA,dl)


# save data for merging
df_election <- dfA
save(df_election, file="data/df_election.rda")
rm(dfA)
rm(df_election)



