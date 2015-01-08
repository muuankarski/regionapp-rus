## -------------------------------------------------------- ##
## Attribute data from Rosstat

dfA <- data.frame()


## --------------------------- ##
# Gross regional product
df <- read.csv("data/regional_product.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation
dl$indicator_en[dl$indicator_ru == "всего"] <- "all"
dl$indicator_en[dl$indicator_ru == "раздел d обрабатывающие производства"] <- "manufacturing"
dl$indicator_en[dl$indicator_ru == "раздел f строительство"] <- "construction"
dl$indicator_en[dl$indicator_ru == "раздел g оптовая и розничная торговля; ремонт  автотранспортных средств, мотоциклов, бытовых изделий и  предметов личного пользования"] <- "retail trade; repair of motor vehicles, motorcycles and personal and household goods for personal use"
dl$indicator_en[dl$indicator_ru == "раздел i транспорт и связь"] <- "Transport and communications"
dl$indicator_en[dl$indicator_ru == "раздел j финансовая деятельность"] <- "financial activities"
dl$indicator_en[dl$indicator_ru == "раздел k операции с недвижимым имуществом, аренда и  предоставление услуг"] <- "operations with real estate, renting and business activities"
dl$indicator_en[dl$indicator_ru == "Раздел L Государственное управление и обеспечение  военной безопасности"] <- "Public administration and defense"
dl$indicator_en[dl$indicator_ru == "Раздел M Образование"] <- "Education"
dl$indicator_en[dl$indicator_ru == "Раздел N Здравоохранение и предоставление социальных  услуг"] <- "Health and social services"
dl$indicator_en[dl$indicator_ru == "Раздел O Предоставление прочих коммунальных,  социальных и персональных услуг"] <- "Other community, social and personal services"
dl$indicator_en[dl$indicator_ru == "Раздел А Сельское хозяйство, охота и лесное хозяйство"] <- "Agriculture, hunting and forestry"
dl$indicator_en[dl$indicator_ru == "раздел в рыболовство, рыбоводство"] <- "Fishing and fish farming"
dl$indicator_en[dl$indicator_ru == "раздел е производство и распределение электроэнергии,  газа и воды"] <- "production and distribution of electricity, gas and water"
dl$indicator_en[dl$indicator_ru == "раздел н гостиницы и рестораны"] <- "Hotels and restaurants"
dl$indicator_en[dl$indicator_ru == "Раздел Р Деятельность домашних  хозяйств"] <- "Activities of households"
dl$indicator_en[dl$indicator_ru == "раздел с добыча полезных ископаемых"] <- "mining"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "thousand roubles"
# define class
dl$class <- "Gross Regional Product"

dfA <- rbind(dfA,dl)

source("data/trim_region_names.R")

dl <- dfA

## Relative values - data from census 2010

# 2010 census

download.file(url = "http://www.gks.ru/free_doc/new_site/perepis2010/croc/Documents/Vol1/pub-01-04.xlsx", destfile = "data/pub-01-04.xlsx")
library(gdata)
dat <- read.xls("data/pub-01-04.xlsx", sheet = 1, header = FALSE, skip=5)
dat <- dat[2:3]
names(dat) <- c("region",
                "value")

dat$value <- factor(dat$value)
dat$value <- as.numeric(levels(dat$value))[dat$value]
dat <- dat[!is.na(dat$value),]

dat$region <- as.character(dat$region) # trimmausta varten

dfA <- dat

source("data/trim_region_names.R")
dat <- dfA
dl2 <- merge(dl,dat,by="region")

dl2$value <- dl2$value.x/dl2$value.y
dl2$value.x <- NULL
dl2$value.y <- NULL

dl2$indicator_en <-  paste0(dl2$indicator_en," per capita")

## sitten per capita in dollars (2012)

# rouble rate for 2012 source: http://www.x-rates.com/average/?from=USD&to=RUB&amount=1.00&year=2012
rate2012 <- (31.353674*31 + 29.793101*29 +
               29.351398*31 + 29.481763*30 + 
               30.903382*31 + 32.815221*30 + 
               32.456826*31 + 31.934083*31 + 
               31.370721*30 + 31.122351*31 + 
               31.396497*30 + 30.712430*31) / 
  (31+29+31+30+31+30+31+31+30+31+30+31)

dl3 <- dl2

dl3$value <- dl3$value / rate2012 * 1000
dl3$indicator_en <-  paste0(dl3$indicator_en," in US$ 2012")

dfA <- rbind(dl,dl2,dl3)



## ------------------------------------------------------------------------------------------ ##
# Average per capita casch income
df <- read.csv("data/average_percapita_cash_income.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation
dl$indicator_en[dl$indicator_ru == "Денежные доходы (в среднем на душу)"] <- "Cash income (average per capita)"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Living Standards"


dfA <- rbind(dfA,dl)


## ------------------------------------------------------------------------------------------ ##
# Average per capita casch income
df <- read.csv("data/average_percapita_cash_income.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation
dl$indicator_en[dl$indicator_ru == "Денежные доходы (в среднем на душу)"] <- "Cash income (average per capita)"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Living Standards"

dfA <- rbind(dfA,dl)

## ------------------------------------------------------------------------------------------ ##
# Real Pensions
df <- read.csv("data/real_pensions.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation
dl$indicator_en[dl$indicator_ru == "Реальный размер назначенных пенсий"] <- "Real pensions"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Pensions"

dfA <- rbind(dfA,dl)


## ------------------------------------------------------------------------------------------ ##
# pension_ratio_to_subsistence_minimum.csv
df <- read.csv("data/pension_ratio_to_subsistence_minimum.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation
dl$indicator_en[dl$indicator_ru == "Соотношение среднего размера назначенных пенсий с величиной прожиточного минимума пенсионера"] <- "The ratio of average pension to the subsistence minimum for pensioners "
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Pensions"

dfA <- rbind(dfA,dl)


## ------------------------------------------------------------------------------------------ ##
# Average pension
df <- read.csv("data/pension_average.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation unique(dl$indicator_ru)
dl$indicator_en[dl$indicator_ru == "Средний размер назначенных пенсий"] <- "Average size of pensions"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Pensions"

dfA <- rbind(dfA,dl)

## ------------------------------------------------------------------------------------------ ##
# Average pension
df <- read.csv("data/pension_average.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation unique(dl$indicator_ru)
dl$indicator_en[dl$indicator_ru == "Средний размер назначенных пенсий"] <- "Average size of pensions"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Pensions"

dfA <- rbind(dfA,dl)


## ------------------------------------------------------------------------------------------ ##
# Public sector wages
df <- read.csv("data/public_sector_wages.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("indicator_ru","region"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation unique(dl$indicator_ru)
dl$indicator_en[dl$indicator_ru == "Среднемесячная заработная плата работников сельского хозяйства (без субъектов малого предпринимательства)"] <- "The average monthly wage of agricultural workers (except small businesses)"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников, занятых в сфере экономики региона"] <- "The average monthly nominal wage of workers in the economy of the region"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников государственных образовательных учреждений среднего профессионального образования"] <- "The average monthly nominal wage of employees of public educational institutions of secondary education"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников государственных (муниципальных) общеобразовательных учреждений"] <- "The average monthly nominal wage of employees of state (municipal) educational institutions"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников государственных (муниципальных)  учреждений физической культуры и спорта"] <- "The average monthly nominal wage of employees of state (municipal) institutions of physical culture and sport"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников государственных (муниципальных)  учреждений социальной защиты населения"] <- "The average monthly nominal wage of employees of state (municipal) institutions of social protection of the population"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников государственных (муниципальных)  учреждений образования"] <- "The average monthly nominal wage of employees of state (municipal) institutions of education"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников государственных (муниципальных)  учреждений культуры и искусства"] <- "The average monthly nominal wage of employees of state (municipal) institutions of culture and art"
dl$indicator_en[dl$indicator_ru == "Среднемесячная номинальная начисленная заработная плата работников государственных (муниципальных)  учреждений здравоохранения"] <- "The average monthly nominal wage of employees of state (municipal) health care facilities"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Public sector wages"

dfA <- rbind(dfA,dl)

## ------------------------------------------------------------------------------------------ ##
# Below subsistence minimum

df <- read.csv("data/below_subsistence_minimum.csv", header=TRUE)
library(reshape2)
names(df)[3] <- "unit"
df$unit <- "percent"
dl <- melt(df, id.vars=c("indicator_ru","region","unit"))
library(stringr)
# as.character
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- str_replace_all(dl$value, ",",".")
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation unique(dl$indicator_ru)
dl$indicator_en[dl$indicator_ru == "Численность населения с денежными доходами ниже величины прожиточного минимума, в процентах от общей численности населения"] <- "The number of people with incomes below the subsistence level, as a percentage of the total population"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "percent"
# define class
dl$class <- "Living Standards"

dfA <- rbind(dfA,dl)


## ------------------------------------------------------------------------------------------ ##
# subsistence minimum
# Average pension
df <- read.csv("data/subsistence_minimum.csv", header=TRUE)
library(reshape2)
dl <- melt(df, id.vars=c("region","unit"))
library(stringr)
# as.character
dl$indicator_ru <- "Величина прожиточного минимума"
dl$indicator_ru <- as.character(dl$indicator_ru)
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
dl$variable <- str_replace_all(dl$variable, "\\.00","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- str_replace_all(dl$value, ",",".")
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation unique(dl$indicator_ru)
dl$indicator_en[dl$indicator_ru == "Величина прожиточного минимума"] <- "Subsistence minimum"
# clean vars
dl$value[dl$value == ""] <- NA
#dl <- dl[!is.na(dl$value),]
dl$indicator_en[dl$indicator_en == ""] <- NA
dl$indicator_ru[dl$indicator_ru == ""] <- NA
#dl <- dl[!is.na(dl$variable),]
dl$variable[dl$variable == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Living Standards"

# only 1998 onwards, rouble inflation
dl <- dl[dl$variable >= 1998, ]

dfA <- rbind(dfA,dl)

## ------------------------------------------------------------------------------------------ ##
# Life expectancy
# Average pension

dat <- read.csv("/home/aurelius/workspace/russia/regionapp-rus/data/life_expectancy_region.csv")
dat <- dat[-1:-2,]
names(dat) <- c("region","sex","strata","god",1990:2013)

library(reshape2)
dl <- melt(dat, id.vars=c("region","sex","strata","god"))
library(zoo)
dl[dl==""] <- NA 
dl <- na.locf(dl)
dl <- dl[!is.na(dl$value),]

library(stringr)

dl$value <- str_replace_all(dl$value, ",", ".")
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
dl$value[dl$value == 0] <- NA
dat <- dl[!is.na(dl$value),]

dat$variable <- factor(dat$variable)
dat$variable <- as.numeric(levels(dat$variable))[dat$variable]

# -- both sexes
#
dl <- dat[dat$sex == "оба пола" & dat$strata == "все население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in urban & rural fo men & women"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d1 <- dl
#
dl <- dat[dat$sex == "оба пола" & dat$strata == "городское население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in urban areas for men & women"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d2 <- dl
#
dl <- dat[dat$sex == "оба пола" & dat$strata == "сельское население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in rural areas for men & women"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d3 <- dl
# -- men
#
dl <- dat[dat$sex == "мужчины" & dat$strata == "все население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in urban & rural for men"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d4 <- dl
#
dl <- dat[dat$sex == "мужчины" & dat$strata == "городское население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in urban areas for men"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d5 <- dl
#
dl <- dat[dat$sex == "мужчины" & dat$strata == "сельское население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in rural areas for men"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d6 <- dl
# -- women
#
dl <- dat[dat$sex == "женщины" & dat$strata == "все население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in urban & rural for women"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d7 <- dl
#
dl <- dat[dat$sex == "женщины" & dat$strata == "городское население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in urban areas for women"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d8 <- dl
#
dl <- dat[dat$sex == "женщины" & dat$strata == "сельское население",]
dl$strata <- NULL
dl$sex <- NULL
dl$god <- NULL
dl$indicator_en <- "Life expectancy in rural areas for women"
dl$indicator_ru <- dl$indicator_en
dl$unit <- "years"
dl$class <- "Population"
d9 <- dl

dl <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)

dfA <- rbind(dfA,dl)


## ------------------------------------------------------------------------------------------ ##
# Unemployment rate fedstat:sta termillä "Уровень безработицы (по методологии МОТ)"

df <- read.csv("data/unemployment.csv", header=TRUE, skip=2)
df <- df[-1,]
names(df)[names(df)=="X"] <- "region"
library(reshape2)
dl <- melt(df, id.vars=c("region"))
library(stringr)
# as.character
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- str_replace_all(dl$value, ",",".")
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation
dl$indicator_en <- "unemployment rate"
dl$indicator_ru <- "Уровень безработицы (по методологии МОТ)"
# clean vars
dl$value[dl$value == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "percent"
# define class
dl$class <- "Labour market"

dfA <- rbind(dfA,dl)

## ------------------------------------------------------------------------------------------ ##
# gross regional produc per capita fedstat:sta termillä "Объем валового регионального продукта в расчете на одного жителя субъекта Российской Федерации (значение показателя за год) "
## Changed the numerical values into numerical and language in English with dot as separator
df <- read.csv("data/regional_gdp.csv", header=TRUE, skip=2, sep=";")
df <- df[-1,]
df[[2]] <- NULL
names(df)[names(df)=="X"] <- "region"
library(reshape2)
dl <- melt(df, id.vars=c("region"))
library(stringr)
# as.character
dl$region <- as.character(dl$region)
dl$variable <- as.character(dl$variable)
# remove whitespace
dl$region <- str_trim(dl$region)
# remove X and dots
dl$variable <- str_replace_all(dl$variable, "X","")
# variable to numeric
dl$variable <- factor(dl$variable)
dl$variable <- as.numeric(levels(dl$variable))[dl$variable]
# value to numeric
dl$value <- str_replace_all(dl$value, ",",".")
dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
# translation
dl$indicator_en <- "gross regional product per capita in roubles"
dl$indicator_ru <- "Объем валового регионального продукта в расчете на одного жителя субъекта Российской Федерации (значение показателя за год)"
# clean vars
dl$value[dl$value == ""] <- NA
dl <- na.omit(dl)

# define unit
dl$unit <- "roubles"
# define class
dl$class <- "Gross Regional Product"

dfA <- rbind(dfA,dl)






# save data for merging
df_rosstat <- dfA
save(df_rosstat, file="data/df_rosstat.rda")
rm(dfA)
rm(df_rosstat)

