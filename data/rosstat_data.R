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
dl$unit <- "million roubles"
# define class
dl$class <- "Gross Regional Product "

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


## --------------------------- ##
# Write the file
## --------------------------- ##
# dfA$indicator_en <- factor(dfA$indicator_en)
# dfA$indicator_ru <- factor(dfA$indicator_ru)
save(dfA, file="data/attribute_data.rda")