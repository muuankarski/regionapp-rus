## List of current heads of federal subjects of Russia
# http://en.wikipedia.org/wiki/List_of_current_heads_of_federal_subjects_of_Russia

library(XML)
theurl <- "http://www.socpol.ru/atlas/typology/Typology_tabl.htm"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

t <- tables[[which.max(n.rows)]]
names(t) <- c("X2008","X2009","X2009","X2010","X2009","X2010","X2010","X2006-2010","X2009","XXX")
t <- t[c(-1:-2),]
t <- t[c(-2:-3,-6,-10:-12,-26,-31:-33,-55,-71,-86:-88,-90,-98,-102:-168),] # remove uselees rows

names(t)[1] <- "region"
names(t)[2] <- "GRP_per_capita_%_of_the_national_average_2008"
names(t)[3] <- "GRP_per_capita_%_of_the_national_average_2009"
names(t)[4] <- "the_ratio_of_per_capita_income_to_substance_minimum_2009"
names(t)[5] <- "the_ratio_of_per_capita_income_to_substance_minimum_2010"
names(t)[6] <- "poverty_rate_2009"
names(t)[7] <- "ILO_unemployment_rate_2010"
names(t)[8] <- "population_density_per_sq_km2010"
names(t)[9] <- "avg_annual-rate_of_migration_growth_decline_2006_2010"
names(t)[10] <- "life_expectancy"

dl <- melt(t, id.vars="region")

head(dl)

dl$value <- as.factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]

library(stringr)
dl$region <- str_replace_all(dl$region, "\\n", " ") # dots into spaces
dl$region <- str_replace_all(dl$region, "\\r", " ") # dots into spaces
dl$region <- str_replace_all(dl$region, "  ", " ") # double space into single
dl$region <- str_replace_all(dl$region, "  ", " ") # double space into single
dl$region <- str_replace_all(dl$region, "  ", " ") # double space into single
dl$region <- str_trim(dl$region, side = "both") # whitespace from either end
dl$region <- str_replace_all(dl$region, "обл.", "область") # double space into single

dl$indicator_ru <- NA
dl$unit <- "percent"
dl$class <- "socpol-ru_regional_indicator"

names(dl)[names(dl)=="variable"] <- "indicator_en"

dlx <- dl

dl <- data.frame()
for (i in 1992:2014) {
  dd <- dlx
  dd$variable <- i
  dl <- rbind(dl,dd)
}

dfA <- data.frame()
dfA <- rbind(dfA,dl)

df_socpol <- dfA
save(df_socpol, file="data/df_socpol.rda")
rm(dfA)
rm(df_socpol)
