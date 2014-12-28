# 2010 census

download.file(url = "http://www.gks.ru/free_doc/new_site/perepis2010/croc/Documents/Vol1/pub-01-04.xlsx", destfile = "data/pub-01-04.xlsx")
library(gdata)
dat <- read.xls("data/pub-01-04.xlsx", sheet = 1, header = FALSE, skip=5)
dat <- dat[-1]
names(dat) <- c("region",
                "men and women in city and country in 2010",
                "men in city and country in 2010",
                "women in city and country in 2010",
                "men and women in city in 2010",
                "men in city in 2010",
                "women in city in 2010",
                "men and women in country in 2010",
                "men in country in 2010",
                "women in country in 2010")

library(reshape2)
dl <- melt(dat, id.vars="region")

dl$value <- factor(dl$value)
dl$value <- as.numeric(levels(dl$value))[dl$value]
dl <- dl[!is.na(dl$value),]

# vars
names(dl)[names(dl)=="variable"] <- "indicator_en"
dl$indicator_ru <- NA
# define unit
dl$unit <- "individuals"
# define class
dl$class <- "Population"

dlx <- dl

dl <- data.frame()
for (i in 1992:2014) {
  dd <- dlx
  dd$variable <- i
  dl <- rbind(dl,dd)
}

df_census <- dl

save(df_census, file="data/df_census.rda")
