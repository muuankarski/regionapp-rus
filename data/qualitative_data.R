## fixed over time

dfA <- data.frame()


## By-the-sea
df <- read.csv("data/qualit_by-the-sea.csv")

dl <- data.frame()
for (i in 1992:2014) {
  dd <- df
  dd$variable <- i
  dl <- rbind(dl,dd)
}

dfA <- rbind(dfA,dl)


## variates over time

# party in power



# save data for merging
df_qualitative <- dfA
save(df_qualitative, file="data/df_qualitative.rda")
rm(dfA)
rm(df_qualitative)
