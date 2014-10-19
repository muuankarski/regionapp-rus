# # --------------------------- ##
# # From rustfare
# 
# head(dfA)
# 
# ## ----------------------------
# # Infant mortality
# library(rustfare)
# df <- GetRosstat("infant_mortality_rate", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "year"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # average_percapita_income
# df <- GetRosstat("average_percapita_income", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# library(stringr)
# dl$region <- str_replace_all(dl$region, "2)","")
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "roubles"
# dl$class <- "Living Standards"
# 
# dfA <- rbind(dfA,dl)
# 
# 
# ## ----------------------------
# # gross_regional_product
# df <- GetRosstat("gross_regional_product", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "millions roubles"
# dl$class <- "Gross Regional Product"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # population_total
# df <- GetRosstat("population_total", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "thousands"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # population_urban
# df <- GetRosstat("population_urban", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "thousands"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # population_rural
# df <- GetRosstat("population_rural", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "thousands"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # life_expectancy_total
# df <- GetRosstat("life_expectancy_total", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "year"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # life_expectancy_women
# df <- GetRosstat("life_expectancy_women", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "year"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # life_expectancy_men
# df <- GetRosstat("life_expectancy_men", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "year"
# dl$class <- "Population"
# 
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # crude_birth_rate
# df <- GetRosstat("crude_birth_rate", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "rate"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# 
# ## ----------------------------
# # mortality_rate
# df <- GetRosstat("mortality_rate", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "rate"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # infant_mortality_rate
# df <- GetRosstat("infant_mortality_rate", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "rate"
# dl$class <- "Population"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # average_nominal_monthly_salary
# df <- GetRosstat("average_nominal_monthly_salary", "region")
# dl <- df[c(1,3,4,7)]
# names(dl)[2] <- "variable"
# names(dl)[4] <- "indicator_en"
# dl$indicator_ru <- dl$indicator_en
# dl$unit <- "roubles"
# dl$class <- "Living Standards"
# 
# dfA <- rbind(dfA,dl)
# 
# ## ----------------------------
# # average_size_of_pensions
# # df <- GetRosstat("average_size_of_pensions", "region")
# # dl <- df[c(1,3,4,7)]
# # names(dl)[2] <- "variable"
# # library(stringr)
# # dl$unit <- "roubles"
# # 
# # dfA <- rbind(dfA,dl)
# 
