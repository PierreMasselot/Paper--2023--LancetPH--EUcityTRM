################################################################################
#
#                       Exhaustion Level 1
#
#                        Result Tables
#
################################################################################



#-------------------------------
# Descriptive tables
#-------------------------------

tab <- data.frame(
  name = citydesc$cityname,
  period = sapply(dlist, function(d) sprintf("%i - %i", 
    min(d$year), max(d$year))),
  totnat = sapply(dlist, function(d) sum(d$nat_main, na.rm = T)),
  meannat = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$nat_main, na.rm = T), 
      sd(d$nat_main, na.rm = T))}),
  totcvresp = sapply(dlist, function(d) sum(d$cvresp_main, na.rm = T)),
  meancvresp = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$cvresp_main, na.rm = T),
      sd(d$cvresp_main, na.rm = T))}),
  totcvd = sapply(dlist, function(d) sum(d$cvd_main, na.rm = T)),
  meancvd = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$cvd_main, na.rm = T), 
      sd(d$cvd_main, na.rm = T))}),
  totresp = sapply(dlist, function(d) sum(d$resp_main, na.rm = T)),
  meanresp = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$resp_main, na.rm = T), 
      sd(d$resp_main, na.rm = T))})
)

write.table(tab, file = "figures/MCCdeathdesc.csv", sep = ",", quote = F,
  row.names = F, col.names = T)

tabtmean <- data.frame(
  name = citydesc$cityname,
  period = sapply(dlist, function(d) sprintf("%i - %i", 
    min(d$year), max(d$year))),
  mean = sapply(dlist, function(d){
    sprintf("%2.1f (%2.1f)", mean(d$tmean, na.rm = T), 
      sd(d$tmean, na.rm = T))}),
  summary = t(sapply(dlist, function(d) sprintf("%2.1f",
    quantile(d$tmean, c(0, .25, .5, .75, 1), na.rm = T))))
)

write.table(tabtmean, file = "figures/MCCtmean.csv", sep = ",", quote = F,
  row.names = F, col.names = T)

#-------------------------------
# Table 6: cold effect of main model
#-------------------------------

#----- Extract percentiles

# Continental
contRR <- sapply(continentERF_cold[["10"]], function(x){
  with(x$main, cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[1],])
})

# Country specific
countryRR <- lapply(countryERF_cold[["10"]], function(x){
  sapply(x$main, function(y){
    cbind(y$allRRfit, y$allRRlow, y$allRRhigh)[y$predvar %in% resultper[1],]
  })
})

#----- Create table
# Put data together
tableCold <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
# pretty numbers
tableCold <- formatC(tableCold, format = "f", digits = 2)
# Add country info
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(tableCold)[-1], citydesc$cityname)]
tableCold <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  tableCold)
# Add column names
colnames(tableCold) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
tableCold <- tableCold[,c(1:5, 12:14, 6:11)]

# Save
write.table(tableCold, file = "figures/Table6.csv", sep = ",", quote = F,
  row.names = F)

#-------------------------------
# Table 7: heat effect of main model
#-------------------------------

#----- Extract percentiles

# Continental
contRR <- sapply(continentERF_heat[["10"]], function(x){
  with(x$main, cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[2],])
})

# Country specific
countryRR <- lapply(countryERF_heat[["10"]], function(x){
  sapply(x$main, function(y){
    cbind(y$allRRfit, y$allRRlow, y$allRRhigh)[y$predvar %in% resultper[2],]
  })
})

#----- Create table
tableHeat <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
tableHeat <- formatC(tableHeat, format = "f", digits = 2)
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(tableHeat)[-1], citydesc$cityname)]
tableHeat <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  tableHeat)
colnames(tableHeat) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
tableHeat <- tableHeat[,c(1:5, 12:14, 6:11)]

# Save
write.table(tableHeat, file = "figures/Table7.csv", sep = ",", quote = F,
  row.names = F)

#-------------------------------
# Table 8: cold effect of cause-specific models - sensitivity analysis & 
#   effect modification
#-------------------------------

#----- Extract percentiles

# Lags
lagRR <- t(sapply(continentERF_cold, sapply, function(x){
  with(x$main, 
    cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[1],])
}))

# Effect modification
effRR <- lapply(continentERF_cold[["10"]], sapply, function(x){
  t(with(x, 
    cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[1],]))
})


#----- Create table
tableSensCold <- rbind(lagRR,
  t(do.call(rbind, effRR))[-1,]
)
tableSensCold <- formatC(tableSensCold, format = "f", digits = 2)
colnames(tableSensCold) <- c(sapply(names(effRR), sprintf, fmt = "%s %s", 
  c("RR", "Low", "High")))
# reorder columns
tableSensCold <- tableSensCold[,c(1:3, 10:12, 4:9)]

# Save
write.table(tableSensCold, file = "figures/Table8.csv", sep = ",", quote = F,
  row.names = T)


#-------------------------------
# Table 9: heat effect of cause-specific models - sensitivity analysis & 
#   effect modification
#-------------------------------

#----- Extract percentiles

# Lags
lagRR <- t(sapply(continentERF_heat, sapply, function(x){
  with(x$main, 
    cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[2],])
}))

# Effect modification
effRR <- lapply(continentERF_heat[["10"]], sapply, function(x){
  t(with(x, 
    cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[2],]))
})

#----- Create table
tableSensHeat <- rbind(lagRR,
  t(do.call(rbind, effRR))[-1,]
)
tableSensHeat <- formatC(tableSensHeat, format = "f", digits = 2)
colnames(tableSensHeat) <- c(sapply(names(effRR), sprintf, fmt = "%s %s", 
  c("RR", "Low", "High")))
# reorder columns
tableSensHeat <- tableSensHeat[,c(1:3, 10:12, 4:9)]

# Save
write.table(tableSensHeat, file = "figures/Table9.csv", sep = ",", quote = F,
  row.names = T)

#-------------------------------
# Table 10: cold effect of main model - MMT as reference point
#-------------------------------

#----- Extract percentiles

# Continental
contRR <- sapply(continentERF_mmp[["10"]], function(x){
  with(x$main, cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[1],])
})

# Country specific
countryRR <- lapply(countryERF_mmp[["10"]], function(x){
  sapply(x$main, function(y){
    cbind(y$allRRfit, y$allRRlow, y$allRRhigh)[y$predvar %in% resultper[1],]
  })
})

#----- Create table
tableMMTcold <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
tableMMTcold <- formatC(tableMMTcold, format = "f", digits = 2)
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(tableMMTcold)[-1], citydesc$cityname)]
tableMMTcold <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  tableMMTcold)
colnames(tableMMTcold) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
tableMMTcold <- tableMMTcold[,c(1:5, 12:14, 6:11)]

# Save
write.table(tableMMTcold, file = "figures/table10.csv", sep = ",", quote = F,
  row.names = F)

#-------------------------------
# Table 11: heat effect of main model - MMT as reference point
#-------------------------------

#----- Extract percentiles

# Continental
contRR <- sapply(continentERF_mmp[["10"]], function(x){
  with(x$main, cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper[2],])
})

# Country specific
countryRR <- lapply(countryERF_mmp[["10"]], function(x){
  sapply(x$main, function(y){
    cbind(y$allRRfit, y$allRRlow, y$allRRhigh)[y$predvar %in% resultper[2],]
  })
})

#----- Create table
tableMMTheat <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
tableMMTheat <- formatC(tableMMTheat, format = "f", digits = 2)
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(tableMMTheat)[-1], citydesc$cityname)]
tableMMTheat <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  tableMMTheat)
colnames(tableMMTheat) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
tableMMTheat <- tableMMTheat[,c(1:5, 12:14, 6:11)]

# Save
write.table(tableMMTheat, file = "figures/table11.csv", sep = ",", quote = F,
  row.names = F)