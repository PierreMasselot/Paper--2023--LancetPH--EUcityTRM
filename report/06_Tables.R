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
  name = cities$cityname,
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

#-------------------------------
# Table 7: cold effect of main model
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
table7 <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
# pretty numbers
table7 <- formatC(table7, format = "f", digits = 2)
# Add country info
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(table7)[-1], citydesc$cityname)]
table7 <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  table7)
# Add column names
colnames(table7) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
table7 <- table7[,c(1:5, 12:14, 6:11)]

# Save
write.table(table7, file = "figures/Table7.csv", sep = ",", quote = F,
  row.names = F)

#-------------------------------
# Table 8: heat effect of main model
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
table8 <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
table8 <- formatC(table8, format = "f", digits = 2)
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(table8)[-1], citydesc$cityname)]
table8 <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  table8)
colnames(table8) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
table8 <- table8[,c(1:5, 12:14, 6:11)]

# Save
write.table(table8, file = "figures/Table8.csv", sep = ",", quote = F,
  row.names = F)

#-------------------------------
# Table 9: cold effect of cause-specific models - sensitivity analysis & 
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
table9 <- rbind(lagRR,
  t(do.call(rbind, effRR))[-1,]
)
table9 <- formatC(table9, format = "f", digits = 2)
colnames(table9) <- c(sapply(names(effRR), sprintf, fmt = "%s %s", 
  c("RR", "Low", "High")))
# reorder columns
table9 <- table9[,c(1:3, 10:12, 4:9)]

# Save
write.table(table9, file = "figures/Table9.csv", sep = ",", quote = F,
  row.names = T)


#-------------------------------
# Table 10: heat effect of cause-specific models - sensitivity analysis & 
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
table10 <- rbind(lagRR,
  t(do.call(rbind, effRR))[-1,]
)
table10 <- formatC(table10, format = "f", digits = 2)
colnames(table10) <- c(sapply(names(effRR), sprintf, fmt = "%s %s", 
  c("RR", "Low", "High")))
# reorder columns
table10 <- table10[,c(1:3, 10:12, 4:9)]

# Save
write.table(table10, file = "figures/Table10.csv", sep = ",", quote = F,
  row.names = T)

#-------------------------------
# Table 11: cold effect of main model - MMT as reference point
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
table11 <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
table11 <- formatC(table11, format = "f", digits = 2)
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(table11)[-1], citydesc$cityname)]
table11 <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  table11)
colnames(table11) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
table11 <- table11[,c(1:5, 12:14, 6:11)]

# Save
write.table(table11, file = "figures/Table11.csv", sep = ",", quote = F,
  row.names = F)

#-------------------------------
# Table 12: heat effect of main model - MMT as reference point
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
table12 <- rbind(c(contRR),
  t(do.call(rbind, countryRR))
)
table12 <- formatC(table12, format = "f", digits = 2)
tabcountry <- as.character(citydesc$countryname)[
  match(rownames(table12)[-1], citydesc$cityname)]
table12 <- cbind(c("Continental", tabcountry),
  c(nrow(citydesc), table(citydesc$country)[tabcountry]),
  table12)
colnames(table12) <- c("Geo", "Ncities", 
  sapply(colnames(contRR), sprintf, fmt = "%s %s", c("RR", "Low", "High")))
# reorder columns
table12 <- table12[,c(1:5, 12:14, 6:11)]

# Save
write.table(table12, file = "figures/Table12.csv", sep = ",", quote = F,
  row.names = F)