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
  totnat = sapply(dlist, function(d) sum(d$nat_main)),
  meannat = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$nat_main), sd(d$nat_main))}),
  totcvresp = sapply(dlist, function(d) sum(d$cvresp_main)),
  meancvresp = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$cvresp_main), sd(d$cvresp_main))}),
  totcvd = sapply(dlist, function(d) sum(d$cvd_main)),
  meancvd = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$cvd_main), sd(d$cvd_main))}),
  totresp = sapply(dlist, function(d) sum(d$resp_main)),
  meanresp = sapply(dlist, function(d) {
    sprintf("%2.1f (%2.1f)", mean(d$resp_main), sd(d$resp_main))})
)

write.table(tab, file = "figures/UKdeathdesc.csv", sep = ",", quote = F,
  row.names = F, col.names = T)

#-------------------------------
# Tables 8 & 9 Cold and heat effects for main model
#-------------------------------

#----- Extract percentiles

# Continental
contRR <- lapply(continentERF[["10"]], function(x){
  with(x$main, cbind(allRRfit, allRRlow, allRRhigh)[predvar %in% resultper,])
})

# Country specific
countryRR <- lapply(countryERF[["10"]], function(x){
  lapply(x$main, function(y){
    cbind(y$allRRfit, y$allRRlow, y$allRRhigh)[y$predvar %in% resultper,]
  })
})

#----- Table 8
table8 <- rbind(
  sapply(contRR, function(x){
    sprintf("%1.2f (%1.2f - %1.2f)", x[1,1], x[1,2], x[1,3])
  }),
  sapply(countryRR, sapply, function(x){
    sprintf("%1.2f (%1.2f - %1.2f)", x[1,1], x[1,2], x[1,3])
  })
)
table8 <- cbind(c("Continental", unique(as.character(citydesc$countryname))),
  table8)

# Save
write.table(table8, file = "figures/Table8.csv", sep = ",", quote = F,
  row.names = F)

#----- Table 9
table9 <- rbind(
  sapply(contRR, function(x){
    sprintf("%1.2f (%1.2f - %1.2f)", x[2,1], x[2,2], x[2,3])
  }),
  sapply(countryRR, sapply, function(x){
    sprintf("%1.2f (%1.2f - %1.2f)", x[2,1], x[2,2], x[2,3])
  })
)
table9 <- cbind(c("Continental", unique(as.character(citydesc$countryname))),
  table9)

# Save
write.table(table9, file = "figures/Table9.csv", sep = ",", quote = F,
  row.names = F)