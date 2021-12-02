################################################################################
#
#                         MCC-EUcityTRM
#
#                       Country level results
#
################################################################################

source("00_Packages_Parameters.R")

load("results/cityResults.RData")

#---------------------------
# Excess rates by country
#---------------------------

#----- Compute excess deaths by country and age group

# Aggregate AN estimates
attrcountry <- tapply(attrlist, cityageres[, c("agegroup", "CNTR_CODE")], 
  function(attr){
    est <- rowSums(sapply(attr, function(x) x$est[1,]))
    cntrarr <- sapply(attr, "[[", "sim", simplify = "array")
    sim <- apply(cntrarr, 1:2, sum)
    list(est = est, sim = sim)
  })

# Aggregate pop by country and age
popcountry <- aggregate(agepop ~ agegroup + CNTR_CODE, data = cityageres, sum)

# Compute excess rates
excesscountry <- Map(function(attr, pop) lapply(attr, "/", pop),
  attrcountry, popcountry$agepop)

# Extract point estimates and CIs
excessest <- t(sapply(excesscountry, "[[", "est")) * byrate
colnames(excessest) <- sprintf("rate_%s_est", colnames(excessest))
excessCIs <- t(sapply(excesscountry, 
  function(x) c(apply(x$sim, 2, quantile, c(.025, .975))))) * byrate
colnames(excessCIs) <- sprintf("rate_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

#----- Compute excess deaths by country and age group compared to total pop

# Total pop by country
cntrtotpop <- aggregate(agepop ~ CNTR_CODE, data = popcountry, sum)
names(cntrtotpop)[2] <- "pop"

# Compute excess rates
excesstotpop <- Map(function(attr, pop) lapply(attr, "/", pop),
  attrcountry, 
  cntrtotpop[match(popcountry$CNTR_CODE, cntrtotpop$CNTR_CODE),"pop"])

# Extract point estimates and CIs
excesstotpopest <- t(sapply(excesstotpop, "[[", "est")) * byrate
colnames(excesstotpopest) <- sprintf("ratetotpop_%s_est", 
  colnames(excesstotpopest))
excesstotpopCIs <- t(sapply(excesstotpop, 
  function(x) c(apply(x$sim, 2, quantile, c(.025, .975))))) * byrate
colnames(excesstotpopCIs) <- sprintf("ratetotpop_%s", 
  t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

#----- Put everything in a data.frame

# Bind them
countryageres <- cbind(popcountry, excessest, excessCIs,
  excesstotpopest, excesstotpopCIs)

# Add country name
countryageres$cntr_name <- eurcntr[match(countryageres$CNTR_CODE, eurcntr[,1]),2]

# Add region
countryageres$region <- regionlist[countryageres$CNTR_CODE]

#---------------------------
# Standardized rates
#---------------------------

#----- Compute standardized rates by country

# Aggregate by country
countrystd <- tapply(seq_len(nrow(popcountry)), popcountry$CNTR_CODE, 
  function(i){
    # Divide everything by pop
    agerates <- excesscountry[i]
    
    # Compute std rates
    stdest <- apply(sapply(agerates, "[[", "est"), 1, 
      weighted.mean, esptot[popcountry[i, "agegroup"]])
    stdsim <- apply(sapply(agerates, "[[", "sim", simplify = "array"), 1:2,
      weighted.mean, esptot[popcountry[i, "agegroup"]])
    
    # Summary
    c(stdest, t(apply(stdsim, 2, quantile, c(.025, .975)))) * byrate
  })

#----- Create results as a data.frame

# Reorganise as a data.frame
countryres <- data.frame(do.call(rbind, countrystd))
names(countryres) <- sprintf("stdrate_%s", c(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), paste, sep = "_")))

# Add country codes and names
countryres$CNTR_CODE <- names(countrystd)
countryres$cntr_name <- eurcntr[match(countryres$CNTR_CODE, eurcntr[,1]),2]

# Add pop
countryres <- merge(countryres, cntrtotpop)

# Add country name
countryres$cntr_name <- eurcntr[match(countryres$CNTR_CODE, eurcntr[,1]),2]

# Add region
countryres$region <- regionlist[countryres$CNTR_CODE]

#---------------------------
# Results by region
#---------------------------

#----- Compute excess deaths by region and age group

# Aggregate AN estimates
attrregion <- tapply(attrlist, cityageres[, c("agegroup", "region")], 
  function(attr){
    est <- rowSums(sapply(attr, function(x) x$est[1,]))
    cntrarr <- sapply(attr, "[[", "sim", simplify = "array")
    sim <- apply(cntrarr, 1:2, sum)
    list(est = est, sim = sim)
  })

# Aggregate pop by region and age
popregion <- aggregate(agepop ~ agegroup + region, data = cityageres, sum)

# Compute excess rates
excessregion <- Map(function(attr, pop) lapply(attr, "/", pop),
  attrregion, popregion$agepop)

# Extract point estimates and CIs
rateregion <- t(sapply(excessregion, "[[", "est")) * byrate
colnames(rateregion) <- sprintf("rate_%s_est", colnames(rateregion))
rateregCIs <- t(sapply(excessregion, 
  function(x) c(apply(x$sim, 2, quantile, c(.025, .975))))) * byrate
colnames(rateregCIs) <- sprintf("rate_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

#----- Compute excess deaths by region and age group compared to total pop

# Total pop by country
regtotpop <- aggregate(agepop ~ region, data = popregion, sum)
names(regtotpop)[2] <- "pop"

# Compute excess rates
excessregtotpop <- Map(function(attr, pop) lapply(attr, "/", pop),
  attrregion, 
  regtotpop[match(popregion$region, regtotpop$region),"pop"])

# Extract point estimates and CIs
rateregtotpop <- t(sapply(excessregtotpop, "[[", "est")) * byrate
colnames(rateregtotpop) <- sprintf("ratetotpop_%s_est", 
  colnames(rateregtotpop))
rateregtotpopCIs <- t(sapply(excessregtotpop, 
  function(x) c(apply(x$sim, 2, quantile, c(.025, .975))))) * byrate
colnames(rateregtotpopCIs) <- sprintf("ratetotpop_%s", 
  t(outer(c("total", "cold", "heat"), 
    c("low", "hi"), FUN = "paste", sep = "_")))

#----- Put everything in a data.frame

# Bind them
regionageres <- cbind(popregion, rateregion, rateregCIs,
  rateregtotpop, rateregtotpopCIs)

#---------------------------
# Standardized rates by region
#---------------------------

#----- Compute standardized rates by country

# Aggregate by country
regionstd <- tapply(seq_len(nrow(popregion)), popregion$region, 
  function(i){
    # Divide everything by pop
    agerates <- excessregion[i]
    
    # Compute std rates
    stdest <- apply(sapply(agerates, "[[", "est"), 1, 
      weighted.mean, esptot[popregion[i, "agegroup"]])
    stdsim <- apply(sapply(agerates, "[[", "sim", simplify = "array"), 1:2,
      weighted.mean, esptot[popregion[i, "agegroup"]])
    
    # Summary
    c(stdest, t(apply(stdsim, 2, quantile, c(.025, .975)))) * byrate
  })

#----- Create results as a data.frame

# Reorganise as a data.frame
regionres <- data.frame(do.call(rbind, regionstd))
names(regionres) <- sprintf("stdrate_%s", c(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), paste, sep = "_")))

# Add region
regionres$region <- rownames(regionres)

# Add pop
regionres <- merge(regionres, regtotpop)

#---------------------------
# Total for whole dataset
#---------------------------

#----- Compute excess deaths by region and age group

# Aggregate AN estimates
attrtot <- tapply(attrlist, cityageres$agegroup, 
  function(attr){
    est <- rowSums(sapply(attr, function(x) x$est[1,]))
    cntrarr <- sapply(attr, "[[", "sim", simplify = "array")
    sim <- apply(cntrarr, 1:2, sum)
    list(est = est, sim = sim)
  })

# Aggregate pop by age
popage <- aggregate(agepop ~ agegroup, data = cityageres, sum)

# Compute excess rates
excesstot <- Map(function(attr, pop) lapply(attr, "/", pop),
  attrtot, popage$agepop)

# Extract point estimates and CIs
ratetot <- t(sapply(excesstot, "[[", "est")) * byrate
colnames(ratetot) <- sprintf("rate_%s_est", colnames(ratetot))
ratetotCIs <- t(sapply(excesstot, 
  function(x) c(apply(x$sim, 2, quantile, c(.025, .975))))) * byrate
colnames(ratetotCIs) <- sprintf("rate_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

#----- Compute excess deaths by age group compared to total pop

# Compute excess rates
excesstotpop <- lapply(attrtot, lapply, "/", sum(popage[,2]))

# Extract point estimates and CIs
ratetotpop <- t(sapply(excesstotpop, "[[", "est")) * byrate
colnames(ratetotpop) <- sprintf("ratetotpop_%s_est", 
  colnames(ratetotpop))
ratetotpopCIs <- t(sapply(excesstotpop, 
  function(x) c(apply(x$sim, 2, quantile, c(.025, .975))))) * byrate
colnames(ratetotpopCIs) <- sprintf("ratetotpop_%s", 
  t(outer(c("total", "cold", "heat"), 
    c("low", "hi"), FUN = "paste", sep = "_")))

#----- Add to regional data.frame

# Bind them
totageres <- cbind(popage, ratetot, ratetotCIs,
  ratetotpop, ratetotpopCIs)
totageres$region <- "Total"

# Add to regional data.frame
regionageres <- rbind(regionageres, totageres)

#----- Compute total standardized rates

# Standardized rates
stdtot <- apply(sapply(excesstot, "[[", "est"), 1, 
  weighted.mean, esptot[popage[, "agegroup"]])

# Confidence intervals
stdsim <- apply(sapply(excesstot, "[[", "sim", simplify = "array"), 1:2,
  weighted.mean, esptot[popage[, "agegroup"]])
stdtotCIS <- t(apply(stdsim, 2, quantile, c(.025, .975)))

# Put into a df
totres <- as.data.frame(t(c(stdtot, stdtotCIS) * byrate))
names(totres) <- sprintf("stdrate_%s", c(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), paste, sep = "_")))

# Add region
totres$region <- "Total"

# Add pop
totres$pop <- sum(popage[,2])

# Add to regional results
regionres <- rbind(regionres, totres)