################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Part 11: Aggregate city results by country and region
#
################################################################################

if (length(ls()) == 0){
  source("10_ResultsCityOverall.R")
}

#---------------------------
# Country-age results
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

# Extract AN estimates
attrest <- t(sapply(attrcountry, "[[", "est"))
colnames(attrest) <- sprintf("excess_%s_est", colnames(attrest))

# Compute CIs for AN
attrCIs <- t(sapply(attrcountry, 
  function(x) apply(x$sim, 2, quantile, c(.025, .975))))
colnames(attrCIs) <- sprintf("excess_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

#----- Compute excess rates by country and age group

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
countryageres <- cbind(popcountry, attrest, attrCIs, excessest, excessCIs,
  excesstotpopest, excesstotpopCIs)

# Add country name and region
countryageres <- merge(countryageres, 
  unique(metadata[,c("CNTR_CODE", "cntr_name", "region")]))

#---------------------------
# Country overall results
#---------------------------

#----- Aggregate age-group rates

# Sum excess and rates
countrysum <- aggregate(
  cbind(excess_total_est, excess_cold_est, excess_heat_est, 
    ratetotpop_total_est, ratetotpop_cold_est, ratetotpop_heat_est) ~ CNTR_CODE,
  countryageres, sum)
colnames(countrysum) <- gsub("ratetotpop", "rate", colnames(countrysum))

# Compute CIs for excess number
countryexcesssumCIs <- tapply(seq_along(attrcountry), popcountry$CNTR_CODE,
  function(i){
    totalsim <- Reduce("+", lapply(attrcountry[i], "[[", "sim"))
    c(apply(totalsim, 2, quantile, c(.025, .975)))
  }
)
countryexcesssumCIs <- do.call(rbind, countryexcesssumCIs)
colnames(countryexcesssumCIs) <- sprintf("excess_%s", 
  t(outer(c("total", "cold", "heat"), 
    c("low", "hi"), FUN = "paste", sep = "_")))

# Compute CIs for excess rates
countryratesumCIs <- tapply(seq_along(excesstotpop), popcountry$CNTR_CODE,
  function(i){
    totalsim <- Reduce("+", lapply(excesstotpop[i], "[[", "sim"))
    c(apply(totalsim, 2, quantile, c(.025, .975))) * byrate
  }
)
countryratesumCIs <- do.call(rbind, countryratesumCIs)
colnames(countryratesumCIs) <- sprintf("rate_%s", 
  t(outer(c("total", "cold", "heat"), 
    c("low", "hi"), FUN = "paste", sep = "_")))

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

# Reorganise as a data.frame
countrystd <- data.frame(do.call(rbind, countrystd))
names(countrystd) <- sprintf("stdrate_%s", c(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), paste, sep = "_")))

#----- Create results as a data.frame

# Put together in a df
countryres <- cbind(countrysum, countryexcesssumCIs, countryratesumCIs,
  countrystd)

# Add country names
countryres$cntr_name <- countryageres$cntr_name[
  match(countryres$CNTR_CODE, countryageres$CNTR_CODE)]

# Add pop
countryres <- merge(countryres, cntrtotpop)

# Add region
countryres$region <- factor(regionlist[as.character(countryres$CNTR_CODE)], 
  level = regord)

#----- Attributable fractions

# Compute total deaths by country
countrydeaths <- aggregate(death ~ CNTR_CODE, data = cityageres, sum)
countryres <- merge(countryres, countrydeaths, sort = F)

# Compute AFs
countryAFs <- sapply(countryres[grep("excess", names(countryres))], "/", 
  countryres$death, simplify = "data.frame") * 100

# Add to results data.frame
colnames(countryAFs) <- gsub("excess", "af", colnames(countryAFs))
countryres <- cbind(countryres, countryAFs)

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

# Extract excess deaths
attrregest <- t(sapply(attrregion, "[[", "est"))
colnames(attrregest) <- sprintf("excess_%s_est", colnames(attrregest))

# Compute CIs for excesses
attrregCIs <- t(sapply(attrregion, 
  function(x) apply(x$sim, 2, quantile, c(.025, .975))))
colnames(attrregCIs) <- sprintf("excess_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

#----- Compute excess rates by region and age group

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
regionageres <- cbind(popregion, attrregest, attrregCIs, rateregion, rateregCIs,
  rateregtotpop, rateregtotpopCIs)

#---------------------------
# Region results all ages
#---------------------------

#----- Aggregate age-group rates

# Sum excess and rates
regionsum <- aggregate(
  cbind(excess_total_est, excess_cold_est, excess_heat_est, 
    ratetotpop_total_est, ratetotpop_cold_est, ratetotpop_heat_est) ~ region,
  regionageres, sum)
colnames(regionsum) <- gsub("ratetotpop", "rate", colnames(regionsum))

# Compute CIs for excess number
regionexcesssumCIs <- tapply(seq_along(attrregion), popregion$region,
  function(i){
    totalsim <- Reduce("+", lapply(attrregion[i], "[[", "sim"))
    c(apply(totalsim, 2, quantile, c(.025, .975)))
  }
)
regionexcesssumCIs <- do.call(rbind, regionexcesssumCIs)
colnames(regionexcesssumCIs) <- sprintf("excess_%s", 
  t(outer(c("total", "cold", "heat"), 
    c("low", "hi"), FUN = "paste", sep = "_")))

# Compute CIs for excess rates
regionratesumCIs <- tapply(seq_along(excessregtotpop), popregion$region,
  function(i){
    totalsim <- Reduce("+", lapply(excessregtotpop[i], "[[", "sim"))
    c(apply(totalsim, 2, quantile, c(.025, .975))) * byrate
  }
)
regionratesumCIs <- do.call(rbind, regionratesumCIs)
colnames(regionratesumCIs) <- sprintf("rate_%s", 
  t(outer(c("total", "cold", "heat"), 
    c("low", "hi"), FUN = "paste", sep = "_")))

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

# Reorganise as a data.frame
regstdres <- data.frame(do.call(rbind, regionstd))
names(regstdres) <- sprintf("stdrate_%s", c(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), paste, sep = "_")))

#----- Create results as a data.frame

regionres <- cbind(regionsum, regionexcesssumCIs, 
  regionratesumCIs, regstdres)

# Add pop
regionres <- merge(regionres, regtotpop)

#---------------------------
# Total for whole dataset
#---------------------------

#----- Compute total excess deaths by age group

# Aggregate AN estimates
attrtot <- tapply(attrlist, cityageres$agegroup, 
  function(attr){
    est <- rowSums(sapply(attr, function(x) x$est[1,]))
    cntrarr <- sapply(attr, "[[", "sim", simplify = "array")
    sim <- apply(cntrarr, 1:2, sum)
    list(est = est, sim = sim)
  })

# Extract excess deaths
attrtotest <- t(sapply(attrtot, "[[", "est"))
colnames(attrtotest) <- sprintf("excess_%s_est", colnames(attrtotest))

# Compute CIs for excesses
attrtotCIs <- t(sapply(attrtot, 
  function(x) apply(x$sim, 2, quantile, c(.025, .975))))
colnames(attrtotCIs) <- sprintf("excess_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

#----- Compute total excess rates by age group

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
totageres <- cbind(popage, attrtotest, attrtotCIs, ratetot, ratetotCIs,
  ratetotpop, ratetotpopCIs)
totageres$region <- "Total"

# Add to regional data.frame
regionageres <- rbind(regionageres, totageres)

#----- Results for all ages

# Total excess
totexcess <- apply(sapply(attrtot, "[[", "est"), 1, sum)
names(totexcess) <- sprintf("excess_%s_est", names(totexcess))
totexcessCIs <- c(apply(Reduce("+", lapply(attrtot, "[[", "sim")), 2, 
  quantile, c(.025, .975)))
names(totexcessCIs) <- sprintf("excess_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

# Total rates
totrates <- apply(ratetotpop, 2, sum)
names(totrates) <- sprintf("rate_%s_est", c("total", "cold", "heat"))
totratesCIs <- apply(Reduce("+", lapply(excesstotpop, "[[", "sim")), 2, 
  quantile, c(.025, .975)) * byrate
names(totratesCIs) <- sprintf("rate_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

# Standardized rates
stdtot <- apply(sapply(excesstot, "[[", "est"), 1, 
  weighted.mean, esptot[popage[, "agegroup"]]) * byrate
names(stdtot) <- sprintf("stdrate_%s_est", names(stdtot))

# Confidence intervals
stdsim <- apply(sapply(excesstot, "[[", "sim", simplify = "array"), 1:2,
  weighted.mean, esptot[popage[, "agegroup"]])
stdtotCIs <- c(apply(stdsim, 2, quantile, c(.025, .975))) * byrate
names(stdtotCIs) <- sprintf("stdrate_%s", t(outer(c("total", "cold", "heat"), 
  c("low", "hi"), FUN = "paste", sep = "_")))

# Put into a df
totres <- as.data.frame(t(c(totexcess, totexcessCIs, totrates, totratesCIs, 
  stdtot, stdtotCIs)))

# Add region
totres$region <- "Total"

# Add pop
totres$pop <- sum(popage[,2])

# Add to regional results
regionres <- rbind(regionres, totres)

#----- Add attributable fraction to the totals

# Compute total deaths by region and total
regiondeaths <- aggregate(death ~ region, data = cityageres, sum)
regiondeaths <- rbind(regiondeaths, 
  data.frame(region = "Total", death = sum(regiondeaths$death)))
regionres <- merge(regionres, regiondeaths, sort = F)

# Compute AF
totalAFs <- sapply(regionres[grep("excess", names(regionres))], "/", regionres$death,
  simplify = "data.frame") * 100
colnames(totalAFs) <- gsub("excess", "af", colnames(totalAFs))

# Add to result data.frame
regionres <- cbind(regionres, totalAFs)
