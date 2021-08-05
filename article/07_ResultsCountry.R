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
attrcountry <- tapply(attrlist, cityres[, c("agegroup", "CNTR_CODE")], 
  function(attr){
    est <- rowSums(sapply(attr, function(x) x$est[1,]))
    cntrarr <- sapply(attr, "[[", "sim", simplify = "array")
    sim <- apply(cntrarr, 1:2, sum)
    list(est = est, sim = sim)
  })

# Aggregate pop
popcountry <- aggregate(agepop ~ agegroup + CNTR_CODE, data = cityres, sum)

# Compute excess deaths
excesscountry <- Map(function(attr, pop) lapply(attr, "/", pop),
  attrcountry, popcountry$agepop)

# Extract point estimates and CIs
excessest <- t(sapply(excesscountry, "[[", "est")) * byrate
colnames(excessest) <- sprintf("rate_%s_est", colnames(excessest))
excessCIs <- t(sapply(excesscountry, 
  function(x) c(apply(x$sim, 2, quantile, c(.025, .975))))) * byrate
colnames(excessCIs) <- sprintf("rate_%s", t(outer(c("total", "cold", "heat"), 
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

#----- Put everything in a data.frame

# In the darkness bind them
countryres <- cbind(popcountry, excessest, excessCIs, 
  countrystd[rep(seq_len(nrow(countrystd)), each = length(agelabs)),])

# Add country name
countryres$cntr_name <- eurcntr[match(countryres$CNTR_CODE, eurcntr[,1]),2]

# Add region
countryres$region <- regionlist[countryres$CNTR_CODE]
