################################################################################
#
#                         MCC-EUcityTRM
#
#                      Overall city level results
#
################################################################################

#---------------------------
# Initialize results object
#---------------------------

# Extract from metadata
cityres <- metadata[, c("URAU_CODE", "LABEL", "CNTR_CODE", 
  "region", "lon", "lat", "pop", "inmcc")]

#---------------------------
# Standardised rates
#---------------------------

# Loop on cities
stdratecity <- tapply(seq_len(nca), cityageres$URAU_CODE, function(i){

  # Compute crude death rate for both point estimate and simulations
  deathrate <- Map(function(death, pop) list(death$est[1,] / pop, 
      death$sim / pop), 
    attrlist[i], cityageres[i, "agepop"])
  
  # Weighted mean by standard population for point estimate
  stdest <- apply(sapply(deathrate, "[[", 1), 1, weighted.mean, 
    w = esptot[cityageres[i, "agegroup"]])
  
  # Weighted mean for each simulation
  drarray <- do.call(abind, c(lapply(deathrate, "[[", 2), list(along = 3)))
  stdratesim <- apply(drarray, 1:2, weighted.mean, 
    w = esptot[cityageres[i, "agegroup"]])
  
  # Put together for output
  rbind(stdest, apply(stdratesim, 2, quantile, c(.025, .975))) * byrate
})

# Put together point estimates and CIs
allcitystdrt <- t(sapply(stdratecity, "c"))
colnames(allcitystdrt) <- sprintf("stdrate_%s", t(outer(
  c("total", "cold", "heat"), c("est", "low", "hi"), 
  FUN = "paste", sep = "_")))

# Add to result summary object
cityres <- cbind(cityres, allcitystdrt)

#---------------------------
# Overall ERF: at life expectancy
#---------------------------

#----- Predict coefficients for each city

# Consider life expectancy at birth as age
cityres$age <- metadata$lifexp_00

# Add PLS values
cityres <-  cbind(cityres, pcvar)

# Predict coefficients
citycoefs <- predict(stage2res, cityres, vcov = T)

#----- Predict overall curves

cityERF <- Map(function(b, era5){
    # percentiles of era5 for this city
    tmeanper <- quantile(era5$era5landtmean, predper / 100)
    
    # Basis for overall
    bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
      knots = quantile(era5$era5landtmean, varper / 100))
    
    # MMT
    firstpred <- bvar %*% b$fit
    mmt <- tmeanper[inrange][which.min(firstpred[inrange])]
    
    # Final prediction centred on the MMT
    crosspred(bvar, coef = b$fit, vcov = b$vcov, cen = mmt, 
      model.link = "log", at = quantile(era5$era5landtmean, predper / 100))
  }, citycoefs, era5series)
names(cityERF) <- cityres$URAU_CODE

#----- ERF summary

# MMP & MMT
cityres$mmt <- sapply(cityERF, "[[", "cen")
cityres$mmp <- as.numeric(gsub("%", "", 
  sapply(cityERF, function(x) attr(x$cen, "names"))))

# Relative risks at extreme percentiles
cityres$rrcold <- sapply(cityERF, "[[", "allRRfit")[predper == resultper[1],]
cityres$rrcold_low <- sapply(cityERF, "[[", "allRRlow")[
  predper == resultper[1],]
cityres$rrcold_hi <- sapply(cityERF, "[[", "allRRhigh")[
  predper == resultper[1],]
cityres$rrheat <- sapply(cityERF, "[[", "allRRfit")[predper == resultper[2],]
cityres$rrheat_low <- sapply(cityERF, "[[", "allRRlow")[
  predper == resultper[2],]
cityres$rrheat_hi <- sapply(cityERF, "[[", "allRRhigh")[
  predper == resultper[2],]


#---------------------------
# Save
#---------------------------

save.image("results/cityResults.RData")
