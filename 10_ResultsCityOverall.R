################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Part 10: Compute standardised excess death rates for each city
#
################################################################################

if (length(ls()) == 0) source("09_ResultsCityAge.R")

#---------------------------
# Initialize results object
#---------------------------

# Sum AN, pop and deaths from city-age results
cityres <- group_by(cityageres, pick(URAU_CODE:inmcc, starts_with("pls"))) |> 
  dplyr::summarise(pop = sum(agepop), death = sum(death), 
    across(starts_with("an_"), sum)) |>
  ungroup()
 
# Recompute impact summaries
cityres <- mutate(cityres, across(starts_with("an"), 
  list(rate = ~ byrate * .x / pop, af = ~ 100 * .x / death),
  .names = "{.fn}_{gsub('an_', '', .col)}"))

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
allcitystdrt <- data.frame(URAU_CODE = rownames(allcitystdrt), 
  allcitystdrt)

# Add to result summary objects
cityres <- merge(cityres, allcitystdrt)

#---------------------------
# Overall ERF: at life expectancy
#---------------------------

#----- Predict coefficients for each city

# Consider life expectancy at birth as age
cityres$age <- metadata$lifexp_00

# Predict coefficients
citycoefs <- predict(stage2res, cityres, vcov = T)

# Add spatial blup predictions
citycoefs <- Map(function(cfix, cran){
  csum <- cfix$fit + cran$fit
  cvcov <- cfix$vcov + cran$vcov
  list(fit = csum, vcov = cvcov)
}, citycoefs, ranpred)

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

#----- MMP & MMT uncertainty

# create design matrix for mixmeta
cityXdes <- model.matrix(delete.response(terms(stage2res)), cityres)

# Loop on cities
mmtci <- sapply(seq_len(nrow(metadata)), simplify = "array", function(i){
  
  # Simulate fixed part
  fixsim <- metacoefsim %*% (cityXdes[i,] %x% diag(nc))
  
  # Simulate the random part
  iran <- ranpred[[i]]
  ransim <- mvrnorm(nsim, iran$fit, nearPD(iran$vcov)$mat)
  
  # Total simulated coefs
  coefsim <- fixsim + ransim
  
  # Compute MMP and MMT
  whichmmt <- apply((ov_basis %*% t(coefsim))[inrange,], 2, which.min)
  mmpsim <- predper[inrange][whichmmt]
  mmtsim <- quantile(era5series[[i]]$era5landtmean, 
    predper / 100)[inrange][whichmmt]
  
  # Confidence intervals
  cbind(mmp = quantile(mmpsim, c(.025, .975)), 
    mmt = quantile(mmtsim, c(.025, .975)))
})

#----- ERF summary

# MMP
cityres$mmp <- as.numeric(gsub("%", "", 
  sapply(cityERF, function(x) attr(x$cen, "names"))))
cityres$mmp_low <- mmtci[1,1,]
cityres$mmp_hi <- mmtci[2,1,]

# MMT
cityres$mmt <- sapply(cityERF, "[[", "cen")
cityres$mmt_low <- mmtci[1,2,]
cityres$mmt_hi <- mmtci[2,2,]

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
