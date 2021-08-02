################################################################################
#
#                         MCC-EUcityTRM
#
#                            City specific results
#
################################################################################

source("05_PrepResults.R")

#---------------------------
# Prepare predictions
#---------------------------

#----- Create city-age result data.frame

# Grid between cities and age
cityagegrid <- expand.grid(seq_len(nrow(metadata)), 
  seq_len(length(agebreaks) + 1))
nca <- nrow(cityagegrid)

# Initialize data.frame
cityres <- metadata[cityagegrid[,1], c("URAU_CODE", "LABEL", "CNTR_CODE", 
  "region", "lon", "lat", "pop", "inmcc")]

# Add PLS components
cityres <- cbind(cityres, pcvar[cityagegrid[,1],])

# Add Age group information
cityres$agegroup <- agelabs[cityagegrid[,2]]

# Add country name
eurcntr <- rbind(eu_countries, efta_countries) # Objects from eurostat
cityres$cntr_name <- eurcntr[match(cityres$CNTR_CODE, eurcntr[,1]),2]

#----- Compute average age of death for each city/age group

# Determine average death age for each prediction group and each city
agegrps <- cut(ageseq[ageseq > minage], c(minage, agebreaks, 100), right = F)
agepred <- tapply(ageseq[ageseq > minage], agegrps, function(a){
  apply(agedeaths[,as.character(a)], 1, weighted.mean, x = a)
})
agepred <- do.call(cbind, agepred)

# Average age for each age group
agetot <- colMeans(agepred)

# Add to result object
cityres$age <- c(agepred)

#---------------------------
# Dose response predictions for all cities at different ages
#---------------------------

#----- Predict coefficients for each city

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
      model.link="log", at = quantile(era5$era5landtmean, predper / 100))
  }, citycoefs, era5series[cityagegrid[,1]])
names(cityERF) <- paste(cityres$URAU_CODE, cityres$agegroup, sep = "_")

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
# Excess rates
#---------------------------

#----- Simulations

# Simulate metacoefficients from multivariate normal distribution
set.seed(12345)
metacoefsim <- mvrnorm(nsim, coef(stage2res), vcov(stage2res))

#----- Construct deaths for each city / age group

# Sum by group
cityagedeaths <- tapply(as.list(as.data.frame(agedeaths))[ageseq > minage], 
  agegrps, function(x) rowSums(do.call(cbind, x)))
cityres$death <- unlist(cityagedeaths)

#----- Compute age group population

# Extract population structure variables from metadata
propvars <- sort(grep("prop_[[:digit:]]{4}", names(metadata), value = T))

# Extract min, and max of age group
agemin <- substr(propvars, 6, 7)
agemax <- substr(propvars, 8, 9)

# Repeat values for each age
ageseqs <- Map(seq, agemin, agemax, by = 1)
ageprop <- mapply(function(d, a){ 
  do.call(cbind, rep(list(d), length(a))) / length(a)
}, metadata[,propvars], ageseqs)
ageprop <- do.call(cbind, ageprop)

# Sum by age group
popgrps <- tapply(as.list(as.data.frame(ageprop))[ageseq > minage], 
  agegrps, function(x) rowSums(do.call(cbind, x)))
popgrps <- unlist(popgrps)

# Multiply by total population
cityres$agepop <- popgrps * rep(metadata$pop, length(agebreaks) + 1) / 100

#----- Standard european population for each age group

# Define minimum age of groups (5 years bands)
espbreaks <- (seq_along(esp2013) - 1) * 5

# Create groups
espgrps <- cut(espbreaks[espbreaks >= minage], c(minage, agebreaks, 100), 
  right = F, labels = agelabs)

# Sum stamdard population for each group
esptot <- tapply(esp2013[espbreaks >= minage], espgrps, sum)

#----- Prepare simulation

# Recreate model matrix with the city / age grid
cityageXdes <- model.matrix(delete.response(terms(stage2res)), cityres)

# Prepare parallelisation
ncores <- detectCores()
cl <- makeCluster(max(1, ncores - 2))
registerDoParallel(cl)

#----- Write text file to trace iterations
writeLines(c(""), "temp/logres.txt")
cat(as.character(as.POSIXct(Sys.time())), file = "temp/logres.txt", append = T)

#----- Compute annual AN / AF
attrlist <- foreach(i = seq_len(nca), .packages = c("dlnm")) %dopar% {
  
  # Store iteration (1 every 100)
  if(i %% 100 == 0) cat("\n", "iter = ", i, as.character(Sys.time()), "\n",
    file = "temp/logres.txt", append = T)
  
  # Get object for this city age
  era5 <- era5series[[cityagegrid[i,1]]]$era5landtmean
  cityagecoefs <- citycoefs[[i]]
  cityagemmt <- cityres$mmt[i] 
  
  # Basis value for each day
  bvar <- onebasis(era5, fun = varfun, degree = vardegree, 
    knots = quantile(era5, varper / 100))
  cenvec <- onebasis(cityagemmt, fun = varfun, degree = vardegree, 
    knots = quantile(era5, varper / 100), Boundary.knots = range(era5))
  bvarcen <- scale(bvar, center = cenvec, scale = F)
  
  # Compute daily AF and AN
  afday <- (1 - exp(-bvarcen %*% cityagecoefs$fit))
  anday <- afday * cityres[i, "death"]
  
  # Indicator of heat days
  heatind <- era5 >= cityagemmt
  
  # Sum all
  anlist <- c(total = sum(anday), cold = sum(anday[!heatind]),
    heat = sum(anday[heatind])) / length(era5)
  
  #----- Simulations for CI
  
  # Obtain predicted city coef for each simulated meta coef set
  coefsim <- metacoefsim %*% (cityageXdes[i,] %x% diag(nc))
  
  # Estimate mmt for each simulation
  whichmmt <- apply((ov_basis %*% t(coefsim))[inrange,], 2, which.min)
  mmtsim <- quantile(era5, predper / 100)[inrange][whichmmt]
  cenmat <- onebasis(mmtsim, fun = varfun, degree = vardegree, 
    knots = quantile(era5, varper / 100), Boundary.knots = range(era5))
  
  # Center basis and multiply and compute daily an
  andaysim <- mapply(function(cen, coef) (1 - exp(-scale(bvar, center = cen, 
      scale = F) %*% coef)) * cityres[i, "death"],
    as.data.frame(t(cenmat)), as.data.frame(t(coefsim)))
  
  # Heat index for each simulation
  heatindsim <- outer(era5, mmtsim, ">=")
  
  # Sum total, heat and cold
  ansimlist <- cbind(total = colSums(andaysim), 
    cold = colSums(andaysim * (!heatindsim)), 
    heat = colSums(andaysim * heatindsim))
  ansimlist <- ansimlist / length(era5)
  ansimCI <- apply(ansimlist, 2, quantile, c(.025, .975))
  
  # Output divided by the total number of day to obtain annual values
  list(est = rbind(anlist, ansimCI), sim = ansimlist)
}

# Stop parallel
stopCluster(cl)

# Put together point estimates and CIs
allcityan <- t(sapply(attrlist, function(x) c(x$est)))
colnames(allcityan) <- sprintf("an_%s", t(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), FUN = "paste", sep = "_")))

# Add to result summary object
cityres <- cbind(cityres, allcityan)

#----- Compute standardised rates

# Loop on cities
stdratecity <- tapply(seq_along(attrlist), cityres$URAU_CODE, function(i){
  
  # Compute crude death rate for both point estimate and simulations
  deathrate <- Map(function(death, pop) list(death$est[1,] / pop, 
      death$sim / pop), 
    attrlist[i], cityres[i, "pop"])
  
  # Weighted mean by standard population for point estimate
  stdest <- apply(sapply(deathrate, "[[", 1), 1, weighted.mean, 
    w = esptot[cityres[i, "agegroup"]])
  
  # Weighted mean for each simulation
  drarray <- do.call(abind, c(lapply(deathrate, "[[", 2), list(along = 3)))
  stdratesim <- apply(drarray, 1:2, weighted.mean, 
    w = esptot[cityres[i, "agegroup"]])
  
  # Put together for output
  rbind(stdest, apply(stdratesim, 2, quantile, c(.025, .975))) * byrate
})

# Put together point estimates and CIs
allcitystdrt <- t(sapply(stdratecity, "c"))
colnames(allcitystdrt) <- sprintf("stdrate_%s", t(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), FUN = "paste", sep = "_")))

# Add to result summary object
cityres <- cbind(cityres, allcitystdrt)

#---------------------------
# Excess rates by country
#---------------------------

#----- Aggregate ANs and pop by country

# Aggregate AN estimates
attrcountry <- tapply(attrlist, cityres[, c("CNTR_CODE", "agegroup")], 
  function(attr){
    est <- rowSums(sapply(attr, function(x) x$est[1,]))
    cntrarr <- sapply(attr, "[[", "sim", simplify = "array")
    sim <- apply(cntrarr, 1:2, sum)
    list(est = est, sim = sim)
})

# Aggregate pop
popcountry <- aggregate(agepop ~ CNTR_CODE + agegroup, data = cityres, sum)

#----- Compute standardized rates by country

# By country
countrystd <- tapply(seq_len(nrow(popcountry)), popcountry$CNTR_CODE, 
  function(i){
    # Divide everything by pop
    agerates <- Map(function(attr, pop) lapply(attr, "/", pop),
      attrcountry[i], popcountry[i, "agepop"])
    
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
names(countrystd) <- c(outer(c("total", "cold", "heat"), c("est", "low", "hi"),
  paste, sep = "_"))

# Add country name
cntrlab <- merge(data.frame(rownames(countrystd)), 
  rbind(eu_countries, efta_countries), 
  by.x = 1, by.y = "code", all.x = T, all.y = F)
countrystd$name <- cntrlab$name

# Add region
countrystd$region <- regionlist[rownames(countrystd)]
