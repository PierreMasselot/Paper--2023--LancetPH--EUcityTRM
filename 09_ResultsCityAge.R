################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Part 9: Compute attributable numbers for each city and age group
#
################################################################################

if (length(ls()) == 0) source("08_Spatial.R")

#---------------------------
# Prepare predictions
#---------------------------

#----- Create city-age result data.frame

# Grid between cities and age
cityagegrid <- expand.grid(seq_along(agelabs), seq_len(nrow(metadata)))
nca <- nrow(cityagegrid)

# Initialize data.frame
cityageres <- metadata[cityagegrid[,2], c("URAU_CODE", "LABEL", "CNTR_CODE", 
  "cntr_name", "region", "lon", "lat", "pop", "inmcc")]

# Add PLS components
cityageres <- cbind(cityageres, pcvar[cityagegrid[,2],])

# Add Age group information
cityageres$agegroup <- agelabs[cityagegrid[,1]]

#----- Compute average age of death for each city/age group

# Extract and sort age group deaths
deathvars <- sort(grep("deathrate_[[:digit:]]", names(metadata), value = T))

# Extract min, and max of age group
agemin <- substr(deathvars, 11, 12)
agemax <- substr(deathvars, 13, 14)

# Repeat values for each age
ageseqs <- Map(seq, agemin, agemax, by = 1)
agedeaths <- mapply(function(d, a){ 
    do.call(cbind, rep(list(d), length(a)))
  }, metadata[,deathvars], ageseqs)
agedeaths <- do.call(cbind, agedeaths)
ageseq <- unlist(ageseqs, use.names = F)
colnames(agedeaths) <- ageseq
rownames(agedeaths) <- metadata$URAU_CODE

# Determine average death age for each prediction group and each city
agegrps <- cut(ageseq[ageseq > minage], c(agebreaks, 100), right = F)
agepred <- tapply(ageseq[ageseq > minage], agegrps, function(a){
  apply(agedeaths[,as.character(a)], 1, weighted.mean, x = a)
})
agepred <- do.call(cbind, agepred)

# for the oldest age group, use life expectancy instead
highbreak <- tail(agebreaks, 1)
agepred[,ncol(agepred)] <- highbreak + 
  metadata[,sprintf("lifexp_%s", highbreak)]

# Average age for each age group
agetot <- colMeans(agepred)

# Add to result object
cityageres$age <- c(t(agepred))

#---------------------------
# Dose response predictions for all cities at different ages
#---------------------------

#----- Predict coefficients for each city

# Fixed effects prediction
cityagecoefs <- predict(stage2res, cityageres, vcov = T)
names(cityagecoefs) <- with(cityageres, paste(URAU_CODE, agegroup, sep = "_"))

# Add spatial blup predictions
cityagecoefs <- Map(function(cfix, cran){
  csum <- cfix$fit + cran$fit
  cvcov <- cfix$vcov + cran$vcov
  list(fit = csum, vcov = cvcov)
}, cityagecoefs, ranpred[cityagegrid[,2]])

#----- Construct curves

cityageERF <- Map(function(b, era5){
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
  }, cityagecoefs, era5series[cityagegrid[,2]])
names(cityageERF) <- paste(cityageres$URAU_CODE, cityageres$agegroup, sep = "_")

#----- ERF summary

# MMP & MMT
cityageres$mmt <- sapply(cityageERF, "[[", "cen")
cityageres$mmp <- as.numeric(gsub("%", "", 
  sapply(cityageERF, function(x) attr(x$cen, "names"))))

# Relative risks at extreme percentiles
cityageres$rrcold <- sapply(cityageERF, "[[", "allRRfit")[
  predper == resultper[1],]
cityageres$rrcold_low <- sapply(cityageERF, "[[", "allRRlow")[
  predper == resultper[1],]
cityageres$rrcold_hi <- sapply(cityageERF, "[[", "allRRhigh")[
  predper == resultper[1],]
cityageres$rrheat <- sapply(cityageERF, "[[", "allRRfit")[
  predper == resultper[2],]
cityageres$rrheat_low <- sapply(cityageERF, "[[", "allRRlow")[
  predper == resultper[2],]
cityageres$rrheat_hi <- sapply(cityageERF, "[[", "allRRhigh")[
  predper == resultper[2],]

#---------------------------
# Excess rates
#---------------------------

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
popgrps <- c(do.call(rbind, popgrps))

# Multiply by total population
cityageres$agepop <- popgrps * rep(metadata$pop, each = length(agebreaks)) / 
  100

#----- Construct deaths for each city / age group

# Weighted average of death rates by group
cityagedeaths <- tapply(seq_along(agegrps), agegrps, function(i){
  ad <- agedeaths[,ageseq > minage][,i]
  ap <- ageprop[,ageseq > minage][,i]
  rowSums(ad * ap) / rowSums(ap)
})
cityageres$deathrate <- c(do.call(rbind, cityagedeaths))

# Compute deaths by age
cityageres$death <- with(cityageres, deathrate * agepop)

#----- Standard european population for each age group

# Define minimum age of groups (5 years bands)
espbreaks <- (seq_along(esp2013) - 1) * 5

# Create groups
espgrps <- cut(espbreaks[espbreaks >= minage], c(agebreaks, 100), 
  right = F, labels = agelabs)

# Sum stamdard population for each group
esptot <- tapply(esp2013[espbreaks >= minage], espgrps, sum)

#----- Prepare simulation

# Simulate fixed effect part: 
# metacoefficients from multivariate normal distribution
set.seed(12345)
metacoefsim <- mvrnorm(nsim, coef(stage2res), vcov(stage2res))

# Recreate model matrix with the city / age grid
cityageXdes <- model.matrix(delete.response(terms(stage2res)), cityageres)

# Prepare random number generation for random part of coefficients (parallel)
rng <- RNGseq(nca, 6789)

# Prepare parallelisation
cl <- makeCluster(ncores)
registerDoParallel(cl)

#----- Write text file to trace iterations
writeLines(c(""), "temp/logres.txt")
cat(as.character(as.POSIXct(Sys.time())), file = "temp/logres.txt", append = T)

#----- Compute annual AN / AF
attrlist <- foreach(i = seq_len(nca), 
  .packages = c("dlnm", "MASS", "Matrix", "rngtools")) %dopar% {
  
  # Store iteration (1 every 100)
  if(i %% 100 == 0) cat("\n", "iter = ", i, as.character(Sys.time()), "\n",
    file = "temp/logres.txt", append = T)
  
  # Get object for this city age
  era5 <- era5series[[cityagegrid[i,2]]]$era5landtmean
  icoefs <- cityagecoefs[[i]]
  immt <- cityageres$mmt[i]
  
  # Basis value for each day
  bvar <- onebasis(era5, fun = varfun, degree = vardegree, 
    knots = quantile(era5, varper / 100))
  cenvec <- onebasis(immt, fun = varfun, degree = vardegree, 
    knots = quantile(era5, varper / 100), Boundary.knots = range(era5))
  bvarcen <- scale(bvar, center = cenvec, scale = F)
  
  # Compute daily AF and AN
  afday <- (1 - exp(-bvarcen %*% icoefs$fit))
  anday <- afday * cityageres[i, "death"]
  
  # Indicator of heat days
  heatind <- era5 >= immt
  
  # Sum all
  anlist <- c(total = sum(anday), cold = sum(anday[!heatind]),
    heat = sum(anday[heatind])) / length(era5)
  
  #----- Simulations for CI
  
  # Obtain fixed city coef for each simulated meta coef set
  fixsim <- metacoefsim %*% (cityageXdes[i,] %x% diag(nc))
  
  # Simulate the random part
  iran <- ranpred[[cityagegrid[i,2]]]
  setRNG(rng[[i]])
  ransim <- mvrnorm(nsim, iran$fit, nearPD(iran$vcov)$mat)
  
  # Total simulated coefs
  coefsim <- fixsim + ransim
  
  # Compute daily AN
  andaysim <- apply(coefsim, 1, function(coef) (1 - exp(-bvarcen %*% coef)) * 
      cityageres[i, "death"])
  
  # Sum for total, heat and cold
  ansimlist <- cbind(total = colSums(andaysim), 
    cold = colSums(andaysim[!heatind,]), 
    heat = colSums(andaysim[heatind,]))
  ansimlist <- ansimlist / length(era5)
  ansimCI <- apply(ansimlist, 2, quantile, c(.025, .975))
  
  
  # Output divided by the total number of day to obtain annual values
  list(est = rbind(anlist, ansimCI), sim = ansimlist, coefsim = coefsim)
}
names(attrlist) <- names(cityagecoefs)

# Stop parallel
stopCluster(cl)

# Put together point estimates and CIs
allcityan <- t(sapply(attrlist, function(x) c(x$est)))
colnames(allcityan) <- sprintf("an_%s", t(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), FUN = "paste", sep = "_")))

# Add to result summary object
cityageres <- cbind(cityageres, allcityan)

#----- Death rates
nm_suf <- c(t(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), FUN = "paste", sep = "_")))
cityageres[, sprintf("rate_%s", nm_suf)] <- 
  cityageres[, sprintf("an_%s", nm_suf)] / cityageres[, "agepop"] * byrate
cityageres[, sprintf("ratetotpop_%s", nm_suf)] <- 
  cityageres[, sprintf("an_%s", nm_suf)] / cityageres[, "pop"] * byrate

#----- Add Attributable fractions

# Compute attributable fractions
cityageAFs <- sapply(cityageres[grep("an", names(cityageres))], "/", 
  cityageres$death, simplify = "data.frame") * 100

# Add to data.frame
colnames(cityageAFs) <- gsub("an", "af", colnames(cityageAFs))
cityageres <- cbind(cityageres, cityageAFs)