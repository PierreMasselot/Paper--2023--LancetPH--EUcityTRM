################################################################################
#
#                         MCC-EUcityTRM
#
#                             Results
#
################################################################################

source("05_PrepResults.R")

#---------------------------
# Curve by age
#---------------------------

#----- Predict coefficients for different ages

# Get design matrix for age only
mixterms <- delete.response(terms(stage2res))
agemm <- model.matrix(mixterms[grep("age", attr(mixterms, "term.labels"))],
  data.frame(age = agetot))

# Get meta coefficients and vcov for intercept and age
allmetac <- coef(stage2res)
ageinds <- grep("age|(Intercept)", names(allmetac))
agemetac <- allmetac[ageinds]
agemetav <- vcov(stage2res)[ageinds, ageinds]

# Predict DLNM coefficients and vcov for all ages
agepreds <- apply(agemm, 1, function(x){
  xexp <- t(x) %x% diag(nc)
  coefpred <- xexp %*% agemetac
  vcovpred <- xexp %*% agemetav %*% t(xexp)
  list(fit = coefpred, vcov = vcovpred)
})

#----- Compute ERF

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(agepreds, "[[", "fit")

# For each find MMP
agemmp <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Obtain list of ERF
agecp <- Map(crosspred, basis = list(ov_basis), 
  coef = lapply(agepreds, "[[", "fit"), vcov = lapply(agepreds, "[[", "vcov"),
  model.link = "log", cen = agemmp, at = list(ovper))

#---------------------------
# Dose response predictions for all cities at different ages
#---------------------------

#----- Predict coefficients for each city

# Grid between cities and age
cityagegrid <- expand.grid(seq_len(nrow(metadata)), 
  seq_along(unique(agegrps)))
nca <- nrow(cityagegrid)

# Prediction data.frame
allpreddf <- data.frame(pcvar[cityagegrid[,1],], 
  region = metadata[cityagegrid[,1], "region"], age = c(agepred))

# Predict coefficients
citycoefs <- predict(stage2res, allpreddf, vcov = T)

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
names(cityERF) <- paste(metadata$URAU_CODE[cityagegrid[,1]], 
  agelabs[cityagegrid[,2]], sep = "_")

#----- Create summary object

# Start with metapredictor component valued
cityres <- allpreddf
cityres$city <- metadata$URAU_CODE[cityagegrid[,1]]
cityres$agegroup <- agelabs[cityagegrid[,2]]
cityres <- cbind(cityres, metacomplete[,c("lon", "lat")])

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
# Attributable fraction and number for city and age group
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

#----- Population proportion for each city and age group

# Extract population structure variables from metadata
popvars <- sort(grep("prop_[[:digit:]]{4}", names(metadata), value = T))

# Extract min, and max of age group
agemin <- substr(popvars, 6, 7)
agemax <- substr(popvars, 8, 9)

# Repeat values for each age
ageseqs <- Map(seq, agemin, agemax, by = 1)
agepop <- mapply(function(d, a){ 
  do.call(cbind, rep(list(d), length(a))) / length(a)
}, metadata[,popvars], ageseqs)
agepop <- do.call(cbind, agepop)

# Sum by age group
popgrps <- tapply(as.list(as.data.frame(agepop))[ageseq > minage], 
  agegrps, function(x) rowSums(do.call(cbind, x)))
popgrps <- unlist(popgrps)

# Multiply by total population
cityres$pop <- popgrps * rep(metadata$pop, length(agebreaks) + 1) / 100

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
cityageXdes <- model.matrix(delete.response(terms(stage2res)), allpreddf)

#----- Prepare parallelisation
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
    cold = colSums(andaysim * (!heatind)), 
    heat = colSums(andaysim * heatind))
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
stdratecity <- tapply(seq_along(attrlist), cityres$city, function(i){
  
  # Compute crude death rate for both point estimate and simulations
  deathrate <- Map(function(death, pop) list(death$est[1,] / pop, death$sim / pop), 
    attrlist[i], cityres[i, "pop"])
  
  # Weighted mean by standard population for point estimate
  stdest <- apply(sapply(deathrate, "[[", 1), 1, weighted.mean, 
    w = esptot[cityres[i, "agegroup"]])
  
  # Weighted mean for each simulation
  drarray <- do.call(abind, c(lapply(deathrate, "[[", 2), list(along = 3)))
  stdratesim <- apply(drarray, 1:2, weighted.mean, 
    w = esptot[cityres[i, "agegroup"]])
  
  # Put together for output
  rbind(stdest, apply(stdratesim, 2, quantile, c(.025, .975))) * 10^5
})

# Put together point estimates and CIs
allcitystdrt <- t(sapply(stdratecity, "c"))
colnames(allcitystdrt) <- sprintf("stdrate_%s", t(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), FUN = "paste", sep = "_")))

# Add to result summary object
cityres <- cbind(cityres, allcitystdrt)

#---------------------------
# Regional effect
#---------------------------

#----- Predict coefs for each country

# Create prediction df
countrydf <- data.frame(region = regionlist[euromap$CNTR_CODE], 
  age = mean(agevals), rep(list(0), npc), 
  row.names = euromap$CNTR_CODE)
names(countrydf)[-(1:2)] <- sprintf("pls%i", seq_len(npc))

# Predict
cntrcoefs <- predict(stage2res, na.omit(countrydf), vcov = T)

#----- Summaries for each grid point

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(cntrcoefs, "[[", "fit")

# Find MMP
cntrmmt <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Predict RR at specifiec percentiles
cntrrr <- Map(function(b, mm){
  cp <- crosspred(ov_basis, coef = b$fit, vcov = b$vcov, cen = mm, 
    model.link="log", at = ovper[predper %in% resultper])
  t(rbind(RR = cp$allRRfit, low = cp$allRRlow, high = cp$allRRhigh))
}, cntrcoefs, cntrmmt)

# Summary data.frame
countrydf[!is.na(countrydf$region), "mmt"] <- cntrmmt
countrydf[!is.na(countrydf$region), "rr"] <- t(sapply(cntrrr, "[", , 1))

#---------------------------
# Interpret components
#---------------------------

#----- Backtransform coefficients to have effect of each metavariable
# Get eigenvectors
# loads <- pcares$rotation[,seq_len(npc)]
loads <- plsres$projection[,1:npc]
colnames(loads) <- sprintf("pls%i", 1:npc)

# Backtransform coefficients
st2coefs <- coef(stage2res, format = "matrix")
inds <- grep("pls", rownames(st2coefs))
plscoefs <- st2coefs[inds,]
backcoefs <- loads %*% plscoefs

# Vcov matrix
inds <- grep("pls", rownames(vcov(stage2res)))
mixvcov <- vcov(stage2res)[inds, inds]
reploads <- loads %x% diag(nc)
backvcov <- reploads %*% mixvcov %*% t(reploads)

#----- Create curves for increase in one SD for each variable

# Crosspred with backtransformed coefficients
backcp <- lapply(seq_along(metaprednames), function(i){
  inds <- (1:nc) + (i - 1) * nc
  crosspred(ov_basis, coef = backcoefs[i,], vcov = backvcov[inds,inds],
    model.link = "log", cen = median(ovper), at = ovper)
})

# Wald test
waldres <- t(sapply(seq_along(metaprednames), function(i){
  inds <- (1:nc) + (i - 1) * nc
  waldstat <- backcoefs[i,,drop = F] %*% solve(backvcov[inds,inds]) %*%
    backcoefs[i,]
  pval <- 1 - pchisq(waldstat, nc)
  return(list(waldstat = waldstat, pvalue = pval))
}))
rownames(waldres) <- metaprednames

#----- Difference between extreme values

# Compute high and low values (of scaled meta-variables)
extmeta <- apply(scale(metavar), 2, function(x) diff(quantile(x , c(.01, .99))),
  simplify = F)
newmetax <- as.matrix(bdiag(extmeta))

