################################################################################
#
#                         MCC-EUcityTRM
#
#                             Results
#
################################################################################


#---------------------------
# Prepare predictions
#---------------------------

#----- Common temperature for prediction

# Estimate an overall empirical distribution of temperature
tmeandist <- t(sapply(era5series, 
  function(x) quantile(x$era5landtmean, predper / 100)))
ovper <- colMeans(tmeandist)

# Create basis for overall relationship
ovknots <- ovper[sprintf("%i.0%%",varper)]
ov_basis <- onebasis(ovper, fun = varfun, degree = vardegree, knots = ovknots)

# Acceptable MMP values 
inrange <- predper >= mmprange[1] & predper <= mmprange[2]

#----- Simulations

# Simulate metacoefficients from multivariate normal distribution
set.seed(12345)
metacoefsim <- mvrnorm(nsim, coef(stage2res), vcov(stage2res))

#----- Ages for prediction

# Determine average death age for each prediction group and each city
agegrps <- cut(ageseq[ageseq > minage], c(minage, agebreaks, 100), right = F)
agepred <- tapply(ageseq[ageseq > minage], agegrps, function(a){
  apply(agedeaths[,as.character(a)], 1, weighted.mean, x = a)
})
agepred <- do.call(cbind, agepred)

# Average age for each age group
agetot <- colMeans(agepred)

#---------------------------
# Curve by age
#---------------------------

#----- Predict coefficients for different ages

# Average value of lat/lon and PCS
meangeo <- lapply(stage2df[c("lon", "lat")], mean)
meanpcs <- rep(list(0), npc); names(meanpcs) <- colnames(pcvar)

# Create prediction data.frame
agepreddf <- as.data.frame(c(list(age = agetot, meangeo, meanpcs)))

# Predict coefficients
agecoefs <- predict(stage2res, agepreddf, vcov = T)

#----- Compute ERF

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(agecoefs, "[[", "fit")

# For each find MMP
agemmp <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Obtain list of ERF
agecp <- Map(crosspred, basis = list(ov_basis), 
  coef = lapply(agecoefs, "[[", "fit"), vcov = lapply(agecoefs, "[[", "vcov"),
  model.link = "log", cen = agemmp, at = list(ovper))

#---------------------------
# Dose response predictions for all cities at different ages
#---------------------------

#----- Predict coefficients for each city

# Lon-lat df
allcitycoords <- do.call(rbind, metageo$geometry)
colnames(allcitycoords) <- c("lon", "lat")

# Grid between cities and age
cityagegrid <- expand.grid(seq_len(nrow(allcitycoords)), 
  seq_along(unique(agegrps)))
nca <- nrow(cityagegrid)

# Prediction data.frame
allpreddf <- data.frame(pcvar[cityagegrid[,1],], 
  allcitycoords[cityagegrid[,1],], age = c(agepred))

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
cityres$agegroup <- agelabs[cityagegrid[,2]]

# MMP & MMT
cityres$mmt <- sapply(cityERF, "[[", "cen")
cityres$mmp <- as.numeric(gsub("%", "", 
  sapply(cityERF, function(x) attr(x$cen, "names"))))

# Relatie risks at extreme percentiles
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

#----- Construct deaths for each city / age group

# Sum by group
cityagedeaths <- tapply(as.list(as.data.frame(agedeaths)), agegrps, 
  function(x) rowSums(do.call(cbind, x)))
deathlist <- unlist(cityagedeaths)

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

#----- Compute AN / AF
attrlist <- foreach(i = seq_len(nca), .packages = c("dlnm")) %dopar% {
  
  #----- Store iteration (1 every 100)
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
    knots = quantile(era5, varper / 100))
  bvarcen <- scale(bvar, center = cenvec, scale = F)
  
  # Compute daily AF and AN (naively)
  afday <- (1 - exp(-bvarcen %*% cityagecoefs$fit))
  anday <- afday * deathlist[i] / 365.25
  
  # Indicator of heat days
  heatind <- era5 >= cityagemmt
  
  # Sum all
  anlist <- c(total = sum(anday), cold = sum(anday[!heatind]),
    heat = sum(anday[heatind]))
  
  # Simulations for CI
  coefsim <- metacoefsim %*% (cityageXdes[i,] %x% diag(nc))
  andaysim <- (1 - exp(-bvarcen %*% t(coefsim))) * deathlist[i] / 365.25
  ansimlist <- cbind(total = colSums(andaysim), 
    cold = colSums(andaysim[!heatind,]), 
    heat = colSums(andaysim[heatind,]))
  ansimCI <- apply(ansimlist, 2, quantile, c(.025, .975))
  
  # Output
  rbind(anlist, ansimCI)
}

# Put together point estimates and CIs
allcityan <- t(sapply(attrlist, "c"))
colnames(allcityan) <- sprintf("an_%s", t(outer(c("total", "cold", "heat"), 
  c("est", "low", "hi"), FUN = "paste", sep = "_")))

# Add to result summary object
cityres <- cbind(cityres, allcityan)

#---------------------------
# 'Background' effect
#---------------------------

#----- Predict coefs on a grid across europe
# Create grid
bggrid <- expand.grid(lon = seq(urauext[1], urauext[3], length.out = ngrid),
  lat = seq(urauext[2], urauext[4], length.out = ngrid)
)

# Fill data.frame
bggrid[colnames(pcvar)] <- 0
bggrid["age"] <- mean(stage2df$age)

# Predict
bgcoefs <- predict(stage2res, bggrid, vcov = T)

#----- Summaries for each grid point

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(bgcoefs, "[[", "fit")

# Find MMP
bgmmp <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Predict RR at specifiec percentiles
bgrr <- Map(function(b, mm){
  cp <- crosspred(ov_basis, coef = b$fit, vcov = b$vcov, cen = mm, 
    model.link="log", at = ovper[predper %in% resultper])
  t(rbind(RR = cp$allRRfit, low = cp$allRRlow, high = cp$allRRhigh))
}, bgcoefs, bgmmp)

# Summary data.frame
bggrid$mmp <- bgmmp
bggrid$rr <- t(sapply(bgrr, "[", , 1))


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

#----- Create two curves for high and low values of metapredictor

# Compute high and low values (of scaled meta-variables)
extmeta <- apply(scale(metavar), 2, quantile, c(.01, .99))
newmetax <- as.matrix(bdiag(as.data.frame(extmeta)))
colnames(newmetax) <- metaprednames

# Predict coefficients and vcov
newpls <- newmetax %*% loads
newdf <- data.frame(newpls, lat = mean(stage2df$lat), lon = mean(stage2df$lon),
  age = 65)
extpreds <- predict(stage2res, newdata = newdf, vcov = T)

# Crosspred
extcp <- lapply(seq_len(nrow(newmetax)), function(i){
  betas <- extpreds[[i]]$fit
  firstpred <- ov_basis %*% betas
  mmp <- ovper[inrange][which.min(firstpred[inrange])]
  crosspred(ov_basis, coef = betas, vcov = extpreds[[i]]$vcov,
    model.link = "log", cen = mmp, at = ovper)
})

