################################################################################
#
#                         MCC-EUcityTRM
#
#                             Results
#
################################################################################


#---------------------------
# Prepare objects
#---------------------------

# Create basis for overall relationship
ov_basis <- onebasis(predper, fun = varfun, degree = vardegree, knots = varper)

# Acceptable MMP values 
inrange <- predper >= mmprange[1] & predper <= mmprange[2]

# Simulate metacoefficients from multivariate normal distribution
set.seed(12345)
metacoefsim <- mvrnorm(nsim, coef(stage2res), vcov(stage2res))

#---------------------------
# Curve by age
#---------------------------

#----- Predict coefficients for different ages

# Average value of lat/lon and PCS
meangeo <- lapply(stage2df[c("lon", "lat")], mean)
meanpcs <- rep(list(0), npc); names(meanpcs) <- colnames(pcvar)

# Create prediction data.frame
agepreddf <- as.data.frame(c(list(age = agepred, meangeo, meanpcs)))

# Predict coefficients
agecoefs <- predict(stage2res, agepreddf, vcov = T)

#----- Compute ERF

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(agecoefs, "[[", "fit")

# For each find MMP
agemmp <- predper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Obtain list of ERF
agecp <- Map(crosspred, basis = list(ov_basis), 
  coef = lapply(agecoefs, "[[", "fit"), vcov = lapply(agecoefs, "[[", "vcov"),
  model.link = "log", cen = agemmp, at = list(predper))

#---------------------------
# Dose response predictions for all cities at different ages
#---------------------------

#----- Predict coefficients for each city

# Lon-lat df
allcitycoords <- do.call(rbind, metageo$geometry)
colnames(allcitycoords) <- c("lon", "lat")

# Grid between cities and age
cityagegrid <- expand.grid(seq_len(nrow(allcitycoords)), agepred)
nca <- nrow(cityagegrid)

# Prediction data.frame
allpreddf <- data.frame(pcvar[cityagegrid[,1],], 
  allcitycoords[cityagegrid[,1],], age = cityagegrid[,2])

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
names(cityERF) <- metadata$URAU_CODE

#----- Create summary object

# Start with metapredictor component valued
cityres <- allpreddf
cityres$age <- rep(paste(c(0, agebreaks), c(agebreaks, 99), sep = "-"), 
  each = nrow(allcitycoords))

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

# Get age group death data
citydeaths <- metadata[,grep("death_[[:digit:]]{4}", names(metadata))]

# Sum by group
agegrpind <- cut(as.numeric(substr(names(citydeaths), 7, 8)), 
  c(0, agebreaks, 100), right = F)
cityagedeaths <- tapply(as.list(citydeaths), agegrpind, 
  function(x) rowSums(do.call(cbind, x)))
deathlist <- unlist(cityagedeaths)

#----- Prepare simulation

# Recreate model matrix with the city / age grid
cityageXdes <- model.matrix(delete.response(terms(stage2res)), allpreddf)

#----- Prepare parallelisation
ncores <- detectCores()
cl <- makeCluster(max(1, ncores - 2))
registerDoParallel(cl)

#----- Compute AN / AF
attrlist <- foreach(i = seq_len(nca), .packages = c("dlnm")) %dopar% {
  
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
bgmmp <- predper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Predict RR at specifiec percentiles
bgrr <- Map(function(b, mm){
  cp <- crosspred(ov_basis, coef = b$fit, vcov = b$vcov, cen = mm, model.link="log",
    at = resultper)
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
backcoefs <- t(plscoefs) %*% t(loads)

# Vcov matrix
mixvcov <- vcov(stage2res)[inds, inds]
reploads <- loads %x% diag(nc)
backvcov <- reploads %*% mixvcov %*% t(reploads)


# Monte-Carlo standard error and confidence intervals (not sure about this)
# plscsim <- metacoefsim[,grep("pls", colnames(metacoefsim))]
# backsim <- apply(plscsim, 1, function(x) 
#   matrix(x, ncol = npc) %*% t(loads))
# vcovSim <- var(t(backsim))
# 
# backCIs <- apply(backsim, 1, quantile, c(.025, .975))
# rownames(backCIs) <- c("low", "hi")

#----- Create curves for increase in one SD for each variable

# Crosspred with backtransformed coefficients
backcp <- lapply(seq_along(metaprednames), function(i){
  inds <- (1:nc) + (i - 1) * nc
  crosspred(ov_basis, coef = backcoefs[,i], vcov = backvcov[inds,inds],
    model.link = "log", cen = 50, at = predper)
})

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
  # firstpred <- ov_basis %*% betas
  # mmp <- predper[inrange][which.min(firstpred[inrange])]
  crosspred(ov_basis, coef = betas, vcov = extpreds[[i]]$vcov,
    model.link = "log", cen = 50, at = predper)
})

