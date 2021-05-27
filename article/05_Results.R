################################################################################
#
#                         MCC-EUcityTRM
#
#                             Results
#
################################################################################

library(mixmeta)
library(dlnm)

#---------------------------
# Parameters
#---------------------------

# Prediction percentiles
predper <- c(seq(0,1,0.1), 2:98, seq(99,100,0.1))

# Acceptable MMP range
mmprange <- c(1, 99)

# Reported percentiles for cold and heat
resultper <- c(1, 99)

# Number of grid point for background surface
ngrid <- 50

# Age sequence for ERF
agepred <- c(seq(45,85, by = 10))

#---------------------------
# Prepare objects
#---------------------------

# Create basis for overall relationship
ov_basis <- onebasis(predper, fun = "bs", degree = 2, knots = c(10, 75, 90))

# Acceptable MMP values 
inrange <- predper >= mmprange[1] & predper <= mmprange[2]

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
# Dose response predictions
#---------------------------

#----- Predict coefficients for each city

# Prepare prediction data.frame
allcitycoords <- do.call(rbind, metageo$geometry)
colnames(allcitycoords) <- c("lon", "lat")
allpreddf <- data.frame(pcvar, allcitycoords, age = 65)

# Predict coefficients
citycoefs <- predict(stage2res, allpreddf, vcov = T)

#----- Predict overall curves

cityERF <- Map(function(b, era5){
    # percentiles of era5 for this city
    tmeanper <- quantile(era5$era5landtmean, predper / 100)
    
    # Basis for overall
    bvar <- onebasis(tmeanper, fun = "bs", degree = 2, 
      knots = quantile(era5$era5landtmean, c(10, 75, 90) / 100))
    
    # MMT
    firstpred <- bvar %*% b$fit
    mmt <- tmeanper[inrange][which.min(firstpred[inrange])]
    
    # Final prediction centred on the MMT
    crosspred(bvar, coef = b$fit, vcov = b$vcov, cen = mmt, 
      model.link="log", at = quantile(era5$era5landtmean, predper / 100))
  }, citycoefs, era5series)
names(cityERF) <- metadata$URAU_CODE

#----- Create summary object

# Start with metapredictor component valued
cityres <- allpreddf

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
# Attributable fraction and number
#---------------------------



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


