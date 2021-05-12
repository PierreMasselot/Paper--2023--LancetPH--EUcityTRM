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

# Reported percentiles
resultper <- c(1, 99)

# Number of grid point for background surface
ngrid <- 50

# Age sequence for ERF
agepred <- c(seq(45,85, by = 10))

#---------------------------
# Curve by age
#---------------------------

#----- Predict coefficients for different ages

# Average value of lat/lon and PCS
meangeo <- lapply(stage2df[c("lon", "lat")], mean)
meanpcs <- rep(list(0), npc); names(meanpcs) <- colnames(pcs)

# Create prediction data.frame
agepreddf <- as.data.frame(c(list(age = agepred, meangeo, meanpcs)))

# Predict coefficients
agecoefs <- predict(stage2res, agepreddf, vcov = T)

#----- Compute ERF

# Create basis for overall relationship
ov_basis <- onebasis(predper, fun = "bs", degree = 2, knots = c(10, 75, 90))

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(agecoefs, "[[", "fit")

# For each find MMP
inrange <- predper >= mmprange[1] & predper <= mmprange[2]
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

# Create basis for overall relationship
ov_basis <- onebasis(predper, fun = "bs", degree = 2, knots = c(10, 75, 90))

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(citycoefs, "[[", "fit")

# Find MMP
inrange <- predper >= mmprange[1] & predper <= mmprange[2]
citymmp <- predper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Predict RR at specifiec percentiles
cityrr <- Map(function(b, mm){
    cp <- crosspred(ov_basis, coef = b$fit, vcov = b$vcov, cen = mm, model.link="log",
      at = resultper)
    t(rbind(RR = cp$allRRfit, low = cp$allRRlow, high = cp$allRRhigh))
  }, citycoefs, citymmp)

# Create sumary object
cityres <- allpreddf
cityres$mmp <- citymmp
cityres$rr <- t(sapply(cityrr, "[", , 1))

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

# Predict
bgcoefs <- predict(stage2res, bggrid, vcov = T)

#----- Summaries for each grid point

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(bgcoefs, "[[", "fit")

# Find MMP
inrange <- predper >= mmprange[1] & predper <= mmprange[2]
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