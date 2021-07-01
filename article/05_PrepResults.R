################################################################################
#
#                         MCC-EUcityTRM
#
#                       Result and plot preparation
#
################################################################################

source("04_SecondStage.R")

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
# Prepare plotting
#---------------------------

#----- For reporting ERFs

# Axis locations for plots
ovaxis <- ovper[predper %in% axisper]

#----- For maps
# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Complete data.frame with all info
metacomplete <- cbind(metadata, do.call(rbind, metageo$geometry))
names(metacomplete)[(-1:0) + ncol(metacomplete)] <- c("lon", "lat")

# Limits of cities considered
urauext <- st_bbox(metageo)
bnlon <- urauext[c(1,3)]
bnlat <- urauext[c(2,4)]