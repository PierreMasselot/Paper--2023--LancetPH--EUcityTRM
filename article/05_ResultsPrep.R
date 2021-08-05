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


#---------------------------
# Prepare plotting
#---------------------------

#----- For reporting ERFs

# Axis locations for plots
ovaxis <- ovper[predper %in% axisper]

#----- For maps
# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Limits of cities considered
urauext <- st_bbox(metageo)
bnlon <- urauext[c(1,3)]
bnlat <- urauext[c(2,4)]