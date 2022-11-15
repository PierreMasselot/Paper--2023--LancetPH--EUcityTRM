################################################################################
#
#                         MCC-EUcityTRM
#
#         Prepare objects for second stage & prediction
#
################################################################################

if (length(ls()) == 0){
  load("data/FirstStage.RData")
  source("00_Packages_Parameters.R")
}

#---------------------------
#  Prepare stage 2 dataset
#---------------------------

# Put all city and age results in a big list and keep only model which converged
unlistresults <- unlist(sapply(stage1res, "[[", "modelres"), recursive = F)

# Exclude models that did not converge
unlistresults <- unlistresults[sapply(unlistresults, "[[", "conv")]

# Get coefs, vcov, average age and convergence
coefs <- t(sapply(unlistresults, "[[", "coef"))
vcovs <- lapply(unlistresults, "[[", "vcov")
agevals <- sapply(unlistresults, "[[", "ageval")

# Match mcc cities to each obs of meta regression
st2mcccodes <- apply(t(sapply(strsplit(names(unlistresults), "\\."), "[", 1:2)),
  1, paste, collapse = ".")
repmcc <- match(st2mcccodes, metadata$mcc_code)

# Set contrast for region
contrasts(metadata$region) <- "contr.helmert"

# Create stage 2 data.frame
stage2df <- data.frame(region = metadata$region[repmcc], age = agevals, 
  city = metadata[repmcc, "URAU_CODE"], 
  country = as.factor(metadata[repmcc, "CNTR_CODE"]))

# Store model dimensions
nc <- ncol(coefs) # Number of first-stage coefficients
nm <- length(metapreds) # Number of metapredictors

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