################################################################################
#
#                         MCC-EUcityTRM
#
#             Add spatial interpolation of random effect
#
################################################################################

source("00_Packages_Parameters.R")

load("results/cityResults.RData")

#---------------------------
# Prepare data
#---------------------------

# Extract random effect
blupext <- blup(stage2res, type = "residual")

# Remove duplicates
dups <- duplicated(blupext)
mccblup <- blupext[!dups,]

# Get city lat long
mcccoords <- metadata[repmcc, c("lon", "lat")][!dups,]

# Create spatial object
mccgeo <- st_as_sf(cbind(mccblup, mcccoords), coords = c("lon","lat"), 
  crs = st_crs(4326))
mccgeo <- st_transform(mccgeo, crs = st_crs(metageo))


#---------------------------
# Spatial interpolation to all cities
#---------------------------

# Create gstat object: we add all coefficients in the object for co-kriging
cokrig <- NULL
for (i in 1:nc) {
  form <- sprintf("b%i ~ 1", i)
  cokrig <- gstat(cokrig, formula = as.formula(form), data = mccgeo,
    set = list(nocheck = 1))
}

#----- Fit variogram for correlation structure

# Compute semi variogram
mccvario <- variogram(cokrig)

# Fit variogram: Spehrical model
varmod <- vgm("Gau")
vgfit <- fit.lmc(mccvario, cokrig, varmod)

#----- Predict at every location

# Predict from model at every URAU city
allkrig <- predict(vgfit, as_Spatial(metageo))

# Extract random effect and vcov
ranpred <- apply(data.matrix(allkrig@data), 1, function(x){
  fit <- x[1:nc * 2 - 1]
  vcov <- matrix(NA, nc, nc)
  diag(vcov) <- x[1:nc * 2]
  vcov[upper.tri(vcov)] <- x[-(1:(nc * 2))]
  vcov[lower.tri(vcov)] <- t(vcov)[lower.tri(vcov)]
  list(fit = fit, vcov = vcov)
}, simplify = F)

#---------------------------
# Predictions at each city
#---------------------------

# Add the coefficients and compute vcov matrix for each city
newcitycoefs <- Map(function(cfix, cran){
  csum <- cfix$fit + cran$fit
  cvcov <- cfix$vcov + cran$vcov # This is probably not right
  list(fit = csum, vcov = cvcov)
}, citycoefs, ranpred)

# Compute ERFs
newcityERF <- Map(function(b, era5){
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
}, newcitycoefs, era5series)
names(newcityERF) <- metadata$URAU_CODE

#---------------------------
# Now try with IDW
#---------------------------

# Create gstat object including all variables
idwmod <- NULL
idpvec <- c(.7, 1.3, 1.0, 1.1, 1.7)
for (i in 1:nc) idwmod <- gstat(idwmod, 
  formula = as.formula(sprintf("b%i ~ 1", i)), 
  data = mccgeo, set = list(idp = idpvec[i]))

# predict at every location
allidw <- predict(idwmod, as_Spatial(metageo))

# Add to coefs
newcitycoefs2 <- t(allidw@data[,1:nc * 2 - 1]) + sapply(citycoefs, "[[", "fit")

# Compute ERFs
newcityERF2 <- Map(function(b, era5){
  # percentiles of era5 for this city
  tmeanper <- quantile(era5$era5landtmean, predper / 100)
  
  # Basis for overall
  bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
    knots = quantile(era5$era5landtmean, varper / 100))
  
  # MMT
  firstpred <- bvar %*% b
  mmt <- tmeanper[inrange][which.min(firstpred[inrange])]
  
  # Final prediction centred on the MMT
  list(x = tmeanper, y = exp(firstpred - firstpred[tmeanper == mmt]), cen = mmt)
}, as.data.frame(newcitycoefs2), era5series)
names(newcityERF2) <- metadata$URAU_CODE

#---------------------------
# Plot
#---------------------------


# Prepare output
pdf("figures/ERFcities_krig.pdf", width = 11, height = 13)
layout(matrix(seq(6 * length(agelabs)), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(cityERF)){
  
  # Prediction
  plot(cityERF[[i]], xlab = "Temperature (C)", ylab = "RR", 
    main = metadata$LABEL[i], lwd = 2, ylim = c(.5, 3), 
    cex.main = .9, col.main = ifelse(metadata$inmcc[i], 2, 1))
  abline(v = cityERF[[i]]$cen, lty = 3)
  
  # Krig BLUP
  lines(newcityERF[[i]], col = 4, lwd = 2, lty = 2)
  abline(v = newcityERF[[i]]$cen, lty = 3, col = 4)
  
  # IDW BLUP
  lines(newcityERF2[[i]], col = 2, lwd = 2, lty = 4)
  abline(v = newcityERF2[[i]]$cen, lty = 3, col = 2)
  
  # Add percentiles
  cityper <- cityERF[[i]]$predvar[predper %in% c(1, 99)]
  abline(v = cityper, lty = 2)
}

dev.off()