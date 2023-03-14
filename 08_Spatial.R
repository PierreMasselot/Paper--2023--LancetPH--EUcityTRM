################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Part 8: Krige BLUP residuals from meta-regression
#
################################################################################

if (length(ls()) == 0) source("07_SecondStage.R")

#---------------------------
# Prepare BLUP spatial information
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
mccgeo <- st_transform(mccgeo, crs = as.numeric(geoproj))

#---------------------------
# Spatial interpolation to all cities
#---------------------------

# Create gstat object: add all coefficients in the object for co-kriging
cokrig <- NULL
for (i in 1:nc) {
  form <- sprintf("b%i ~ 1", i)
  cokrig <- gstat(cokrig, id = sprintf("b%i", i), formula = as.formula(form), 
    data = mccgeo, set = list(nocheck = 1))
}

#----- Fit variogram for correlation structure

# Compute semi variogram
mccvario <- variogram(cokrig, cutoff = 600)

# Fit variogram model
varmod <- do.call(vgm, variopars)
vgfit <- fit.lmc(mccvario, cokrig, varmod,
  fit.method = 6, correct.diagonal = 1.01)

#----- Predict at every location

# Predict from model at every URAU city
metageo <- metadata[,c("lon", "lat")]
coordinates(metageo) <- ~ lon + lat
proj4string(metageo) <- CRS(sprintf("EPSG:%s", geoproj))
allkrig <- predict(vgfit, metageo)

# Extract random effect and vcov
ranpred <- apply(data.matrix(allkrig@data), 1, function(x){
  fit <- x[1:nc * 2 - 1]
  vcov <- matrix(NA, nc, nc)
  diag(vcov) <- x[1:nc * 2]
  vcov[upper.tri(vcov)] <- x[-(1:(nc * 2))]
  vcov[lower.tri(vcov)] <- t(vcov)[lower.tri(vcov)]
  list(fit = fit, vcov = vcov)
})
names(ranpred) <- metadata$URAU_CODE
