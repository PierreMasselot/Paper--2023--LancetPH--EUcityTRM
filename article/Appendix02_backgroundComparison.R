################################################################################
#
#                         MCC-EUcityTRM
#
#                      Appendix : Background comparison
#
################################################################################

source("05_ResultsPrep.R")

#---------------------------
#  Prepare comparison
#---------------------------

#----- Baseline model

# Baseline formula
baseform <- as.formula(sprintf("coefs ~ %s + 
    ns(age, knots = 65, Boundary.knots = c(0, 100))",
  paste(colnames(pcvar), collapse = " + ")))

# Baseline model for comparison
basemod <- mixmeta(baseform, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Function for wald test

# Wald test
fwald <- function(full, null) {
  ind <- !names(coef(full)) %in% names(coef(null))
  coef <- coef(full)[ind]
  vcov <- vcov(full)[ind,ind]
  waldstat <- coef %*% solve(vcov) %*% coef
  df <- length(coef)
  pval <- 1 - pchisq(waldstat, df)
  return(list(waldstat = waldstat, pvalue = pval))
}

#----- Objects for prediction

# Background data.frame
bggrid <- expand.grid(lon = seq(urauext[1], urauext[3], length.out = ngrid),
  lat = seq(urauext[2], urauext[4], length.out = ngrid)
)
inland <- !is.na(as.numeric(st_intersects(
  st_as_sf(bggrid, coords = 1:2, crs=4326), euromap)))
bggrid <- bggrid[inland,]
bggrid[colnames(pcvar)] <- 0
bggrid["age"] <- mean(stage2df$age)

# Prediction function
bgpred <- function(coef){
  firstpred <- ov_basis %*% coef$fit
  mmt <- ovper[inrange][which.min(firstpred[inrange,])]
  crosspred(ov_basis, coef = coef$fit, vcov = coef$vcov, cen = mmt, 
    model.link = "log", at = ovper)
}

#----- City data.frame for curve prediction

# Random cities 
cityind <- sample.int(nrow(metadata), 5)

# Get geographical values
citygeo <- do.call(rbind, metageo$geometry[cityind])
colnames(citygeo) <- c("lon", "lat")

# Put everything in a data.frame
citydf <- data.frame(id = paste(metadata[cityind, "URAU_NAME"], 
    metadata[cityind, "CNTR_CODE"], sep = " - "), 
  citygeo, scale(metavar)[cityind,], age = mean(stage2df$age))


#---------------------------
#  Indicator for region
#---------------------------

# #----- Extract Koppen-Geiger class
# # Rename values
# coorddata <- stage2df[,c("city", "lon", "lat")]
# names(coorddata) <- c("Site", "Longitude", "Latitude")
# 
# # Extract class
# stage2df$kgc <- LookupCZ(coorddata, rc = T)
# 
# #----- Fit mixmeta
# # Create formula
# kgcform <- as.formula(sprintf("coefs ~ %s + ns(age, knots = c(50, 75)) + kgc",
#   paste(colnames(pcvar), collapse = " + ")))
# 
# # Apply meta regression model
# kgcres <- mixmeta(kgcform, data = stage2df, 
#   S = vcovs, random = ~ 1|city, na.action = na.exclude) 
# 
# #----- Scores
# 
# # For Wald test
# withoutres <- mixmeta(update(kgcform, ~ . - kgc), data = stage2df, 
#   S = vcovs, random = ~ 1|city, na.action = na.exclude) 
# 
# # Keep scores
# kgcscores <- c(aic = AIC(kgcres), i2 = summary(kgcres)$i2[1],
#   wald = fwald(kgcres, withoutres)$pvalue)
# 
# #----- Plot climate zone predictions
# # Create prediction data.frame
# bgdf <- data.frame(kgc = unique(stage2df$kgc), age = mean(agevals))
# bgdf[colnames(pcvar)] <- 0
# 
# # Predict coefs
# bgcoefs <- predict(kgcres, bgdf, vcov = T)
# 
# # Obtain corresponding curves
# bgcp <- lapply(bgcoefs, bgpred)
# 
# # Color palette
# pal <- hue_pal()(length(bgcp))
# 
# # Plot all for climate zones
# layout(matrix(1:2, ncol = 2), width = c(4, 1))
# plot(NA, bty = "l", xaxt = "n", 
#   xlab = "Temperature percentile", ylab = "RR",
#   xlim = range(ovper), ylim = c(.9, 2.5)
#   # ylim = c(min(sapply(bgcp, "[[", "allRRlow")), 
#   #   max(sapply(bgcp, "[[", "allRRhigh")))
# )
# abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
# axis(1, at = ovaxis, labels = axisper)
# for (i in seq_along(bgcp)){
#   lines(bgcp[[i]], ptype = "overall", col = pal[i], lwd = 2)
# }
# abline(h = 1)
# par(mar = c(5, 0, 4, 0) + .1)
# plot.new()
# legend("topleft", legend = sprintf("%s (n = %i)", unique(stage2df$kgc), 
#     aggregate(city ~ kgc, 
#       data = unique(stage2df[,c("city", "kgc")]), length)[,2]), 
#   col = pal, lty = 1, lwd = 2, title = "KG climate zone", bty = "n")
# 
# #----- Plot background predictions
# bgdf <- bggrid
# names(bgdf)[1:2] <- c("Longitude", "Latitude")
# bgdf$Site <- 1:nrow(bgdf)
# bgdf$kgc <- LookupCZ(bgdf, rc = T)
# bgdf <- subset(bgdf, kgc %in% unique(stage2df$kgc))
# 
# # Predict coefs
# bgcoefs <- predict(kgcres, bgdf, vcov = T)
# 
# # Obtain corresponding curves
# bgcp <- lapply(bgcoefs, bgpred)
# 
# # Add summaries
# bgdf$mmp <- predper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$mmt <- ovper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
# bgdf$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])
# 
# # Plot MMP
# mmpkgc <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = Longitude, y = Latitude, fill = mmp)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
#     name = "MMP", limits = c(50, 99)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "KGC")
# 
# # Cold
# coldkgc <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = Longitude, y = Latitude, fill = rrcold)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkblue",
#     name = sprintf("RR at percentile %i", resultper[1]),
#     limits = c(1,1.7)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "KGC")
# 
# # Heat
# heatkgc <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = Longitude, y = Latitude, fill = rrheat)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkred",
#     name = sprintf("RR at percentile %i", resultper[2]),
#     limits = c(1,1.5)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "KGC")


#---------------------------
#  Spline separated lat/lon
#---------------------------

#----- Fit mixmeta

# Add to the data.frame
latlon <- do.call(rbind, metageo$geometry[repmcc])
colnames(latlon) <- c("lon", "lat")
stage2df <- cbind(stage2df, latlon)

# Create formula
latlonform <- update(baseform, ~ . + ns(lon, df = 2, Boundary.knots = bnlon) + 
    ns(lat, df = 2, Boundary.knots = bnlat))

# Apply meta regression model
latlonres <- mixmeta(as.formula(latlonform), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# Keep scores
latlonscores <- c(aic = AIC(latlonres), i2 = summary(latlonres)$i2[1],
  wald = fwald(latlonres, basemod)$pvalue)

#----- Background plots

# Predict coefs
bgcoefs <- predict(latlonres, bggrid, vcov = T)

# Obtain curves
bgcp <- lapply(bgcoefs, bgpred)

# Summary data.frame
bgres <- bggrid
bgres$mmp <- predper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgres$mmt <- ovper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgres$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
bgres$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])

# Plot MMP
mmplatlon <- ggplot(data = bgres) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP", limits = c(50, 99)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat + lon")

# Cold
coldlatlon <- ggplot(data = bgres) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrcold)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]),
    limits = c(1,1.7)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat + lon")

# Heat
heatlatlon <- ggplot(data = bgres) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrheat)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]),
    limits = c(1,1.5)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat + lon")

#----- Curve prediction for cities

# Prediction data.frame
latlondf <- cbind(citydf, pcvar[cityind,])

# Predict coefficients
citycoefs <- predict(latlonres, latlondf, vcov = T)

# Obtain curves
latloncitycp <- lapply(citycoefs, bgpred)


#---------------------------
#  Spline tensor product lat/lon
#---------------------------

#----- Fit mixmeta

# Create formula
latlontensform <- update(baseform, 
  ~ . + ns(lon, df = 2, Boundary.knots = bnlon) * 
    ns(lat, df = 2, Boundary.knots = bnlat))

# Apply meta regression model
latlontensres <- mixmeta(as.formula(latlontensform), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# Keep scores
latlontenscores <- c(aic = AIC(latlontensres), i2 = summary(latlontensres)$i2[1],
  wald = fwald(latlontensres, basemod)$pvalue)

#----- Background plots

# Predict coefs
bgcoefs <- predict(latlontensres, bggrid, vcov = T)

# Obtain curves
bgcp <- lapply(bgcoefs, bgpred)

# Summary data.frame
bgres <- bggrid
bgres$mmp <- predper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgres$mmt <- ovper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgres$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
bgres$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])

# Plot MMP
mmplatlontens <- ggplot(data = bgres) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP", limits = c(50, 99)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat * lon")

# Cold
coldlatlontens <- ggplot(data = bgres) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrcold)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]),
    limits = c(1,1.7)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat * lon")

# Heat
heatlatlontens <- ggplot(data = bgres) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrheat)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]),
    limits = c(1,1.5)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat * lon")

#----- Curve prediction for cities

# Prediction data.frame
latlontensdf <- cbind(citydf, pcvar[cityind,])

# Predict coefficients
citycoefs <- predict(latlontensres, latlontensdf, vcov = T)

# Obtain curves
latlontenscitycp <- lapply(citycoefs, bgpred)


# #---------------------------
# # Lat/lon integrated to PLS
# #---------------------------
# 
# # Compute PLS with lat/lon
# cityscale <- scale(citycoords)
# metapls2 <- cbind(metapls, cityscale)
# plsres2 <- plsr(coefs ~ metapls2, center = F)
# pcvar2 <- predict(plsres2, type = "scores")
# colnames(pcvar2) <- sprintf("pls%i", seq_len(ncol(pcvar2)))
# newdf <- stage2df
# newdf[,sprintf("pls%i", 1:npc)] <- pcvar2[,1:npc]
# 
# #----- Fit mixmeta
# # Create formula
# latlonplsform <- as.formula(sprintf("coefs ~ %s + 
#     ns(age, knots = 65, Boundary.knots = c(0, 100))",
#   paste(colnames(pcvar)[1:npc], collapse = " + ")))
# 
# # Apply meta regression model
# latlonplsres <- mixmeta(as.formula(latlonplsform), data = newdf, 
#   S = vcovs, random = ~ 1|city, na.action = na.exclude) 
# 
# #----- Scores
# 
# # Keep scores
# latlonplsscores <- c(aic = AIC(latlonplsres), i2 = summary(latlonplsres)$i2[1])
# 
# #----- Background plots
# 
# # Predict PLS components
# bgplsdf <- cbind(matrix(0, nrow = nrow(bggrid), ncol = length(metaprednames), 
#     dimnames = list(NULL, metaprednames)), 
#   scale(bggrid[,c("lon", "lat")], attr(cityscale, "scaled:center"), 
#     attr(cityscale, "scaled:scale")))
# newpls <- predict(plsres2, bgplsdf, type = "scores", ncomp = 1:npc)
# colnames(newpls) <- sprintf("pls%i", 1:npc)
# 
# # Predict coefs
# bgdf <- as.data.frame(cbind(newpls, bggrid[,c("lon", "lat", "age")]))
# bgcoefs <- predict(latlonplsres, bgdf, vcov = T)
# 
# # Obtain curves
# bgcp <- lapply(bgcoefs, bgpred)
# 
# # Summary data.frame
# bgdf$mmp <- predper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$mmt <- ovper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
# bgdf$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])
# 
# # Plot MMP
# mmplatlonpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
#     name = "MMP", limits = c(50, 99)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "lat lon pls")
# 
# # Cold
# coldlatlonpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = rrcold)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkblue",
#     name = sprintf("RR at percentile %i", resultper[1]),
#     limits = c(1,1.7)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "lat lon pls")
# 
# # Heat
# heatlatlonpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = rrheat)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkred",
#     name = sprintf("RR at percentile %i", resultper[2]),
#     limits = c(1, 1.5)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "lat lon pls")
# 
# #----- Curve prediction for cities
# 
# # Prediction data.frame
# citypls <- predict(plsres2, data.matrix(cbind(citydf[,c(metaprednames)], 
#     scale(citydf[,c("lon", "lat")], attr(cityscale, "scaled:center"), 
#       attr(cityscale, "scaled:scale")))),
#   type = "scores", ncomp = 1:npc)
# colnames(citypls) <- sprintf("pls%i", 1:npc)
# latlonplsdf <- cbind(citydf, citypls)
# 
# # Predict coefficients
# citycoefs <- predict(latlonplsres, latlonplsdf, vcov = T)
# 
# # Obtain curves
# latlonplscitycp <- lapply(citycoefs, bgpred)

# #---------------------------
# # KGC integrated to PLS
# #---------------------------
# 
# # Compute PLS with lat/lon
# metapls2 <- cbind(metapls, model.matrix(~ kgc, stage2df)[,-1])
# plsres2 <- plsr(coefs ~ metapls2, center = F)
# pcvar2 <- predict(plsres2, type = "scores")
# colnames(pcvar2) <- sprintf("pls%i", seq_len(ncol(pcvar2)))
# newdf <- stage2df
# newdf[,sprintf("pls%i", 1:npc)] <- pcvar2[,1:npc]
# 
# #----- Fit mixmeta
# # Create formula
# kgcplsform <- as.formula(sprintf("coefs ~ %s + 
#     ns(age, knots = c(50, 75), Boundary.knots = c(0, 100))",
#   paste(colnames(pcvar)[1:npc], collapse = " + ")))
# 
# # Apply meta regression model
# kgcplsres <- mixmeta(as.formula(kgcplsform), data = newdf, 
#   S = vcovs, random = ~ 1|city, na.action = na.exclude) 
# 
# #----- Scores
# 
# # Keep scores
# kgcplsscores <- c(aic = AIC(kgcplsres), i2 = summary(kgcplsres)$i2[1])
# 
# #----- Background plots
# 
# # Predict PLS components
# bgdf <- bggrid[,c("lon", "lat", "age")]
# names(bgdf)[1:2] <- c("Longitude", "Latitude")
# bgdf$Site <- 1:nrow(bgdf)
# bgdf$kgc <- LookupCZ(bgdf, rc = T)
# bgdf <- cbind(bgdf, model.matrix(~ kgc, bgdf)[,-1])
# bgdf <- subset(bgdf, kgc %in% unique(stage2df$kgc))
# bgdf <- cbind(matrix(0, nrow = nrow(bgdf), ncol = length(metaprednames), 
#     dimnames = list(NULL, metaprednames)), 
#   bgdf)
# newpls <- predict(plsres2, newdata = data.matrix(bgdf[,colnames(metapls2)]), 
#   type = "scores", ncomp = 1:npc)
# colnames(newpls) <- sprintf("pls%i", 1:npc)
# 
# # Predict coefs
# bgdf <- cbind(bgdf, newpls)
# bgcoefs <- predict(kgcplsres, bgdf, vcov = T)
# 
# # Obtain curves
# bgcp <- lapply(bgcoefs, bgpred)
# 
# # Summary data.frame
# bgdf$mmp <- predper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$mmt <- ovper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
# bgdf$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])
# 
# # Plot MMP
# mmpkgcpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = Longitude, y = Latitude, fill = mmp)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
#     name = "MMP", limits = c(50, 99)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "KGC pls")
# 
# # Cold
# coldkgcpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = Longitude, y = Latitude, fill = rrcold)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkblue",
#     name = sprintf("RR at percentile %i", resultper[1]),
#     limits = c(1,1.7)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "KGC pls")
# 
# # Heat
# heatkgcpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = Longitude, y = Latitude, fill = rrheat)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkred",
#     name = sprintf("RR at percentile %i", resultper[2]),
#     limits = c(1,1.5)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "KGC pls")

#---------------------------
# Indicator for region
#---------------------------

#----- Include region information

# Lookup table
region <- c(BG = "Eastern", CZ = "Eastern", HU = "Eastern", RO = "Eastern", 
  SK = "Eastern", PL = "Eastern", DK = "Northern", FI = "Northern", 
  SE = "Northern", EE = "Northern", LV = "Northern", UK = "Northern", 
  IE = "Northern", LT = "Northern", NO = "Northern", ES = "Southern", 
  HR = "Southern", IT = "Southern", CY = "Southern", EL = "Southern", 
  PT = "Southern", MT = "Southern", SI = "Southern", AT = "Western", 
  BE = "Western", FR = "Western", LU = "Western", DE = "Western", 
  NL = "Western", CH = "Western")

# Add region of each city
metadata$region <- as.factor(region[metadata$CNTR_CODE])
stage2df$region <- metadata$region[repmcc]

#----- Fit mixmeta

# Create formula
regionform <- update(baseform, ~ . + region)

# Apply meta regression model
regionres <- mixmeta(regionform, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# Keep scores
regionscores <- c(aic = AIC(regionres), i2 = summary(regionres)$i2[1],
  wald = fwald(regionres, basemod)$pvalue)

#----- Background prediction

# Intersection with european map
whichpoly <- st_intersects(st_as_sf(bggrid, coords = 1:2, crs = 4326), euromap)

# Determine country
whichcountry <- euromap$CNTR_CODE[unlist(whichpoly)]

# Determine region
bggrid$region <- region[whichcountry]
regiongrid <- na.omit(bggrid)

# Predict coefs
bgcoefs <- predict(regionres, regiongrid, vcov = T)

# Obtain curves
bgcp <- lapply(bgcoefs, bgpred)

# Summary data.frame
regiongrid$mmp <- predper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
regiongrid$mmt <- ovper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
regiongrid$rrcold <- sapply(bgcp, function(x) 
  x$allRRfit[predper %in% resultper[1]])
regiongrid$rrheat <- sapply(bgcp, function(x) 
  x$allRRfit[predper %in% resultper[2]])

# Plot MMP
mmpregion <- ggplot(data = regiongrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP", limits = c(50, 99)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "Region")

# Cold
coldregion <- ggplot(data = regiongrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrcold)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]),
    limits = c(1,1.7)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "Region")

# Heat
heatregion <- ggplot(data = regiongrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrheat)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]),
    limits = c(1,1.5)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "Region")

#----- Curve prediction for cities

# Prediction data.frame
regiondf <- cbind(citydf, pcvar[cityind,], region = metadata[cityind, "region"])

# Predict coefficients
citycoefs <- predict(regionres, regiondf, vcov = T)

# Obtain curves
regioncitycp <- lapply(citycoefs, bgpred)

# #---------------------------
# # Region integrated to PLS
# #---------------------------
# 
# # Compute PLS with lat/lon
# metapls2 <- cbind(metapls, model.matrix(~ 0 + region, metadata)[repmcc,])
# plsres2 <- plsr(coefs ~ metapls2, center = F)
# pcvar2 <- predict(plsres2, type = "scores")
# colnames(pcvar2) <- sprintf("pls%i", seq_len(ncol(pcvar2)))
# newdf <- stage2df
# newdf[,sprintf("pls%i", 1:npc)] <- pcvar2[,1:npc]
# 
# #----- Fit mixmeta
# # Create formula
# regionplsform <- as.formula(sprintf("coefs ~ %s + 
#     ns(age, knots = 65, Boundary.knots = c(0, 100))",
#   paste(colnames(pcvar)[1:npc], collapse = " + ")))
# 
# # Apply meta regression model
# regionplsres <- mixmeta(as.formula(regionplsform), data = newdf, 
#   S = vcovs, random = ~ 1|city, na.action = na.exclude) 
# 
# #----- Scores
# 
# # Keep scores
# regionplsscores <- c(aic = AIC(regionplsres), i2 = summary(regionplsres)$i2[1])
# 
# #----- Background plots
# 
# # Predict PLS components
# bgplsdf <- cbind(matrix(0, nrow = nrow(regiongrid), ncol = length(metaprednames), 
#   dimnames = list(NULL, metaprednames)), 
#   model.matrix(~ 0 + region, bggrid))
# newpls <- predict(plsres2, bgplsdf, type = "scores", ncomp = 1:npc)
# colnames(newpls) <- sprintf("pls%i", 1:npc)
# 
# # Predict coefs
# bgdf <- as.data.frame(cbind(newpls, 
#   bggrid[-attr(regiongrid, "na.action"),c("lon", "lat", "age")]))
# bgcoefs <- predict(regionplsres, bgdf, vcov = T)
# 
# # Obtain curves
# bgcp <- lapply(bgcoefs, bgpred)
# 
# # Summary data.frame
# bgdf$mmp <- predper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$mmt <- ovper[inrange][sapply(bgcp, 
#   function(x) which.min(x$allRRfit[inrange]))]
# bgdf$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
# bgdf$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])
# 
# # Plot MMP
# mmpregionpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
#     name = "MMP", limits = c(50, 99)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "Region pls")
# 
# # Cold
# coldregionpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = rrcold)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkblue",
#     name = sprintf("RR at percentile %i", resultper[1]),
#     limits = c(1,1.7)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "Region pls")
# 
# # Heat
# heatregionpls <- ggplot(data = bgdf) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = rrheat)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkred",
#     name = sprintf("RR at percentile %i", resultper[2]),
#     limits = c(1, 1.5)) + 
#   # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#   #   alpha = .4, pch = 1) + 
#   labs(title = "Region pls")
# 
# #----- Curve prediction for cities
# 
# # Prediction data.frame
# citypls <- predict(plsres2, data.matrix(cbind(citydf[,c(metaprednames)], 
#   model.matrix(~ 0 + region, metadata[cityind, ]))),
#   type = "scores", ncomp = 1:npc)
# colnames(citypls) <- sprintf("pls%i", 1:npc)
# regionplsdf <- cbind(citydf, citypls)
# 
# # Predict coefficients
# citycoefs <- predict(regionplsres, regionplsdf, vcov = T)
# 
# # Obtain curves
# regionplscitycp <- lapply(citycoefs, bgpred)

#---------------------------
# Comparison
#---------------------------

# Scores
allaic <- c('lon+lat' = latlonscores[1], 'lon*lat' = latlontenscores[1], 
  Region = regionscores[1])
alli2 <- c('lon+lat' = latlonscores[2], 'lon*lat' = latlontenscores[2], 
  Region = regionscores[2])
allWald <- c('lon+lat' = latlonscores[3], 'lon*lat' = latlontenscores[3], 
  Region = regionscores[3])

# Background summary
(mmplatlon + mmplatlontens + mmpregion) # / (mmplatlonpls + mmpregionpls)
(coldlatlon + coldlatlontens + coldregion) # / (coldlatlonpls + coldregionpls)
(heatlatlon + heatlatlontens + heatregion) # / (heatlatlonpls + heatregionpls)

# Random city prediction
pal <- viridis(3)

par(mfrow = c(3,2))
for(i in 1:5){
  plot(NA, xlim = range(ovper), ylim = c(1, 3),
    xlab = c("Temperature percentile"), ylab = "RR", main = citydf$id[i],
    xaxt = "n")
  abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  lines(latloncitycp[[i]], lwd = 2, col = pal[1], 
    ci = "area", ci.arg = list(col = adjustcolor(pal[1], .2)))
  lines(latlontenscitycp[[i]], lwd = 2, col = pal[2], 
    ci = "area", ci.arg = list(col = adjustcolor(pal[2], .2)))
  lines(regioncitycp[[i]], lwd = 2, col = pal[3], 
    ci = "area", ci.arg = list(col = adjustcolor(pal[3], .2)))
  # lines(latlonplscitycp[[i]], lwd = 2, col = pal[4], 
  #   ci = "area", ci.arg = list(col = adjustcolor(pal[4], .2)))
  # lines(regionplscitycp[[i]], lwd = 2, col = pal[5], 
  #   ci = "area", ci.arg = list(col = adjustcolor(pal[5], .2)))
}
plot.new()
legend("topleft", 
  legend = c("lat + lon", "lat * lon", "Region"), #, "Lat/lon PLS", "Region PLS"),
  col = pal, lwd = 2, bty = "n")