################################################################################
#
#                         MCC-EUcityTRM
#
#                      Appendix : Background comparison
#
################################################################################

library(kgc)
library(RColorBrewer)
library(patchwork)

##### EXECUTE 04_SecondStage.R (except mixmeta) #####
##### EXECUTE FIRST SECTION OF 05_Results.R #####  
##### EXECUTE FIRST SECTION OF 06_Plots.R #####  

#---------------------------
#  Common objects
#---------------------------

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

# background data.frame for prediction
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

#---------------------------
#  Indicator for region
#---------------------------

#----- Extract Koppen-Geiger class
# Rename values
coorddata <- stage2df[,c("city", "lon", "lat")]
names(coorddata) <- c("Site", "Longitude", "Latitude")

# Extract class
stage2df$kgc <- LookupCZ(coorddata, rc = T)

#----- Fit mixmeta
# Create formula
kgcform <- as.formula(sprintf("coefs ~ %s + ns(age, knots = c(50, 75)) + kgc",
  paste(colnames(pcvar), collapse = " + ")))

# Apply meta regression model
kgcres <- mixmeta(kgcform, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# For Wald test
withoutres <- mixmeta(update(kgcform, ~ . - kgc), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

# Keep scores
kgcscores <- c(aic = AIC(kgcres), i2 = summary(kgcres)$i2[1],
  wald = fwald(kgcres, withoutres)$pvalue)

#----- Plot climate zone predictions
# Create prediction data.frame
bgdf <- data.frame(kgc = unique(stage2df$kgc), age = mean(agevals))
bgdf[colnames(pcvar)] <- 0

# Predict coefs
bgcoefs <- predict(kgcres, bgdf, vcov = T)

# Obtain corresponding curves
bgcp <- lapply(bgcoefs, bgpred)

# Color palette
pal <- hue_pal()(length(bgcp))

# Plot all for climate zones
layout(matrix(1:2, ncol = 2), width = c(4, 1))
plot(NA, bty = "l", xaxt = "n", 
  xlab = "Temperature percentile", ylab = "RR",
  xlim = range(ovper), ylim = c(.9, 2.5)
  # ylim = c(min(sapply(bgcp, "[[", "allRRlow")), 
  #   max(sapply(bgcp, "[[", "allRRhigh")))
)
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)
for (i in seq_along(bgcp)){
  lines(bgcp[[i]], ptype = "overall", col = pal[i], lwd = 2)
}
abline(h = 1)
par(mar = c(5, 0, 4, 0) + .1)
plot.new()
legend("topleft", legend = sprintf("%s (n = %i)", unique(stage2df$kgc), 
    aggregate(city ~ kgc, 
      data = unique(stage2df[,c("city", "kgc")]), length)[,2]), 
  col = pal, lty = 1, lwd = 2, title = "KG climate zone", bty = "n")

#----- Plot background predictions
bgdf <- bggrid
names(bgdf)[1:2] <- c("Longitude", "Latitude")
bgdf$Site <- 1:nrow(bgdf)
bgdf$kgc <- LookupCZ(bgdf, rc = T)
bgdf <- subset(bgdf, kgc %in% unique(stage2df$kgc))

# Predict coefs
bgcoefs <- predict(kgcres, bgdf, vcov = T)

# Obtain corresponding curves
bgcp <- lapply(bgcoefs, bgpred)

# Add summaries
bgdf$mmp <- predper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgdf$mmt <- ovper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgdf$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
bgdf$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])

# Plot MMP
mmpkgc <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = Longitude, y = Latitude, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP", limits = c(50, 99)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "KGC")

# Cold
coldkgc <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = Longitude, y = Latitude, fill = rrcold)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]),
    limits = c(1,1.7)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "KGC")

# Heat
heatkgc <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = Longitude, y = Latitude, fill = rrheat)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]),
    limits = c(1,1.5)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "KGC")

#---------------------------
#  Spline separated lat/lon
#---------------------------

#----- Fit mixmeta
# Create formula
latlonform <- as.formula(sprintf("coefs ~ %s + 
    ns(lon, df = 2, Boundary.knots = bnlon) + 
    ns(lat, df = 2, Boundary.knots = bnlat) + 
    ns(age, knots = c(50, 75), Boundary.knots = c(0, 100))",
  paste(colnames(pcvar), collapse = " + ")))

# Apply meta regression model
latlonres <- mixmeta(as.formula(latlonform), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# For Wald test
withoutres <- mixmeta(as.formula(sprintf("coefs ~ %s + 
    ns(age, knots = c(50, 75), Boundary.knots = c(0, 100))",
  paste(colnames(pcvar), collapse = " + "))), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

# Keep scores
latlonscores <- c(aic = AIC(latlonres), i2 = summary(latlonres)$i2[1],
  wald = fwald(latlonres, withoutres)$pvalue)

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

#---------------------------
#  Spline tensor product lat/lon
#---------------------------

#----- Fit mixmeta
# Create formula
latlontensform <- as.formula(sprintf("coefs ~ %s + 
    ns(lon, df = 2, Boundary.knots = bnlon) * 
    ns(lat, df = 2, Boundary.knots = bnlat) + 
    ns(age, knots = c(50, 75), Boundary.knots = c(0, 100))",
  paste(colnames(pcvar), collapse = " + ")))

# Apply meta regression model
latlontensres <- mixmeta(as.formula(latlontensform), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# For Wald test
withoutres <- mixmeta(as.formula(sprintf("coefs ~ %s + 
    ns(age, knots = c(50, 75), Boundary.knots = c(0, 100))",
  paste(colnames(pcvar), collapse = " + "))), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

# Keep scores
latlontenscores <- c(aic = AIC(latlontensres), i2 = summary(latlontensres)$i2[1],
  wald = fwald(latlontensres, withoutres)$pvalue)

#----- Background plots

# Predict coefs
bgcoefs <- predict(latlontensres, bggrid, vcov = T)

# Obtain curves
bgcp <- lapply(bgcoefs, bgpred)

# Summary data.frame
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


#---------------------------
# Lat/lon integrated to PLS
#---------------------------

# Compute PLS with lat/lon
cityscale <- scale(citycoords)
metapls2 <- cbind(metapls, cityscale)
plsres2 <- plsr(coefs ~ metapls2, center = F)
pcvar2 <- predict(plsres2, type = "scores")
colnames(pcvar2) <- sprintf("pls%i", seq_len(ncol(pcvar2)))
newdf <- stage2df
newdf[,sprintf("pls%i", 1:npc)] <- pcvar2[,1:npc]

#----- Fit mixmeta
# Create formula
latlonplsform <- as.formula(sprintf("coefs ~ %s + 
    ns(age, knots = c(50, 75), Boundary.knots = c(0, 100))",
  paste(colnames(pcvar)[1:npc], collapse = " + ")))

# Apply meta regression model
latlonplsres <- mixmeta(as.formula(latlonplsform), data = newdf, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# Keep scores
latlonplsscores <- c(aic = AIC(latlonplsres), i2 = summary(latlonplsres)$i2[1])

#----- Background plots

# Predict PLS components
bgplsdf <- cbind(matrix(0, nrow = nrow(bggrid), ncol = length(metaprednames), 
    dimnames = list(NULL, metaprednames)), 
  scale(bggrid[,c("lon", "lat")], attr(cityscale, "scaled:center"), 
    attr(cityscale, "scaled:scale")))
newpls <- predict(plsres2, bgplsdf, type = "scores", ncomp = 1:npc)
colnames(newpls) <- sprintf("pls%i", 1:npc)

# Predict coefs
bgdf <- as.data.frame(cbind(newpls, bggrid[,c("lon", "lat", "age")]))
bgcoefs <- predict(latlonplsres, bgdf, vcov = T)

# Obtain curves
bgcp <- lapply(bgcoefs, bgpred)

# Summary data.frame
bgdf$mmp <- predper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgdf$mmt <- ovper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgdf$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
bgdf$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])

# Plot MMP
mmplatlonpls <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP", limits = c(50, 99)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat lon pls")

# Cold
coldlatlonpls <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrcold)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]),
    limits = c(1,1.7)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat lon pls")

# Heat
heatlatlonpls <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rrheat)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]),
    limits = c(1, 1.5)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "lat lon pls")


#---------------------------
# KGC integrated to PLS
#---------------------------

# Compute PLS with lat/lon
metapls2 <- cbind(metapls, model.matrix(~ kgc, stage2df)[,-1])
plsres2 <- plsr(coefs ~ metapls2, center = F)
pcvar2 <- predict(plsres2, type = "scores")
colnames(pcvar2) <- sprintf("pls%i", seq_len(ncol(pcvar2)))
newdf <- stage2df
newdf[,sprintf("pls%i", 1:npc)] <- pcvar2[,1:npc]

#----- Fit mixmeta
# Create formula
kgcplsform <- as.formula(sprintf("coefs ~ %s + 
    ns(age, knots = c(50, 75), Boundary.knots = c(0, 100))",
  paste(colnames(pcvar)[1:npc], collapse = " + ")))

# Apply meta regression model
kgcplsres <- mixmeta(as.formula(kgcplsform), data = newdf, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Scores

# Keep scores
kgcplsscores <- c(aic = AIC(kgcplsres), i2 = summary(kgcplsres)$i2[1])

#----- Background plots

# Predict PLS components
bgdf <- bggrid[,c("lon", "lat", "age")]
names(bgdf)[1:2] <- c("Longitude", "Latitude")
bgdf$Site <- 1:nrow(bgdf)
bgdf$kgc <- LookupCZ(bgdf, rc = T)
bgdf <- cbind(bgdf, model.matrix(~ kgc, bgdf)[,-1])
bgdf <- subset(bgdf, kgc %in% unique(stage2df$kgc))
bgdf <- cbind(matrix(0, nrow = nrow(bgdf), ncol = length(metaprednames), 
    dimnames = list(NULL, metaprednames)), 
  bgdf)
newpls <- predict(plsres2, newdata = data.matrix(bgdf[,colnames(metapls2)]), 
  type = "scores", ncomp = 1:npc)
colnames(newpls) <- sprintf("pls%i", 1:npc)

# Predict coefs
bgdf <- cbind(bgdf, newpls)
bgcoefs <- predict(kgcplsres, bgdf, vcov = T)

# Obtain curves
bgcp <- lapply(bgcoefs, bgpred)

# Summary data.frame
bgdf$mmp <- predper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgdf$mmt <- ovper[inrange][sapply(bgcp, 
  function(x) which.min(x$allRRfit[inrange]))]
bgdf$rrcold <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[1]])
bgdf$rrheat <- sapply(bgcp, function(x) x$allRRfit[predper %in% resultper[2]])

# Plot MMP
mmpkgcpls <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = Longitude, y = Latitude, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP", limits = c(50, 99)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "KGC pls")

# Cold
coldkgcpls <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = Longitude, y = Latitude, fill = rrcold)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]),
    limits = c(1,1.7)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "KGC pls")

# Heat
heatkgcpls <- ggplot(data = bgdf) + theme_void() + 
  geom_raster(aes(x = Longitude, y = Latitude, fill = rrheat)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]),
    limits = c(1,1.5)) + 
  # geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
  #   alpha = .4, pch = 1) + 
  labs(title = "KGC pls")

#---------------------------
# Comparison
#---------------------------

allaic <- c(kgc = kgcscores[1], 'lon+lat' = latlonscores[1], 
  'lon*lat' = latlontenscores[1], lonlat_pls = latlonplsscores[1],
  kgc_pls = kgcplsscores[1])
alli2 <- c(kgc = kgcscores[2], 'lon+lat' = latlonscores[2], 
  'lon*lat' = latlontenscores[2], lonlat_pls = latlonplsscores[2],
  kgc_pls = kgcplsscores[2])


(mmpkgc + mmplatlon + mmplatlontens) / (mmplatlonpls + mmpkgcpls)
(coldkgc + coldlatlon + coldlatlontens) / (coldlatlonpls + coldkgcpls)
(heatkgc + heatlatlon + heatlatlontens) / (heatlatlonpls + heatkgcpls)
