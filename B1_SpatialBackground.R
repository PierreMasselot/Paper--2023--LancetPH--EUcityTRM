################################################################################
#
#                         MCC-EUcityTRM
#
#                      Appendix : Background comparison
#
################################################################################

if (length(ls()) == 0) source("11_ResultsVulnerability.R")

#---------------------------
#  Fit model
#---------------------------

#----- Baseline model: random intercept model

# Baseline formula
baseform <- coefs ~ 1

# Baseline model for comparison
basemod <- mixmeta(baseform, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Koppen-Geiger model

# Rename values
coorddata <- metadata[, c("URAU_CODE", "lon", "lat")]
names(coorddata) <- c("Site", "Longitude", "Latitude")

# Extract KGC
kgc <- LookupCZ(coorddata, rc = T)
stage2df$kgc <- kgc[repmcc]

# Create formula
kgcform <- update(baseform, ~ . + kgc)

# Fit mixmeta
kgcres <- mixmeta(kgcform, data = stage2df,
  S = vcovs, random = ~ 1|city, na.action = na.exclude)

#----- Spline lat/lon

# Add to the data.frame
latlon <- do.call(rbind, metageo$geometry[repmcc])
colnames(latlon) <- c("lon", "lat")
stage2df <- cbind(stage2df, latlon)

# Create formula
latlonform <- update(baseform, ~ . + ns(lon, df = 2, Boundary.knots = bnlon) + 
    ns(lat, df = 2, Boundary.knots = bnlat))

# Fit mixmeta
latlonres <- mixmeta(as.formula(latlonform), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Spline tensor product lat/lon

# Create formula
latlontensform <- update(baseform, 
  ~ . + ns(lon, df = 2, Boundary.knots = bnlon) * 
    ns(lat, df = 2, Boundary.knots = bnlat))

# Fit mixmeta
latlontensres <- mixmeta(as.formula(latlontensform), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Indicator for region

# Add region of each city
stage2df$region <- metadata$region[repmcc]

# Create formula
regionform <- update(baseform, ~ . + region)

# Fit mixmeta
regionres <- mixmeta(regionform, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#---------------------------
# Results
#---------------------------

# Scores
allaic <- c(Base = AIC(basemod), 'lon+lat' = AIC(latlonres), 
  'lon*lat' = AIC(latlontensres), Region = AIC(regionres), 
  kgc = AIC(kgcres))

#----- Background map

# Add region information to euromap
euromap$region <- regionlist[euromap$CNTR_CODE]

# Color palette
regpal <- viridis(4)
names(regpal) <- c("Western", "Northern", "Eastern", "Southern")
regpal <- regpal[regord]

# Plot regions
ggplot(data = euromap) + theme_void() + 
  geom_sf(data = euromap, aes(fill = region), alpha = .6, col = "black",
    size = .1) + 
  scale_fill_manual(values = regpal, name = "Region", 
    na.value = grey(.95), na.translate = F) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box")

# Save
ggsave("figures/FigS_Region_maps.png", width = 7, height = 7)

#----- Region-specific ERF

# Palette
regpal <- viridis(4)
names(regpal) <- c("Western", "Northern", "Eastern", "Southern")
regpal <- regpal[regord]

# Plot outline
plot(NA, bty = "l", xaxt = "n", 
  xlab = "Temperature percentile", ylab = "RR",
  xlim = range(ovper), 
  # ylim = c(min(sapply(regERF, "[[", "allRRlow")), 
  #   max(sapply(regERF, "[[", "allRRhigh")))
  ylim = c(.8, 3.5))
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)

# Add region curves
for (i in seq_along(regERF)){
  lines(regERF[[i]], ptype = "overall", col = regpal[i], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(regpal[i], .2)))
}
abline(h = 1)

# Add legend
legpars <- list(legend = names(regpal), col = regpal, lty = 1, lwd = 2, 
  title = "Region", bty = "n", horiz = T, xpd = T)
legdim <- do.call(legend, c(legpars, list(x = "center", plot = F)))
do.call(legend, c(legpars, 
  list(x = mean(par("usr")[1:2]) - legdim$rect$w / 2, 
    y = par("usr")[4] + legdim$rect$h)))

dev.print(png, file = "figures/FigS_regionERF.png", width = 9, height = 6,
  units = "in", res = 300)
