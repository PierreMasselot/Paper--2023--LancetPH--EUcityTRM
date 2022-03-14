################################################################################
#
#                         MCC-EUcityTRM
#
#                      Appendix B4: Kriging
#
################################################################################

if (length(ls()) == 0) source("11_ResultsVulnerability.R")

#---------------------------
# Extract BLUPs and predictions for comparison
#---------------------------

# Extract BLUPs and create ERF
blups <- blup(stage2res, vcov = T)
firstpred <- ov_basis %*% sapply(blups, "[[", "blup")
mmt <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]
blupcp <- Map(crosspred, basis = list(ov_basis), 
  coef = lapply(blups, "[[", "blup"),
  vcov = lapply(blups, "[[", "vcov"), cen = mmt, model.link = "log",
  at = list(ovper))

# Extract fixed effect prediction and create ERF
preds <- predict(stage2res, vcov = T)
firstpred <- ov_basis %*% sapply(preds, "[[", "fit")
mmt <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]
predcp <- Map(crosspred, basis = list(ov_basis), 
  coef = lapply(preds, "[[", "fit"),
  vcov = lapply(preds, "[[", "vcov"), cen = mmt, model.link = "log",
  at = list(ovper))

#---------------------------
# Compare BLUP and fixed effect predictions
#---------------------------

#----- Extract extreme percentiles for comparison

# Extract first percentiles
blup_cold <- sapply(blupcp, "[[", "allRRfit")[predper == 1,]
pred_cold <- sapply(predcp, "[[", "allRRfit")[predper == 1,]

# Extract 99th percentiles
blup_heat <- sapply(blupcp, "[[", "allRRfit")[predper == 99,]
pred_heat <- sapply(predcp, "[[", "allRRfit")[predper == 99,]

#----- Plot

# Prepare layout
layout(t(1:2))

# Plot cold
lims <- range(c(blup_cold, pred_cold))
lims <- c(1, 2)
plot(blup_cold, pred_cold, pch = 16, col = 4, xlab = "RR (BLUP)", 
  ylab = "RR (prediction)", main = "Cold", xlim = lims, ylim = lims)
abline(a = 0, b = 1)

# Plot heat
lims <- range(c(blup_heat, pred_heat))
lims <- c(1, 2)
plot(blup_heat, pred_heat, pch = 15, col = 2, xlab = "RR (BLUP)", 
  ylab = "RR (prediction)", main = "Heat", xlim = lims, ylim = lims)
abline(a = 0, b = 1)

# Save
dev.print(png, file = "figures/FigS_BLUPresiduals.png", 
  width = 12, height = 6, units = "in", res = 300)

#-------------------------
# Example of difference between BLUP and predicted
#-------------------------

#----- Extract curves for a city

# Which first stage
toplot <- "mnch.ger9315.0099"
whichobs <- which(rownames(stage2df) == toplot)

# Also create first-stage curves
firstpred <- ov_basis %*% t(coefs)
mmt <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]
fscp <- Map(crosspred, basis = list(ov_basis), 
  coef = as.data.frame(t(coefs)), vcov = vcovs, cen = mmt, model.link = "log",
  at = list(ovper))

# Select curves
ext_curves <- list(fs = fscp[[whichobs]], blup = blupcp[[whichobs]],
  pred = predcp[[whichobs]])
extnms <- c("First-stage", "BLUP", "Mixmeta prediction")

#----- Plot

# Palette
pal <- viridis(3, direction = -1)

# Plot outline
par(mfrow = c(1, 1))
plot(NA, bty = "l", xaxt = "n", 
  xlab = "Temperature percentile (%)", ylab = "RR",
  xlim = range(ovper), ylim = c(.9, 3))
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)

# Add curves
for (i in seq_along(ext_curves)) lines(ext_curves[[i]], ptype = "overall", 
  col = pal[i], ci = "area", 
  lwd = 2, ci.arg = list(col = adjustcolor(pal[i], .2)))

# RR one line
abline(h = 1)

# Add legend
legpars <- list(legend = extnms, col = pal, lty = 1, lwd = 2, 
  title = "", bty = "n", horiz = T, xpd = T)
legdim <- do.call(legend, c(legpars, list(x = "center", plot = F)))
do.call(legend, c(legpars, 
  list(x = mean(par("usr")[1:2]) - legdim$rect$w / 2, 
    y = par("usr")[4] + legdim$rect$h)))

# Save
dev.print(png, file = "figures/FigS_FSpredBLUP.png", width = 10, height = 8,
  units = "in", res = 300)

#---------------------------
# Co-variogram
#---------------------------

#----- Plot variogram

# Plot
plot(mccvario, vgfit, pch = 16, col = 1, lwd = 2, ylab = "Semi-variance",
  xlab = "Distance (km)")

# Save
dev.print(png, file = "figures/FigS_variogram.png", width = 10, height = 7,
  units = "in", res = 300)


#-------------------------
# Kriging
#-------------------------

#----- Predict each random effect on a grid

# Create grid
createraster <- raster(extent(metageo), nrow = 100, ncol = 100, 
  crs = st_crs(metageo))
rastcoord <- st_as_sf(as.data.frame(coordinates(createraster)), 
  coords = c("x","y"), crs = st_crs(metageo))
euroraster <- st_intersection(rastcoord, st_union(euromap))

# Predict kriging on each cell
surfpred <- predict(vgfit, euroraster)

#----- Plot each coefficient

# Create plot layout
rasterplot <- ggplot(data = surfpred) + 
  geom_sf(pch = 15) +
  geom_sf(data = euromap, fill = NA, inherit.aes = F, col = grey(.5)) + 
  coord_sf(xlim = extent(surfpred)[1:2], ylim = extent(surfpred)[3:4],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  scale_colour_gradient2(low = muted("blue"), high = muted("red"),
    limits = c(-1, 1) * max(abs(st_drop_geometry(surfpred[,1:nc * 2 - 1]))),
    name = "Random coefficient") +
  theme_void()

# Loop on variables to create plots
ranplots <- vector("list", nc)
for (i in seq_len(nc)){
  ranplots[[i]] <- rasterplot + aes_string(col = sprintf("b%i.pred", i)) + 
    ggtitle(sprintf("b%i", i))
} 

# Put plots together
design = "12
  34
  55
"
wrap_plots(ranplots, guides = "collect", design = design)

# Save
ggsave("figures/FigS_krigmaps.png", width = 10, height = 20)

