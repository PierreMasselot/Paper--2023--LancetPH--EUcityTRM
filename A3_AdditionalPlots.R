################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix 3: Data description
#
################################################################################

if (length(ls()) == 0) source("14_Plots.R")

#---------------------------
# Regional background
#---------------------------

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
  ylim = c(.9, 2))
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

#---------------------------
# Composite indices of vulnerability
#---------------------------

#----- Correlation matrix between metapredictors variables

# Compute correlation matrix
metacor <- cor(metadata[,metapreds])
colnames(metacor) <- rownames(metacor) <- metadesc$label[match(metapreds, 
  metadesc$metavar)]

# Color scale
pal <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
  "#4393C3", "#2166AC", "#053061")))

# Plot correlation matrix
corrplot.mixed(metacor, upper = "square", tl.pos = "lt",
  tl.srt = 45, tl.col = "black", cl.cex = 1.1, cl.align.text = "l",
  upper.col = pal(200), lower.col = pal(200))

# Save
dev.print(png, file = "figures/FigS_metacor.png", width = 15, height = 15,
  units = "in", res = 300)

#----- Image of PLS components

# Change col and row names for plot labelling
plotload <- plsres$loadings[,1:npc]
colnames(plotload) <- sprintf("Comp. %i", 1:npc)
rownames(plotload) <- metadesc$label[match(metapreds, metadesc$metavar)]

# Color scale
pal <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
  "#4393C3", "#2166AC", "#053061")))

# Plot loadings (correlation between components and meta-variables)
corrplot(t(plotload), method = "square", is.corr = F, col.lim = c(-1, 1), 
  tl.srt = 45, tl.col = "black", cl.cex = .7, cl.align.text = "l",
  col = pal(200))

# Save
dev.print(png, file = "figures/FigS_PLScor.png", units = "in", res = 300,
  width = 10, height = 7)

#----- Create map for each PLS component

# Loop
plsmaps <- lapply(seq_len(npc), function(i){
  cutpts <- unname(round(quantile(cityres[, sprintf("pls%i", i)], 
    seq(0, 1, length.out = 5))))
  basic_map + aes_string(fill = sprintf("pls%i", i)) + 
    # scale_fill_viridis(name = sprintf("Comp. %i", i), direction = -1) + 
    guides(fill = guide_coloursteps(title.position = "top", title.hjust = .5,
      barwidth = 10, barheight = .8, even.steps = T)) +
    scale_fill_stepsn(colours = viridis(length(cutpts) - 1, direction = -1),
      values = rescale(cutpts), breaks = cutpts,
      name = sprintf("Comp. %i", i))
})
names(plsmaps) <- letters[1:npc]

# Add legend for size
plsmaps[[letters[npc + 1]]] <- basic_map + coord_sf(xlim = c(0, 0), ylim = c(0, 0)) + 
  scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
    labels = c(1, 5, 10, 50) / 10, name = "Population (millions)",
    guide = guide_legend(override.aes = list(colour = "black"), 
      title.position = "top")) + 
  theme(legend.position = "right")

# Design
design <- matrix(letters[1:(2 * floor(npc / 2))], ncol = 2, byrow = T)
if (npc %% 2 == 1) design <- rbind(design, letters[npc])
design <- cbind(design, letters[npc + 1])
design_char <- paste(apply(design, 1, paste, collapse = ""), collapse = "\n")

# Plot
wrap_plots(plsmaps, widths = c(1, 1, .1), design = design_char)

# Save
ggsave("figures/FigS_PLSmaps.png", width = 9, height = 4 * floor(npc / 2) + 1)


#----- Curve changes at extreme PLS

# Colors
pal <- viridis(2, direction = -1)

# Plot layout
design <- matrix(1:(2 * floor(npc / 2)), ncol = 2, byrow = T)
if (npc %% 2 == 1) {
  design <- design[,rep(1:2, each = 2)]
  design <- rbind(design, c(0, npc, npc, 0))
}
design <- cbind(design, npc + 1)

# Initialize layout
layout(design, widths = c(rep(1, 2 * (1 + (npc %% 2))), .2 * (1 + (npc %% 2))))

# Loop on PLS components
for (i in seq_len(npc)){
  inds <- 1:2 + (2 * (i - 1))
  
  # Lowest
  plot(plscp[inds][[1]], xlab = "Temperature percentile", ylab = "RR", 
    main = sprintf("Comp. %i", i), col = pal[1], lwd = 2, ylim = c(.8, 2.5), 
    cex.main = .9, ci.arg = list(col = adjustcolor(pal[1], .2)), xaxt = "n")
  
  # Highest
  lines(plscp[inds][[2]], lwd = 2, col = pal[2], ci = "area", 
    ci.arg = list(col = adjustcolor(pal[2], .2)))
  
  # Axis
  abline(v = ovaxis, h = axTicks(2), lty = 3, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  abline(h = 1)
  
  # MMTs
  abline(v = plscp[inds][[1]]$cen, col = pal[1], lty = 2)
  abline(v = plscp[inds][[2]]$cen, col = pal[2], lty = 2)
}

# Add legend
par(mar = rep(0, 4))
plot.new()
legend("center", legend = c("1st", "99th"), lwd = 2,
  col = pal, bty = "n", title = "Component\npercentile")

# Save
dev.print(png, file = "figures/FigS_PLS_ERF.png", 
  height = 4 * floor(npc / 2) + 1, width = 10, units = "in", res = 300)

#---------------------------
# Kriging description
#---------------------------

#----- Compare BLUPs and predictions

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

# Extract first percentiles
blup_cold <- sapply(blupcp, "[[", "allRRfit")[predper == 1,]
pred_cold <- sapply(predcp, "[[", "allRRfit")[predper == 1,]

# Extract 99th percentiles
blup_heat <- sapply(blupcp, "[[", "allRRfit")[predper == 99,]
pred_heat <- sapply(predcp, "[[", "allRRfit")[predper == 99,]

# Prepare layout
for(i in dev.list()) dev.off(i)
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

#----- Example of difference between BLUP and predicted

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

#----- Plot variogram

# Plot
plot(mccvario, vgfit, pch = 16, col = 1, lwd = 2, ylab = "Semi-variance",
  xlab = "Distance (km)")

# Save
dev.print(png, file = "figures/FigS_variogram.png", width = 10, height = 7,
  units = "in", res = 300)

#----- Grid of predictions

# Create grid
createraster <- raster(extent(metageo), nrow = 100, ncol = 100, 
  crs = st_crs(metageo))
rastcoord <- st_as_sf(as.data.frame(coordinates(createraster)), 
  coords = c("x","y"), crs = st_crs(metageo))
euroraster <- st_intersection(rastcoord, st_union(euromap))

# Predict kriging on each cell
surfpred <- predict(vgfit, euroraster)

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

#-----------------------------
# Final results
#-----------------------------

#----- Summary ERF plot by country

# Create order vector
ordvec <- with(metadata, order(region, cntr_name))

# Reorder
reg_cntr <- unique(metadata[ordvec, c("region", "CNTR_CODE", "cntr_name")])

# Prepare palette
pal <- viridis(4)
names(pal) <- c("Western", "Northern", "Eastern", "Southern")
pal <- pal[regord]

# Prepare layout
laydims <- c(6, 5)
laymat <- rbind(1, matrix(1:prod(laydims) + 1, 
  nrow = laydims[1], ncol = laydims[2], byrow = T))

# RR range
rrlims <- c(.8, 3)

# Default parameters
defpars <- par()

# Open file
png("figures/FigS_ERFbyCountry.png", width = 15, height = 20,
  units = "in", res = 300)

# Layout
layout(laymat, heights = c(.1, rep(1, laydims[1])))

# Legend
par(mar = c(0, 4, 0, 2) + .1)
plot.new()
legpars <- list(legend = names(pal), col = pal, lty = 1, lwd = 2,
  title = "Region", horiz = T, bty = "n", xpd = T)
leg <- do.call(legend, c(legpars, x = "topleft", plot = F))
do.call(legend, c(legpars, x = with(leg$rect, .45 - w), y = 1))

legend(.55, 1, legend = c("Capital", "Other"), 
  lty = 1, lwd = c(2, 1), col = grey(c(0, .6)), horiz = T, title = "City",
  bty = "n", xpd = T)

par(mar = c(5, 4, 4, 2) + .1)
# Loop on countries
for (i in seq_len(nrow(reg_cntr))){
  
  # Extract ERFs
  erfind <- substr(names(cityERF), 1, 2) == reg_cntr$CNTR_CODE[i]
  erfi <- cityERF[erfind]
  
  # Extract
  rrmat <- sapply(erfi, "[[", "allRRfit")
  
  # Index of capital city
  capind <- grepl("001", names(erfi))
  
  # Plot
  matplot(ovper, rrmat, type = "l", lty = 1, 
    col = adjustcolor(pal[reg_cntr$region[i]], .2), lwd = 1,
    ylab = "RR", xlab = "Temperature percentile", main = reg_cntr$cntr_name[i],
    xaxt = "n", ylim = rrlims)
  lines(ovper, rrmat[,capind], col = pal[reg_cntr$region[i]], lwd = 2)
  axis(1, ovaxis, axisper)
  abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
  abline(h = 1)
}

# Close pdf
dev.off()