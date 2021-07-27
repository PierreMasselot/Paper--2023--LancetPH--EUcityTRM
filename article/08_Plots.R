################################################################################
#
#                         MCC-EUcityTRM
#
#                             Plots
#
################################################################################

source("06_Results.R")

#---------------------------
#  Figure 1: maps of risk 
#---------------------------

#----- Create a basic map layout

# Map background
basic_map <- ggplot(data = subset(cityres, agegroup == agelabs[3]),
    aes(x = lon, y = lat, size = pop)) + 
  theme_void() + 
  geom_sf(data = euromap, fill = grey(.95), inherit.aes = F, col = grey(.5)) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) +
  scale_size(name = "Population", range = c(1, 5),
    guide = "none") + 
  theme(legend.position = "bottom", legend.box = "vertical") + 
  geom_point(alpha = .9, pch = 21, colour = "white", stroke = .1) + 
  guides(fill = guide_colourbar(title.position = "top", title.hjust = .5,
    barwidth = 12, barheight = .8))

#----- Add aesthetic and scale for different result variables

# MMT with heat colors
mmtmap <- basic_map + aes(fill = mmt) + 
  scale_fill_gradientn(colours = c("black", "darkgreen", "lightgoldenrod", 
    "darkred", "black"),
    name = "MMT", limits = c(10, 30), oob = squish)
  # scale_fill_binned(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
  #   name = "MMT", oob = squish)

# Cold with white to blue
coldmap <- basic_map + aes(fill = rrcold) + 
  scale_fill_gradient2(low = "lightgoldenrod", mid = "darkblue", 
    high = "black", 
    name = sprintf("RR at percentile %i", resultper[1]),
    limits = c(1, 2), midpoint = 1.5, oob = squish)
# scale_fill_binned(low = "white", high = "darkblue", 
#   name = sprintf("RR at percentile %i", resultper[2]))

# Heat with white to red
heatmap <- basic_map + aes(fill = rrheat) + 
  scale_fill_gradient2(low = "lightgoldenrod", mid = "darkred", 
    high = "black", 
    name = sprintf("RR at percentile %i", resultper[2]), oob = squish,
    limits = c(1, 1.6), midpoint = 1.3)
  # scale_fill_binned(low = "white", high = "darkred", 
  #   name = sprintf("RR at percentile %i", resultper[2]))

#----- Put maps together

# Put them side-by-side
(coldmap + mmtmap + heatmap) /

# Add legend for point size
  basic_map + coord_sf(xlim = c(0, 0), ylim = c(0, 0)) + 
    scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
      labels = c(1, 5, 10, 50) / 10, name = "Population (millions)",
      guide = guide_legend(title.position = "top", title.hjust = 0.5,
        label.position = "bottom", override.aes = list(colour = "black"))) + 
  theme(legend.position = "bottom") +
  plot_layout(height = c(1, .05))

# Save
ggsave("figures/Fig1_citiesResults.pdf", width = 15)

#---------------------------
#  Figure 1bis: Risks for capital cities
#---------------------------

#----- Select capital city of each country

# Find largest pop in each country
# capitals <- unlist(by(metadata, metadata$CNTR_CODE, function(d){
#   d[which.max(d$pop), "URAU_CODE"]
# }))

# Select cities in result (city with number 001 is usually the capital)
# big_cityres <- subset(cityres, URAU_CODE %in% capitals)
big_cityres <- subset(cityres, substr(URAU_CODE, 3, 5) == "001")

# Order by region and age
big_cityres <- big_cityres[with(big_cityres, 
  order(region, URAU_CODE, agegroup)),]
big_cityres$id <- as.numeric(factor(big_cityres$URAU_CODE, 
  levels = unique(big_cityres$URAU_CODE)))

#----- Prepare useful objects

# Background rectangles
bgreg <- aggregate(id ~ region, big_cityres, range)
bgreg <- do.call(data.frame, bgreg)
names(bgreg)[-1] <- c("min", "max")

#----- Create background plot
bgplot <- ggplot(big_cityres, aes(y = id, group = agegroup, col = agegroup)) + 
  theme_classic() + 
  geom_rect(data = bgreg, mapping = aes(ymin = min - .5, ymax = max + .5, 
    xmin = -Inf, xmax = Inf, fill = region), alpha = .1, inherit.aes = F) + 
  scale_fill_manual(values = brewer.pal(4, "Accent"),
    name = "Region") + 
  scale_y_continuous(name = "City", 
    breaks = seq_along(unique(big_cityres$URAU_NAME)), 
    labels = unique(big_cityres$URAU_NAME),
    trans = "reverse") + 
  geom_vline(xintercept = 1) + 
  geom_hline(aes(yintercept = id - .5), lty = 3) + 
  theme(axis.ticks.y = element_blank()) + 
  scale_x_continuous(n.breaks = 4, limits = c(.8, 2.5)) + 
  geom_pointrangeh(position = position_dodgev(.8), size = .3)

#----- Add values for heat and cold

coldplot <- bgplot + 
  aes(x = rrcold, xmin = rrcold_low, xmax = rrcold_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Blues")[-(1:2)]) +
  xlab(sprintf("RR at percentile %i", resultper[1])) 
  
heatplot <- bgplot + 
  aes(x = rrheat, xmin = rrheat_low, xmax = rrheat_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Reds")[-(1:2)]) +
  xlab(sprintf("RR at percentile %i", resultper[2]))  + 
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank())

#----- Put together and save

# Create a "legend-plot" for the common color scale legend
legplot <- ggplot(big_cityres, aes(y = id, group = agegroup, col = agegroup)) + 
  theme_void() + xlim(c(0,0)) + 
  geom_pointrangeh(aes(x = rrheat, xmin = rrheat_low, xmax = rrheat_hi)) +
  scale_color_manual(name = "Age group",
    values = brewer.pal(length(agebreaks) + 3, "Greys")[-(1:2)])

# Put everything together
coldplot + heatplot + 
  legplot + plot_layout(widths = c(1, 1, .1), guides = "collect")

# Save
ggsave("figures/Fig1_CapitalRes.pdf", height = 10)

#---------------------------
#  Figure 2: Standardized rates
#---------------------------

#----- Standardised rates

# Cold AN
stdcoldmap <- ggplot(data = subset(cityres, agegroup == agelabs[1])) + 
  theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = stdrate_cold_est, size = pop), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue", 
    name = "Cold (x100 000)") + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

# Heat
stdheatmap <- ggplot(data = subset(cityres, agegroup == agelabs[1])) + 
  theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = stdrate_heat_est, size = pop), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred", 
    name = "Heat (x100 000)") + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

# Put them together and save

stdcoldmap + stdheatmap

ggsave("figures/Fig2_StdRates.pdf", width = 10)


#----------------------
# Age effect
#----------------------

pal <- viridis(length(agebreaks), direction = -1)

# Plot all for ages
layout(matrix(1:2, ncol = 2), width = c(4, 1))
plot(NA, bty = "l", xaxt = "n", 
  xlab = "Temperature percentile", ylab = "RR",
  xlim = range(ovper), 
  ylim = c(min(sapply(agecp, "[[", "allRRlow")), 
    max(sapply(agecp, "[[", "allRRhigh"))))
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)
for (i in seq_along(agebreaks)){
  lines(agecp[[i]], ptype = "overall", col = pal[i], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(pal[i], .2)))
}
abline(h = 1)
par(mar = c(5, 0, 4, 0) + .1)
plot.new()
legend("topleft", legend = agebreaks, col = pal, lty = 1, lwd = 2, 
  title = "Age", bty = "n")

dev.print(pdf, file = "figures/Fig3 - AgeERF.pdf")

#---------------------------
#  Metavariable components
#---------------------------

#----- Screeplot
# plot(summary(pcares)$importance[3,] * 100, type = "b", pch = 16, col = 4, 
#   ylab = "Cumulative variance proportion (%)", xlab = "Principal component")
# abline(h = (summary(pcares)$importance[3, npc] * 100), lty = 2)

#----- Interpretation of PCs

# # Common scale
# ylims <- range(loads)
# 
# # Plot sequentially
# x11(height = 15, width = 10)
# par(mfrow = c(7, 1), mar = c(1, 4, 3, 2) + .1, oma = c(7, 0, 0, 0))
# for (i in seq_len(npc)) {
#   largest <- rank(-abs(loads[,i])) <= 5
#   bp <- barplot(loads[,i], names.arg = "", ylim = ylims, 
#     main = sprintf("PC%i", i), col = adjustcolor(ifelse(largest, 2, 4), .5))
#   text(bp[largest,], loads[largest,i] / 2, rownames(loads)[largest], srt = 90,
#     adj = c(0.5, 0.5), cex = .9)
#   abline(h = 0)
# }
# axis(1, at = bp, labels = rownames(loads), las = 3)
# 
# dev.print(pdf, file = "figures/PCs_bp.pdf")

# Plot as a corplot
x11(width = 10)
corrplot(t(loads), method = "square", is.corr = F, cl.lim = c(-1, 1))
dev.print(pdf, file = "figures/PLScor.pdf")
dev.off()

#----- Coefficients associated to metapredictors

# Create overall data.frame for plotting
backdf <- data.frame(est = c(t(backcoefs)), se = sqrt(diag(backvcov)), 
  coef = rep(colnames(coefs), nm),
  var = factor(rep(metaprednames, each = nc), levels = metaprednames))

# Plot all of them
ggplot(backdf, aes(x = var)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), 
    width = .3) + 
  geom_point(aes(y = est), col = 4) + 
  facet_wrap(~ coef, ncol = 1) + 
  xlab("Metapredictor") + ylab("Coefficient") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/MetapredCoefficients.pdf", height = 15)

#----- Effect of metapredictors

# By an increase of 1 SD
x11(height = 15, width = 10)
par(mfrow = c(7, 3), mar = c(4, 4, 3, 1))
for (i in seq_along(metaprednames)){
  plot(backcp[[i]], ptype = "overall", ylab = "RR increase", 
    xlab = "Temperature percentile", main = metaprednames[i],
    ylim = c(.9, 1.1), lwd = 2, col = NA, xaxt = "n")
  abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  ovcurve <- backcp[[i]]$allRRfit
  lines(ovper, ovcurve, lwd = 2, col = "darkred")
  ovcurve[ovcurve > 1] <- NA
  lines(ovper, ovcurve, lwd = 2, col = "forestgreen")
  abline(h = 1)
  text(mean(par("usr")[1:2]), par("usr")[3], 
    sprintf("p-value = %0.4f", waldres[i,2]), pos = 3, cex = 1.2)
}

dev.print(pdf, file = "figures/Fig4 - MetapredEffectSD.pdf", 
  width = 10, height = 15)

# Difference between low and large values
cols <- plasma(2, direction = -1, begin = .1, end = .9)

x11(height = 15, width = 10)
par(mfrow = c(7, 3), mar = c(4, 4, 3, 1))
for (i in seq_along(metaprednames)){
  inds <- 1:2 + 2*(i-1)
  plot(NA, ylab = "RR", xlab = "Temperature percentile", 
    main = metaprednames[i], xaxt = "n", bty = "l",
    xlim = range(ovper), ylim = c(.95, 2))
  abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  lines(extcp[[inds[1]]], ptype = "overall", col = cols[1], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(cols[1], .2)))
  lines(extcp[[inds[2]]], ptype = "overall", col = cols[2], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(cols[2], .2)))
  abline(h = 1)
  legend("topleft", legend = c("low", "high"), bty = "n", ncol = 2, cex = .8,
    lwd = 2, col = cols)
}
dev.print(pdf, file = "figures/MetapredEffectDiff.pdf", width = 10, height = 15)

#-------------------------
# Background effect
#-------------------------

# MMP
ggplot(data = bggrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP") + 
  geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
    alpha = .4, pch = 16)

ggsave("figures/bg_mmp.pdf")

# Cold
ggplot(data = bggrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rr[,1])) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]))

ggsave("figures/bg_rrcold.pdf")

# Heat
ggplot(data = bggrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rr[,2])) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]))

ggsave("figures/bg_rrheat.pdf")


#---------------------------
# Exposure response functions
#---------------------------

#----- All ERF

pdf("figures/ERFcities.pdf", width = 9, height = 13)
layout(matrix(seq(6 * 4), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(cityERF)){
  # Part of the curve above MMP
  heatind <- predper >= cityres[i, "mmp"] 
  
  # Plot cold and heat separately
  plot(cityERF[[i]], xlab = "Temperature (?C)", ylab = "RR", 
    main = metadata[i, "URAU_NAME"], col = 4, lwd = 2)
  lines(cityERF[[i]]$predvar[heatind], cityERF[[i]]$allRRfit[heatind], 
    col = 2, lwd = 2)
  abline(v = cityres[i, "mmt"], lty = 2)
}

dev.off()

