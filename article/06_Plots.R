################################################################################
#
#                         MCC-EUcityTRM
#
#                             Plots
#
################################################################################

library(sf)
library(eurostat)
library(ggplot2)
library(colorspace)
library(fields)
library(scales)
library(viridis)
library(corrplot)

#---------------------------
#  Metavariable components
#---------------------------

#----- Screeplot
# plot(summary(pcares)$importance[3,] * 100, type = "b", pch = 16, col = 4, 
#   ylab = "Cumulative variance proportion (%)", xlab = "Principal component")
# abline(h = (summary(pcares)$importance[3, npc] * 100), lty = 2)

#----- Interpretation of PCs

# Get eigenvectors
# loads <- pcares$rotation[,seq_len(npc)]
loads <- loadings(plsres)[,1:npc]
# rownames(loads) <- metadesc[match(rownames(loads), tolower(metadesc$metavar)), 
#   "label"]

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

#----- Effect of each metapredictor

# Backtransform coefficients
st2coefs <- coef(stage2res)
plscoefs <- st2coefs[grep("pls", names(st2coefs))]
backcoefs <- loads %*% matrix(plscoefs, nrow = npc, byrow = T)

# Monte-Carlo confidence intervals (not sure about this)
plscsim <- metacoefsim[,grep("pls", colnames(metacoefsim))]
backsim <- apply(plscsim, 1, function(x) 
  loads %*% matrix(x, nrow = npc, byrow = T))
backCIs <- apply(backsim, 1, quantile, c(.025, .975))
rownames(backCIs) <- c("low", "hi")

# Create overall data.frame for plotting
backdf <- data.frame(est = c(backcoefs), t(backCIs), 
  coef = rep(colnames(coefs), each = ncol(metavar)),
  var = factor(rep(metaprednames, ncol(coefs)), levels = metaprednames))

# Plot all of them
ggplot(backdf, aes(x = var)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(aes(ymin = low, ymax = hi), width = .3) + 
  geom_point(aes(y = est), col = 4) + 
  facet_wrap(~ coef, ncol = 1) + 
  xlab("Metapredictor") + ylab("Coefficient") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/MetapredCoefficients.pdf", device = "pdf", height = 15)

#---------------------------
#  Maps
#---------------------------

#----- Objects used in all maps

# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Complete data.frame with all info
metacomplete <- cbind(metadata, do.call(rbind, metageo$geometry))
names(metacomplete)[(-1:0) + ncol(metacomplete)] <- c("lon", "lat")

#----- Cities with population

ggplot(data = metacomplete) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, size = pop, col = inmcc), 
    alpha = .4, pch = 16) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5)) + 
  scale_color_discrete(name = "", labels = c("Predicted", "MCC"))

ggsave("figures/urau_cities.pdf", device = pdf)

#----- City predictions
# MMT
ggplot(data = cityres) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, 
    size = rep(metadata$pop, length(agepred)), fill = mmt), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMT", limit = c(0, 30), oob = squish) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5)) + 
  facet_wrap(~ age, labeller = labeller(age = label_both)) + 
  theme(strip.text = element_text(size = 15))

ggsave("figures/cities_mmt.pdf", device = pdf, width = 15, height = 10)

# Cold
ggplot(data = cityres) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = rrcold, 
    size = rep(metadata$pop, length(agepred))), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue", 
    name = sprintf("RR at percentile %i", resultper[1])) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5)) + 
  facet_wrap(~ age, labeller = labeller(age = label_both)) + 
  theme(strip.text = element_text(size = 15))

ggsave("figures/cities_rrcold.pdf", device = pdf)

# Heat
ggplot(data = cityres) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = rrheat, 
    size = rep(metadata$pop, length(agepred))), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred", 
    name = sprintf("RR at percentile %i", resultper[2])) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5)) + 
  facet_wrap(~ age, labeller = labeller(age = label_both)) + 
  theme(strip.text = element_text(size = 15))

ggsave("figures/cities_rrheat.pdf", device = pdf)

#----- Attributable number

# Sum attributable number across all ages
ansum <- aggregate(cbind(an_total_est, an_cold_est, an_heat_est) ~ lon + lat, 
  data = cityres, sum)

# Plot total AN
ggplot(data = ansum) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = an_total_est, size = metadata$pop), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_viridis(option = "inferno", oob = squish, name = "Total AN", 
    limits = c(0, quantile(ansum$an_total_est, .99))) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

ggsave("figures/cities_ANtot.pdf", device = pdf)

# Plot cold AN
ggplot(data = ansum) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = an_cold_est, size = metadata$pop), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue", oob = squish,
    name = "Cold AN", limits = c(0, quantile(ansum$an_cold_est, .99))) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

ggsave("figures/cities_ANcold.pdf", device = pdf)

# Plot heat AN
ggplot(data = ansum) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = an_heat_est, size = metadata$pop), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred", oob = squish,
    name = "Heat AN", limits = c(0, quantile(ansum$an_heat_est, .99))) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

ggsave("figures/cities_ANheat.pdf", device = pdf)

#----- Background effect

# MMP
ggplot(data = bggrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP") + 
  geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
    alpha = .4, pch = 16)

ggsave("figures/bg_mmp.pdf", device = pdf)

# Cold
ggplot(data = bggrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rr[,1])) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue",
    name = sprintf("RR at percentile %i", resultper[1]))

ggsave("figures/bg_rrcold.pdf", device = pdf)

# Heat
ggplot(data = bggrid) + theme_void() + 
  geom_raster(aes(x = lon, y = lat, fill = rr[,2])) + 
  geom_sf(data = euromap, fill = NA) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred",
    name = sprintf("RR at percentile %i", resultper[2]))

ggsave("figures/bg_rrheat.pdf", device = pdf)


#---------------------------
# Exposure response functions
#---------------------------

#----- Average ERF for different ages

pal <- rev(viridis(length(agepred)))

# Retrieve overall ERF
ageERF <- sapply(agecp, "[[", "allRRfit")

# Plot all for ages
layout(matrix(1:2, ncol = 2), width = c(4, 1))
matplot(predper, ageERF, type = "l", lwd = 2, col = pal, 
  xlab = "Temperature percentile", ylab = "RR", lty = 1)
abline(h = 1)
par(mar = c(5, 0, 4, 0) + .1)
plot.new()
legend("topleft", legend = agepred, col = pal, lty = 1, lwd = 2, 
  title = "Age", bty = "n")

dev.print(pdf, file = "figures/AgeERF.pdf")

#----- All ERF

pdf("figures/ERFcities.pdf", width = 9, height = 13)
layout(matrix(seq(6 * 4), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(cityERF)){
  # Part of the curve above MMP
  heatind <- predper >= cityres[i, "mmp"] 
  
  # Plot cold and heat separately
  plot(cityERF[[i]], xlab = "Temperature (°C)", ylab = "RR", 
    main = metadata[i, "URAU_NAME"], col = 4, lwd = 2)
  lines(cityERF[[i]]$predvar[heatind], cityERF[[i]]$allRRfit[heatind], 
    col = 2, lwd = 2)
  abline(v = cityres[i, "mmt"], lty = 2)
}

dev.off()

