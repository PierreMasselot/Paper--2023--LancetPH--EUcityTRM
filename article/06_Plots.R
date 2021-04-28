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

#---------------------------
#  PCA
#---------------------------

#----- Screeplot
plot(summary(pcares)$importance[3,] * 100, type = "b", pch = 16, col = 4, 
  ylab = "Cumulative variance proportion (%)", xlab = "Principal component")
abline(h = (summary(pcares)$importance[3, npc] * 100), lty = 2)

#----- Interpretation of PCs

# Get eigenvectors
loads <- pcares$rotation[,seq_len(npc)] 

# Common scale
ylims <- range(loads)

# Plot sequentially
x11(height = 15, width = 10)
par(mfrow = c(4, 1), mar = c(1, 4, 3, 2) + .1, oma = c(7, 0, 0, 0))
for (i in seq_len(npc)) {
  largest <- rank(-abs(loads[,i])) <= 5
  bp <- barplot(loads[,i], names.arg = "", ylim = ylims, 
    main = sprintf("PC%i", i), col = adjustcolor(ifelse(largest, 2, 4), .5))
  text(bp[largest,], loads[largest,i] / 2, rownames(loads)[largest], srt = 90,
    adj = c(0.5, 0.5), cex = .9)
  abline(h = 0)
}
axis(1, at = bp, labels = rownames(loads), las = 3)

dev.print(pdf, file = "figures/PCs.pdf")

#---------------------------
#  Maps
#---------------------------

#----- Objects used in all maps

# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Complete data.frame with all info
metacomplete <- cbind(metadesc, metavar, do.call(rbind, metageo$geometry))
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
# MMP
ggplot(data = cityres) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, size = metavar$pop, fill = mmp), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
    name = "MMP", limit = c(50, 100)) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

ggsave("figures/cities_mmp.pdf", device = pdf)

# Cold
ggplot(data = cityres) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = rr[,1], size = metavar$pop), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkblue", 
    name = sprintf("RR at percentile %i", resultper[1])) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

ggsave("figures/cities_rrcold.pdf", device = pdf)

# Heat
ggplot(data = cityres) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = rr[,2], size = metavar$pop), 
    alpha = .9, pch = 21) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_fill_gradient(low = "white", high = "darkred", 
    name = sprintf("RR at percentile %i", resultper[2])) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5))

ggsave("figures/cities_rrheat.pdf", device = pdf)

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