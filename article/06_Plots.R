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

#---------------------------
#  Maps
#---------------------------

#----- Objects used in all maps

# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Extent of selected cities
urauext <- st_bbox(metageo)

# Complete data.frame with all info
metacomplete <- cbind(metadesc, metavar, do.call(rbind, metageo$geometry))
names(metacomplete)[(-1:0) + ncol(metacomplete)] <- c("lon", "lat")

#----- Cities with population

ggplot(data = metacomplete) + theme_classic() + 
  geom_sf(data = euromap, fill = NA) + 
  geom_point(aes(x = lon, y = lat, size = pop, col = inmcc)) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)])
