################################################################################
#
#                         MCC-EUcityTRM
#
#                             Plots
#
################################################################################

library(maps)
library(mapdata)
library(sf)

#---------------------------
#  PCA
#---------------------------

#----- Screeplot
screeplot(pcares, type = "l", main = "Screeplot",
  pch = 16, col = 4, lwd = 2, cex = 1.2, xlab = "Component")
box()
abline(h = (summary(pcares)$importance[1, npc])^2, lty = 2)

#---------------------------
#  Maps
#---------------------------

#----- Cities
# get the extent of cities in dataset
# NB: here France is excluded only for extent computation because of overseas 
# territories
urauext <- st_bbox(subset(metageo, substr(URAU_CODE, 1, 2) != "FR"))

# Point size according to population
maxsize <- 2
popcuts <- c(0, 100000, 500000, 1000000, 5000000, 10000000)

poplabs <- sprintf("%s - %s", popcuts[-length(popcuts)], popcuts[-1])
poplabs[1] <- sprintf("< %s", popcuts[2])
poplabs[length(poplabs)] <- sprintf("> %s", popcuts[length(popcuts) - 1])
ptsize <- maxsize * as.numeric(cut(metavar$pop_NUTS3, popcuts)) / 
  (length(popcuts) - 1)

# Draw country layout
map("worldHires", mar = c(0, 0, 0, 0), col = grey(0.95),
  myborder = 0, fill = T, border = grey(0.5), lwd = 0.3,
  xlim = urauext[c(1,3)], ylim = urauext[c(2,4)])

# Add cities
plot(metageo[!metadesc$inmcc,], add = T, col = adjustcolor(2, .5), 
  pch = 16, cex = ptsize[!metadesc$inmcc])
plot(metageo[metadesc$inmcc,], add = T, col = adjustcolor(4, .5), 
  pch = 16, cex = ptsize[metadesc$inmcc])

# Add legends
leg <- legend("topleft", legend = poplabs, pch = 16, 
  col = grey(.5), pt.cex = sort(unique(ptsize)), xpd = NA, inset = .02)
legend(leg$rect$left, leg$rect$top - leg$rect$h, c("MCC", "Predict"),
  pch = 16, col = adjustcolor(c(4,2), .5), xpd = NA)
