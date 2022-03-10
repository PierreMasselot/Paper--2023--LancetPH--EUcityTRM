################################################################################
#
#                         MCC-EUcityTRM
#
#         Appendix C: alternative representations of results
#
################################################################################

source("11_ResultsVulnerability.R")

#-----------------------------
# Summary ERF plot by country
#-----------------------------

#----- List of countries to plot

# Create order vector
ordvec <- with(metadata, order(region, cntr_name))

# Reorder
reg_cntr <- unique(metadata[ordvec, c("region", "CNTR_CODE", "cntr_name")])

#----- Prepare plot 

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

#----- Plot 

# Open pdf
pdf("figures/FigS_ERFbyCountry.pdf", width = 15, height = 20)

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
  
  # # Color and width
  # icol <- ifelse(capind, 1, 
  #   adjustcolor(pal[reg_cntr$region[i]], .4))
  # iwd <- ifelse(capind, 2, 1)
    
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
