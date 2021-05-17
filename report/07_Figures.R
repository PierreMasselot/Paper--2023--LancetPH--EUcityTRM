################################################################################
#
#                       Exhaustion Level 1
#
#                        Result Figures
#
################################################################################

library(RColorBrewer)

#-------------------------------
# Parameters
#-------------------------------

# Parameters for different outcomes
outcol <- c(1, 2, 4, "darkorchid")
outlab <- c("All", "Cardiovascular", "Respiratory", "Cardiorespiratory")

# Parameters for different lags
lagcol <- brewer.pal(4, "Greens")[c(3,2,4)]

# Parameters for ages
agecol <- brewer.pal(4, "Blues")[2:4]
agelab <- c("All", "65+", "75+")

#-------------------------------
# Figure 3: Continental ERF
#-------------------------------

# Determine maximum RR
mainERFs <- lapply(continentERF[["10"]], "[[", "main")
maxRR <- max(sapply(mainERFs, "[[", "allRRfit"))
ylim = c(1, maxRR)

#----- Plot ERFs

# Initialize plot
layout(cbind(1,2), width = c(4, 2))
plot(NA, xlim = c(0, 100), ylim = ylim, 
  xlab = "Temperature percentile", ylab = "RR")
abline(h = 1)

# Add ERFs
for (i in seq_along(mainERFs)){
  lines(mainERFs[[i]], col = outcol[i], lwd = 2)
}

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("topleft", legend = outlab, col = outcol, lwd = 2, bty = "n")

# Save
dev.print(pdf, file = "results/Figure3.pdf")


#-------------------------------
# Figure 5: Lag sensitivity analysis
#-------------------------------

# Initialize plot
layout(cbind(matrix(1:4, 2, 2), 5), width = c(2, 2, 1))

#----- Loop on outcomes
for (i in 1:4){
  # Extract ERFs
  lagERFs <- lapply(continentERF, function(x) x[[i]]$main)
  
  # Max RR
  maxRR <- max(sapply(lagERFs, "[[", "allRRfit"))
  
  # Initialize plot
  plot(NA, xlim = c(0, 100), ylim = c(1, maxRR), 
    xlab = "Temperature percentile", ylab = "RR",
    main = outlab[i])
  
  # loop on lags
  for (j in seq_along(continentERF)){
    lines(lagERFs[[j]], col = lagcol[j], lwd = 2)
  }
  
  # Add ref
  abline(h = 1)
}

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("topleft", legend = sort(as.numeric(names(continentERF))), 
  col = lagcol[order(as.numeric(names(continentERF)))], lwd = 2, bty = "n",
  title = "Maximum lag")

# Save
dev.print(pdf, file = "results/Figure5.pdf")

#-------------------------------
# Figure 6: Age specific results
#-------------------------------

# Initialize plot
layout(cbind(matrix(1:4, 2, 2), 5), width = c(2, 2, 1))

#----- Loop on outcomes
for (i in 1:4){
  # Extract ERFs
  ageERFs <- continentERF[["10"]][[i]]
  
  # Max RR
  maxRR <- max(sapply(ageERFs, "[[", "allRRfit"))
  
  # Initialize plot
  plot(NA, xlim = c(0, 100), ylim = c(1, maxRR), 
    xlab = "Temperature percentile", ylab = "RR",
    main = outlab[i])
  
  # loop on lags
  for (j in seq_along(ageERFs)){
    lines(ageERFs[[j]], col = agecol[j], lwd = 2)
  }
  
  # Add ref
  abline(h = 1)
}

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("topleft", legend = agelab, col = agecol, lwd = 2, bty = "n",
  title = "Age group")

# Save
dev.print(pdf, file = "results/Figure6.pdf")
