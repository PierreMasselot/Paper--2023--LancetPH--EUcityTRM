################################################################################
#
#                       Exhaustion Level 1
#
#                        Result Figures
#
################################################################################

library(RColorBrewer)
library(eurostat)
library(ggplot2)

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

# Parameters for sex
sexcol <- c(4, 2)
sexlab <- c("M", "F")

#-------------------------------
# Locations
#-------------------------------

# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Plot map
ggplot(data = citydesc) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, size = pop), 
    alpha = .6, col = "cornflowerblue") + 
  coord_sf(xlim = c(-10, 30), ylim = c(35, 65)) + 
  scale_size(breaks = c(10^5, 5*10^5, 10^6, 5*10^6), 
    name = "Population")

ggsave("figures/Map.pdf")


#-------------------------------
# Figure 3: Continental ERF
#-------------------------------

# Determine maximum RR
mainERFs <- lapply(continentERF_mmp[["10"]], "[[", "main")
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
dev.print(pdf, file = "figures/Figure3.pdf")


#-------------------------------
# Figure 4: Lag sensitivity analysis
#-------------------------------

# Initialize plot
layout(cbind(matrix(1:4, 2, 2), 5), width = c(2, 2, 1))

#----- Loop on outcomes
for (i in 1:4){
  # Extract ERFs
  lagERFs <- lapply(continentERF_mmp, function(x) x[[i]]$main)
  
  # Max RR
  maxRR <- max(sapply(lagERFs, "[[", "allRRfit"))
  
  # Initialize plot
  plot(NA, xlim = c(0, 100), ylim = c(1, maxRR), 
    xlab = "Temperature percentile", ylab = "RR",
    main = outlab[i])
  
  # loop on lags
  for (j in seq_along(continentERF_mmp)){
    lines(lagERFs[[j]], col = lagcol[j], lwd = 2)
  }
  
  # Add ref
  abline(h = 1)
}

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("topleft", legend = sort(as.numeric(names(continentERF_mmp))), 
  col = lagcol[order(as.numeric(names(continentERF_mmp)))], lwd = 2, bty = "n",
  title = "Maximum lag")

# Save
dev.print(pdf, file = "figures/Figure4.pdf")

#-------------------------------
# Figure 5: Age specific results
#-------------------------------

# Initialize plot
layout(cbind(matrix(1:4, 2, 2), 5), width = c(2, 2, 1))

#----- Loop on outcomes
for (i in 1:4){
  # Extract ERFs
  ageERFs <- continentERF_mmp[["10"]][[i]]
  
  # Max RR
  maxRR <- max(sapply(ageERFs, "[[", "allRRfit"))
  
  # Initialize plot
  plot(NA, xlim = c(0, 100), ylim = c(1, maxRR), 
    xlab = "Temperature percentile", ylab = "RR",
    main = outlab[i])
  
  # loop on lags
  for (j in 1:3){
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
dev.print(pdf, file = "figures/Figure5.pdf")


#-------------------------------
# Figure 6: Sex specific results
#-------------------------------

# Initialize plot
layout(cbind(matrix(1:4, 2, 2), 5), width = c(2, 2, 1))

#----- Loop on outcomes
for (i in 1:4){
  # Extract ERFs
  sexERFs <- continentERF_mmp[["10"]][[i]]
  
  # Max RR
  maxRR <- max(sapply(sexERFs, "[[", "allRRfit"))
  
  # Initialize plot
  plot(NA, xlim = c(0, 100), ylim = c(1, maxRR), 
    xlab = "Temperature percentile", ylab = "RR",
    main = outlab[i])
  
  # loop on lags
  for (j in 4:5){
    lines(sexERFs[[j]], col = sexcol[j - 3], lwd = 2)
  }
  
  # Add ref
  abline(h = 1)
}

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("topleft", legend = sexlab, col = sexcol, lwd = 2, bty = "n",
  title = "Sex")

# Save
dev.print(pdf, file = "figures/Figure6.pdf")
