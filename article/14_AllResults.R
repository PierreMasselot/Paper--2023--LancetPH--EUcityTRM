################################################################################
#
#                         MCC-EUcityTRM
#
#                   Big city-level results Table
#
################################################################################

source("11_ResultsVulnerability.R")

#-------------------------
# Parameters
#-------------------------

# Select a specific country
whichcount <- "FR"

#-------------------------
# Creates pretty Table
#-------------------------

# Merge age-specific and total results
bigtable <- merge(cityageres, 
  cityres[,c("URAU_CODE", grep("stdrate", names(cityres), value = T))], 
  by = "URAU_CODE")

if (whichcount != ""){
  bigtable <- subset(bigtable, CNTR_CODE %in% "FR")
}

#----- Pretty variables

# Age specific RRs
bigtable$RR_cold <- with(bigtable, sprintf("%1.2f (%1.2f - %1.2f)", 
  rrcold, rrcold_low, rrcold_hi))
bigtable$RR_heat <- with(bigtable, sprintf("%1.2f (%1.2f - %1.2f)", 
  rrheat, rrheat_low, rrheat_hi))

# Death rates
bigtable$Rate_cold <- with(bigtable, sprintf("%1.0f (%1.0f - %1.0f)", 
  rate_cold_est, rate_cold_low, rate_cold_hi))
bigtable$Rate_heat <- with(bigtable, sprintf("%1.0f (%1.0f - %1.0f)", 
  rate_heat_est, rate_heat_low, rate_heat_hi))

# Standardized death rates
bigtable$StdRate_cold <- with(bigtable, sprintf("%1.0f (%1.0f - %1.0f)", 
  stdrate_cold_est, stdrate_cold_low, stdrate_cold_hi))
bigtable$StdRate_heat <- with(bigtable, sprintf("%1.0f (%1.0f - %1.0f)", 
  stdrate_heat_est, stdrate_heat_low, stdrate_heat_hi))

#----- Tidy table

# Reorder Table
bigtable <- bigtable[with(bigtable, order(region, cntr_name, URAU_CODE)),]

# Select variables to export
bigtable <- bigtable[,c("region", "cntr_name", "URAU_CODE", "LABEL", 
  "agegroup", "agepop", "mmp", "mmt", "RR_cold", "RR_heat", 
  "Rate_cold", "Rate_heat", "StdRate_cold", "StdRate_heat")]

# Rename variables
names(bigtable) <- c("Region", "Country", "City code", "City", "Age group",
  "Age-specific population", "MMP (%)", "MMT (C)", "RR cold", "RR heat",
  sprintf("%s (x %s)", c("Death rate cold", "Death rate heat", 
    "Standardized death rate cold", "Standardized death rate heat"),
    formatC(byrate, format = "f", big.mark = " ", digits = 0)))

#----- Excel export

# Create excel sheet
wb <- createWorkbook()
sh <- createSheet(wb, 'Results')

# Cell styles
cs <- CellStyle(wb, alignment = Alignment(vertical = "VERTICAL_CENTER"))
cslist <- rep(list(cs), ncol(bigtable))
names(cslist) <- 1:ncol(bigtable)

# Add data
addDataFrame(bigtable, sh, col.names = T, row.names = F, 
  colStyle = cslist)

# Merge region
regioncut <- which(diff(as.numeric(bigtable$Region)) > 0)
regionstart <- c(1, regioncut + 1)
regionend <- c(regioncut, nrow(bigtable))
for (i in seq_along(regionstart)) 
  addMergedRegion(sh, regionstart[i] + 1, regionend[i] + 1, 1, 1)

# Merge Country
countrycut <- which(bigtable$Country[-1] != bigtable$Country[-nrow(bigtable)])
countrystart <- c(1, countrycut + 1)
countryend <- c(countrycut, nrow(bigtable))
for (i in seq_along(countrystart)) 
  addMergedRegion(sh, countrystart[i] + 1, countryend[i] + 1, 2, 2)

# Merge city level variables
for (i in seq_len(nrow(metadata))){
  startline <- (i - 1) * 5 + 2
  endline <- i * 5 + 1
  addMergedRegion(sh, startline, endline, 3, 3)
  addMergedRegion(sh, startline, endline, 4, 4)
  addMergedRegion(sh, startline, endline, 13, 13)
  addMergedRegion(sh, startline, endline, 14, 14)
}

# Save workbook
path <- sprintf("figures/TableX_AllCityResults%s.xlsx", whichcount)
saveWorkbook(wb, path)

#---------------------------
# Sup. Figure : Exposure response functions
#---------------------------

#----- Recompute all ERF with a common MMT for cities

# Loop on cities
cityERFplot <- lapply(metadata$URAU_CODE, function(cd){
  # Get era 5 series for current city
  era5cd <- era5series[[cd]]$era5landtmean
  
  # Compute basis
  bvar <- onebasis(quantile(era5cd, predper / 100), 
    fun = varfun, degree = vardegree, 
    knots = quantile(era5cd, varper / 100))
  
  # Extract common mmt
  mmt <- median(subset(cityageres, URAU_CODE == cd, mmt, drop = T))
  
  # Extract coefs for each age group
  coefcd <- cityagecoefs[cityageres$URAU_CODE == cd]
  
  # Final prediction centered on the MMT
  lapply(coefcd, function(b){
    crosspred(bvar, coef = b$fit, vcov = b$vcov, cen = mmt, 
      model.link = "log", at = quantile(era5cd, predper / 100))
  })
})
names(cityERFplot) <- metadata$URAU_CODE

# Reorder
# cityERFplot <- cityERFplot[with(metadata, 
#   order(region, cntr_name, URAU_CODE))]

#----- Plot all ERF

# Prepare palettes
# coldpal <- brewer.pal(length(agelabs) + 1, "Blues")[-1]
# coldpal <- mako(length(agelabs), direction = -1)
# heatpal <- brewer.pal(length(agelabs) + 1, "Reds")[-1]
# heatpal <- rocket(length(agelabs), direction = -1)
# cipal <- rev(grey(seq(.1, .5, length.out = length(agelabs)), .2))
pal <- viridis(length(agelabs), direction = -1)

# Dimensions
nrows <- 6
ncols <- 4

# Prepare output
pdf(sprintf("figures/FigX_AllERF%s.pdf", whichcount), width = 11, height = 15)
layout(matrix(seq(nrows * ncols), nrow = nrows, byrow = T))
par(mar = c(4, 3.8, 3, 2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in which(substr(names(cityERFplot), 1, 2) %in% whichcount)){
  # Part of the curve above MMP
  # heatind <- cityERFplot[[i]][[1]]$predvar >= cityERFplot[[i]][[1]]$cen
  
  # Initialize plot
  plot(cityERFplot[[i]][[1]], xlab = "Temperature (C)", ylab = "RR", 
    main = sprintf("%s\n(%s)", metadata$LABEL[i], metadata$cntr_name[i]), 
    col = NA, lwd = 2, ylim = c(.5, 3), 
    cex.main = .8, cex.lab = .8, cex.axis = .8, ci = "n")
  
  # Loop on age groups
  for (a in seq_along(cityERFplot[[i]])){
    # # Cold part
    # lines(cityERFplot[[i]][[a]], col = coldpal[a], lwd = 2,
    #   ci = "area", ci.arg = list(col = cipal[a]))
    # 
    # # Heat part
    # lines(cityERFplot[[i]][[a]]$predvar[heatind], 
    #   cityERFplot[[i]][[a]]$allRRfit[heatind], 
    #   col = heatpal[a], lwd = 2)
    
    lines(cityERFplot[[i]][[a]], col = pal[a], lwd = 1,
      ci = "area", ci.arg = list(col = adjustcolor(pal[a], .1)))
  }
  
  # MMT
  abline(v = cityERFplot[[i]][[1]]$cen)
  
  # Add percentiles
  cityper <- cityERFplot[[i]][[1]]$predvar[predper %in% c(1, 99)]
  abline(v = cityper, lty = 2)
  
  # Unit line
  abline(h = 1)
  
  # Add legend
  if (i %% ncols == 1) legend("top", legend = agelabs, lwd = 1,
    col = pal, horiz = T, cex = .4, bg = "white")
}

dev.off()

