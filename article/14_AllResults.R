################################################################################
#
#                         MCC-EUcityTRM
#
#                   Big city-level results Table
#
################################################################################

source("11_ResultsVulnerability.R")

#-------------------------
# Creates pretty Table
#-------------------------

# Merge age-specific and total results
bigtable <- merge(cityageres, 
  cityres[,c("URAU_CODE", grep("stdrate", names(cityres), value = T))], 
  by = "URAU_CODE")

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
bigtable <- bigtable[with(bigtable, order(region, CNTR_CODE, URAU_CODE)),]

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
path <- "temp/CityResults.xlsx"
saveWorkbook(wb, path)