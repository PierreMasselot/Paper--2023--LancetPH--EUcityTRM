################################################################################
#
#                         MCC-EUcityTRM
#
#                             Tables
#
################################################################################

if (length(ls()) == 0) source("11_ResultsVulnerability.R")

#---------------------------
# Table 1 : country summary
#---------------------------

#----- Info for all countries

# Extract metadata summary for each country
meta_summary <- by(metadata, metadata$CNTR_CODE, function(x){
  data.frame(region = unique(x$region), cntr_name = unique(x$cntr_name), 
    ncities = nrow(x), nmcc = sum(x$inmcc), 
    pop = sum(x$pop), deaths = with(x, sum(deathrate_tot * pop)),
    tmean = mean(x$tmean))
})

# Bind everything
metasumdf <- do.call(rbind, meta_summary)
metasumdf$cntr_code <- names(meta_summary)

# Add info from era5
tquartiles <- tapply(era5series, metadata$CNTR_CODE, function(x){
  quantile(unlist(lapply(x, "[[", "era5landtmean")), 
    c(.25, .75), na.rm = T)
})
matquart <- do.call(rbind, tquartiles)
colnames(matquart) <- sprintf("quartile_%i", c(25, 75))

# Add to summary df
country_summary <- cbind(metasumdf, matquart[metasumdf$cntr_code,])

# Add a total line
totline <- summarise(country_summary, region = NA, cntr_name = NA, 
  ncities = sum(ncities),
  nmcc = sum(nmcc), pop = sum(pop), deaths = sum(deaths), tmean = mean(tmean), 
  cntr_code = NA, quartile_25 = mean(quartile_25), 
  quartile_75 = mean(quartile_75))
country_summary <- rbind(country_summary, totline)

#----- Clean and export

# Format population
country_summary$pop <- formatC(country_summary$pop,
  format = "f", big.mark = ",", digits = 0)

# Format total deaths
country_summary$deaths <- gsub("NA", "", formatC(country_summary$deaths,
  format = "f", big.mark = ",", digits = 0))

# Add new variable for temp IQR
country_summary$tmean_IQR <- with(country_summary, 
  sprintf("%2.1f (%2.1f - %2.1f)", tmean, quartile_25, quartile_75))

# Add percentage of MCC
country_summary$nmcc <- with(country_summary, 
  sprintf("%i (%1.0f%%)", nmcc, 100 * nmcc / ncities))

# Reorder lines and columns
tab_export <- country_summary[with(country_summary, order(region, cntr_name)),
  c("region", "cntr_name", "ncities", 
  "nmcc", "pop", "deaths", "tmean_IQR")]

# Export
write.table(tab_export, "figures/Table1.txt", sep = ";",
  row.names = F, quote = F, na = "-")

#---------------------------
# Table 2 : country results
#---------------------------

# How to display results
disp <- "%s (%s - %s)"

# Create pretty display
country_res_export <- data.frame(countryres[,c("region", "cntr_name")],
  mapply(function(x, digits) sprintf(disp, 
    formatC(countryres[, paste0(x, "_est")], format = "f", big.mark = ",", 
      digits = digits), 
    formatC(countryres[, paste0(x, "_low")], format = "f", 
      big.mark = ",", digits = digits),
    formatC(countryres[, paste0(x, "_hi")], format = "f", 
      big.mark = ",", digits = digits)),
  x = c("excess_cold", "excess_heat", "af_cold", "af_heat", 
    "rate_cold", "rate_heat", "stdrate_cold", "stdrate_heat"), 
  digits = rep(c(0, 2, 0, 0), each = 2)))

# Total lines
tot_pretty <- mapply(function(x, digits) sprintf(disp, 
  formatC(regionres[, paste0(x, "_est")], format = "f", big.mark = ",", 
    digits = digits), 
  formatC(regionres[, paste0(x, "_low")], format = "f", 
    big.mark = ",", digits = digits),
  formatC(regionres[, paste0(x, "_hi")], format = "f", 
    big.mark = ",", digits = digits)),
  x = c("excess_cold", "excess_heat", "af_cold", "af_heat", 
    "rate_cold", "rate_heat", "stdrate_cold", "stdrate_heat"), 
  digits = rep(c(0, 2, 0, 0), each = 2))

# Add total lines
tot_pretty <- data.frame(regionres[,1], "Total", tot_pretty)
colnames(tot_pretty) <- colnames(country_res_export)
country_res_export <- rbind(country_res_export, tot_pretty)

# Reorder
# country_res_export$region <- factor(country_res_export$region, 
#   levels = c(regord, "Total"))
# country_res_export$cntr_name <- factor(country_res_export$cntr_name, 
#   levels = c(sort(unique(countryres$cntr_name)), "Total"))
country_res_export <- country_res_export[with(country_res_export, 
  order(region, cntr_name)),]

# Export
write.table(country_res_export, "figures/Table2.txt", sep = ";",
  row.names = F, quote = F, na = "-")
