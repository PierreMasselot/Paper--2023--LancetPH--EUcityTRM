################################################################################
#
#                         MCC-EUcityTRM
#
#                             Tables
#
################################################################################

source("11_ResultsVulnerability.R")

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
  sapply(grep("est", colnames(countryres), value = T),
    function(x)  sprintf(disp, 
      formatC(countryres[, x], format = "f", big.mark = ",", digits = 0), 
      formatC(countryres[, gsub("est", "low", x)], format = "f", 
        big.mark = ",", digits = 0),
      formatC(countryres[, gsub("est", "hi", x)], format = "f", 
        big.mark = ",", digits = 0))))

# Remove sum of heat and cold
country_res_export <- country_res_export[,-grep("total", 
  colnames(country_res_export))]

# Total lines
totform <- formatC(data.matrix(regionres[-1]), 
  format = "f", big.mark = ",", digits = 0)
totline <- apply(expand.grid(c("cold", "heat"), c("excess", "rate", "stdrate")), 
  1, function(x){
    sprintf(disp, totform[,paste(x[2], x[1], "est", sep = "_")],
      totform[,paste(x[2], x[1], "low", sep = "_")],
      totform[,paste(x[2], x[1], "hi", sep = "_")])
})

# Add total lines
totline <- data.frame(regionres[,1], "Total", totline)
colnames(totline) <- colnames(country_res_export)
country_res_export <- rbind(country_res_export, totline)

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
