################################################################################
#
#                         MCC-EUcityTRM
#
#                             Tables
#
################################################################################

source("10_ResultsVulnerability.R")

#---------------------------
# Table 1 : country summary
#---------------------------

#----- Info for all countries

# Extract metadata summary for each country
meta_summary <- by(metadata, metadata$CNTR_CODE, function(x){
  data.frame(region = unique(x$region), ncities = nrow(x), nmcc = sum(x$inmcc), 
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
totline <- summarise(country_summary, region = NA, ncities = sum(ncities),
  nmcc = sum(nmcc), pop = sum(pop), deaths = sum(deaths), tmean = mean(tmean), 
  cntr_code = NA, quartile_25 = mean(quartile_25), 
  quartile_75 = mean(quartile_75))
country_summary <- rbind(country_summary, totline)

# #----- Add summary from MCC series
# 
# # Match countries
# mcc_cntrcode <- metadata$CNTR_CODE[match(names(dlist), metadata$mcc_code)]
# 
# # MCC summaries by country
# mccbycountry <- tapply(dlist, mcc_cntrcode, function(x){
#   data.frame(
#     deaths = sum(sapply(x, function(y) sum(y[[ifelse(any(
#       grepl("all", names(y))), "all", "nonext")]], na.rm = T)), na.rm = T),
#     agegrps = paste(substr(grep("all_", names(x[[1]]), value = T), 5, 9),
#       collapse = "; "),
#     mcc_period = paste(range(sapply(x, "[[", "year")), collapse = " - "))
# })
# 
# # Bind everything
# mccsumdf <- do.call(rbind, mccbycountry)
# mccsumdf$cntr_code <- names(mccbycountry)
# 
# # Merge with summary from metadata
# country_summary <- merge(metasumdf, mccsumdf, by = "cntr_code", all.x = T)

#----- Clean and export

# Format population
country_summary$pop <- formatC(country_summary$pop,
  format = "f", big.mark = " ", digits = 0)

# Format total deaths
country_summary$deaths <- gsub("NA", "", formatC(country_summary$deaths,
  format = "f", big.mark = " ", digits = 0))

# Add names
country_summary$cntr_name <- eurcntr[match(country_summary$cntr_code, 
  eurcntr[,1]),2]
country_summary$cntr_name[is.na(country_summary$cntr_name)] <- "Total"


# Add new variable for temp IQR
country_summary$tmean_IQR <- with(country_summary, 
  sprintf("%2.1f (%2.1f - %2.1f)", tmean, quartile_25, quartile_75))

# Add percentage of MCC
country_summary$nmcc <- with(country_summary, 
  sprintf("%i (%1.0f%%)", nmcc, 100 * nmcc / ncities))

# Reorder variables
tab_export <- country_summary[,c("cntr_name", "region", "ncities", 
  "nmcc", "pop", "deaths", "tmean_IQR")]

# Export
write.table(tab_export, "figures/Table1.csv", sep = ",",
  row.names = F, quote = F, na = "-")

#---------------------------
# Table 2 : country results
#---------------------------

# How to display results
disp <- "%s (%s - %s)"

# Create pretty display
country_res_export <- data.frame(countryres[,c("cntr_name", "region")],
  sapply(grep("est", colnames(countryres), value = T),
    function(x)  sprintf(disp, 
      formatC(countryres[, x], format = "f", big.mark = " ", digits = 0), 
      formatC(countryres[, gsub("est", "low", x)], format = "f", 
        big.mark = " ", digits = 0),
      formatC(countryres[, gsub("est", "hi", x)], format = "f", 
        big.mark = " ", digits = 0))))

# Remove sum of heat and cold
country_res_export <- country_res_export[,-grep("total", 
  colnames(country_res_export))]

# Total line
totform <- formatC(unlist(tail(regionres[-1], 1)), 
  format = "f", big.mark = " ", digits = 0)
totline <- outer(c("excess", "rate", "stdrate"), c("cold", "heat"), 
  function(x, y){
    sprintf(disp, totform[paste(x, y, "est", sep = "_")],
      totform[paste(x, y, "low", sep = "_")],
      totform[paste(x, y, "hi", sep = "_")])
})
country_res_export <- rbind(country_res_export, c("Total", NA, t(totline)))

# Export
write.table(country_res_export, "figures/Table2.csv", sep = ",",
  row.names = F, quote = F, na = "-")

#---------------------------
# Supplementary tables: meta-variables
#---------------------------

#----- Used in meta-regression model

# Select variables in meta-regression
metaregvars <- metadesc[match(unlist(metapreds), metadesc$metavar),]

# Create table
desctab <- metaregvars[,2:3]
names(desctab) <- c("Variable", "Source")

# Add a category column
desctab$Category <- rep(names(metapreds), lengths(metapreds))

# Add info about imputed values
desctab$Imputed <- sprintf("%i (%i%%)", metaregvars$nmis, metaregvars$propmis)

# Export
write.table(desctab, "figures/SupTable_metavars.csv", sep = ",",
  row.names = F, quote = F, na = "-")

#----- Others

# Unselect variables
othervars <- subset(metadesc, !metavar %in% unlist(metapreds))

# Regroup age group variables
splitnames <- strsplit(othervars$metavar, "_")
groupages <- tapply(sapply(splitnames, "[", 2), sapply(splitnames, "[", 1),
  function(x) paste(sort(x), collapse = "; "))

# Add other info
extractgroup <- by(othervars, sapply(splitnames, "[", 1), function(x) 
  data.frame(c(x[1,c("label", "source")], sprintf("%i (%i%%)", 
    x$nmis[1], x$propmis[1]))))
otherdesc <- do.call(rbind, extractgroup)

# Clean 
names(otherdesc) <- c("Variable", "Source", "Imputed")
otherdesc$"Age groups" <- groupages

# Export
write.table(otherdesc, "figures/SupTable_othermetavars.csv", sep = ",",
  row.names = F, quote = F, na = "-")