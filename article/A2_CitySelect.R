################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix A2: MCC description
#
################################################################################

source("13_Plots.R") # To get the basic_map object

#---------------------------
# Supp Figure: List of cities in MCC
#---------------------------

#----- Cities in and out of MCC

basic_map + aes(fill = inmcc) + 
  scale_fill_discrete(name = "", labels = c("Urban Audit", "MCC")) +
  guides(size = guide_legend(override.aes = list(colour = "black")),
    fill = guide_legend(override.aes = list(size = 5)))  +
  theme(legend.position = "right") 

#----- Save 

ggsave("figures/SupFig_URAUcities.pdf", width = 7, height = 7)

#---------------------------
# Supp Table: Cities in MCC by country
#---------------------------

#----- Prepare information

# Extract metadata summary for each country
meta_summary <- by(metadata, metadata$CNTR_CODE, function(x){
  data.frame(region = unique(x$region), cntr_name = unique(x$cntr_name), 
    ncities = nrow(x), nmcc = sum(x$inmcc))
})

# Bind everything
metasumdf <- do.call(rbind, meta_summary)
metasumdf$cntr_code <- names(meta_summary)

# Match countries
mcc_cntrcode <- metadata$CNTR_CODE[match(names(dlist), metadata$mcc_code)]

# MCC summaries by country
mccbycountry <- tapply(dlist, mcc_cntrcode, function(x){
  data.frame(
    deaths = sum(sapply(x, function(y) sum(y[[ifelse(any(
      grepl("all", names(y))), "all", "nonext")]], na.rm = T)), na.rm = T),
    agegrps = paste(substr(grep("all_", names(x[[1]]), value = T), 5, 9),
      collapse = "; "),
    mcc_period = paste(range(sapply(x, "[[", "year")), collapse = " - "))
})

# Bind everything
mccsumdf <- do.call(rbind, mccbycountry)
mccsumdf$cntr_code <- names(mccbycountry)

# Merge with summary from metadata
mcc_summary <- merge(metasumdf, mccsumdf, by = "cntr_code", all.x = T)

#----- Clean and export

# Format total deaths
mcc_summary$deaths <- gsub("NA", "-", formatC(mcc_summary$deaths,
  format = "f", big.mark = " ", digits = 0))

# Add percentage of MCC
mcc_summary$nmcc <- with(mcc_summary, 
  sprintf("%i (%1.0f%%)", nmcc, 100 * nmcc / ncities))

# Reorder variables
mcc_summary <- mcc_summary[with(mcc_summary, order(region, cntr_name)),
  c("region", "cntr_name", "ncities", "nmcc", "deaths")]

# Rename columns
colnames(mcc_summary) <- c("Region", "Country", "City number", "Cities in MCC",
  "MCC deaths")

# Export
write.table(mcc_summary, "figures/SuppTable_MCCdesc.csv", sep = ",",
  row.names = F, quote = F, na = "-")
