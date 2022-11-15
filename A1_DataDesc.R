################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix 1: Data description
#
################################################################################

if (length(ls()) == 0) source("06_PrepSecondStage.R")

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
  c("region", "cntr_name", "ncities", "nmcc", "deaths", 
    "agegrps", "mcc_period")]

# Rename columns
colnames(mcc_summary) <- c("Region", "Country", "City number", "Cities in MCC",
  "MCC deaths", "Age groups", "Data period")

# Export
write.table(mcc_summary, "figures/TableS_MCCdesc.csv", sep = ",",
  row.names = F, quote = F, na = "-")

#---------------------------
# Supp Figure: Map of cities in MCC
#---------------------------

#----- Cities in and out of MCC

ggplot(data = metadata, aes(x = lon, y = lat, size = pop)) + 
  theme_void() + 
  geom_sf(data = euromap, fill = grey(.95), inherit.aes = F, col = grey(.5)) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  scale_size(range = c(2, 8), guide = "none",
    breaks = c(1, 5, 10, 50) * 10^5, labels = c(1, 5, 10, 50) / 10, 
    name = "Population (millions)") + 
  theme(legend.position = "right", legend.box = "vertical") + 
  geom_point(aes(fill = inmcc), alpha = .9, pch = 21, 
    colour = "white", stroke = .1) + 
  scale_fill_discrete(name = "", labels = c("Urban Audit", "MCC")) + 
  guides(size = guide_legend(override.aes = list(colour = "black")),
    fill = guide_legend(override.aes = list(size = 5)))

#----- Save 

ggsave("figures/FigS_URAUcities.png", width = 7, height = 7)

#---------------------------
# Supplementary tables: meta-variables
#---------------------------

#----- Used in meta-regression model

# Select variables in meta-regression
metaregvars <- metadesc[match(unlist(metapreds), metadesc$metavar),]

# Create table
desctab <- metaregvars[,2:3]
names(desctab) <- c("Variable", "Source")

# Export
write.table(desctab, "figures/TableS_metavars.csv", sep = ",",
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
  data.frame(c(x[1,c("label", "source")])))
otherdesc <- do.call(rbind, extractgroup)

# Clean 
names(otherdesc) <- c("Variable", "Source")
otherdesc$"Age groups" <- groupages

# Export
write.table(otherdesc, "figures/TableS_othermetavars.csv", sep = ",",
  row.names = F, quote = F, na = "-")

#---------------------------
# Metavariables availability
#---------------------------

#----- Prepare data

# Compute % of location with data for each year
avail_year <- aggregate(metacityyear[, metadesc$metavar],
  by = metacityyear["year"], function(x) mean(complete.cases(x)) * 100)

# Select variables to plot
avail_year <- avail_year[,c("year", unlist(metapreds), 
  c("deathrate_tot", "prop_0004"))]

# Reshape as long for ggplot
avail_long <- reshape(avail_year, direction = "long", 
  varying = names(avail_year)[-1], v.names = "value", timevar = "variable", 
  idvar = "year", times = names(avail_year)[-1])
avail_long$variable <- factor(avail_long$variable, unique(avail_long$variable))

#----- Plot

# Prepare metavariable labels
varlabs <- metadesc[match(names(avail_year)[-1], metadesc$metavar),"label"]
varlabs[length(varlabs)] <- "Population structure"
names(varlabs) <- names(avail_year)[-1]

# Prepare breaks
xbr <- seq(year[1] - .5, tail(year, 1) + .5, by = .5)
xlabs <- xbr; xlabs[xlabs != round(xlabs)] <- ""

# Make plot
ggplot(avail_long, aes(x = year, y = 1, fill = value)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradientn(colors = c("white", brewer.pal(9, "Blues")), 
    name = "% of cities", guide = guide_colorbar(
      ticks.colour = "black", frame.colour = "black")) + 
  scale_x_continuous(name = "", breaks = xbr, labels = xlabs, 
    expand = c(0, 0)) + 
  facet_wrap(~variable, ncol = 1, labeller = labeller(variable = varlabs),
    strip.position = "left") + 
  ylab("") + 
  theme_classic() + 
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    axis.text.y.left = element_blank(), 
    axis.text.x.bottom = element_text(angle = 45, vjust = .7),
    axis.ticks.x.bottom = element_line(color = c(NA, "black")),
    strip.background = element_rect(color = NA), 
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 12),
    legend.title.align = 1)

# Save
ggsave("figures/FigS_MetavarAvail.png", height = 12, width = 8)