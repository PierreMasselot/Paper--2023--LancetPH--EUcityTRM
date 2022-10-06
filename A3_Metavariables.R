################################################################################
#
#                         MCC-EUcityTRM
#
#                 Appendix A3: City-level characteristics
#
################################################################################

if (length(ls()) == 0) source("11_ResultsVulnerability.R")

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
  data.frame(c(x[1,c("label", "source")], sprintf("%i (%i%%)", 
    x$nmis[1], x$propmis[1]))))
otherdesc <- do.call(rbind, extractgroup)

# Clean 
names(otherdesc) <- c("Variable", "Source", "Imputed")
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

#---------------------------
# Plotting metavariables
#---------------------------

ggplot(data = metadata, aes(x = lon, y = lat, size = pop, fill = no2)) + 
  theme_void() + 
  geom_sf(data = euromap, fill = grey(.95), inherit.aes = F, col = grey(.5)) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  scale_size(range = c(2, 8), guide = "none",
    breaks = c(1, 5, 10, 50) * 10^5, labels = c(1, 5, 10, 50) / 10, 
    name = "Population (millions)") + 
  theme(legend.position = "bottom", legend.box = "vertical") + 
  geom_point(alpha = .9, pch = 21, colour = "white", stroke = .1) + 
  guides(fill = guide_coloursteps(title.position = "top", title.hjust = .5,
    barwidth = 20, barheight = .8, even.steps = T)) + 
  scale_fill_viridis(name = "NO2 (mg/m3)")

ggsave("figures/FigX_no2Map.pdf", width = 7, height = 7, units = "in")
