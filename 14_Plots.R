################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Part 14: Create graphical output
#
################################################################################

if (length(ls()) == 0) source("12_ResultsVulnerability.R")

#----------------------
# Figure 1a: Age effect
#----------------------

#----- Prepare plot

# Select specific ages
selagecp <- agecp[agegrid %in% agebreaks[-1]]

# Palette
pal <- viridis(length(selagecp), direction = -1)

#----- Plot all for ages

# Plot outline
plot(NA, bty = "l", xaxt = "n", 
  xlab = "Temperature percentile", ylab = "RR",
  xlim = range(ovper), 
  ylim = c(min(sapply(selagecp, "[[", "allRRlow")), 
    max(sapply(selagecp, "[[", "allRRhigh"))))
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)

# Add age curves
for (i in seq_along(selagecp)){
  lines(selagecp[[i]], ptype = "overall", col = pal[i], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(pal[i], .2)))
}
abline(h = 1)

# Add legend
legpars <- list(legend = agebreaks[-1], col = pal, lty = 1, lwd = 2, 
  title = "Age", bty = "n", horiz = T, xpd = T)
legdim <- do.call(legend, c(legpars, list(x = "center", plot = F)))
do.call(legend, c(legpars, 
  list(x = mean(par("usr")[1:2]) - legdim$rect$w / 2, 
    y = par("usr")[4] + legdim$rect$h)))

# Save
dev.print(pdf, file = "figures/Fig1a_AgeERF.pdf", width = 10, height = 8)

#----------------------
# Figure 1b: MMP versus age
#----------------------

#----- Prepare

# Create data.frame
mmpdf <- data.frame(age = agegrid, mmp = agemmp, mmpci)

# Color palette
cipal <- adjustcolor(viridis(length(alphalist), direction = -1), .4)
names(cipal) <- sprintf("a%02i", alphalist * 100)

#----- Plot

# Initialize ribbons
ribs <- lapply(seq_along(alphalist), function(i) {
  varnames <- sprintf("%s_%i", c("low", "high"), alphalist[i] * 100)
  geom_ribbon(aes(ymin = .data[[varnames[1]]], ymax = .data[[varnames[2]]], 
    fill = names(cipal)[i]))
})

# Labels
labs <- sprintf("%i%%", 100 - alphalist * 100)
names(labs) <- names(cipal)

# Create full plot with Ribbons
Reduce("+", ribs, init = ggplot(mmpdf, aes(x = age))) + 
  scale_fill_manual(values = cipal, name = "CI", labels = labs, 
    breaks = rev(names(cipal))) +
  geom_line(aes(y = mmp), linewidth = 1.5) + 
  scale_x_continuous(name = "Age", 
    breaks = seq(minage, 100, by = 10)) + 
  scale_y_continuous(name = "MMP",
    breaks = ovper[predper %in% seq(30, 90, by = 10)], 
    labels = seq(30, 90, by = 10)) + 
  # coord_cartesian(ylim = ovper[predper %in% c(50, 90)]) +
  theme_classic() +
  theme(axis.text = element_text(size = 12), 
    axis.text.x = element_text(margin = margin(t = 6)),
    axis.text.y = element_text(margin = margin(r = 6)),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    panel.grid.major = element_line(linetype = 2, color = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(6, "pt"),
    legend.position = "top")

ggsave(file = "figures/Fig1b_AgeMMP.pdf")


#---------------------------
#  Figure 2: Risks for capital cities
#---------------------------

#----- Select capital city of each country

# Select cities in result (city with number 001 is usually the capital)
big_cityres <- subset(cityageres, substr(URAU_CODE, 3, 5) == "001")

# Order by region, country name and age
big_cityres <- big_cityres[with(big_cityres, 
  order(region, cntr_name, URAU_CODE, agegroup)),]
big_cityres$id <- as.numeric(factor(big_cityres$URAU_CODE, 
  levels = unique(big_cityres$URAU_CODE))) + 
  as.numeric(factor(big_cityres$region, 
    levels = unique(big_cityres$region)))
  
#----- Prepare useful objects

# Background lines
unid <- unique(big_cityres$id)
bglines <- data.frame(pos = unid[-1][diff(unid) == 1] - .5)

# Region label position
regpos <- aggregate(id ~ region, data = big_cityres, mean)

#----- Create background plot
bgplot <- ggplot(big_cityres, 
    aes(x = id, group = agegroup, col = agegroup)) + 
  theme_classic() + 
  scale_x_continuous(name = "", 
    breaks = unique(big_cityres$id), 
    labels = unique(big_cityres$LABEL)) + 
  geom_hline(yintercept = 1) + 
  geom_vline(data = bglines, aes(xintercept = pos), lty = 3, colour = "grey") +
  theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(),
    axis.text.x.bottom = element_text(angle = 90, vjust = .5, hjust = 1,
      size = 12),
    axis.text.x.top = element_text(size = 15),
    panel.grid.major.y = element_line(linetype = 3, colour = "grey")) + 
  geom_pointrange(position = position_dodge(.8), size = .3) + 
  # coord_cartesian(ylim = c(.8, 3)) + 
  scale_y_continuous(limits = 
      range(with(big_cityres, c(rrcold_low, rrcold_hi, rrheat_low, rrheat_hi))))

#----- Add values for heat and cold

coldplot <- bgplot + 
  aes(y = rrcold, ymin = rrcold_low, ymax = rrcold_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 2, "Blues")[-(1:2)]) +
  ylab(sprintf("RR at temperature\npercentile %i", resultper[1])) + 
  scale_x_continuous(name = "", breaks = NULL,
    sec.axis = sec_axis(trans = ~., name = "", breaks = regpos$id, 
      labels = regpos$region))

heatplot <- bgplot + 
  aes(y = rrheat, ymin = rrheat_low, ymax = rrheat_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 2, "Reds")[-(1:2)]) +
  ylab(sprintf("RR at temperature\npercentile %i", resultper[2]))

#----- Put together and save

# Create a "legend-plot" for the common color scale legend
legplot <- ggplot(big_cityres, aes(x = id, group = agegroup, col = agegroup)) +
  theme_void() + ylim(c(0,0)) +
  geom_pointrange(aes(y = rrheat, ymin = rrheat_low, ymax = rrheat_hi)) +
  scale_color_manual(name = "Age group",
    values = brewer.pal(length(agebreaks) + 2, "Greys")[-(1:2)]) + 
  theme(legend.position = "top", legend.direction = "horizontal")

# Put everything together
coldplot / heatplot / legplot + 
  plot_layout(heights = c(1, 1, .1))

# Save
ggsave("figures/Fig2_CapitalRes.pdf", height = 8, width = 15)

#---------------------------
#  Figure 3: Excess rates by country and age group
#---------------------------

#----- Prepare data

# Order by region, country name and age
plotageres <- countryageres[with(countryageres, 
  order(region, cntr_name, agegroup)),]
plotageres$id <- as.numeric(factor(plotageres$CNTR_CODE, 
  levels = unique(plotageres$CNTR_CODE))) + 
  as.numeric(factor(plotageres$region, 
    levels = unique(plotageres$region))) - 1
plotageres$type <- "cntr"

# Add total info
regionageres2 <- merge(regionageres, 
  aggregate(id ~ region, data = plotageres, max), all.x = T)
regionageres2$id <- regionageres2$id + 1
regionageres2[regionageres2$region == "Total","id"] <- 
  max(regionageres2$id, na.rm = T) + 1

# Add to plot data.frame
regionageres2$type <- ifelse(regionageres2$region == "Total", "eu", "reg")
regionageres2$CNTR_CODE <- regionageres2$region
regionageres2$cntr_name <- ifelse(regionageres2$region == "Total", 
  "Europe", "Total")
levels(regionageres2$region)[5] <- ""
plotageres <- rbind(plotageres, regionageres2)

# Transform some variables into factors
plotageres$region <- factor(plotageres$region, 
  levels = c(regord, ""))
plotageres$type <- factor(plotageres$type, 
  levels = c("cntr", "reg", "eu"), ordered = T)

# Compute scaling factor for cold and heat
scalecold <- max(aggregate(ratetotpop_cold_est ~ CNTR_CODE, 
  plotageres, sum)[,2])
scaleheat <- max(aggregate(ratetotpop_heat_est ~ CNTR_CODE, 
  plotageres, sum)[,2])

# Derive pretty breaks for heat and cold
prettycold <- pretty(c(0, scalecold))
prettyheat <- pretty(c(0, scaleheat))[-1]

# Inds for labels
labind <- seq(1, nrow(plotageres), by = 5)

#----- Create plot
ggplot(plotageres, aes(y = id)) + theme_classic() + 
  scale_y_continuous(name = "", labels = plotageres$cntr_name[labind], 
    breaks = plotageres$id[labind], trans = "reverse") + 
  geom_colh(aes(x = -ratetotpop_cold_est / scalecold, fill = agegroup, 
    alpha = type), width = .8, colour = "white") +
  scale_fill_manual(values = brewer.pal(length(agelabs) + 2, "Blues")[-(1:2)],
    guide = "none") +
  new_scale("fill") + 
  geom_colh(aes(x = ratetotpop_heat_est / scaleheat, fill = agegroup, 
    alpha = type), width = .8, colour = "white") +
  scale_fill_manual(values = brewer.pal(length(agelabs) + 2, "Reds")[-(1:2)], 
    guide = "none") + 
  scale_x_continuous(
    breaks = c(prettyheat / scaleheat, -prettycold / scalecold),
    labels = c(prettyheat, prettycold),
    name = sprintf("Excess death rate (x %s)",
      formatC(byrate, digits = 0, format = "f", big.mark = ","))) +
  geom_vline(xintercept = 0) + 
  facet_grid(rows = vars(region), scales = "free_y", space = "free_y") +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    axis.text.y.left = element_text(size = 10, vjust = 0.2, colour = "black"),
    panel.grid.major.x = element_line(linetype = 3, colour = "grey"),
    strip.background = element_rect(color = NA), 
    strip.text = element_text(size = 12),
    legend.position = "top", legend.direction = "horizontal") + 
  scale_alpha_manual(values = c(.5, .9, 1), guide = "none") + 
  new_scale("fill") + 
  geom_colh(aes(x = ratetotpop_cold_est - ratetotpop_cold_est, 
    fill = agegroup)) + 
  scale_fill_manual(values = brewer.pal(length(agelabs) + 2, "Greys")[-(1:2)],
    name = "Age group")

# Save
ggsave("figures/Fig3_CountryExcess.pdf", height = 8, width = 12)

#---------------------------
#  Figure 4: Big maps of results
#---------------------------

#----- Create a standard map layout

basic_map <- ggplot(data = cityres, aes(x = lon, y = lat, size = pop)) + 
  theme_void() + 
  geom_sf(data = euromap, fill = grey(.95), inherit.aes = F, col = grey(.5)) + 
  coord_sf(xlim = bnlon, ylim = bnlat,
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  scale_size(range = c(2, 8), guide = "none",
    breaks = c(1, 5, 10, 50) * 10^5, labels = c(1, 5, 10, 50) / 10, 
    name = "Population (millions)") + 
  theme(legend.position = "bottom", legend.box = "vertical") + 
  geom_point(alpha = .9, pch = 21, colour = "white", stroke = .1) + 
  guides(fill = guide_coloursteps(title.position = "top", title.hjust = .5,
    barwidth = 20, barheight = .8, even.steps = T))

#----- Add aesthetic and scale for different result variables

# MMT 
cutpts <- unname(round(quantile(cityres$mmt, seq(0, 1, length.out = 7))))
# cutpts <- c(13, 16, 17, 18, 19, 20, 21, 22, 25)
mmtmap <- basic_map + aes(fill = mmt) + 
  scale_fill_stepsn(colours = magma(length(cutpts) - 1, direction = 1),
    values = rescale(cutpts), breaks = cutpts,
    name = "\nMMT (C) at 65 years")
# scale_fill_gradientn(colours = viridis(length(cutpts) - 1, direction = 1),
#   values = rescale(cutpts),
#   name = "MMT")

# MMP 
cutpts <- unname(round(quantile(cityres$mmp, seq(0, 1, length.out = 7))))
mmpmap <- basic_map + aes(fill = mmp) + 
  scale_fill_stepsn(colours = cividis(length(cutpts) - 1, direction = 1),
    values = rescale(cutpts), breaks = cutpts,
    name = "\nMMP (%) at 65 years")
# scale_fill_gradientn(colours = viridis(length(cutpts) - 1, direction = 1),
#   values = rescale(cutpts),
#   name = "MMT")

# Cold excess rates
cutpts <- unname(round(quantile(cityres$stdrate_cold_est, 
  seq(0, 1, length.out = 9)), -1))
stdcoldmap <- basic_map + aes(fill = stdrate_cold_est) + 
  scale_fill_stepsn(
    colours = mako(length(cutpts) - 1, direction = -1, begin = .3),
    values = rescale(cutpts), breaks = cutpts,
    name = sprintf("Cold-related\nstd excess death rate (x %s)", 
      formatC(byrate, digits = 0, format = "f", big.mark = ",")),
    limits = c(0, max(cityres$stdrate_cold_est)))


# Heat with white to red
cutpts <- unique(unname(round(quantile(cityres$stdrate_heat_est / 5, 
  seq(0, 1, length.out = 7))) * 5))
stdheatmap <- basic_map + aes(fill = stdrate_heat_est) + 
  scale_fill_stepsn(
    colours = rocket(length(cutpts) - 1, direction = -1, begin = .3),
    values = rescale(cutpts), breaks = cutpts, 
    name = sprintf("Heat-related\nstd excess death rate (x %s)", 
      formatC(byrate, digits = 0, format = "f", big.mark = ",")),
    limits = c(0, max(cityres$stdrate_heat_est)))

#----- Put maps together

# Put them side-by-side
(mmtmap + mmpmap) / (stdcoldmap + stdheatmap) / 
  
  # Add legend for point size
  basic_map + coord_sf(xlim = c(0, 0), ylim = c(0, 0)) + 
  # scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
  #   labels = c(1, 5, 10, 50) / 10, name = "Population (millions)",
  #   guide = guide_legend(title.position = "top", title.hjust = 0.5,
  #     label.position = "bottom", override.aes = list(colour = "black"))) + 
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5,
    label.position = "bottom", override.aes = list(colour = "black"))) +
  theme(legend.position = "top", 
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  plot_layout(height = c(1, 1, .05))

# Save
ggsave("figures/Fig4_cityMap.pdf", width = 10, height = 15, units = "in")

