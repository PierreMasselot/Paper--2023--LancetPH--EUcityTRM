################################################################################
#
#                         MCC-EUcityTRM
#
#                         Other Plots
#
################################################################################

#---------------------------
#  Figure 1bis: Risks for capital cities
#---------------------------

#----- Select capital city of each country

# Find largest pop in each country
# capitals <- unlist(by(metadata, metadata$CNTR_CODE, function(d){
#   d[which.max(d$pop), "URAU_CODE"]
# }))

# Select cities in result (city with number 001 is usually the capital)
# big_cityres <- subset(cityres, URAU_CODE %in% capitals)
big_cityres <- subset(cityres, substr(URAU_CODE, 3, 5) == "001")

# Order by region and age
big_cityres <- big_cityres[with(big_cityres, 
  order(region, cntr_name, URAU_CODE, agegroup)),]
big_cityres$id <- as.numeric(factor(big_cityres$URAU_CODE, 
  levels = unique(big_cityres$URAU_CODE)))

#----- Prepare useful objects

# Background rectangles
bgreg <- aggregate(id ~ region, big_cityres, range)
bgreg <- do.call(data.frame, bgreg)
names(bgreg)[-1] <- c("min", "max")

#----- Create background plot
bgplot <- ggplot(big_cityres, aes(y = id, group = rev(agegroup), col = agegroup)) + 
  theme_classic() + 
  geom_rect(data = bgreg, mapping = aes(ymin = min - .5, ymax = max + .5, 
    xmin = -Inf, xmax = Inf, fill = region), alpha = .2, inherit.aes = F) + 
  scale_fill_manual(values = brewer.pal(4, "Accent"),
    name = "Region") + 
  scale_y_continuous(name = "", 
    breaks = seq_along(unique(big_cityres$LABEL)), 
    labels = unique(big_cityres$LABEL),
    trans = "reverse") + 
  geom_vline(xintercept = 1) + 
  geom_hline(aes(yintercept = id - .5), lty = 3) + 
  theme(axis.ticks.y = element_blank()) + 
  scale_x_continuous(n.breaks = 4, limits = c(.8, 2.5)) + 
  geom_pointrangeh(position = position_dodgev(.8), size = .3)

#----- Add values for heat and cold

coldplot <- bgplot + 
  aes(x = rrcold, xmin = rrcold_low, xmax = rrcold_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Blues")[-(1:2)]) +
  xlab(sprintf("RR at percentile %i", resultper[1])) 

heatplot <- bgplot + 
  aes(x = rrheat, xmin = rrheat_low, xmax = rrheat_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Reds")[-(1:2)]) +
  xlab(sprintf("RR at percentile %i", resultper[2]))  + 
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank())

#----- Put together and save

# Create a "legend-plot" for the common color scale legend
legplot <- ggplot(big_cityres, aes(y = id, group = agegroup, col = agegroup)) + 
  theme_void() + xlim(c(0,0)) + 
  geom_pointrangeh(aes(x = rrheat, xmin = rrheat_low, xmax = rrheat_hi)) +
  scale_color_manual(name = "Age group",
    values = brewer.pal(length(agebreaks) + 3, "Greys")[-(1:2)])

# Put everything together
coldplot + heatplot + 
  legplot + plot_layout(widths = c(1, 1, .1), guides = "collect")

# Save
ggsave("figures/Fig1_CapitalRes.pdf", height = 10)

#---------------------------
#  Figure 2: Excess rates by country and age group
#---------------------------

#----- Prepare useful objects

# Order by region and age
countryres <- countryres[with(countryres, 
  order(region, cntr_name, agegroup)),]
countryres$id <- as.numeric(factor(countryres$CNTR_CODE, 
  levels = unique(countryres$CNTR_CODE)))

# Background rectangles
bgreg <- aggregate(id ~ region, countryres, range)
bgreg <- do.call(data.frame, bgreg)
names(bgreg)[-1] <- c("min", "max")

#----- Create background plot
bgplot <- ggplot(countryres, 
  aes(y = id, group = rev(agegroup), col = agegroup)) + 
  theme_classic() + 
  geom_rect(data = bgreg, mapping = aes(ymin = min - .5, ymax = max + .5, 
    xmin = -Inf, xmax = Inf, fill = region), alpha = .2, inherit.aes = F) + 
  scale_fill_manual(values = brewer.pal(4, "Accent"),
    name = "Region") + 
  scale_y_continuous(name = "", 
    breaks = seq_along(unique(countryres$name)), 
    labels = unique(countryres$name),
    trans = "reverse") + 
  geom_vline(xintercept = 0) + 
  geom_hline(aes(yintercept = id - .5), lty = 3) + 
  theme(axis.ticks.y = element_blank()) + 
  geom_pointrangeh(position = position_dodgev(.8), size = .3)

#----- Add values for heat and cold

ratecoldplot <- bgplot + 
  aes(x = rate_cold_est, xmin = rate_cold_low, xmax = rate_cold_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Blues")[-(1:2)]) +
  xlab(sprintf("Cold-related\nexcess deaths (x %s)", 
    formatC(byrate, digits = 0, format = "f", big.mark = ","))) 

rateheatplot <- bgplot + 
  aes(x = rate_heat_est, xmin = rate_heat_low, xmax = rate_heat_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Reds")[-(1:2)]) +
  xlab(sprintf("Heat-related\nexcess deaths (x %s)", 
    formatC(byrate, digits = 0, format = "f", big.mark = ",")))  + 
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank())

#----- Put together and save

# Create a "legend-plot" for the common color scale legend
legplot <- ggplot(countryres, aes(y = id, group = agegroup, col = agegroup)) + 
  theme_void() + xlim(c(0,0)) + 
  geom_pointrangeh(aes(x = rate_heat_est, xmin = rate_heat_low, 
    xmax = rate_heat_hi)) +
  scale_color_manual(name = "Age group",
    values = brewer.pal(length(agebreaks) + 3, "Greys")[-(1:2)])

# Put everything together
ratecoldplot + rateheatplot + 
  legplot + plot_layout(widths = c(1, 1, .1), guides = "collect")

# Save
ggsave("figures/Fig2_CountryExcess.pdf", height = 10)


#---------------------------
#  Figure 3bis: Standardized rates by country
#---------------------------

#----- Create background plot
bgplot <- ggplot(countryres, aes(y = id)) + theme_classic() + 
  geom_rect(data = bgreg, mapping = aes(ymin = min - .5, ymax = max + .5, 
    xmin = -Inf, xmax = Inf, fill = region), alpha = .2, inherit.aes = F) + 
  scale_fill_manual(values = brewer.pal(4, "Accent"), name = "Region") + 
  scale_y_continuous(name = "", labels = unique(countryres$cntr_name), 
    breaks = unique(countryres$id), trans = "reverse") +
  geom_hline(aes(yintercept = id - .5), lty = 3) + 
  theme(axis.ticks.y = element_blank()) + 
  geom_pointrangeh(aes(col = "a"), position = position_dodgev(.8), 
    show.legend = F)

#----- Add values for heat and cold

# Cold
stdcoldplot <- bgplot + 
  aes(x = stdrate_cold_est, xmin = stdrate_cold_low, xmax = stdrate_cold_hi) + 
  scale_colour_manual(values = "darkblue") + 
  xlab(sprintf("Cold-related\nstd excess deaths (x %s)", 
    formatC(byrate, digits = 0, format = "f", big.mark = ","))) 

# Heat
stdheatplot <- bgplot + 
  aes(x = stdrate_heat_est, xmin = stdrate_heat_low, xmax = stdrate_heat_hi) + 
  scale_colour_manual(values = "darkred") + 
  xlab(sprintf("Heat-related\nstd excess deaths (x %s)", 
    formatC(byrate, digits = 0, format = "f", big.mark = ","))) + 
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank())

#----- Put together and save

# Put everything together
stdcoldplot + stdheatplot + 
  plot_layout(guides = "collect")

# Save
ggsave("figures/Fig3_CountryStdRate.pdf", height = 10)


#----------------------
# Figure 4: Age effect
#----------------------

#----- Prepare plot

# Select specific ages
selagecp <- agecp[agegrid %in% agebreaks]

# Palette
pal <- viridis(length(agebreaks), direction = -1)

#----- Plot all for ages

# Plot outline
layout(matrix(1:2, ncol = 2), width = c(4, 1))
plot(NA, bty = "l", xaxt = "n", 
  xlab = "Temperature percentile", ylab = "RR",
  xlim = range(ovper), 
  ylim = c(min(sapply(selagecp, "[[", "allRRlow")), 
    max(sapply(selagecp, "[[", "allRRhigh"))))
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)

# Add age curves
for (i in seq_along(agebreaks)){
  lines(selagecp[[i]], ptype = "overall", col = pal[i], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(pal[i], .2)))
}
abline(h = 1)
par(mar = c(5, 0, 4, 0) + .1)
plot.new()
legend("topleft", legend = agebreaks, col = pal, lty = 1, lwd = 2, 
  title = "Age", bty = "n")

dev.print(pdf, file = "figures/Fig4_AgeERF.pdf")

#----------------------
# Figure 4bis: Continuous age effect
#----------------------

#----- Extract information at all ages and put

# Creates matrix grid
agedf <- expand.grid(age = agegrid, temp = ovper)

# Add width between temp grid points for plotting
rectlims <- (c(ovper[1] - .1, ovper) + c(ovper, tail(ovper, 1) + .1)) / 2
agedf$xmin <- rep(rectlims[-length(rectlims)], each = length(agegrid))
agedf$xmax <- rep(rectlims[-1], each = length(agegrid))

# Extract all ERF and add as long format
ageERFs <- sapply(agecp, "[[", "allRRfit")
agedf$rr <- c(t(ageERFs))

# Extract all MMT
ageMMT <- sapply(agecp, "[[", "cen")
agedf$mmt <- rep(ageMMT, length(ovper))

#----- Plot as an image
ggplot(agedf) + theme_classic() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = age - .5, ymax = age + .5, 
    fill = rr)) + 
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
    midpoint = 1, name = "RR") + 
  geom_line(aes(x = mmt, y = age, col = "MMT"), size = 1) +
  scale_colour_manual(name = "", values = "darkgrey") + 
  scale_x_continuous(name = "Temperature percentile", 
    breaks = ovper[predper %in% axisper], labels = axisper, expand = c(0, 0)) + 
  scale_y_continuous(name = "Age", expand = c(0, 0)) + 
  theme(panel.border = element_rect(fill = NA), 
    panel.grid.major = element_line(linetype = 2, colour = "darkgrey", 
      size = .4), 
    panel.ontop = T, panel.background = element_rect(fill = NA), 
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14),
    axis.title.x = element_text(margin = margin(15, 0, 1, 0)),
    axis.title.y = element_text(margin = margin(0, 15, 0, 1)),
    legend.title = element_text(size = 14))

dev.print(pdf, file = "figures/Fig4_AgeImage.pdf")

#---------------------------
#  Figure 5: PLS components
#---------------------------

# Change col and row names for plot labelling
plotload <- loads
colnames(plotload) <- sprintf("Comp. %i", 1:npc)
rownames(plotload) <- compres$label

# Plot loadings (correlation between components and meta-variables)
corrplot(t(plotload), method = "square", is.corr = F, cl.lim = c(-1, 1), 
  tl.srt = 45, tl.col = "black")

# Save
dev.print(pdf, file = "figures/Fig5_PLScor.pdf")

#---------------------------
#  Figure 5: RR increase
#---------------------------

#----- Prepare data

# Add id
compres$id <- 1:nrow(compres)

# Prepare background
bgreg <- aggregate(id ~ category, compres, range)
bgreg <- do.call(data.frame, bgreg)
names(bgreg)[-1] <- c("min", "max")

#----- Create background plot
bgplot <- ggplot(compres, aes(y = id)) + theme_classic() +
  geom_rect(data = bgreg, mapping = aes(ymin = min - .5, ymax = max + .5, 
    xmin = -Inf, xmax = Inf, fill = category), alpha = .2, inherit.aes = F) +
  scale_fill_viridis(discrete = T, name = "Category") + 
  scale_y_continuous(name = "", labels = compres$label, 
    breaks = compres$id, trans = "reverse") +
  geom_hline(aes(yintercept = id - .5), lty = 3) + 
  geom_vline(xintercept = 1) + 
  theme(axis.ticks.y = element_blank()) + 
  scale_x_continuous(limits = c(.95, 1.05))

#----- Add RR change for heat and cold

# Cold
compcoldplot <- bgplot + 
  geom_pointrangeh(aes(x = rrcold, xmin = rrcold_low, xmax = rrcold_hi),
    col = "darkblue") + 
  xlab(sprintf("RR change at percentile %i", resultper[1]))

# Heat
compheatplot <- bgplot + 
  geom_pointrangeh(aes(x = rrheat, xmin = rrheat_low, xmax = rrheat_hi),
    col = "darkred") + 
  xlab(sprintf("RR change at percentile %i", resultper[2])) + 
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank())

#----- Put together and save

# Put everything together
compcoldplot + compheatplot + 
  plot_layout(guides = "collect")

# Save
ggsave("figures/Fig5_EffectModification.pdf", height = 10)


#---------------------------
# Sup. Figure : Exposure response functions
#---------------------------

#----- Recompute all ERF with a common MMT for cities

# Take the median MMT for each city
citymmt <- aggregate(mmt ~ URAU_CODE, data = cityres, median)

# Recompute ERF
cityERFplot <- Map(function(b, era5, mmt){
  # percentiles of era5 for this city
  tmeanper <- quantile(era5$era5landtmean, predper / 100)
  
  # Basis for overall
  bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
    knots = quantile(era5$era5landtmean, varper / 100))
  
  # Final prediction centred on the MMT
  crosspred(bvar, coef = b$fit, vcov = b$vcov, cen = mmt, 
    model.link="log", at = quantile(era5$era5landtmean, predper / 100))
}, citycoefs, era5series[cityagegrid[,2]], 
  citymmt[match(cityres$URAU_CODE, citymmt$URAU_CODE),2])

#----- Plot all ERF

pdf("figures/ERFcities.pdf", width = 11, height = 13)
layout(matrix(seq(6 * length(agelabs)), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(cityERF)){
  # Part of the curve above MMP
  heatind <- predper >= cityres[i, "mmp"] 
  
  # Panel title
  ititle <- sprintf("%s (%s) - %s", cityres[i, "LABEL"], 
    cityres[i, "cntr_name"], cityres[i, "agegroup"]) 
  
  # Plot cold part
  plot(cityERFplot[[i]], xlab = "Temperature (C)", ylab = "RR", 
    main = ititle, col = 4, lwd = 2, ylim = c(.8, 2.5), 
    cex.main = .9)
  
  # Add heat part
  lines(cityERFplot[[i]]$predvar[heatind], cityERFplot[[i]]$allRRfit[heatind], 
    col = 2, lwd = 2)
  
  # MMT
  abline(v = cityERFplot[[i]]$cen)
  
  # Add percentiles
  cityper <- tmeandist[rep(1:nrow(metadata), each = length(agelabs))[i],
    predper %in% c(1, 99)]
  abline(v = cityper, lty = 2)
}

dev.off()
