################################################################################
#
#                         MCC-EUcityTRM
#
#                         Other Plots
#
################################################################################

source("10_ResultsVulnerability.R")

#----------------------
# Figure 1a: Age effect
#----------------------

#----- Prepare plot

# Select specific ages
selagecp <- agecp[agegrid %in% agebreaks]

# Palette
pal <- viridis(length(agebreaks), direction = -1)

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
for (i in seq_along(agebreaks)){
  lines(selagecp[[i]], ptype = "overall", col = pal[i], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(pal[i], .2)))
}
abline(h = 1)

# Add legend
legpars <- list(legend = agebreaks, col = pal, lty = 1, lwd = 2, 
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

#----- Create data.frame

mmpdf <- data.frame(age = agegrid, mmp = agemmp, 
  low = mmpci[1,], high = mmpci[2,])

#----- Plot it

ggplot(subset(mmpdf, age >= 45), aes(x = age)) + theme_classic() +
  geom_ribbon(aes(ymin = low, ymax = high), fill = adjustcolor(4, .4)) + 
  geom_line(aes(y = mmp), size = 1.5) + 
  scale_x_continuous(name = "Age", 
    breaks = seq(50, 90, by = 10)) + 
  scale_y_continuous(name = "MMP",
    breaks = ovper[predper %in% c(50, 60, 70, 80, 90)], 
    labels = c(50, 60, 70, 80, 90)) + 
  coord_cartesian(ylim = ovper[predper %in% c(50, 90)]) +
  theme(axis.text = element_text(size = 12), 
    axis.text.x = element_text(margin = margin(t = 6)),
    axis.text.y = element_text(margin = margin(r = 6)),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    panel.grid.major = element_line(linetype = 2, color = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(6, "pt"))

dev.print(pdf, file = "figures/Fig1b_AgeMMP.pdf")


#---------------------------
#  Figure 2: Risks for capital cities
#---------------------------

#----- Select capital city of each country

# Find largest pop in each country
# capitals <- unlist(by(metadata, metadata$CNTR_CODE, function(d){
#   d[which.max(d$pop), "URAU_CODE"]
# }))

# Select cities in result (city with number 001 is usually the capital)
# big_cityres <- subset(cityres, URAU_CODE %in% capitals)
big_cityres <- subset(cityageres, substr(URAU_CODE, 3, 5) == "001")

# Order by region and age
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
  geom_pointrange(position = position_dodge(.8), size = .3)

#----- Add values for heat and cold

coldplot <- bgplot + 
  aes(y = rrcold, ymin = rrcold_low, ymax = rrcold_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Blues")[-(1:2)]) +
  ylab(sprintf("RR at temperature\npercentile %i", resultper[1])) + 
  scale_x_continuous(name = "", breaks = NULL,
    sec.axis = sec_axis(trans = ~., name = "", breaks = regpos$id, 
      labels = regpos$region)) + 
  coord_cartesian(ylim = c(.8, 2.2)) + 
  scale_y_continuous(breaks = c(1, 1.5, 2))

heatplot <- bgplot + 
  aes(y = rrheat, ymin = rrheat_low, ymax = rrheat_hi) + 
  scale_color_manual(guide = "none",
    values = brewer.pal(length(agebreaks) + 3, "Reds")[-(1:2)]) +
  ylab(sprintf("RR at temperature\npercentile %i", resultper[2])) + 
  coord_cartesian(ylim = c(.8, 4)) + 
  scale_y_continuous(n.breaks = 4)

#----- Put together and save

# Create a "legend-plot" for the common color scale legend
legplot <- ggplot(big_cityres, aes(x = id, group = agegroup, col = agegroup)) +
  theme_void() + ylim(c(0,0)) +
  geom_pointrange(aes(y = rrheat, ymin = rrheat_low, ymax = rrheat_hi)) +
  scale_color_manual(name = "Age group",
    values = brewer.pal(length(agebreaks) + 3, "Greys")[-(1:2)]) + 
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

# Order by region and age
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
  levels = c("Northern", "Western", "Eastern", "Southern", ""))
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

# #----- Create background plot
# bgplot <- ggplot(countryageres, 
#     aes(x = id, group = rev(agegroup), fill = agegroup)) + 
#   theme_classic() + 
#   scale_x_continuous(name = "",
#     breaks = unique(countryageres$id), 
#     labels = unique(countryageres$cntr_name)) + 
#   theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(),
#     axis.text.x.top = element_text(size = 15),
#     axis.text.x.bottom = element_text(size = 12, angle = 90, vjust = .5, 
#       hjust = 1),
#     panel.grid.major.y = element_line(linetype = 3, colour = "grey")) +
#   geom_hline(yintercept = 0)
# 
# #----- Add values for heat and cold
# 
# ratecoldplot <- bgplot +
#   geom_col(aes(y = ratetotpop_cold_est)) +
#   scale_fill_manual(guide = "none",
#     values = brewer.pal(length(agebreaks) + 3, "Blues")[-(1:2)]) +
#   scale_y_continuous(name = sprintf("Cold-related\ndeath rate (x %s)",
#       formatC(byrate, digits = 0, format = "f", big.mark = ",")))  +
#   scale_x_continuous(name = "", breaks = NULL, 
#     sec.axis = sec_axis(trans = ~., name = "", breaks = regpos$id,
#       labels = regpos$region))
# 
# rateheatplot <- bgplot +
#   geom_col(aes(y = ratetotpop_heat_est)) +
#   scale_fill_manual(guide = "none",
#     values = brewer.pal(length(agebreaks) + 3, "Reds")[-(1:2)]) +
#   scale_y_continuous(name = sprintf("Heat-related\ndeath rate (x %s)",
#       formatC(byrate, digits = 0, format = "f", big.mark = ",")))
# 
# #----- Put together and save
# 
# # Create a "legend-plot" for the common color scale legend
# g <- ggplot(countryageres, aes(x = id, group = agegroup, fill = agegroup)) + 
#   geom_col(aes(y = rate_heat_est)) +
#   scale_fill_manual(name = "Age group",
#     values = brewer.pal(length(agebreaks) + 3, "Greys")[-(1:2)])
# 
# # Extract only legend
# legplot <- as_ggplot(get_legend(g))
# 
# # Put everything together
# design <- "
#   13
#   23
# "
# ratecoldplot + rateheatplot + legplot + 
#   plot_layout(widths = c(1, .2), design = design, guides = "collect")

# Save
ggsave("figures/Fig3_CountryExcess.pdf", height = 8, width = 12)




# #----- Extract information at all ages and put
# 
# # Creates matrix grid
# agedf <- expand.grid(age = agegrid, temp = ovper)
# 
# # Add width between temp grid points for plotting
# rectlims <- (c(ovper[1] - .1, ovper) + c(ovper, tail(ovper, 1) + .1)) / 2
# agedf$xmin <- rep(rectlims[-length(rectlims)], each = length(agegrid))
# agedf$xmax <- rep(rectlims[-1], each = length(agegrid))
# 
# # Extract all ERF and add as long format
# ageERFs <- sapply(agecp, "[[", "allRRfit")
# agedf$rr <- c(t(ageERFs))
# 
# # Extract all MMT
# ageMMT <- sapply(agecp, "[[", "cen")
# agedf$mmt <- rep(ageMMT, length(ovper))
# 
# #----- Plot as an image
# ggplot(agedf) + theme_classic() + 
#   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = age - .5, ymax = age + .5, 
#     fill = rr)) + 
#   scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
#     midpoint = 1, name = "RR") + 
#   geom_line(aes(x = mmt, y = age, col = "MMT"), size = 1) +
#   scale_colour_manual(name = "", values = "darkgrey") + 
#   scale_x_continuous(name = "Temperature percentile", 
#     breaks = ovper[predper %in% axisper], labels = axisper, expand = c(0, 0)) + 
#   scale_y_continuous(name = "Age", expand = c(0, 0)) + 
#   theme(panel.border = element_rect(fill = NA), 
#     panel.grid.major = element_line(linetype = 2, colour = "darkgrey", 
#       size = .4), 
#     panel.ontop = T, panel.background = element_rect(fill = NA), 
#     axis.text = element_text(size = 12), 
#     axis.title = element_text(size = 14),
#     axis.title.x = element_text(margin = margin(15, 0, 1, 0)),
#     axis.title.y = element_text(margin = margin(0, 15, 0, 1)),
#     legend.title = element_text(size = 14))
# 
# dev.print(pdf, file = "figures/Fig4_AgeImage.pdf")


#---------------------------
#  Supplementary Figure: Standardized death rates by country 
#---------------------------

# #----- Prepare useful objects
# 
# # Order by region and country
# countryres <- countryres[with(countryres, 
#   order(region, cntr_name)),]
# countryres$id <- as.numeric(factor(countryres$CNTR_CODE, 
#   levels = unique(countryres$CNTR_CODE))) + 
#   as.numeric(factor(countryres$region, 
#     levels = unique(countryres$region))) - 1
# 
# # Region label position
# regpos <- aggregate(id ~ region, data = countryres, mean)
#
# #----- Create background plot
# bgplot <- ggplot(countryres, aes(x = id)) + theme_classic() + 
#   scale_x_continuous(name = "", labels = unique(countryres$cntr_name), 
#     breaks = unique(countryres$id)) +
#   # geom_hline(data = bglines, aes(yintercept = pos), lty = 3) + 
#   theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(),
#     axis.text.x.top = element_text(size = 15),
#     axis.text.x.bottom = element_text(size = 12, angle = 90, vjust = .5, 
#       hjust = 1),
#     panel.grid.major.y = element_line(linetype = 3, colour = "grey")) + 
#   geom_col(aes(fill = "a"), position = position_dodge(), show.legend = F,
#     width = .8) +
#   geom_hline(yintercept = 0)
# 
# #----- Add values for heat and cold
# 
# # Cold
# stdcoldplot <- bgplot + 
#   aes(y = stdrate_cold_est) + 
#   geom_errorbar(aes(ymin = stdrate_cold_low, ymax = stdrate_cold_hi),
#     width = .5) +
#   scale_fill_manual(values = 4) + 
#   ylab(sprintf("Cold-related\nstd death rate (x %s)", 
#     formatC(byrate, digits = 0, format = "f", big.mark = ","))) + 
#   scale_x_continuous(name = "", breaks = NULL, 
#     sec.axis = sec_axis(trans = ~., name = "", breaks = regpos$id, 
#       labels = regpos$region))
# 
# # Heat
# stdheatplot <- bgplot + 
#   aes(y = stdrate_heat_est) + 
#   geom_errorbar(aes(ymin = stdrate_heat_low, ymax = stdrate_heat_hi),
#     width = .5) +
#   scale_fill_manual(values = 2) + 
#   ylab(sprintf("Heat-related\nstd death rate (x %s)", 
#     formatC(byrate, digits = 0, format = "f", big.mark = ",")))
# 
# #----- Put together and save
# 
# # Put everything together
# stdcoldplot + stdheatplot + 
#   plot_layout(ncol = 1, guides = "collect")

#####################################

# # Store the scaling factor
# scalecold <- max(countryres$stdrate_cold_est)
# scaleheat <- max(countryres$stdrate_heat_est)
# 
# # Derive pretty breaks for heat and cold
# prettycold <- pretty(c(0, scalecold))
# prettyheat <- pretty(c(0, scaleheat))[-1]
# 
# # Plot back-to-back bars
# ggplot(countryres, aes(x = id)) + theme_classic() + 
#   scale_x_continuous(name = "", labels = unique(countryres$cntr_name), 
#     breaks = unique(countryres$id),
#     sec.axis = sec_axis(trans = ~., name = "", breaks = regpos$id,
#       labels = regpos$region)) +
#   theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(),
#     axis.text.x.top = element_text(size = 15),
#     axis.text.x.bottom = element_text(size = 12, angle = 90, vjust = .5, 
#       hjust = 1),
#     panel.grid.major.y = element_line(linetype = 3, colour = "grey")) + 
#   geom_col(aes(y = stdrate_cold_est / scalecold), fill = 4, width = .8,
#     position = position_dodge()) +
#   geom_col(aes(y = -stdrate_heat_est / scaleheat), fill = 2, width = .8) +
#   scale_y_continuous(
#     breaks = c(-prettyheat / scaleheat, prettycold / scalecold),
#     labels = c(prettyheat, prettycold),
#     name = sprintf("Std death rate (x %s)",
#       formatC(byrate, digits = 0, format = "f", big.mark = ","))) +
#   geom_hline(yintercept = 0) + 
#   geom_errorbar(aes(ymin = stdrate_cold_low / scalecold, 
#     ymax = stdrate_cold_hi / scalecold), width = .5, size = .7) +
#   geom_errorbar(aes(ymax = -stdrate_heat_low / scaleheat, 
#     ymin = -stdrate_heat_hi / scaleheat), width = .5, size = .7)

#----- Prepare useful objects

# Order by region and country
plotres <- countryres[with(countryres, 
  order(region, cntr_name)),]
plotres$id <- seq_len(nrow(plotres)) + 
  as.numeric(factor(plotres$region, 
    levels = unique(plotres$region))) - 1

# Compute id for regional and total results
regionres2 <- merge(regionres, aggregate(id ~ region, data = plotres, max),
  all.x = T)
regionres2$id <- regionres2$id + 1
regionres2[regionres2$region == "Total", "id"] <- 
  max(regionres2$id, na.rm = T) + 1

# Add to plot data.frame
regionres2$CNTR_CODE <- "TOT"
regionres2$cntr_name <- "Total"
plotres <- rbind(plotres, regionres2)

# Add some info
plotres[plotres$region == "Total", c("CNTR_CODE", "cntr_name", "region")] <- 
  c("EU", "Europe", "")
plotres$region <- factor(plotres$region, 
  levels = c("Northern", "Western", "Eastern", "Southern", ""))
# plotres$istotal <- as.factor(plotres$CNTR_CODE %in% c("TOT", "EU"))
plotres$istotal <- factor(plotres$CNTR_CODE, levels = c("CNTR", "TOT", "EU"))
plotres$istotal[is.na(plotres$istotal)] <- "CNTR"

# Compute scaling factor for cold and heat
scalecold <- max(plotres$stdrate_cold_est)
scaleheat <- max(plotres$stdrate_heat_est)

# Derive pretty breaks for heat and cold
prettycold <- pretty(c(0, scalecold))
prettyheat <- pretty(c(0, scaleheat))[-1]

#----- Create plot
ggplot(plotres, aes(y = id)) + theme_classic() + 
  scale_y_continuous(name = "", labels = plotres$cntr_name, 
    breaks = plotres$id, trans = "reverse") + 
  geom_colh(aes(x = -stdrate_cold_est / scalecold, fill = istotal), 
    width = .8) +
  scale_fill_manual(values = brewer.pal(4, "Blues")[-1],
    guide = "none") +
  new_scale("fill") + 
  geom_colh(aes(x = stdrate_heat_est / scaleheat, fill = istotal), 
    width = .8) +
  scale_fill_manual(values = brewer.pal(4, "Reds")[-1], 
    guide = "none") + 
  scale_x_continuous(
    breaks = c(prettyheat / scaleheat, -prettycold / scalecold),
    labels = c(prettyheat, prettycold),
    name = sprintf("Std death rate (x %s)",
      formatC(byrate, digits = 0, format = "f", big.mark = ","))) +
  geom_vline(xintercept = 0) + 
  geom_errorbarh(aes(xmin = -stdrate_cold_low / scalecold, 
    xmax = -stdrate_cold_hi / scalecold), width = .5, size = .7) +
  geom_errorbarh(aes(xmax = stdrate_heat_low / scaleheat, 
    xmin = stdrate_heat_hi / scaleheat), width = .5, size = .7) +
  facet_grid(rows = vars(region), scales = "free_y", space = "free_y") +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    axis.text.y.left = element_text(size = 10, vjust = 0.2),
    panel.grid.major.x = element_line(linetype = 3, colour = "grey"),
    strip.background = element_rect(color = NA), 
    strip.text = element_text(size = 12))

# Save
ggsave("figures/SupFig_CountryStdRate.pdf", height = 10, width = 15)


#---------------------------
#  Figure 5: PLS components
#---------------------------

# Change col and row names for plot labelling
plotload <- plsres$projection[,1:npc]
colnames(plotload) <- sprintf("Comp. %i", 1:npc)
rownames(plotload) <- metadesc$label[match(metaprednames, metadesc$metavar)]

# Color scale
pal <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
  "#4393C3", "#2166AC", "#053061")))

# Plot loadings (correlation between components and meta-variables)
corrplot(t(plotload), method = "square", is.corr = F, col.lim = c(-1, 1), 
  tl.srt = 45, tl.col = "black", cl.cex = .7, cl.align.text = "l",
  col = pal(200))

# Save
dev.print(pdf, file = "figures/Fig5_PLScor.pdf")

#---------------------------
#  Sup Figure: Co-variogram
#---------------------------

#----- Plot variogram

# Plot
plot(mccvario, vgfit, pch = 16, col = 1, lwd = 2, ylab = "Semi-variance",
  xlab = "Distance (km)")

# Save
dev.print(pdf, file = "figures/SupFig_variogram.pdf", width = 10, height = 7)

#---------------------------
#  Sup Figure: Correlation matrix between metapredictors variables
#---------------------------

# Compute correlation matrix
metacor <- cor(metadata[,metaprednames])
colnames(metacor) <- rownames(metacor) <- metadesc$label[match(metaprednames, 
  metadesc$metavar)]

# Color scale
pal <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
  "#4393C3", "#2166AC", "#053061")))

# Plot correlation matrix
corrplot.mixed(metacor, upper = "square", tl.pos = "lt",
  tl.srt = 45, tl.col = "black", cl.cex = 1.1, cl.align.text = "l",
  upper.col = pal(200), lower.col = pal(200))

# Save
dev.print(pdf, file = "figures/SupFig_metacor.pdf", width = 15, height = 15)

#---------------------------
#  Sup Figure: Curve changes at extreme PLS
#---------------------------

# Colors
pal <- viridis(2, direction = -1)

# Plot layout
design <- matrix(1:floor(npc), ncol = 2, byrow = T)
if (npc %% 2 == 1) design <- rbind(design, npc)
design <- cbind(design, npc + 1)

# Initialize layout
layout(design, widths = c(1, 1, .2))

# Loop on PLS components
for (i in seq_len(npc)){
  inds <- 1:2 + (2 * (i - 1))
  
  # Lowest
  plot(plscp[inds][[1]], xlab = "Temperature precentile", ylab = "RR", 
    main = sprintf("Comp. %i", i), col = pal[1], lwd = 2, ylim = c(.8, 2.5), 
    cex.main = .9, ci.arg = list(col = adjustcolor(pal[1], .2)), xaxt = "n")
  
  # Highest
  lines(plscp[inds][[2]], lwd = 2, col = pal[2], ci = "area", 
    ci.arg = list(col = adjustcolor(pal[2], .2)))
  
  # Axis
  abline(v = ovaxis, h = axTicks(2), lty = 3, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  abline(h = 1)
  
  # MMTs
  abline(v = plscp[inds][[1]]$cen, col = pal[1], lty = 2)
  abline(v = plscp[inds][[2]]$cen, col = pal[2], lty = 2)
}

# Add legend
par(mar = rep(0, 4))
plot.new()
legend("center", legend = c("1st", "99th"), lwd = 2,
  col = pal, bty = "n", title = "Component\npercentile")

# Save
dev.print(pdf, file = "figures/SupFig_PLS_ERF.pdf", height = 7, width = 8)

# #---------------------------
# #  Sup Figure: RR increase
# #---------------------------
# 
# #----- Prepare data
# 
# # Add id
# compres$id <- 1:nrow(compres)
# 
# # Prepare background
# bgreg <- aggregate(id ~ category, compres, range)
# bgreg <- do.call(data.frame, bgreg)
# names(bgreg)[-1] <- c("min", "max")
# 
# #----- Create background plot
# bgplot <- ggplot(compres, aes(y = id)) + theme_classic() +
#   geom_rect(data = bgreg, mapping = aes(ymin = min - .5, ymax = max + .5, 
#     xmin = -Inf, xmax = Inf, fill = category), alpha = .2, inherit.aes = F) +
#   scale_fill_viridis(discrete = T, name = "Category") + 
#   scale_y_continuous(name = "", labels = compres$label, 
#     breaks = compres$id, trans = "reverse") +
#   geom_hline(aes(yintercept = id - .5), lty = 3) + 
#   geom_vline(xintercept = 1) + 
#   theme(axis.ticks.y = element_blank()) + 
#   scale_x_continuous(limits = c(.95, 1.05))
# 
# #----- Add RR change for heat and cold
# 
# # Cold
# compcoldplot <- bgplot + 
#   geom_pointrangeh(aes(x = rrcold, xmin = rrcold_low, xmax = rrcold_hi),
#     col = "darkblue") + 
#   xlab(sprintf("RR change at percentile %i", resultper[1]))
# 
# # Heat
# compheatplot <- bgplot + 
#   geom_pointrangeh(aes(x = rrheat, xmin = rrheat_low, xmax = rrheat_hi),
#     col = "darkred") + 
#   xlab(sprintf("RR change at percentile %i", resultper[2])) + 
#   theme(axis.title.y = element_blank(),
#     axis.text.y = element_blank())
# 
# #----- Put together and save
# 
# # Put everything together
# compcoldplot + compheatplot + 
#   plot_layout(guides = "collect")
# 
# # Save
# ggsave("figures/SupFig_EffectModification.pdf", height = 10)

#---------------------------
# Sup. Figure : Regional effect
#---------------------------

# Palette
regpal <- viridis(4)
names(regpal) <- c("Western", "Northern", "Eastern", "Southern")
regpal <- regpal[names(regERF)]

# Plot outline
plot(NA, bty = "l", xaxt = "n", 
  xlab = "Temperature percentile", ylab = "RR",
  xlim = range(ovper), 
  # ylim = c(min(sapply(regERF, "[[", "allRRlow")), 
  #   max(sapply(regERF, "[[", "allRRhigh")))
  ylim = c(.8, 2.5))
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)

# Add age curves
for (i in seq_along(regERF)){
  lines(regERF[[i]], ptype = "overall", col = regpal[i], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(regpal[i], .2)))
}
abline(h = 1)

dev.print(pdf, file = "figures/SupFig_regionERF.pdf")

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

#----- Plot all ERF

# Prepare palettes
coldpal <- brewer.pal(length(agelabs) + 1, "Blues")[-1]
heatpal <- brewer.pal(length(agelabs) + 1, "Reds")[-1]
# cipal <- rev(grey(seq(.1, .5, length.out = length(agelabs)), .2))

# Prepare output
pdf("figures/SupFig_ERFcities.pdf", width = 11, height = 13)
layout(matrix(seq(6 * length(agelabs)), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(cityERFplot)){
  # Part of the curve above MMP
  heatind <- cityERFplot[[i]][[1]]$predvar >= cityERFplot[[i]][[1]]$cen
  
  # Initialize plot
  plot(cityERFplot[[i]][[1]], xlab = "Temperature (C)", ylab = "RR", 
    main = metadata$LABEL[i], col = NA, lwd = 2, ylim = c(.5, 3), 
    cex.main = .9, ci = "n")
  
  # Loop on age groups
  for (a in seq_along(cityERFplot[[i]])){
    # Cold part
    lines(cityERFplot[[i]][[a]], col = coldpal[a], lwd = 2)
    
    # Heat part
    lines(cityERFplot[[i]][[a]]$predvar[heatind], 
      cityERFplot[[i]][[a]]$allRRfit[heatind], 
      col = heatpal[a], lwd = 2)
  }
  
  # MMT
  abline(v = cityERFplot[[i]][[1]]$cen)
  
  # Add percentiles
  cityper <- cityERFplot[[i]][[1]]$predvar[predper %in% c(1, 99)]
  abline(v = cityper, lty = 2)
  
  # Unit line
  abline(h = 1)
}

dev.off()



