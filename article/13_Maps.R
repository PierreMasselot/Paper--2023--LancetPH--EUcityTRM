################################################################################
#
#                         MCC-EUcityTRM
#
#                             Maps
#
################################################################################

source("10_ResultsVulnerability.R")

#---------------------------
#  Create a standard map layout
#---------------------------

basic_map <- ggplot(data = cityres, aes(x = lon, y = lat, size = pop)) + 
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
    barwidth = 20, barheight = .8, even.steps = T))

#---------------------------
#  Figure 4: Big maps of results
#---------------------------

#----- Add aesthetic and scale for different result variables

# MMT 
cutpts <- unname(round(quantile(cityres$mmt, seq(0, 1, length.out = 7))))
# cutpts <- c(13, 16, 17, 18, 19, 20, 21, 22, 25)
mmtmap <- basic_map + aes(fill = mmt) + 
  scale_fill_stepsn(colours = cividis(length(cutpts) - 1, direction = 1),
    values = rescale(cutpts), breaks = cutpts,
    name = "\nMMT (C)")
  # scale_fill_gradientn(colours = viridis(length(cutpts) - 1, direction = 1),
  #   values = rescale(cutpts),
  #   name = "MMT")

# MMP 
cutpts <- unname(round(quantile(cityres$mmp, seq(0, 1, length.out = 7))))
mmpmap <- basic_map + aes(fill = mmp) + 
  scale_fill_stepsn(colours = cividis(length(cutpts) - 1, direction = 1),
    values = rescale(cutpts), breaks = cutpts,
    name = "\nMMP (%)")
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
    name = sprintf("Cold-related\nstd death rate (x %s)", 
      formatC(byrate, digits = 0, format = "f", big.mark = ",")),
    limits = c(0, max(cityres$stdrate_cold_est)))


# Heat with white to red
cutpts <- unname(round(quantile(cityres$stdrate_heat_est / 5, 
  seq(0, 1, length.out = 7))) * 5)
stdheatmap <- basic_map + aes(fill = stdrate_heat_est) + 
  scale_fill_stepsn(
    colours = rocket(length(cutpts) - 1, direction = -1, begin = .3),
    values = rescale(cutpts), breaks = cutpts, 
    name = sprintf("Heat-related\nstd death rate (x %s)", 
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


# # Cold with white to blue
# coldmap <- basic_map + aes(fill = rrcold) + 
#   scale_fill_gradient2(low = "lightgoldenrod", mid = "darkblue", 
#     high = "black", 
#     name = sprintf("RR at percentile %i", resultper[1]),
#     limits = c(1, 2), midpoint = 1.5, oob = squish)
# # scale_fill_binned(low = "white", high = "darkblue", 
# #   name = sprintf("RR at percentile %i", resultper[2]))
# 
# # Heat with white to red
# heatmap <- basic_map + aes(fill = rrheat) + 
#   scale_fill_gradient2(low = "lightgoldenrod", mid = "darkred", 
#     high = "black", 
#     name = sprintf("RR at percentile %i", resultper[2]), oob = squish,
#     limits = c(1, 1.6), midpoint = 1.3)
# # scale_fill_binned(low = "white", high = "darkred", 
# #   name = sprintf("RR at percentile %i", resultper[2]))

#---------------------------
# Supp Figure: List of cities in MCC
#---------------------------

# Execute background map above

#----- Cities in and out of MCC

basic_map + aes(fill = inmcc) + 
  scale_fill_discrete(name = "", labels = c("Urban Audit", "MCC")) +
  guides(size = guide_legend(override.aes = list(colour = "black")),
    fill = guide_legend(override.aes = list(size = 5)))  +
  theme(legend.position = "right") 

#----- Save 

ggsave("figures/SupFig_URAUcities.pdf", width = 7, height = 7)


#---------------------------
# Supp Figure: Maps of each PLS component
#---------------------------

# Execute background map above

#----- Create map for each PLS component

# Loop
plsmaps <- lapply(seq_len(npc), function(i){
  cutpts <- unname(round(quantile(cityres[, sprintf("pls%i", i)], 
    seq(0, 1, length.out = 7))))
  basic_map + aes_string(fill = sprintf("pls%i", i)) + 
    # scale_fill_viridis(name = sprintf("Comp. %i", i), direction = -1) + 
    # guides(fill = guide_colourbar(title.position = "top", title.hjust = .5,
    #   barwidth = 10, barheight = .8))
    scale_fill_stepsn(colours = viridis(length(cutpts) - 1, direction = -1),
      values = rescale(cutpts), breaks = cutpts,
      name = sprintf("Comp. %i", i))
})
names(plsmaps) <- letters[1:npc]

# Add legend for size
plsmaps$leg <- basic_map + coord_sf(xlim = c(0, 0), ylim = c(0, 0)) + 
  scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
    labels = c(1, 5, 10, 50) / 10, name = "Population (millions)",
    guide = guide_legend(override.aes = list(colour = "black"), 
     title.position = "top")) + 
  theme(legend.position = "right")

# Design
design <- matrix(1:floor(npc), ncol = 2, byrow = T)
if (npc %% 2 == 1) design <- rbind(design, npc)
design <- cbind(design, npc + 1)
design_char <- paste(apply(design, 1, paste, collapse = ""), collapse = "\n")

# Plot
wrap_plots(plsmaps, widths = c(1, 1, .1), design = design_char)

# Save
ggsave("figures/SupFig_PLSmaps.pdf", width = 15, height = 12)

#-------------------------
# Background map
#-------------------------

# Add region information to euromap
euromap$region <- regionlist[euromap$CNTR_CODE]

# Color palette
regpal <- viridis(4)
names(regpal) <- c("Western", "Northern", "Eastern", "Southern")

# Plot regions
ggplot(data = euromap) + theme_void() + 
  geom_sf(data = euromap, aes(fill = region), alpha = .6, col = "black",
    size = .1) + 
  scale_fill_manual(values = regpal, name = "Region", 
    na.value = grey(.95), na.translate = F) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box")

# Save
ggsave("figures/SupFig_Region_maps.pdf", width = 7, height = 7)

#-------------------------
# Kriging
#-------------------------

#----- Predict each random effect on a grid

# Create grid
createraster <- raster(extent(metageo), nrow = 100, ncol = 100, 
  crs = st_crs(metageo))
rastcoord <- st_as_sf(as.data.frame(coordinates(createraster)), 
  coords = c("x","y"), crs = st_crs(metageo))
euroraster <- st_intersection(rastcoord, st_union(euromap))

# Predict kriging on each cell
surfpred <- predict(vgfit, euroraster)

#----- Plot each coefficient

# Create plot layout
rasterplot <- ggplot(data = surfpred) + 
  geom_sf(pch = 15) +
  geom_sf(data = euromap, fill = NA, inherit.aes = F, col = grey(.5)) + 
  coord_sf(xlim = extent(surfpred)[1:2], ylim = extent(surfpred)[3:4],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  scale_colour_gradient2(low = muted("blue"), high = muted("red"),
    limits = c(-1, 1) * max(abs(st_drop_geometry(surfpred[,1:nc * 2 - 1]))),
    name = "Random coefficient") +
  theme_void()

# Loop on variables to create plots
ranplots <- vector("list", nc)
for (i in seq_len(nc)){
  ranplots[[i]] <- rasterplot + aes_string(col = sprintf("b%i.pred", i)) + 
    ggtitle(sprintf("b%i", i))
} 

# Put plots together
design = "12
  34
  55
"
wrap_plots(ranplots, guides = "collect", design = design)

# Save
ggsave("figures/SupFig_krigmaps.pdf", width = 10, height = 20)
