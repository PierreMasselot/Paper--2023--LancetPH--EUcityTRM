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
#  Figure 3: Big maps of results
#---------------------------

#----- Add aesthetic and scale for different result variables

# MMT with heat colors
cutpts <- unname(round(quantile(cityres$mmt, seq(0, 1, length.out = 7))))
# cutpts <- c(13, 16, 17, 18, 19, 20, 21, 22, 25)
mmtmap <- basic_map + aes(fill = mmt) + 
  scale_fill_stepsn(colours = cividis(length(cutpts) - 1, direction = 1),
    values = rescale(cutpts), breaks = cutpts,
    name = "\nMMT (C)")
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
(stdcoldmap + mmtmap + stdheatmap) /

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
  plot_layout(height = c(1, .05))

# Save
ggsave("figures/Fig3_cityMap.pdf", width = 15, height = 8, units = "in")


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
  basic_map + aes_string(fill = sprintf("pls%i", i)) + 
    scale_fill_viridis(name = sprintf("Comp. %i", i))
})
names(plsmaps) <- letters[1:npc]

# Add legend for size
plsmaps$leg <- basic_map + coord_sf(xlim = c(0, 0), ylim = c(0, 0)) + 
  scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
    labels = c(1, 5, 10, 50) / 10, name = "Population (millions)",
    guide = guide_legend(override.aes = list(colour = "black"))) + 
  theme(legend.position = "right")

# Design 
plsmaps$nrow = 1
plsmaps$widths = c(rep(1, npc), .1)

# plot
do.call(wrap_plots, plsmaps)

# Save
ggsave("figures/SupFig_PLSmaps.pdf", width = 15)

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

#----- Predict on a grid

xyrst <- raster(extent(euromap), nrow=400, ncol=400) |>
  coordinates() |>
  as.data.frame() |>
  st_as_sf(coords=c("x","y"), crs=st_crs(euromap)) |>
  st_intersection(st_union(euromap)) |>
  st_transform(crs=st_crs(mccgeo))

surfpred <- predict(vgfit, xyrst)

ggplot(data=surfpred) + 
  geom_sf(aes(col=var5.pred), alpha=0.7) +
  geom_sf(data = euromap, fill=NA, inherit.aes = F, col = grey(.5)) + 
  scale_colour_gradient2(low="blue", mid="white", high="red", midpoint=0,
    name = "Coef 5 (heat) pred") + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  theme_void() 