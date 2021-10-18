################################################################################
#
#                         MCC-EUcityTRM
#
#                             Maps
#
################################################################################

# source("07_ResultsCountry.R")

#---------------------------
#  Create a standard map layout
#---------------------------

basic_map <- ggplot(data = subset(cityres, agegroup == agelabs[3]),
  aes(x = lon, y = lat, size = pop)) + 
  theme_void() + 
  geom_sf(data = euromap, fill = grey(.95), inherit.aes = F, col = grey(.5)) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  scale_size(name = "Population", range = c(1, 5),
    guide = "none") + 
  theme(legend.position = "bottom", legend.box = "vertical") + 
  geom_point(alpha = .9, pch = 21, colour = "white", stroke = .1) + 
  guides(fill = guide_colourbar(title.position = "top", title.hjust = .5,
    barwidth = 12, barheight = .8))

#---------------------------
#  Figure 3: Big maps of results
#---------------------------

#----- Add aesthetic and scale for different result variables

# MMT with heat colors
mmtmap <- basic_map + aes(fill = mmt) + 
  scale_fill_gradient2(low = "darkblue", mid = "lightgoldenrod",
    high = "darkred", name = "MMT\n(65-74)", limits = c(15, 25), midpoint = 20, 
    oob = squish, labels = c("< 15.0", "17.5", "20", "22.5", "> 25"))
  # scale_fill_gradientn(colours = c("darkgreen", "lightgoldenrod",
  #   "darkred"),
  #   name = "MMT", limits = c(15, 25), oob = squish)

# Cold excess rates
stdcoldmap <- basic_map + aes(fill = stdrate_cold_est) + 
  scale_fill_gradient2(low = "lightgoldenrod", mid = "darkblue", high = "black", 
    name = sprintf("Cold-related\nstd death rate (x %s)", 
      formatC(byrate, digits = 0, format = "f", big.mark = ",")),
    limits = c(0, max(cityres$stdrate_cold_est)), 
    midpoint = max(cityres$stdrate_cold_est) / 2)


# Heat with white to red
stdheatmap <- basic_map + aes(fill = stdrate_heat_est) + 
  scale_fill_gradient2(low = "lightgoldenrod", mid = "darkred", high = "black", 
    name = sprintf("Heat-related\nstd death rate (x %s)", 
      formatC(byrate, digits = 0, format = "f", big.mark = ",")),
    limits = c(0, max(cityres$stdrate_heat_est)), 
    midpoint = max(cityres$stdrate_heat_est) / 2)

#----- Put maps together

# Put them side-by-side
(stdcoldmap + mmtmap + stdheatmap) /

# Add legend for point size
  basic_map + coord_sf(xlim = c(0, 0), ylim = c(0, 0)) + 
    scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
      labels = c(1, 5, 10, 50) / 10, name = "Population (millions)",
      guide = guide_legend(title.position = "top", title.hjust = 0.5,
        label.position = "bottom", override.aes = list(colour = "black"))) + 
  theme(legend.position = "bottom") +
  plot_layout(height = c(1, .05))

# Save
ggsave("figures/Fig3_cityMap.pdf", width = 15)


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
  scale_fill_discrete(name = "", labels = c("Predicted", "MCC")) +
  scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
    labels = c(1, 5, 10, 50) / 10, name = "Population (millions)") +
  guides(size = guide_legend(override.aes = list(colour = "black")),
    fill = guide_legend(override.aes = list(size = 5)))  +
  theme(legend.position = "right")

#----- Save 

ggsave("figures/SupFig_URAUcities.pdf")


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

# #-------------------------
# # Background effect
# #-------------------------
# 
# # MMP
# ggplot(data = bggrid) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = mmp)) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = heat_hcl(2)[2], high = heat_hcl(2)[1], 
#     name = "MMP") + 
#   geom_point(data = metacomplete, mapping = aes(x = lon, y = lat), 
#     alpha = .4, pch = 16)
# 
# ggsave("figures/bg_mmp.pdf")
# 
# # Cold
# ggplot(data = bggrid) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = rr[,1])) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkblue",
#     name = sprintf("RR at percentile %i", resultper[1]))
# 
# ggsave("figures/bg_rrcold.pdf")
# 
# # Heat
# ggplot(data = bggrid) + theme_void() + 
#   geom_raster(aes(x = lon, y = lat, fill = rr[,2])) + 
#   geom_sf(data = euromap, fill = NA) + 
#   coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
#   scale_fill_gradient(low = "white", high = "darkred",
#     name = sprintf("RR at percentile %i", resultper[2]))
# 
# ggsave("figures/bg_rrheat.pdf")




