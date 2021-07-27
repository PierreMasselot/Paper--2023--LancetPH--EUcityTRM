################################################################################
#
#                         MCC-EUcityTRM
#
#                           Supplementary results
#
################################################################################

#----- Cities with population

mccmap <- ggplot(data = metacomplete) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, size = pop, col = inmcc), 
    alpha = .4, pch = 16) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_size(name = "Population", range = c(.5, 5),
    guide = "none") + 
  scale_color_discrete(name = "", labels = c("Predicted", "MCC"))

ggsave("figures/urau_cities.pdf")

