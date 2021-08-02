################################################################################
#
#                         MCC-EUcityTRM
#
#                           Supplementary results
#
################################################################################

#----- Cities with population

# Execute basic_map

basic_map + aes(fill = inmcc) + 
  scale_fill_discrete(name = "", labels = c("Predicted", "MCC")) + 
  guides(size = guide_legend(override.aes = list(colour = "black")),
    fill = guide_legend(override.aes = list(size = 5)))  +
  theme(legend.position = "right")

ggsave("figures/urau_cities.pdf")

