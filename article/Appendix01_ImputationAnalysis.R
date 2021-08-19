################################################################################
#
#                         MCC-CityEurope
#
#                     Appendix 1: analysis of impited values
#
################################################################################

source("00_Packages_Parameters.R")

load("data/Alldata.RData")

#-----------------------
# Checking missing values
#-----------------------

#----- Number of missing per city

# Extract number of missings
citymis <- apply(imputed, 1, sum)

# Check cities with more than 10 missings
metadata[citymis > 10, ]


#-----------------------
# Comparing density of observed and imputed
#-----------------------

x11(width = 10)

densityplot(meta_imp)

dev.print(pdf, file = "figures/FigS1_1_imputedDensity.pdf")