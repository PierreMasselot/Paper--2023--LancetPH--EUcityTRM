################################################################################
#
#                         MCC-CityEurope
#
#                     Appendix 1: analysis of impited values
#
################################################################################

#-----------------------
# Comparing density of observed and imputed
#-----------------------

x11(width = 10)

densityplot(meta_imp)

dev.print(pdf, file = "figures/FigS1_1_imputedDensity.pdf")