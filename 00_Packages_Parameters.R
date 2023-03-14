################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# R Code
# Part 0: Loading packages and analysis parameters
#
################################################################################

#----------------------
# Necessary packages
#----------------------

#----- Data management
library(sf) # Geo data
library(eurostat) # To download eurostat data (/!\ it might be necessary to 
# install a more recent version from Github because of recent changes 
# in Eurostat API) 
library(giscoR) # To download eurostat geographical information
library(doParallel) # Run loops in parallel
library(MESS) # For function cumsumbinning
library(Matrix) # Functions for matrix operations
library(modeest) # For computing mode of a vector
library(PHEindicatormethods) # For the 2013 European standard population
library(stringr) # For label management
library(abind) # Array binding
library(raster) # Loading of some data
library(readxl) # Load excel files
library(kgc) # Koppen-Geiger climate classification
library(data.table) # Efficient data.frame and function between
library(dplyr) # For data management
library(elevatr) # For elevation data
library(rnaturalearthdata) # For coastlines: distance to coast
library(zen4R) # To download data from Zenodo

#----- Analysis
library(mice) # Missing value imputation
library(dlnm) # DLNM
library(splines) # For natural splines
library(mixmeta) # Second stage meta-analysis
library(pls) # PLS variables
library(MASS) # Multivariate normal simulation
library(gstat) # Performs kriging

#----- Plotting
library(ggplot2) # Plotting
library(scales) # Scale management
library(colorspace) # For specific color palettes
library(fields) # For specific color palettes
library(viridis) # For specific color palettes
library(RColorBrewer) # For specific color palettes
library(corrplot) # Plotting correlation matrices
library(patchwork) # Putting together plots created with ggplot
library(ggpubr) # For get_legend
library(ggstance) # For horizontal plot
library(ggnewscale) # To have several fill scales on the same plot

#----------------------
# Parameters
#----------------------

#----- Data

# Paths
path_euro <- "V:/VolumeQ/AGteam/Eurostat"

# Years selected for metapredictors. Averaged if several
year <- 2000:2020

# Years for analysis
yearanalysis <- 1990:2019

# Projecttion for geo objects
geoproj <- "4326"

# MCC country datasets
mcc_countries <- c('cyp0419', 'cze9420', 'est9718', 'fnl9411', 'fra0015',  
  'grc0110', 'irl8407', 'ita0110', 'nor6916', 'por8012', 
  'spa0913', 'sui9513', 'swe9016', 'uk9020',
  'ger9315', 'net9516c', 'rom9416') # Last line is MCC_all

# Region definition for background taken fro UN M49 
#   (see https://unstats.un.org/unsd/methodology/m49/)
regionlist <- c(BG = "Eastern", CZ = "Eastern", HU = "Eastern", RO = "Eastern", 
  SK = "Eastern", PL = "Eastern", DK = "Northern", FI = "Northern", 
  SE = "Northern", EE = "Northern", LV = "Northern", UK = "Northern", 
  IE = "Northern", LT = "Northern", NO = "Northern", IS = "Northern", 
  ES = "Southern", HR = "Southern", IT = "Southern", CY = "Southern", 
  EL = "Southern", PT = "Southern", MT = "Southern", SI = "Southern", 
  RS = "Southern", ME = "Southern", MK = "Southern", AL = "Southern", 
  AT = "Western", BE = "Western", FR = "Western", LU = "Western", 
  DE = "Western", NL = "Western", CH = "Western", LI = "Western")

# Region order for displaying in Tables and Figure
regord <- c("Northern", "Western", "Eastern", "Southern")

#----- First-stage analysis

# Exposure dimension
varfun <- "bs"
varper <- c(10,75,90)
vardegree <- 2

# Lag dimension
maxlag <- 21
lagfun <- "ns"
lagknots <- logknots(maxlag, 3)

# Minimum number of deaths for being considered in analysis
mindeath <- 5000

# Minimum age to be considered in analysis and prediction
minage <- 20

#----- Second-stage analysis

# Metapredictors
metapreds <- c("pop", "prop_65p", "popdens", "lifexp_00", "isol",
  "gdp", "unempl", "educ", "depriv", "bedrates",
  "imperv", "tree", "grass", "water", "woody",
  "elevation", "coast_dist", "ndvi", "pm25", "no2",
  "tmean", "trange")

# Number of metapredictor components
npc <- 4

# Knots for age spline
ageknots <- NULL

# Parameters for variogram model
variopars <- list(
  model = "Gau",
  nugget = NA,
  range = 250
)

#----- Results exploitation

# Prediction percentiles
predper <- c(seq(0.1,1,0.1), 2:98, seq(99,99.9,0.1))

# Acceptable MMP range
mmprange <- c(25, 99)

# Reported percentiles for cold and heat
resultper <- c(1, 99)

# Percentiles on axis
axisper <- c(1, 25, 50, 75, 99)

# Number of grid point for background surface
ngrid <- 50

# Age groups for excess mortality
agebreaks <- c(20, 45, 65, 75, 85)
agelabs <- c(paste(agebreaks[-length(agebreaks)], agebreaks[-1], sep = "-"), 
  sprintf("%i+", agebreaks[length(agebreaks)]))

# Grid for age prediction
agegrid <- minage:99

# Number of simulations for AN/AF
nsim <- 1000

# Denominator for death rates
byrate <- 10^5

#----- Other

# Number of cores for parallel computation
ncores <- max(1, detectCores() - 2)
