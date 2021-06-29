################################################################################
#
#                         MCC-CityEurope
#
#                     Packages and analysis parameters
#
################################################################################

#----------------------
# Necessary packages
#----------------------

#----- Data management
library(sf) # Geo data
library(eurostat) # To download eurostat data
library(doParallel) # Run loops in parallel
library(MESS) # For function cumsumbinning
library(Matrix) # Functions for matrix operations
library(modeest) # For computing mode of a vector

#----- Analysis
library(mice) # Missing value imputation
library(dlnm) # DLNM
library(splines) # For natural splines
library(mixmeta) # Second stage meta-analysis
library(pls) # PLS variables
library(MASS) # Multivariate normal simulation

#----- Plotting
library(ggplot2) # Plotting
library(scales) # Scale management
library(colorspace) # For specific color palettes
library(fields) # For specific color palettes
library(viridis) # For specific color palettes
library(corrplot) # Plotting correlation matrices

#----------------------
# Parameters
#----------------------

#----- Data
# Years selected for metapredictors. Averaged if several
year <- as.character(2005:2015)

# Starting year for analysis
yearstart <- 1990

# Projecttion for geo objects
geoproj <- "4326"

# MCC country datasets
mcc_countries <- c('cze9415', 'est9718', 'fnl9411', 'fra0014',  
  'grc0110', 'irl8407', 'ita0110', 'nor6916', 'por8012', 
  'spa0913', 'sui9513', 'swe9016', 'uk9016',
  'ger9315', 'net9516c', 'rom9416') # Last line is MCC_all

# Region definition for background taken fro UN M49 
#   (see https://unstats.un.org/unsd/methodology/m49/)
regionlist <- c(BG = "Eastern", CZ = "Eastern", HU = "Eastern", RO = "Eastern", 
  SK = "Eastern", PL = "Eastern", DK = "Northern", FI = "Northern", 
  SE = "Northern", EE = "Northern", LV = "Northern", UK = "Northern", 
  IE = "Northern", LT = "Northern", NO = "Northern", ES = "Southern", 
  HR = "Southern", IT = "Southern", CY = "Southern", EL = "Southern", 
  PT = "Southern", MT = "Southern", SI = "Southern", AT = "Western", 
  BE = "Western", FR = "Western", LU = "Western", DE = "Western", 
  NL = "Western", CH = "Western")

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
mindeath <- 10000

# Minimum age to be considered in analysis and prediction
minage <- 20

#----- Second-stage analysis

# Metapredictors
metaprednames <- c("pop", "popdens", "prop_65p", "isol", # Pop structure
  "lifexp", "bedrates", "gdp", "educ", "unempl", "depriv", # Socio economic
  "urbshare", "greenshare", "blueshare", "mount_type", "urbn_type", 
  "coast_type", # Land
  "cooldegdays", "heatdegdays", "tmean", "greenness", "pm25" # Environment
)

# Number of metapredictor components
npc <- 5

#----- Results exploitation

# Prediction percentiles
predper <- c(seq(0.1,1,0.1), 2:98, seq(99,99.9,0.1))

# Acceptable MMP range
mmprange <- c(1, 99)

# Reported percentiles for cold and heat
resultper <- c(1, 99)

# Percentiles on axis
axisper <- c(1, 25, 50, 75, 99)

# Number of grid point for background surface
ngrid <- 50

# Age groups for predictions
agebreaks <- c(45, 65, 75, 85)
agelabs <- paste(c("00", agebreaks), c(agebreaks, 99), sep = "-")

# Number of simulations for AN/AF
nsim <- 500