################################################################################
#
#                       Exhaustion Level 1
#
#                        Results computation
#
################################################################################

library(mixmeta)
library(dlnm)

load("results/SecondStage.RData")

#-------------------------------
# Parameters
#-------------------------------

# Define regions
# regions <- c(1, 2, 2, 3, 1)
# names(regions) <- unique(citydesc$countryname)
# regionlabs <- c("Western", "Mediterranean", "Scandinavian")

# Define cold and heat percentile
resultper <- c(1, 99)

# Prediction percentiles
predper <- c(seq(0,1,0.1), 2:98, seq(99,100,0.1))

# Acceptable MMP range
mmprange <- c(1, 99)

#-------------------------------
# Extract ERFs
#-------------------------------

#----- Set up objects

# Create basis for overall relationship
ov_basis <- onebasis(predper, fun = "bs", degree = 2, knots = c(10, 75, 90))

# Function to compute ERF
erf <- function(blup){
  firstpred <- ov_basis %*% blup$blup
  mmp <- predper[inrange][which.min(firstpred[inrange,])]
  crosspred(basis = ov_basis, coef = blup$blup, vcov = blup$vcov, 
    cen = mmp, at = predper, model.link = "log")
}

#----- Extract ERFs 

# Extract continental level ERF
continentERF <- lapply(stage2res, lapply, lapply, function(x) {
  blext <- blup(x, level = 0, vcov = T)[[1]]
  erf(blext)
})

# Extract country level ERF
countryERF <- lapply(stage2res, lapply, lapply, function(x) {
  blext <- blup(x, level = 1, vcov = T)
  blext <- blext[!duplicated(blext)]
  lapply(blext, erf)
})


