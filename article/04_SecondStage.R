################################################################################
#
#                         MCC-EUcityTRM
#
#                   Second stage meta analysis
#
################################################################################

library(mixmeta)
library(sf)
library(splines)

load("results/FirstStage.RData")

#---------------------------
#  Parameters
#---------------------------

# Number of principal components (should be checked by screeplot)
npc <- 4

#---------------------------
#  PCA on metavariables
#---------------------------

# Perform PCA on metavariables
pcares <- prcomp(metavar, center = TRUE, scale = TRUE)

# Extract principal components
pcvar <- pcares$x[, seq_len(npc)]

#---------------------------
#  Meta-regression model
#---------------------------

# Extract first-stage coefficients and vcov
coefs <- t(sapply(stage1res, "[[", "coef"))
vcovs <- lapply(stage1res, "[[", "vcov")

# Extract lon / lat coordinates
citycoords <- do.call(rbind, metageo$geometry)
colnames(citycoords) <- c("lon", "lat")

# Compute boundary knots to ensure all predicted cities are covered
urauext <- st_bbox(metageo)
bnlon <- urauext[c(1,3)]
bnlat <- urauext[c(2,4)]

# Create stage 2 data.frame
stage2df <- data.frame(pcvar, citycoords)

# Create formula
st2form <- sprintf("coefs ~ %s + ns(lon, df = 2, Boundary.knots = bnlon) + 
    ns(lat, df = 2, Boundary.knots = bnlat)",
  paste(colnames(pcvar), collapse = " + "))

# Apply meta regression model
stage2res <- mixmeta(as.formula(st2form), data = stage2df[metadesc$inmcc,], 
  S = vcovs) 
# Convergence issues when adding a country level random effect
# random = ~ 1|URAU_CODE/CNTR_CODE, data = subset(metadesc, inmcc))
