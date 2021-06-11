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
library(pls)

load("results/FirstStage.RData")

#---------------------------
#  Parameters
#---------------------------

# Number of metapredictor components
npc <- 4

# Metapredictors
metaprednames <- c("pop", "popdens", "prop_65p", "isol", # Pop structure
  "lifexp", "bedrates", "gdp", "educ", "unempl", "depriv", # Socio economic
  "urbshare", "greenshare", "blueshare", "mount_type", "urbn_type", 
    "coast_type", # Land
  "cooldegdays", "heatdegdays", "tmean", "greenness", "pm25" # Environment
)

#---------------------------
#  Prepare stage 2 dataset
#---------------------------

# Put all city and age results in a big list and keep only model which converged
unlistresults <- unlist(sapply(stage1res, "[[", "modelres"), recursive = F)

# Exclude models that did not converge
unlistresults <- unlistresults[sapply(unlistresults, "[[", "conv")]

# Exclude groups with mean age < 5
# unlistresults <- unlistresults[sapply(unlistresults, "[[", "ageval") > 5]

# Get coefs, vcov, average age and convergence
coefs <- t(sapply(unlistresults, "[[", "coef"))
vcovs <- lapply(unlistresults, "[[", "vcov")
agevals <- sapply(unlistresults, "[[", "ageval")

# Match mcc cities to each obs of meta regression
st2mcccodes <- apply(t(sapply(strsplit(names(unlistresults), "\\."), "[", 1:2)),
  1, paste, collapse = ".")
repmcc <- match(st2mcccodes, metadata$mcc_code)

# Extract lon / lat coordinates
citycoords <- do.call(rbind, metageo$geometry[repmcc,])
colnames(citycoords) <- c("lon", "lat")

# Compute boundary knots to ensure all predicted cities are covered
urauext <- st_bbox(metageo)
bnlon <- urauext[c(1,3)]
bnlat <- urauext[c(2,4)]

# Create stage 2 data.frame
stage2df <- data.frame(citycoords, age = agevals, 
  city = metadata[repmcc, "URAU_CODE"], 
  country = as.factor(metadata[repmcc, "CNTR_CODE"]))

#---------------------------
#  PCA/PLS on metavariables
#---------------------------

# # Select meta predictors
# metavar <- metadata[, metaprednames]
# 
# # Perform PCA on metavariables
# pcares <- prcomp(metavar, center = TRUE, scale = TRUE)
# 
# # Extract principal components
# pcvar <- pcares$x[, seq_len(npc)]

# Select meta predictors
metavar <- metadata[, metaprednames]
metapls <- scale(metavar)[repmcc,]

# Compute PLSR (basic PLS computed as a linear model)
plsres <- plsr(coefs ~ metapls, center = F)

# Extract scores for all cities
pcvar <- predict(plsres, newdata = scale(metavar), ncomp = 1:npc, 
  type = "scores")
colnames(pcvar) <- sprintf("pls%i", seq_len(ncol(pcvar)))

# Add to second stage dataset
stage2df <- cbind(stage2df, pcvar[repmcc,])

#---------------------------
#  Meta-regression model
#---------------------------

# Create formula
st2form <- sprintf("coefs ~ %s + ns(lon, df = 2, Boundary.knots = bnlon) + 
    ns(lat, df = 2, Boundary.knots = bnlat) + ns(age, knots = c(50, 75))",
  paste(colnames(pcvar), collapse = " + "))

# Apply meta regression model
stage2res <- mixmeta(as.formula(st2form), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

## Waaaaaaaaaaaaaaaaaaaaaay too long
# stage2res <- mixmeta(as.formula(st2form), data = stage2df,
#   S = vcovs, random = ~ 1|country/city, 
#   control = list(showiter = T))
