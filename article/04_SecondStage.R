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

# Metapredictors
metaprednames <- c("prop_65p", "lifexp", "gdp", "educ", "unempl", "depriv",
  "bedrates", "pop", "popdens", "urbshare", "greenshare", "blueshare",
  "cooldegdays", "heatdegdays", "isol", "mount_type", "urbn_type", 
  "coast_type", "tmean", "greenness", "pm25")

#---------------------------
#  PCA on metavariables
#---------------------------

# Select meta predictors
metavar <- metadata[, metaprednames]

# Perform PCA on metavariables
pcares <- prcomp(metavar, center = TRUE, scale = TRUE)

# Extract principal components
pcvar <- pcares$x[, seq_len(npc)]

#---------------------------
#  Meta-regression model
#---------------------------

# Put all city and age results in a big list
unlistresults <- unlist(sapply(stage1res, "[[", "modelres"), recursive = F)

# Get coefs, vcov and average age
coefs <- t(sapply(unlistresults, "[[", "coef"))
vcovs <- lapply(unlistresults, "[[", "vcov")
agevals <- sapply(unlistresults, "[[", "ageval")

# Match mcc cities to each obs of meta regression
st2mcccodes <- apply(t(sapply(strsplit(names(unlistresults), "\\."), "[", 1:2)),
  1, paste, collapse = ".")
repmcc <- match(st2mcccodes, metadata$mcc_code)

# Get MCC cities PCs
pcs <- pcvar[repmcc,]

# Extract lon / lat coordinates
citycoords <- do.call(rbind, metageo$geometry[repmcc,])
colnames(citycoords) <- c("lon", "lat")

# Compute boundary knots to ensure all predicted cities are covered
urauext <- st_bbox(metageo)
bnlon <- urauext[c(1,3)]
bnlat <- urauext[c(2,4)]

# Create stage 2 data.frame
stage2df <- data.frame(pcs, citycoords, age = agevals, 
  city = metadata[repmcc, "URAU_CODE"], country = metadata[repmcc, "CNTR_CODE"])

#### SUBSET BY CONVERGENCE
# Create formula
st2form <- sprintf("coefs ~ %s + ns(lon, df = 2, Boundary.knots = bnlon) + 
    ns(lat, df = 2, Boundary.knots = bnlat) + ns(age, knots = c(50, 75))",
  paste(colnames(pcvar), collapse = " + "))

# Apply meta regression model
stage2res <- mixmeta(as.formula(st2form), data = stage2df, 
  S = vcovs, random = ~ 1|city) 

## Waaaaaaaaaaaaaaaaaaaaaay too long
# stage2res <- mixmeta(as.formula(st2form), data = stage2df,
#   S = vcovs, random = ~ 1|country/city, 
#   control = list(showiter = T))
