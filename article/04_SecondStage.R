################################################################################
#
#                         MCC-EUcityTRM
#
#                   Second stage meta analysis
#
################################################################################

library(mixmeta)

load("data/Alldata.RData")

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

# meta data.frame
stage2df <- data.frame(pc = pcvar[metadesc$inmcc,])

# Apply meta regression model
stage2res <- mixmeta(coefs ~ ., data = stage2df, S = vcovs) 
# Convergence issues when adding a country level random effect
# random = ~ 1|URAU_CODE/CNTR_CODE, data = subset(metadesc, inmcc))

#---------------------------
#  Fixed effect prediction
#---------------------------

# data.frame for prediction
preddf <- data.frame(pc = pcvar)

# Predict coefficients for each city
predcoefs <- predict(stage2res, preddf, se = T)
