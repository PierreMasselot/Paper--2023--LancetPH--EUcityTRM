################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Part 7: Fit second-stage meta-regression model
#
################################################################################

# Choice of models is performed by script A2

if (length(ls()) == 0) source("06_PrepSecondStage.R")

#---------------------------
#  PCA/PLS on metavariables
#---------------------------

# Select meta predictors
metavar <- metadata[, metapreds]
metapls <- scale(metavar)[repmcc,]

# Compute PLSR (basic PLS computed as a linear model)
plsres <- plsr(coefs ~ metapls, center = F)

# Extract scores for all cities
pcvar <- predict(plsres, newdata = scale(metavar), ncomp = 1:npc, 
  type = "scores")
colnames(pcvar) <- sprintf("pls%i", seq_len(npc))

# Add to second stage dataset
stage2df <- cbind(stage2df, pcvar[repmcc,])

#---------------------------
#  Meta-regression model
#---------------------------

# Create formula
st2form <- sprintf("coefs ~ %s + region + 
    ns(age, knots = %s, Boundary.knots = c(0, 100))",
  paste(colnames(pcvar), collapse = " + "), deparse(ageknots))

# Apply meta regression model
stage2res <- mixmeta(as.formula(st2form), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 
