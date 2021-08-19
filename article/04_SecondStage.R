################################################################################
#
#                         MCC-EUcityTRM
#
#                   Second stage meta analysis
#
################################################################################

load("results/FirstStage.RData")

source("00_Packages_Parameters.R")

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

# Set contrast for region
contrasts(metadata$region) <- "contr.helmert"

# Create stage 2 data.frame
stage2df <- data.frame(region = metadata$region[repmcc], age = agevals, 
  city = metadata[repmcc, "URAU_CODE"], 
  country = as.factor(metadata[repmcc, "CNTR_CODE"]))

# Store model dimensions
nc <- ncol(coefs) # Number of first-stage coefficients
nm <- sum(lengths(metapreds)) # Number of metapredictors

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
metaprednames <- unlist(metapreds)
metavar <- metadata[, metaprednames]
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
    ns(age, knots = 65, Boundary.knots = c(0, 100))",
  paste(colnames(pcvar), collapse = " + "))

# Apply meta regression model
stage2res <- mixmeta(as.formula(st2form), data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

## Waaaaaaaaaaaaaaaaaaaaaay too long
# stage2res <- mixmeta(as.formula(st2form), data = stage2df,
#   S = vcovs, random = ~ 1|country/city,
#   control = list(showiter = T))
