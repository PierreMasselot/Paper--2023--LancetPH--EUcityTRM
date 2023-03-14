################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Appendix 2: Second-stage model selection
#
################################################################################

if (length(ls()) == 0) source("06_PrepSecondStage.R")

#-------------------------
# Step 1: choose spatial background
#-------------------------

# Initialize object containing formulas
formlist <- modlist <- list()

#----- Baseline model: random intercept model

# Baseline formula
formlist$base <- coefs ~ 1

# Baseline model for comparison
modlist$base <- mixmeta(formlist$base, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Koppen-Geiger model

# Rename values
coorddata <- metadata[, c("URAU_CODE", "lon", "lat")]
names(coorddata) <- c("Site", "Longitude", "Latitude")

# Extract KGC
kgc <- LookupCZ(coorddata, rc = T)
stage2df$kgc <- kgc[repmcc]

# Create formula
formlist$kgc <- update(formlist$base, ~ . + kgc)

# Fit mixmeta
modlist$kgc <- mixmeta(formlist$kgc, data = stage2df,
  S = vcovs, random = ~ 1|city, na.action = na.exclude)

#----- Spline lat/lon

# Add to the data.frame
latlon <- do.call(rbind, metageo$geometry[repmcc])
colnames(latlon) <- c("lon", "lat")
stage2df <- cbind(stage2df, latlon)

# Create formula
formlist$latlon <- update(formlist$base, ~ . + 
    ns(lon, df = 2, Boundary.knots = bnlon) + 
    ns(lat, df = 2, Boundary.knots = bnlat))

# Fit mixmeta
modlist$latlon <- mixmeta(formlist$latlon, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Spline tensor product lat/lon

# Create formula
formlist$latlontens <- update(formlist$base, 
  ~ . + ns(lon, df = 2, Boundary.knots = bnlon) * 
    ns(lat, df = 2, Boundary.knots = bnlat))

# Fit mixmeta
modlist$latlontens <- mixmeta(formlist$latlontens, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Indicator for region

# Add region of each city
stage2df$region <- metadata$region[repmcc]

# Create formula
formlist$region <- update(formlist$base, ~ . + region)

# Fit mixmeta
modlist$region <- mixmeta(formlist$region, data = stage2df, 
  S = vcovs, random = ~ 1|city, na.action = na.exclude) 

#----- Results

# AIC comparison
(allaic <- sapply(modlist, function(x) summary(x)$AIC))

# Keep best model
baseform <- formlist[[which.min(allaic)]]

#-------------------------
# Step 2: choose age specification
#-------------------------

#----- Parametrization

# List of compared ns parametrization
comparedParam <- list(
  Linear = list(),
  '2 df' = list(df = 2),
  '3 df' = list(df = 3),
  '4 df' = list(df = 4),
  'Knot 65' = list(knots = 65),
  'Knot 70' = list(knots = 70),
  'Knot 75' = list(knots = 75),
  'Knot 80' = list(knots = 80),
  'Knots 50, 65' = list(knots = c(50, 65)),
  'Knots 50, 70' = list(knots = c(50, 70)),
  'Knots 50, 75' = list(knots = c(50, 75)),
  'Knots 50, 80' = list(knots = c(50, 80)),
  'Knots 60, 70' = list(knots = c(60, 70)),
  'Knots 60, 75' = list(knots = c(60, 75)),
  'Knots 60, 80' = list(knots = c(60, 80)),
  'Knots 65, 75' = list(knots = c(65, 75)),
  'Knots 65, 80' = list(knots = c(65, 80)),
  'Knots 70, 80' = list(knots = c(70, 80))
)

#----- Apply mixmeta with age spline for each parametrization
allres <- lapply(comparedParam, function(param){
  
  # Expand spline with current parameters
  allnsparams <- c(list(quote(age), Boundary.knots = c(0, 100)), param)
  newform <- sprintf("%s + %s", deparse(baseform), 
    deparse(as.call(c(quote(ns), allnsparams))))

  # Fit mixmeta
  mixmeta(as.formula(newform), data = stage2df, 
    S = vcovs, random = ~ 1|city, na.action = na.exclude) 
})

#----- Get scores

# AIC 
(aicage <- sapply(allres, function(x) summary(x)$AIC))

# Get formula
baseform <- lapply(allres, formula)[[which.min(aicage)]]

#-------------------------
# Step 3: Number of PLS components
#-------------------------

# Maximum number of components
maxk <- 10

# Prepare object to store results
aicscores <- vector("numeric", maxk + 1)

#----- Compute PLS

# Select meta predictors
metavar <- metadata[, metapreds]
metapls <- scale(metavar)[repmcc,]

# Compute PLSR (basic PLS computed as a linear model)
plsres <- plsr(coefs ~ metapls, center = F)

# Extract the maxk first scores to compare
pcvar <- predict(plsres, newdata = scale(metavar), ncomp = 1:maxk, 
  type = "scores")
colnames(pcvar) <- sprintf("pls%i", seq_len(maxk))

# Create data.frame
plsdf <- cbind(stage2df, pcvar[repmcc,])

#----- Apply model with various number of components

# Initialize formula and result object
plsform <- baseform
mixpls <- vector("list", maxk)
aicpls <- vector("numeric", maxk + 1)

# Fit null model
mixpls[[1]] <- mixmeta(plsform, data = stage2df, S = vcovs, random = ~ 1|city) 

# Exctract AIC
aicpls[1] <- summary(mixpls[[1]])$AIC

# Loop on number of components
for (i in seq_len(maxk)){
  
  print(sprintf("PLS : %i / %i", i, maxk))
  
  # Update formula
  plsform <- update(plsform, as.formula(sprintf("~ . + pls%i", i)))
  
  # Fit mixmeta model
  mixpls[[i + 1]] <- mixmeta(plsform, data = plsdf, S = vcovs, random = ~ 1|city)
  
  # Extract scores
  aicpls[i + 1] <- summary(mixpls[[i]])$AIC
}

#----- Extract results

# Get formula
(baseform <- lapply(mixpls, formula)[[which.min(aicpls)]])

#-------------------------
# Plots
#-------------------------

#----- Age specification

par(mar = c(7, 4, 4, 2) + .1)
plot(aicage, pch = 16, ylab = "AIC", xlab = "", xaxt = "n",
  col = ifelse(aicage == min(aicage), 2, 1), cex = 1.5)
axis(1, at = seq_along(aicage), labels = names(aicage), las = 3)

dev.print(png, file = "figures/FigS_agecomparison.png", width = 9, height = 6,
  units = "in", res = 300)

#----- Number of components

# Plot 
plot(0:maxk, aicpls, type = "b", pch = 16, cex = 1.5,
  xlim = c(0, maxk), xlab = "", ylab = "AIC", 
  col = ifelse(aicpls == min(aicpls), 2, 1))
abline(v = which.min(aicpls) - 1, lty = 2)

# Save
dev.print(png, file = "figures/FigS_nPCchoice.png", width = 9, height = 6,
  units = "in", res = 300)