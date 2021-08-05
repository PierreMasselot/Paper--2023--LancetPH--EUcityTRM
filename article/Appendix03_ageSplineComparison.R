################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix: Age spline comparison
#
################################################################################

source("05_ResultsPrep.R")

#-------------------
# Parameters
#-------------------

# List of compared ns parametrization
comparedParam <- list(
  lin = list(),
  df2 = list(df = 2),
  df3 = list(df = 3),
  df4 = list(df = 4),
  k65 = list(knots = 65),
  k70 = list(knots = 70),
  k75 = list(knots = 75),
  k80 = list(knots = 80),
  k5065 = list(knots = c(50, 65)),
  k5070 = list(knots = c(50, 70)),
  k5075 = list(knots = c(50, 75)),
  k5080 = list(knots = c(50, 80)),
  k6070 = list(knots = c(60, 70)),
  k6075 = list(knots = c(60, 75)),
  k6080 = list(knots = c(60, 80)),
  k6575 = list(knots = c(65, 75)),
  k6580 = list(knots = c(65, 80)),
  k7080 = list(knots = c(70, 80))
)

# baseline formula for mixmeta
baseform <- sprintf("coefs ~ %s + region",
  paste(colnames(pcvar), collapse = " + "))

#-------------------
# Apply each parametrization
#-------------------

#----- Apply mixmeta with age spline for each parametrization
allres <- lapply(comparedParam, function(param){
  
  # Expand spline with current parameters
  allnsparams <- c(param, list(x = agevals, Boundary.knots = c(0, 100)))
  expandns <- do.call(ns, allnsparams)
  colnames(expandns) <- sprintf("agesp%i", 1:ncol(expandns))
  
  # Update df and formula
  newdf <- cbind(stage2df, expandns)
  newform <- paste(c(baseform, colnames(expandns)), collapse = " + ")
  .GlobalEnv$newform <- newform
  
  # Fit mixmeta
  mixmeta(as.formula(newform), data = newdf, 
    S = vcovs, random = ~ 1|city, na.action = na.exclude) 
})

#----- Get scores

# AIC 
aicvec <- sapply(allres, function(x) summary(x)$AIC)

#-------------------
# Plots
#-------------------

# Plot AIC
plot(aicvec, pch = 16, ylab = "AIC", xlab = "Parametrization", xaxt = "n")
axis(1, at = seq_along(aicvec), labels = names(aicvec), las = 3)