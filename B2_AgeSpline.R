################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix: Age spline comparison
#
################################################################################

if (length(ls()) == 0) source("07_ResultsPrep.R")

#-------------------
# Parameters
#-------------------

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

# Baseline formula for mixmeta: at this point regional background is chosen
baseform <- "coefs ~ region"

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
par(mar = c(7, 4, 4, 2) + .1)
plot(aicvec, pch = 16, ylab = "AIC", xlab = "", xaxt = "n",
  col = ifelse(aicvec == min(aicvec), 2, 1), cex = 1.5)
axis(1, at = seq_along(aicvec), labels = names(aicvec), las = 3)

dev.print(png, file = "figures/FigS_agecomparison.png", width = 9, height = 6,
  units = "in", res = 300)
