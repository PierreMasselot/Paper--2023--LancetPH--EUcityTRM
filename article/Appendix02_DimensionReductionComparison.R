################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix 2: Compare different handlings of
#                             meta predictors
#
################################################################################

library(mixmeta)
library(splines)
library(rsample)
library(CCA)

## Execute the prepare data part of second stage meta-analysis

#---------------------------
# Parameters
#---------------------------

# Maximum number of components
maxk <- 10

#---------------------------
# Prepare cross validation
#---------------------------

#----- Split data
splitinds <- vfold_cv(data.frame(1:nrow(stage2df)))

#----- Basic formula and model
# baseline formula
basicform <- coefs ~ ns(lon, df = 2, Boundary.knots = bnlon) + 
  ns(lat, df = 2, Boundary.knots = bnlat) + ns(age, knots = c(50, 75))

# Baseline scores (without any meta predictor component)
basicscores <- score_mixmeta(basicform, stage2df, splitinds$splits)

#----- Function to apply mixmeta and extract scores
score_mixmeta <- function(formula, data, splits){
  # First model to obtain I2 stat
  resall <- mixmeta(formula, data = data, S = vcovs, random = ~ 1|city) 
  
  # Cross-validation
  mse <- rep(NA, length(splits))
  for (i in seq_along(splits)){
    data$train <- seq_len(nrow(data)) %in% analysis(splits[[i]])[,1]
    valid <- assessment(splits[[i]])[,1]
    rmm <- mixmeta(formula, data = data, S = vcovs, random = ~ 1|city,
      subset = train)
    pred <- predict(rmm, newdata = data[valid,])
    err <- coefs[valid,] - pred
    mse[i] <- sum(err^2, na.rm = T)
  }
  
  # Output
  list(cv = c(mean = mean(mse), sd = sd(mse)), 
    I2 = summary(resall)$i2stat[1], AIC = summary(resall)$AIC)
}

#----- Select meta predictors
metavar <- metadata[, metaprednames]

#---------------------------
# PCA
#---------------------------

#----- Perform PCA

# Perform PCA on metavariables
pcares <- prcomp(metavar, center = TRUE, scale = TRUE)

# Add all PCs to data.frame
pcdf <- cbind(stage2df, pcares$x[repmcc,])

#----- For each number of PCs, fit model

# Initialize formula and resul object
pcform <- basicform
pcscores <- vector("list", maxk)

# Loop
for (i in seq_len(maxk)){
  print(sprintf("PCA : %i / %i", i, maxk))
  
  # Update formula
  pcform <- update(pcform, as.formula(sprintf("~ . + PC%i", i)))
  
  # Perform CV
  pcscores[[i]] <- score_mixmeta(pcform, pcdf, splitinds$splits)
}

#----- Stepwise on PCs

stepform <- basicform
stepscores <- vector("list", maxk)
remain <- seq_along(metavar)

# Loop
for (i in seq_len(maxk)){
  print(sprintf("PC stepwise : %i / %i", i, maxk))
  
  # Loop on remaining 
  jform <- jscores <- vector("list", length(remain))
  for (j in seq_along(remain)){
    # Update formula
    jform[[j]] <- update(stepform, as.formula(sprintf("~ . + PC%i", remain[j])))
    
    # Perform CV
    jscores[[j]] <- score_mixmeta(jform[[j]], pcdf, splitinds$splits)
  }
  
  # Select the best one
  jsel <- which.min(sapply(jscores, function(x) x$cv[1]))
 
  # Update
  stepform <- jform[[jsel]]
  stepscores[[i]] <- jscores[[jsel]]
  remain <- remain[-jsel]
}


#---------------------------
# CCA
#---------------------------

#----- Perform CCA

# Find the directions
ccares <- cc(data.matrix(metavar[repmcc,]), coefs)

# Create new metavariables
ccavar <- ccares$scores$xscores
ccadf <- cbind(stage2df, cca = ccavar)

#----- For each number of CCA variables, fit model

# Initialize formula and resul object
ccaform <- basicform
ncca <- min(maxk, ncol(coefs))
ccascores <- vector("list", ncca)


# Loop
for (i in seq_len(ncca)){
  print(sprintf("CCA : %i / %i", i, ncca))
  
  # Update formula
  ccaform <- update(ccaform, as.formula(sprintf("~ . + cca.%i", i)))
  
  # Perform CV
  ccascores[[i]] <- score_mixmeta(ccaform, ccadf, splitinds$splits)
}

#---------------------------
# PLS
#---------------------------

#----- Perform PLS to obtain scores

metapls <- data.matrix(metavar[repmcc,])

# Compute PLSR
plsres <- plsr(coefs ~ metapls, scale = T)

# Extract scores
plsvar <- scores(plsres)
colnames(plsvar) <- sprintf("pls%i", seq_len(ncol(plsvar)))
plsdf <- cbind(stage2df, unclass(plsvar))

#----- For each number of PLS variables, fit model

# Initialize formula and resul object
plsform <- basicform
plsscores <- vector("list", maxk)

# Loop
for (i in seq_len(maxlag)){
  print(sprintf("PLS : %i / %i", i, maxk))
  
  # Update formula
  plsform <- update(plsform, as.formula(sprintf("~ . + pls%i", i)))
  
  # Perform CV
  plsscores[[i]] <- score_mixmeta(plsform, plsdf, splitinds$splits)
}


#---------------------------
# Save results
#---------------------------

save(basicscores, pcscores, stepscores, ccascores, plsscores, splitinds,
  file = "results/CVcomparison.RData")

#---------------------------
# Plot prediction scores
#---------------------------

#----- Extract scores values for each method

# CV
allcv <- list()
allcv$pc <- sapply(pcscores, "[[", "cv")[1,]
allcv$step <- sapply(stepscores, "[[", "cv")[1,]
allcv$cca <- sapply(ccascores, "[[", "cv")[1,]
allcv$pls <- sapply(plsscores, "[[", "cv")[1,]

cvmat <- do.call(cbind, lapply(allcv, function(x) 
  c(x, rep(NA, maxk - length(x)))))
cvmat <- rbind(basicscores$cv[1], cvmat)

#----- Extract AIC values for each method
allaic <- list()
allaic$pc <- sapply(pcscores, "[[", "AIC")
allaic$step <- sapply(stepscores, "[[", "AIC")
allaic$cca <- sapply(ccascores, "[[", "AIC")
allaic$pls <- sapply(plsscores, "[[", "AIC")

aicmat <- do.call(cbind, lapply(allaic, function(x) 
  c(x, rep(NA, maxk - length(x)))))
aicmat <- rbind(basicscores$AIC, aicmat)

#----- Extract I2 values for each method
alli2 <- list()
alli2$pc <- sapply(pcscores, "[[", "I2")
alli2$step <- sapply(stepscores, "[[", "I2")
alli2$cca <- sapply(ccascores, "[[", "I2")
alli2$pls <- sapply(plsscores, "[[", "I2")

i2mat <- do.call(cbind, lapply(alli2, function(x) 
  c(x, rep(NA, maxk - length(x)))))
i2mat <- rbind(basicscores$I2, i2mat)

#----- Plot the values

# Plot layout
x11(height = 10)
layout(cbind(1:3, 4), width = c(4, 1))

# CV
matplot(0:maxk, cvmat, type = "b", pch = 16, col = seq_along(allcv), 
  xlim = c(0, maxk), xlab = "", ylab = "Cross-validated MSE",
  main = "CV")

# AIC
matplot(0:maxk, aicmat, type = "b", pch = 16, col = seq_along(allaic), 
  xlim = c(0, maxk), xlab = "", ylab = "AIC",
  main = "AIC")

# I2
matplot(0:maxk, i2mat, type = "b", pch = 16, col = seq_along(alli2), 
  xlim = c(0, maxk), xlab = "", ylab = "I2",
  main = "I2")

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("center", legend = c("PCA", "Stepwise PCA", "CCA", "PLS"),
  col = seq_along(allcv), pch = 16, lty = seq_along(allcv),
  bty = "n", title = "Method")

# Save
dev.print(pdf, file = "figures/FigS2_1_CrossValidation.pdf")


#---------------------------
# Show loadings
#---------------------------

#----- Extract loadings
# For rotations directly
pcaload <- pcares$rotation[,1:maxk]
ccaload <- ccares$scores$corr.X.xscores
plsload <- plsres$loadings[,1:maxk]

# Extract right loading for step pca
fterms <- strsplit(as.character(stepform)[3], " \\+ ")[[1]]
selcomps <- substr(grep("PC", fterms, value = T), 3, 4)
stepload <- pcares$rotation[,as.numeric(selcomps)]

#----- Plot
# Number of components to plot
ncplot <- 5

# Prepare plot layout
x11(height = 15, width = 10)
par(oma = c(7, 0, 0, 0), mar = c(1, 4, 3, 2) + .1)
layout(cbind(1:ncplot, ncplot + 1), widths = c(4, 1))

# Loop on components
for (i in seq_len(ncplot)) {
  loadmat <- cbind(pcaload[,i], stepload[,i], ccaload[,i], plsload[,i])
  
  # Barplot
  bp <- barplot(t(loadmat), beside = T, border = NA, xaxt = "n",
    main = sprintf("Comp %i", i), col = 1:4, ylim = c(-1, 1))
  abline(h = 0)
  abline(v = bp[1,-1] - 1, lty = 3)
}
axis(1, at = colMeans(bp), labels = metaprednames, las = 3)

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("center", legend = c("PCA", "Stepwise PCA", "CCA", "PLS"),
  fill = seq_along(allcv), bty = "n", title = "Method")

# Save
dev.print(pdf, file = "figures/FigS2_2_Loadings.pdf")
