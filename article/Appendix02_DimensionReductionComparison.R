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
library(doParallel)
library(pls)
library(dlnm)

## Execute the prepare data part of second stage meta-analysis

#---------------------------
# Parameters
#---------------------------

# Maximum number of components
maxk <- 15

#---------------------------
# Prepare cross validation
#---------------------------

#----- Prepare objects

# Split data
splitinds <- vfold_cv(data.frame(1:nrow(stage2df)))

# baseline formula
basicform <- coefs ~ ns(lon, df = 2, Boundary.knots = bnlon) + 
  ns(lat, df = 2, Boundary.knots = bnlat) + ns(age, knots = c(50, 75))

# Select meta predictors
metavar <- data.matrix(metadata[repmcc, metaprednames])

# Prepare object to store results
i2scores <- aicscores <- cvscores <- list()

#----- Prepare parallelization

ncores <- detectCores()
cl <- makeCluster(max(1, ncores - 2))
registerDoParallel(cl)

#---------------------------
# Baseline model (no component)
#---------------------------

#----- On all data 

# Fit mixmeta model
mixbasic <- mixmeta(basicform, data = stage2df, S = vcovs, random = ~ 1|city) 

# Extract scores
i2scores$basic <- summary(mixbasic)$i2stat[1] 
aicscores$basic <- summary(mixbasic)$AIC

#----- Cross-validation 
cvres <- foreach(i = iter(seq(splitinds$splits)), .combine = c,
  .packages = c("mixmeta", "rsample", "splines")) %dopar% 
{
  
  # Get training and validation fold indices
  trainind <- analysis(splitinds$splits[[i]])[,1]
  validind <- assessment(splitinds$splits[[i]])[,1]
  
  # Add object to global environment for mixmeta
  .GlobalEnv$basicform <- basicform
  .GlobalEnv$coefs <- coefs
  .GlobalEnv$bnlon <- bnlon
  .GlobalEnv$bnlat <- bnlat
  .GlobalEnv$trainind <- trainind
  
  # Apply mixmeta model on training fold
  resi <- mixmeta(basicform, data = stage2df, S = vcovs, random = ~ 1|city,
    subset = trainind)
  
  # Predict for validation data
  pred <- predict(resi, newdata = stage2df[validind,])
  
  # Compute RMSE
  err <- coefs[validind,] - pred
  sqrt(mean(err^2, na.rm = T))
}

cvscores$basic <- c(cvm = mean(cvres), 
  cvsd = sqrt(var(cvres) / length(splitinds$splits)))

#---------------------------
# PCA
#---------------------------

#----- Perform PCA (unsupervised therefore OK to perform beforehand)

# Perform PCA on metavariables
pcares <- prcomp(metavar, center = TRUE, scale = TRUE)

# Add all PCs to data.frame
pcdf <- cbind(stage2df, pcares$x)

# Initialize formula and result object
pcform <- basicform
i2scores$PCA <- aicscores$PCA <- vector("numeric", maxk)

#----- apply on all data 

# Store model results for later use
mixpca <- vector("list", maxk)

# Loop on number of components
for (i in seq_len(maxk)){
  
  print(sprintf("PCA : %i / %i", i, maxk))
  
  # Update formula
  pcform <- update(pcform, as.formula(sprintf("~ . + PC%i", i)))
  
  # Fit mixmeta model
  mixpca[[i]] <- mixmeta(pcform, data = pcdf, S = vcovs, random = ~ 1|city)
  
  # Extract scores
  i2scores$PCA[i] <- summary(mixpca[[i]])$i2stat[1] 
  aicscores$PCA[i] <- summary(mixpca[[i]])$AIC
}
 
#----- Cross-validation 
pccvres <- foreach(i = iter(seq(splitinds$splits)), .combine = cbind,
  .packages = c("mixmeta", "rsample", "splines")) %dopar% 
{
  # Initialize formula
  pcform <- basicform
  
  # Get training and validation fold indices
  trainind <- analysis(splitinds$splits[[i]])[,1]
  validind <- assessment(splitinds$splits[[i]])[,1]
  
  # Add object to global environment for mixmeta
  .GlobalEnv$coefs <- coefs
  .GlobalEnv$bnlon <- bnlon
  .GlobalEnv$bnlat <- bnlat
  .GlobalEnv$trainind <- trainind
  
  # Loop on number of components
  rmse <- vector("numeric", maxk)
  
  for (j in seq_len(maxk)){
    # Update formula
    pcform <- update(pcform, as.formula(sprintf("~ . + PC%i", j)))
    
    # Fit mixmeta model
    .GlobalEnv$pcform <- pcform
    pcresj <- mixmeta(pcform, data = pcdf, S = vcovs, random = ~ 1|city,
      subset = trainind)
    
    # Predict for validation data
    pred <- predict(pcresj, newdata = pcdf[validind,])
    
    # Compute RMSE
    err <- coefs[validind,] - pred
    rmse[j] <- sqrt(mean(err^2, na.rm = T))
  }
  
  # Export
  rmse
}

cvscores$PCA <- cbind(cvm = apply(pccvres, 1, mean), 
  cvsd = apply(pccvres, 1, function(x) sqrt(var(x) / length(splitinds$splits))))


#---------------------------
# Stepwise on PCs
#---------------------------

# Initialize formula and result object
stepform <- basicform
i2scores$step <- aicscores$step <- vector("numeric", maxk)
cvscores$step <- matrix(NA, nrow = maxk, ncol = 2, 
  dimnames = list(NULL, c("cvm", "cvsd")))
remain <- seq_len(ncol(pcares$x))
mixstep <- vector("list", maxk)

# Loop
for (i in seq_len(maxk)){
  print(sprintf("PC stepwise : %i / %i", i, maxk))
  
  #----- Loop on remaining PCs
  jform <- jscores <- vector("list", length(remain))
  for (j in seq_along(remain)){
    # Update formula
    jform[[j]] <- jf <- 
      update(stepform, as.formula(sprintf("~ . + PC%i", remain[j])))
    
    #----- Perform CV
    jres <- foreach(k = iter(seq(splitinds$splits)), .combine = c,
      .packages = c("mixmeta", "rsample", "splines")) %dopar% 
    {
      # Get training and validation fold indices
      trainind <- analysis(splitinds$splits[[k]])[,1]
      validind <- assessment(splitinds$splits[[k]])[,1]
      
      # Add object to global environment for mixmeta
      .GlobalEnv$coefs <- coefs
      .GlobalEnv$bnlon <- bnlon
      .GlobalEnv$bnlat <- bnlat
      .GlobalEnv$trainind <- trainind
      .GlobalEnv$jf <- jf
        
      # Fit mixmeta model
      stepresk <- mixmeta(jf, data = pcdf, S = vcovs, random = ~ 1|city,
        subset = trainind)
        
      # Predict for validation data
      pred <- predict(stepresk, newdata = pcdf[validind,])
        
      # Compute RMSE
      err <- coefs[validind,] - pred
      sqrt(mean(err^2, na.rm = T))
    }
    
    jscores[[j]] <- c(cvm = mean(jres), 
      cvsd = sqrt(var(jres) / length(splitinds$splits)))
  }
  
  # Select the best one
  jsel <- which.min(sapply(jscores, "[", 1))
 
  # Update
  stepform <- jform[[jsel]]
  cvscores$step[i,] <- jscores[[jsel]]
  remain <- remain[-jsel]
  
  #----- Fit with all data for AIC and I2
  mixstep[[i]] <- mixmeta(stepform, data = pcdf, S = vcovs, random = ~ 1|city)
  
  # Extract scores
  i2scores$step[i] <- summary(mixstep[[i]])$i2stat[1] 
  aicscores$step[i] <- summary(mixstep[[i]])$AIC
}


#---------------------------
# CCA
#---------------------------

#----- apply on all data 

# Apply CCA
ccares <- cancor(metavar, coefs)

# Create new metavariables
ccavar <- scale(metavar) %*% ccares$xcoef
ccadf <- cbind(stage2df, cca = ccavar)

# Initialize formula and result object
ccaform <- basicform
i2scores$CCA <- aicscores$CCA <- vector("numeric", maxk)
mixcca <- vector("list", maxk)

# Loop on number of components
for (i in seq_len(maxk)){
  
  print(sprintf("CCA : %i / %i", i, maxk))
  
  # Update formula
  ccaform <- update(ccaform, as.formula(sprintf("~ . + cca.%i", i)))
  
  # Fit mixmeta model
  mixcca[[i]] <- mixmeta(ccaform, data = ccadf, S = vcovs, random = ~ 1|city)
  
  # Extract scores
  i2scores$CCA[i] <- summary(mixcca[[i]])$i2stat[1] 
  aicscores$CCA[i] <- summary(mixcca[[i]])$AIC
}
 
#----- Cross-validation 
ccacvres <- foreach(i = iter(seq(splitinds$splits)), .combine = cbind,
  .packages = c("mixmeta", "rsample", "splines")) %dopar% 
{
  # Initialize formula
  ccaform <- update(basicform, coeftrain ~ .)
  
  # Get training and validation fold indices
  trainind <- analysis(splitinds$splits[[i]])[,1]
  validind <- assessment(splitinds$splits[[i]])[,1]
  
  # Subset
  coeftrain <- coefs[trainind,]
  metatrain <- metavar[trainind,]
  
  # Create CCA variable on training sample only
  ccares <- cancor(metatrain, coeftrain)
  ccadf <- cbind(stage2df[trainind,], cca = scale(metatrain) %*% ccares$xcoef)
  
  # Add object to global environment for mixmeta
  .GlobalEnv$bnlon <- bnlon
  .GlobalEnv$bnlat <- bnlat
  .GlobalEnv$coeftrain <- coeftrain
  
  # Loop on number of components
  rmse <- vector("numeric", maxk)
  
  for (j in seq_len(maxk)){
    # Update formula
    ccaform <- update(ccaform, as.formula(sprintf("~ . + cca.%i", j)))
    
    # Fit mixmeta model
    .GlobalEnv$ccaform <- ccaform
    ccaresj <- mixmeta(ccaform, data = ccadf, S = vcovs[trainind], 
      random = ~ 1|city)
    
    # Predict CCA scores on validation data
    ccapred <- scale(metavar[validind,], center = ccares$xcenter) %*% 
      ccares$xcoef
    
    # Predict coefs with new scores
    pred <- predict(ccaresj, 
      newdata = cbind(stage2df[validind,], cca = ccapred))
    
    # Compute RMSE
    err <- coefs[validind,] - pred
    rmse[j] <- sqrt(mean(err^2, na.rm = T))
  }
  
  # Export
  rmse
}

cvscores$CCA <- cbind(cvm = apply(ccacvres, 1, mean), 
  cvsd = apply(ccacvres, 1, 
    function(x) sqrt(var(x) / length(splitinds$splits))))


#---------------------------
# PLS
#---------------------------

#----- apply on all data 

# Compute PLSR
plsres <- plsr(coefs ~ metavar, scale = T)

# Extract scores
plsvar <- scores(plsres)
colnames(plsvar) <- sprintf("pls%i", seq_len(ncol(plsvar)))
plsdf <- cbind(stage2df, unclass(plsvar))

# Initialize formula and result object
plsform <- basicform
i2scores$PLS <- aicscores$PLS <- vector("numeric", maxk)
mixpls <- vector("list", maxk)

# Loop on number of components
for (i in seq_len(maxk)){
  
  print(sprintf("PLS : %i / %i", i, maxk))
  
  # Update formula
  plsform <- update(plsform, as.formula(sprintf("~ . + pls%i", i)))
  
  # Fit mixmeta model
  mixpls[[i]] <- mixmeta(plsform, data = plsdf, S = vcovs, random = ~ 1|city)
  
  # Extract scores
  i2scores$PLS[i] <- summary(mixpls[[i]])$i2stat[1] 
  aicscores$PLS[i] <- summary(mixpls[[i]])$AIC
}
 
#----- Cross-validation 
plscvres <- foreach(i = iter(seq(splitinds$splits)), .combine = cbind,
  .packages = c("mixmeta", "rsample", "splines", "pls")) %dopar% 
{
  # Initialize formula
  plsform <- update(basicform, coeftrain ~ .)
  
  # Get training and validation fold indices
  trainind <- analysis(splitinds$splits[[i]])[,1]
  validind <- assessment(splitinds$splits[[i]])[,1]
  
  # Subset
  coeftrain <- coefs[trainind,]
  metatrain <- metavar[trainind,]
  
  # Create PLS variable on training sample only
  plsres <- plsr(coeftrain ~ metatrain, scale = T)
  plsvar <- scores(plsres)
  colnames(plsvar) <- sprintf("pls%i", seq_len(ncol(plsvar)))
  plsdf <- cbind(stage2df[trainind,], unclass(plsvar))
  
  # Predict PLS scores on validation data
  plspred <- predict(plsres, newdata = metavar[validind,], ncomp = 1:maxk, 
    type = "scores")
  colnames(plspred) <- sprintf("pls%i", seq_len(ncol(plspred)))
  validdf <- cbind(stage2df[validind,], plspred)
  
  # Add object to global environment for mixmeta
  .GlobalEnv$bnlon <- bnlon
  .GlobalEnv$bnlat <- bnlat
  .GlobalEnv$coeftrain <- coeftrain
  
  # Loop on number of components
  rmse <- vector("numeric", maxk)
  
  for (j in seq_len(maxk)){
    # Update formula
    plsform <- update(plsform, as.formula(sprintf("~ . + pls%i", j)))
    
    # Fit mixmeta model
    .GlobalEnv$plsform <- plsform
    plsresj <- mixmeta(plsform, data = plsdf, S = vcovs[trainind], 
      random = ~ 1|city)
    
    # Predict coefs with new scores
    pred <- predict(plsresj, newdata = validdf)
    
    # Compute RMSE
    err <- coefs[validind,] - pred
    rmse[j] <- sqrt(mean(err^2, na.rm = T))
  }
  
  # Export
  rmse
}

cvscores$PLS <- cbind(cvm = apply(plscvres, 1, mean), 
  cvsd = apply(plscvres, 1, 
    function(x) sqrt(var(x) / length(splitinds$splits))))

#---------------------------
# Save results
#---------------------------

# Stop cluster
stopCluster(cl)

# Save results
save(cvscores, i2scores, aicscores, splitinds,
  file = "results/CVcomparison.RData")

#---------------------------
# Plot prediction scores
#---------------------------

#----- Extract scores values for each method

# CV
cvmat <- do.call(cbind, lapply(cvscores[-1], "[", , 1))
cvmat <- rbind(cvscores[[1]][1], cvmat)

#----- Extract AIC values for each method
aicmat <- do.call(cbind, aicscores[-1])
aicmat <- rbind(aicscores[[1]], aicmat)

#----- Extract I2 values for each method
i2mat <- do.call(cbind, i2scores[-1])
i2mat <- rbind(i2scores[[1]], i2mat)

#----- Plot the values

# Plot layout
x11(height = 10)
layout(cbind(1:3, 4), width = c(4, 1))

# CV
matplot(0:maxk, cvmat, type = "b", pch = 16, col = seq_len(ncol(cvmat)) + 1,
  xlim = c(0, maxk), xlab = "", ylab = "Cross-validated RMSE",
  main = "CV")

# AIC
matplot(0:maxk, aicmat, type = "b", pch = 16, col = seq_len(ncol(aicmat)) + 1,
  xlim = c(0, maxk), xlab = "", ylab = "AIC",
  main = "AIC")

# I2
matplot(0:maxk, i2mat, type = "b", pch = 16, col = seq_len(ncol(i2mat)) + 1,
  xlim = c(0, maxk), xlab = "", ylab = "I2",
  main = "I2")

# Add legend
par(mar = c(5, 0, 4, 0))
plot.new()
legend("center", legend = c("PCA", "Stepwise PCA", "CCA", "PLS"),
  pch = 16, lty = seq_len(ncol(cvmat)), col = seq_len(ncol(cvmat)) + 1,
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


#---------------------------
# Show predicted curves 
#---------------------------

#----- Parameters

# Prediction percentiles
predper <- c(seq(0,1,0.1), 2:98, seq(99,100,0.1))

# Select models with which to predict
modlist <- list(NoPred = mixbasic, PCA4 = mixpca[[4]], PLS4 = mixpls[[4]])

# Acceptable MMP values 
inrange <- predper >= mmprange[1] & predper <= mmprange[2]

#----- Predict curves from models

# Stage 1
st1curves <- Map(function(b, vc, era5){
  tmeanper <- quantile(era5$era5landtmean, predper / 100)
  bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
    knots = quantile(era5$era5landtmean, varper / 100))
  firstpred <- bvar %*% b
  mmt <- tmeanper[inrange][which.min(firstpred[inrange])]
  crosspred(bvar, coef = b, vcov = vc, cen = mmt, 
    model.link="log", at = quantile(era5$era5landtmean, predper / 100))
}, as.data.frame(t(coefs)), vcovs, era5series[repmcc])

# Fitted
fittedcurves <- lapply(modlist, function(x){
  Map(function(b, era5){
    tmeanper <- quantile(era5$era5landtmean, predper / 100)
    bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
      knots = quantile(era5$era5landtmean, varper / 100))
    firstpred <- bvar %*% b$fit
    mmt <- tmeanper[inrange][which.min(firstpred[inrange])]
    crosspred(bvar, coef = b$fit, vcov = b$vcov, cen = mmt, 
      model.link="log", at = quantile(era5$era5landtmean, predper / 100))
  }, predict(x, vcov = T), era5series[repmcc])
})

#----- Plot
pdf("figures/FigS2_2_fittedERF.pdf", width = 9, height = 13, pointsize = 8)
layout(matrix(seq(6 * 4), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(st1curves)){
  # Plot cold and heat separately
  plot(st1curves[[i]], xlab = "Temperature (°C)", ylab = "RR", 
    main = rownames(coefs)[i], lwd = 2, ylim = c(.5, 3))
  for (j in seq_along(modlist))
    lines(fittedcurves[[j]][[i]], col = 1 + j, lwd = 2)
  abline(h = 1, lty = 2)
  legend("bottomleft", legend = c("Stage 1", names(modlist)),
    lwd = 2, col = seq_len(length(modlist) + 1), bty = "n",
    ncol = length(modlist) + 1, cex = .8)
}

dev.off()
