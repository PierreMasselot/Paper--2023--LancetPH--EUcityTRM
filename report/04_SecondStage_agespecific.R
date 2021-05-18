################################################################################
#
#                       Exhaustion Level 1
#
#                       Second stage meta-analysis
#
################################################################################

library(mixmeta)
library(doParallel)

#---------------------------
#  Prepare paralelization
#---------------------------

ncores <- detectCores()
cl <- makeCluster(max(1, ncores - 2))
registerDoParallel(cl)

#-------------------------------
# Loop on lags and causes for mixmeta
#-------------------------------

# foreach on lags (sensitivity analysis)
stage2res <- foreach(i = iter(seq_along(lags)), 
  .packages = c("mixmeta")) %dopar% {
  
  # To store results  
  outres <- allouts
    
  # Loop on outcomes
  for (j in seq_along(allouts)){
    
    for (k in seq_along(allages)){
    
      st1list <- allstage1[[i]][[j]][[k]]
      
      #----- Extract variables
      # First stage coefficients
      coefs <- t(sapply(st1list, "[[", "coef"))
      
      # Coefs variance-covariance matrices
      vcovs <- lapply(st1list, "[[", "vcov")
      
      # Which model converged
      convs <- sapply(st1list, "[[", "conv")
      
      # City
      city <- names(st1list)
      
      # Country
      country <- citydesc[match(city, citydesc$cityname), "countryname"]
      
      #----- Apply meta-analytical model
      outres[[j]][[k]] <- mixmeta(coefs, random = ~ 1|country/city, 
        S = vcovs, subset = convs, control = list(showiter = T))
    }
  }
  
  outres
}

# Stop parallel
stopCluster(cl)

# Rename
names(stage2res) <- lags

# Save
save(stage2res, citydesc, file = "results/SecondStage.RData")
