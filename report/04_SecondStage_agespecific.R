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
cl <- makeCluster(max(1, ncores - 2), outfile = "stage2track.txt")
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
      
      # Monitor loop
      print(sprintf("i = %i, j = %i, k = %i", i, j, k))
      
      # Extract results
      st1list <- allstage1[[i]][[j]][[k]]
      
      # Remove nulls (cities for which we did not have data)
      st1list <- st1list[!sapply(st1list, is.null)]
      
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
      
      # Region
      region <- citydesc[match(city, citydesc$cityname), "region"]
      
      #----- Apply meta-analytical model
      outres[[j]][[k]] <- mixmeta(coefs, random = ~ 1|region/country/city, 
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
