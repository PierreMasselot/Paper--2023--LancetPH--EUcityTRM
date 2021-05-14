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
    
    st1list <- allstage1[[i]][[j]]
    
    #----- Extract variables
    # First stage coefficients
    coefs <- t(sapply(st1list, "[[", "coef"))
    
    # Coefs variance-covariance matrices
    vcovs <- lapply(st1list, "[[", "vcov")
    
    # Which model converged
    convs <- sapply(st1list, "[[", "conv")
    
    # Age category
    ages <- factor(sapply(st1list, "[[", "age"),
      levels = 1:3, labels = c("main", "65p", "75p"))
    
    # City
    city <- sapply(strsplit(names(st1list), "\\."), "[", 1)
    
    # Country
    country <- droplevels(
      citydesc[match(city, citydesc$cityname), "countryname"])
    contrasts(country) <- contr.sum(nlevels(country))
    
    #----- Apply meta-analytical model
    outres[[j]] <- mixmeta(coefs ~ ages + country, random = ~ 1|city, 
      S = vcovs, subset = convs)
  }
  
  outres
}

# Stop parallel
stopCluster(cl)

# Rename
names(stage2res) <- lags