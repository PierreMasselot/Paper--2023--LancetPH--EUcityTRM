################################################################################
#
#                         MCC-CityEurope
#
#                       First-stage models
#
################################################################################

library(dlnm)
library(splines)
library(doParallel)

load("data/Alldata.RData")

#---------------------------
#  Prepare parallelization
#---------------------------

ncores <- detectCores()
cl <- makeCluster(max(1, ncores - 2))
registerDoParallel(cl)

#---------------------------
# Loop on cities
#---------------------------

stage1res <- foreach(i = iter(seq(dlist)), 
  .packages = c("dlnm", "splines")) %dopar% {
  
  # Extract data
  dat <- dlist[[i]]
  
  # Define outcome
  dat$y <- if(!"all" %in% names(dat)) dat$nonext else dat$all
  
  # Define crossbasis
  argvar <- list(fun = "bs", degree = 2, 
    knots = quantile(dat$era5tmean, c(.1, .75, .9), na.rm = T))
  cb <- crossbasis(dat$era5tmean, lag = 21, argvar = argvar,
    arglag = list(knots = logknots(21, 3)))
  
  # Run model
  res <- glm(y ~ cb + dow + ns(date, df = 7 * length(unique(year))), 
    dat, family = quasipoisson)
  
  # Reduction to overall cumulative
  redall <- crossreduce(cb, res, cen = 10)
  
  # Output
  list(coef = coef(redall), vcov = vcov(redall), 
    tsum = summary(dat$era5tmean), totdeath = sum(dat$y, na.rm=T)
  )
}

# Stop parallel
stopCluster(cl)

# Rename
names(stage1res) <- cities$city

# Export
save(dlist, cities, countries, stage1res, 
  metavar, metadesc, imputed, 
  file = "results/FirstStage.RData")
