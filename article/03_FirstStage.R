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
#  Prepare loop
#---------------------------

#----- Prepare parallelisation
ncores <- detectCores()
cl <- makeCluster(max(1, ncores - 2))
registerDoParallel(cl)

#----- Variables for age attribution

# Extract and sort age group deaths
deathvars <- sort(grep("death_[[:digit:]]", names(metadata), value = T))

# Extract min, max and mid age group
agemin <- substr(deathvars, 7, 8)
agemax <- substr(deathvars, 9, 10)
agemid <- (as.numeric(agemin) + as.numeric(agemax)) / 2

#---------------------------
# Loop on cities
#---------------------------

stage1res <- foreach(i = iter(seq(dlist)), 
  .packages = c("dlnm", "splines")) %dopar% {
  
  #----- Extract data
  # City data
  dat <- dlist[[i]]
  citydesc <- subset(metadata, mcc_code == cities$city[i])
  
  #----- Prepare model
  # Define crossbasis
  argvar <- list(fun = "bs", degree = 2, 
    knots = quantile(dat$era5tmean, c(.1, .75, .9), na.rm = T))
  cb <- crossbasis(dat$era5tmean, lag = 21, argvar = argvar,
    arglag = list(knots = logknots(21, 3)))
  
  # Check for age classes
  checkclasses <- grep("all_", names(dat))
  yvars <- if (length(checkclasses) > 0) names(dat)[checkclasses] else "all"
  
  #----- Loop on age classes
  ageres <- list()
  for (a in seq_along(yvars)){
    # Extract outcome
    y <- dat[,yvars[a]]
    
    # Run model
    res <- glm(y ~ cb + dow + ns(date, df = 7 * length(unique(year))), 
      dat, family = quasipoisson)
    
    # Reduction to overall cumulative
    redall <- crossreduce(cb, res, cen = 10)
    
    # Determine age range
    amin <- substr(yvars[a], 5, 6)
    if (amin == "") amin <- "00"
    amax <- substr(yvars[a], 7, 8)
    if (amax == "") amax <- "99"
    
    # Select concerned range
    whichgrps <- agemin >= amin & agemax <= amax
    ndeaths <- citydesc[,deathvars[whichgrps]]
    
    # Mean age group weighted by deaths
    meanage <- weighted.mean(agemid[whichgrps], ndeaths)
    
    # Output
    ageres[[a]] <- list(coef = coef(redall), vcov = vcov(redall), 
      totdeath = sum(y, na.rm = T), ageval = meanage
    )
    names(ageres)[a] <- paste0(amin,amax)
  }
  
  # Output
  list(tsum = summary(dat$era5tmean), modelres = ageres)
}

# Stop parallel
stopCluster(cl)

# Rename
names(stage1res) <- cities$city

# Export
save(dlist, cities, countries, stage1res, 
  metavar, metadesc, metageo, imputed, 
  file = "results/FirstStage.RData")
