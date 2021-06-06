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

# Repeat values for each age
ageseqs <- Map(seq, agemin, agemax, by = 1)
agedeaths <- mapply(function(d, a){ 
    do.call(cbind, rep(list(d), length(a))) / length(a)
  }, metadata[,deathvars], ageseqs)
agedeaths <- do.call(cbind, agedeaths)
ageseq <- unlist(ageseqs, use.names = F)
colnames(agedeaths) <- ageseq

#---------------------------
# Loop on cities
#---------------------------

stage1res <- foreach(i = iter(seq(dlist)), 
  .packages = c("dlnm", "splines")) %dopar% {
  
  #----- Extract data
  # City data
  dat <- dlist[[i]]
  
  #----- Prepare model
  # Define crossbasis
  argvar <- list(fun = "bs", degree = 2, 
    knots = quantile(dat$era5landtmean, c(.1, .75, .9), na.rm = T))
  cb <- crossbasis(dat$era5landtmean, lag = 21, argvar = argvar,
    arglag = list(knots = logknots(21, 3)))
  
  # Select outcome: if no age classes select only total age
  #   If no all-cause, select non-external (natural)
  outtype <- ifelse(any(grepl("all", names(dat))), "all", "nonext")
  yvars <- grep(sprintf("%s_", outtype), names(dat), value = T)
  if (length(yvars) == 0) yvars <- outtype
  
  #----- Loop on age classes
  ageres <- list()
  for (a in seq_along(yvars)){
    # Extract outcome
    y <- dat[,yvars[a]]
    
    # Determine age range
    agerange <- strsplit(yvars[a], "_")[[1]][2]
    if (is.na(agerange)){
      amin <- "00"
      amax <- "99"
    } else {
      amin <- substr(agerange, 1, 2)
      amax <- substr(agerange, 3, 4)
    }
    
    # Select concerned range
    whichgrps <- ageseq >= as.numeric(amin) & ageseq <= as.numeric(amax)
    ndeaths <- subset(agedeaths, metadata$mcc_code == cities[i, "city"],
      whichgrps)
    
    # Mean age group weighted by deaths
    meanage <- weighted.mean(ageseq[whichgrps], ndeaths)
    
    # Run model
    res <- try(glm(y ~ cb + dow + ns(date, df = 7 * length(unique(year))), 
      dat, family = quasipoisson))
    
    # Some models fail completely (pseudo-weights of IRLS become Inf)
    # Exit current iteration with NAs and a flag for non-convergence
    if (inherits(res, "try-error") || !res$converged){
      nred <- attr(cb, "df")[1]
      ageres[[a]] <- list(coef = rep(NA, nred), 
        vcov = matrix(NA, nred, nred), 
        totdeath = sum(y, na.rm = T), conv = F, ageval = meanage
      )
      names(ageres)[a] <- paste0(amin,amax)
      next
    }
    
    # Reduction to overall cumulative
    redall <- crossreduce(cb, res, cen = median(dat$era5landtmean))
    
    # Output
    ageres[[a]] <- list(coef = coef(redall), vcov = vcov(redall), 
      totdeath = sum(y, na.rm = T), conv = res$converged, ageval = meanage
    )
    names(ageres)[a] <- paste0(amin,amax)
  }
  
  # Output
  list(tsum = summary(dat$era5landtmean), modelres = ageres)
}

# Stop parallel
stopCluster(cl)

# Rename
names(stage1res) <- cities$city

# Export
save(dlist, cities, stage1res, era5series,
  metadata, metageo, imputed, 
  file = "results/FirstStage.RData")
