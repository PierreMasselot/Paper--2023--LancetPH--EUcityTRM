################################################################################
#
#                         MCC-CityEurope
#
#                       First-stage models
#
################################################################################

source("00_Packages_Parameters.R")

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
  .packages = c("dlnm", "splines", "MESS")) %dopar% {
  
  #----- Extract data
  # City data
  dat <- dlist[[i]]
  
  #----- Prepare model
  # Define crossbasis
  argvar <- list(fun = varfun, degree = vardegree, 
    knots = quantile(dat$era5landtmean, varper / 100, na.rm = T))
  cb <- crossbasis(dat$era5landtmean, lag = maxlag, argvar = argvar,
    arglag = list(fun = lagfun, knots = lagknots))
  
  # Select outcome: if no age classes select only total age
  #   If no all-cause, select non-external (natural)
  outtype <- ifelse(any(grepl("all", names(dat))), "all", "nonext")
  yvars <- grep(sprintf("%s_", outtype), names(dat), value = T)
  if (length(yvars) == 0) yvars <- outtype
  
  # Compute cumulative age groupings
  ytot <- apply(dat[,yvars, drop = F], 2, sum, na.rm = T)
  cumgroups <- cumsumbinning(ytot, mindeath, cutwhenpassed = TRUE)
  
  #----- Loop on each cumulative group
  ageres <- list()
  for (a in unique(cumgroups)){
    
    # Outcome in this cumulative group
    avars <- yvars[cumgroups == a]
    
    # Extract outcome
    y <- rowSums(dat[,avars, drop = F])
    
    # Determine age range
    agerange <- sapply(strsplit(avars, "_"), "[", 2)
    if (is.na(agerange)){
      amin <- "00"
      amax <- "99"
    } else {
      amin <- substr(agerange[1], 1, 2)
      amax <- substr(tail(agerange, 1), 3, 4)
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
    names(ageres)[a] <- paste0(amin, amax)
  }
  
  # Output
  list(tsum = summary(dat$era5landtmean), modelres = ageres)
}

# Stop parallel
stopCluster(cl)

# Rename
names(stage1res) <- cities$city

# Export
save(dlist, cities, stage1res, era5series, metadesc,
  metadata, metageo, imputed, 
  file = "results/FirstStage.RData")
