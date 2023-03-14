################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Non-reproducible) R Code
# Part 5: First-stage analysis using MCC data
#
################################################################################


#---------------------------
#  Prepare loop
#---------------------------

#----- Prepare parallelisation
cl <- makeCluster(ncores)
registerDoParallel(cl)

#----- Write text file to trace iterations
writeLines(c(""), "temp/logstage1.txt")
cat(as.character(as.POSIXct(Sys.time())), file = "temp/logstage1.txt", 
  append = T)

#----- Variables for age attribution

# Extract and sort age group deaths
deathvars <- sort(grep("deathrate_[[:digit:]]", names(metadata), value = T))

# Extract min, and max of age group
agemin <- substr(deathvars, 11, 12)
agemax <- substr(deathvars, 13, 14)

# Repeat values for each age
ageseqs <- Map(seq, agemin, agemax, by = 1)
agedeaths <- mapply(function(d, a){ 
    do.call(cbind, rep(list(d), length(a)))
  }, metadata[,deathvars], ageseqs)
agedeaths <- do.call(cbind, agedeaths)
ageseq <- unlist(ageseqs, use.names = F)
colnames(agedeaths) <- ageseq
rownames(agedeaths) <- metadata$URAU_CODE

#---------------------------
# Loop on cities
#---------------------------

stage1res <- foreach(dat = iter(dlist), i = iter(seq(dlist)), 
  .packages = c("dlnm", "splines", "MESS"), .combine = rbind) %dopar% {

  #----- Store iteration (1 every 20)
  if(i %% 20 == 0) cat("\n", "iter = ", i, as.character(Sys.time()), "\n",
    file = "temp/logstage1.txt", append = T)
  
  #----- Prepare model
    
  # Extract ERA5 percentiles
  icode <- metadata[match(cities[i,"city"], metadata$mcc_code), "URAU_CODE"]  
  wholeera5 <- era5series[[icode]]$era5landtmean
    
  # Define crossbasis
  argvar <- list(fun = varfun, degree = vardegree, 
    knots = quantile(wholeera5, varper / 100, na.rm = T),
    Bound = range(wholeera5))
  cb <- crossbasis(dat$era5landtmean, lag = maxlag, argvar = argvar,
    arglag = list(fun = lagfun, knots = lagknots))
  
  # Select outcome: if no age classes select only total age
  #   If no all-cause, select non-external (natural)
  outtype <- ifelse(any(grepl("all", names(dat))), "all", "nonext")
  yvars <- grep(sprintf("%s_", outtype), names(dat), value = T)
  if (length(yvars) == 0) yvars <- outtype
  
  # Determine age ranges
  agerange <- sapply(strsplit(yvars, "_"), "[", 2)
  if (all(is.na(agerange))){
    amin <- "00"
    amax <- "99"
  } else {
    amin <- substr(agerange, 1, 2)
    amax <- substr(agerange, 3, 4)
  }
  
  # Exclude specific groups
  exclage <- amax < minage
  yvars <- yvars[!exclage]
  amin <- amin[!exclage]
  amax <- amax[!exclage]
  
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
    aamin <- amin[cumgroups == a][1]
    aamax <- tail(amax[cumgroups == a], 1)
    
    ## If aamax < 85 (max age cut point), compute mean age weighted by deathrate
    if (aamax < 85){
      # Select concerned range
      whichgrps <- ageseq >= as.numeric(aamin) & ageseq <= as.numeric(aamax)
      ndeaths <- subset(agedeaths, metadata$mcc_code == cities[i, "city"],
        whichgrps)
      
      # Mean age group weighted by deaths
      meanage <- weighted.mean(ageseq[whichgrps], ndeaths)
    } else { ## Otherwise use life expectancy
      meanage <- metadata[match(cities[i, "city"], metadata$mcc_code), 
        sprintf("lifexp_%s", aamin)] + as.numeric(aamin)
    }
    
    # Run model excluding August 2003
    # To keep August 2003 in the analysis, just remove the subset argument
    res <- try(glm(y ~ cb + dow + ns(date, df = 7 * length(unique(year))),
      dat, family = quasipoisson, subset = !(month == 8 & year == 2003)))
    
    # Some models might fail to coverge (pseudo-weights of IRLS become Inf)
    # Exit current iteration with NAs and a flag for non-convergence
    if (inherits(res, "try-error") || !res$converged){
      nred <- attr(cb, "df")[1]
      ageres[[a]] <- data.frame(t(rep(NA, nred)), t(rep(NA, sum(1:nred))), 
        totdeath = sum(y, na.rm = T), conv = F, ageval = meanage ,
        agegr = paste0(aamin, aamax))
      next
    }
    
    # Reduction to overall cumulative
    redall <- crossreduce(cb, res, cen = median(dat$era5landtmean))
    
    # Output
    trimat <- row(vcov(redall)) >= col(vcov(redall))
    vvec <- vcov(redall)[trimat]
    names(vvec) <- sprintf("v%i%i", row(vcov(redall))[trimat], 
      col(vcov(redall))[trimat])
    ageres[[a]] <- data.frame(t(coef(redall)), t(vvec), 
      totdeath = sum(y, na.rm = T), conv = res$converged, 
      ageval = meanage, agegr = paste0(aamin, aamax))
  }
  
  # Output
  cbind(city = cities[i,"city"], do.call(rbind, ageres))
}

# Stop parallel
stopCluster(cl)

#----- Export first-stage results
write.csv(stage1res, file = "data/stage1res.csv", row.names = F)
