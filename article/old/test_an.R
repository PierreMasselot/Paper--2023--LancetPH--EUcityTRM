source("06_ResultsPrep.R")

#----- MCC results

# Extract BLUPs
mccblup <- blup(stage2res, vcov = T)

# Ages
mccages <- sapply(strsplit(names(mccblup), "\\."), "[", 3)
mccamin <- substr(mccages, 1, 2)
mccamax <- substr(mccages, 3, 4)

#----- Construct deaths for each city / age group

# Extract population structure variables from metadata
propvars <- sort(grep("prop_[[:digit:]]{4}", names(metadata), value = T))

# Extract min, and max of age group
agemin <- substr(propvars, 6, 7)
agemax <- substr(propvars, 8, 9)

# Repeat values for each age
ageseqs <- Map(seq, agemin, agemax, by = 1)
ageprop <- mapply(function(d, a){ 
  do.call(cbind, rep(list(d), length(a))) / length(a)
}, metadata[,propvars], ageseqs)
ageprop <- do.call(cbind, ageprop)

# Compute death rates
mccagedeaths <- mapply(function(min, max, i){
  ad <- agedeaths[i,ageseq >= as.numeric(min) & ageseq <= as.numeric(max)]
  ap <- ageprop[i,ageseq >= as.numeric(min) & ageseq <= as.numeric(max)]
  sum(ad * ap) / sum(ap)
}, mccamin, mccamax, repmcc)

# Compute population
popgrps <- mapply(function(min, max, i){
  sum(ageprop[i,ageseq >= as.numeric(min) & ageseq <= as.numeric(max)])
}, mccamin, mccamax, repmcc)
mccpops <- popgrps * metadata[repmcc, "pop"] / 100

# Compute total deaths
mccdeaths <- mccagedeaths * mccpops

#----- Compute annual AN / AF
attrlist <- vector("list", length(mccblup))
for (i in seq_along(mccblup)){
    
    # Get series
    era5 <- era5series[[repmcc[i]]]$era5landtmean
    icoefs <- mccblup[[i]]
    dat <- dlist[[metadata[repmcc[i], "mcc_code"]]]
    
    # Compute mmt
    tmeanper <- quantile(era5, predper / 100)
    bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
      knots = quantile(era5, varper / 100))
    firstpred <- bvar %*% icoefs$blup
    immt <- tmeanper[inrange][which.min(firstpred[inrange])]
    
    # Compute centered basis
    bvar2 <- onebasis(era5, fun = varfun, degree = vardegree, 
      knots = quantile(era5, varper / 100))
    cenvec <- onebasis(immt, fun = varfun, degree = vardegree, 
      knots = quantile(era5, varper / 100), Boundary.knots = range(era5))
    bvarcen <- scale(bvar2, center = cenvec, scale = F)
    
    # Compute daily AF
    afday <- (1 - exp(-bvarcen %*% icoefs$blup))
    
    # Compute MCC response
    outtype <- ifelse(any(grepl("all", names(dat))), "all", "nonext")
    yvars <- grep(sprintf("%s_", outtype), names(dat), value = T)
    if (length(yvars) > 0) {
      yvarsages <- sapply(strsplit(yvars, "_"), "[", 2)
      selage <- as.numeric(substr(yvarsages, 1, 2)) >= as.numeric(mccamin[i]) & 
        as.numeric(substr(yvarsages, 3, 4)) <= as.numeric(mccamax[i]) 
      deaths <- rowSums(dat[,yvars[selage], drop = F])
    } else {
      deaths <- dat[,outtype]
    }
    
    # Store results
    anmat <- matrix(NA, nrow = 3, ncol = 2, 
      dimnames = list(c("series", "mean death", "eurostat death rate"), 
        c("cold", "heat")))
    
    # Compute AN using MCC series
    selaf <- match(dat$date, era5series[[repmcc[i]]]$date)
    heatind <- (era5 >= immt)[selaf]
    anday_series <- afday[selaf] * deaths
    anmat[1,1] <- mean(anday_series[!heatind], na.rm = T) * 365.25
    anmat[1,2] <- mean(anday_series[heatind], na.rm = T) * 365.25
    
    # Compute AN using average # of deaths
    anday_mean <- afday[selaf] * mean(deaths, na.rm = T)
    anmat[2,1] <- mean(anday_mean[!heatind]) * 365.25
    anmat[2,2] <- mean(anday_mean[heatind]) * 365.25
    
    # Compute AN using death rates
    anday_dr <- afday[selaf] * mccdeaths[i]
    anmat[3,1] <- mean(anday_dr[!heatind])
    anmat[3,2] <- mean(anday_dr[heatind])
    
    # Output
    attrlist[[i]] <- anmat
}

pdf("old/ANcomparison.pdf")

oldpar <- par()

# Plot everything
par(mfrow = c(2,2))
for (i in 1:2){ for (j in 1:2){
  x <- sapply(attrlist, "[", 1, j)
  y <- sapply(attrlist, "[", i + 1, j)
  plot(x, y, xlab = "Annual AN (death series)", 
    ylab = sprintf("Annual AN (%s)", rownames(attrlist[[1]])[i + 1]),
    main = paste(rownames(attrlist[[1]])[i + 1], colnames(attrlist[[1]])[j]),
    log = "xy", col = ifelse(j == 1, 4, 2))
  abline(a = 0, b = 1)
}}

# Compute biases
devs <- sapply(attrlist, function(x) (x[-1,] - x[c(1,1),]) / x[c(1,1),],
  simplify = "array")
bias <- apply(devs, 1:2, mean)

do.call(par, oldpar)
barplot(t(bias), beside = T, col = c(4, 2), ylab = "Estimated relative bias")

dev.off()

# dev.print(pdf, "old/ANcomparison.pdf")