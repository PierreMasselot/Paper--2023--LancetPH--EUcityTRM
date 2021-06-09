################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix: Comparison of AN calculations
#
################################################################################

library(MASS)
library(ggplot2)

#### Execute 05_Results.R up to line 136

#--------------------------
# Simulation preparation
#--------------------------

#----- Construct deaths for each city / age group

# Get age group death data
citydeaths <- metadata[,grep("death_[[:digit:]]{4}", names(metadata))]

# Sum by group
agegrpind <- cut(as.numeric(substr(names(citydeaths), 7, 8)), 
  c(0, agebreaks, 100), right = F)
cityagedeaths <- tapply(as.list(citydeaths), agegrpind, 
  function(x) rowSums(do.call(cbind, x)))
deathlist <- unlist(cityagedeaths)

#----- Prepare simulation

set.seed(12345)

# Simulate metacoefficients from multivariate normal distribution
metacoefsim <- mvrnorm(nsim, coef(stage2res), vcov(stage2res))

# Recreate model matrix with the city / age grid
cityageXdes <- model.matrix(delete.response(terms(stage2res)), allpreddf)

#----- Prepare parallelisation
ncores <- detectCores()
cl <- makeCluster(max(1, ncores - 2))
registerDoParallel(cl)

#--------------------------
# By keeping MMT constant
#--------------------------

mmtcons <- foreach(i = seq_len(nca), .packages = c("dlnm")) %dopar% {
  
  # Get object for this city age
  era5 <- era5series[[cityagegrid[i,1]]]$era5landtmean
  cityagecoefs <- citycoefs[[i]]
  cityagemmt <- cityres$mmt[i] 
  
  # Basis value for each day
  bvar <- onebasis(era5, fun = "bs", degree = 2, 
    knots = quantile(era5, c(10, 75, 90) / 100))
  cenvec <- onebasis(cityagemmt, fun = "bs", degree = 2, 
    knots = quantile(era5, c(10, 75, 90) / 100))
  bvarcen <- scale(bvar, center = cenvec, scale = F)
  
  # Compute daily AF and AN (naively)
  afday <- (1 - exp(-bvarcen %*% cityagecoefs$fit))
  anday <- afday * deathlist[i] / 365.25
  
  # Indicator of heat days
  heatind <- era5 >= cityagemmt
  
  # Sum all
  anlist <- c(total = sum(anday), cold = sum(anday[!heatind]),
    heat = sum(anday[heatind]))
  
  # Simulations for CI
  coefsim <- metacoefsim %*% (cityageXdes[i,] %x% diag(ncol(coefs)))
  andaysim <- (1 - exp(-bvarcen %*% t(coefsim))) * deathlist[i] / 365.25
  ansimlist <- cbind(total = colSums(andaysim), 
    cold = colSums(andaysim[!heatind,]), 
    heat = colSums(andaysim[heatind,]))
  ansimCI <- apply(ansimlist, 2, quantile, c(.025, .975))
  
  # Output
  rbind(anlist, ansimCI)
}

#--------------------------
# By recomputing MMT each time
#--------------------------

mmtvar <- foreach(i = seq_len(nca), .packages = c("dlnm")) %dopar% {
  
  # Get object for this city age
  era5 <- era5series[[cityagegrid[i,1]]]$era5landtmean
  cityagecoefs <- citycoefs[[i]]
  cityagemmt <- cityres$mmt[i] 
  
  # Basis value for each day
  bvar <- onebasis(era5, fun = "bs", degree = 2, 
    knots = quantile(era5, c(10, 75, 90) / 100))
  cenvec <- onebasis(cityagemmt, fun = "bs", degree = 2, 
    knots = quantile(era5, c(10, 75, 90) / 100))
  bvarcen <- scale(bvar, center = cenvec, scale = F)
  
  # Compute daily AF and AN (naively)
  afday <- (1 - exp(-bvarcen %*% cityagecoefs$fit))
  anday <- afday * deathlist[i] / 365.25
  
  # Indicator of heat days
  heatind <- era5 >= cityagemmt
  
  # Sum all
  anlist <- c(total = sum(anday), cold = sum(anday[!heatind]),
    heat = sum(anday[heatind]))
  
  # Simulations for CI
  coefsim <- metacoefsim %*% (cityageXdes[i,] %x% diag(ncol(coefs)))
  bvarerf <- onebasis(quantile(era5, predper / 100), fun = "bs", degree = 2, 
    knots = quantile(era5, c(10, 75, 90) / 100))
  erfsim <- bvarerf %*% t(coefsim)
  mmtsim <- quantile(era5, predper / 100)[inrange][
    apply(erfsim[inrange,], 2, which.min)]
  cenvecsim <- onebasis(mmtsim, fun = "bs", degree = 2, 
    knots = quantile(era5, c(10, 75, 90) / 100))
  bvarcensim <- lapply(as.data.frame(t(cenvecsim)), 
    function(cen) -scale(bvar, center = cen, scale = F))
  andaysim <- (1 - exp(mapply("%*%", bvarcensim, as.data.frame(t(coefsim))))) * 
    deathlist[i] / 365.25
  ansimlist <- cbind(total = colSums(andaysim), 
    cold = mapply(function(an, mmt) sum(an[era5 < mmt]), 
      as.data.frame(andaysim), mmtsim), 
    heat = mapply(function(an, mmt) sum(an[era5 >= mmt]), 
      as.data.frame(andaysim), mmtsim))
  ansimCI <- apply(ansimlist, 2, quantile, c(.025, .975))
  
  # Output
  rbind(anlist, ansimCI)
}

#--------------------------
# Plot
#--------------------------

# Extract ANs to put in a df
totalAN <- sapply(mmtcons, "[", 1, 1)
CIcons <- sapply(mmtcons, "[", 2:3, 1)
CIvar <- sapply(mmtvar, "[", 2:3, 1)

ANdf <- data.frame(age = allpreddf$age, 
  city = metadata$URAU_CODE[cityagegrid[,1]], 
  est = totalAN, cons = t(CIcons), var = t(CIvar))

# plot
ggplot(ANdf, aes(x = city)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_errorbar(
    aes(ymin = cons.2.5., ymax = cons.97.5.),
    width = 0.2, size = 1, col = 2
  ) + 
  geom_errorbar(
    aes(ymin = var.2.5., ymax = var.97.5.),
    width = 0.2, size = 1, col = 4
  ) +
  # geom_point(aes(y = est)) + 
  facet_wrap(~ age)