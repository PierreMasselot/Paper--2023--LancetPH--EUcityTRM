################################################################################
#
#                       Exhaustion Level 1
#
#                       First-stage models
#
################################################################################

#######################################
# CODEBOOK

# INPUT:
#
#   cities: data.frame of cities with variables
#     - city: a city identifier
#     - cityname: the name of the city
#     - lon: longitude of city reference location
#     - lat: latitude of city reference location
#
#   dlist: a named list with one element per city. The names must correspond
#     to city identifiers (var 'city' in data.frame 'cities').
#     Each city must contain a data.frame with at least the following variables
#     - date: formatted as "yyyy-mm-dd" or "yyyy/mm/dd"
#     - tmean: daily mean temperature
#     - outcomes as specified in the parameters below
#
# OUPUT:
#   A nested list stage1res the same length as dlist containing:
#     - tsum: summary of temperature for the city
#     - one element per outcome that includes
#         - conv: indicates if the GLM fitting converged
#         - coef: coefficients for the overall association
#         - vcov: variance-covariance of coefs for overall association

#######################################


library(dlnm)
library(splines)
library(doParallel)

#---------------------------
#  Parameters
#---------------------------

# Outcome variables
# This can be changed to match your variable names (please put the total 1st)
outcome_vars <- c("all",
  "resp", "cvd")

# Exposure-response parameters
varfun <- "bs"
varper <- c(10,75,90)
vardegree <- 2

# Lag-response parameters
maxlag <- 10
lagfun <- "ns"
lagknots <- logknots(maxlag, 2)

# Seasonality / trend degrees of freedom
nkseas <- 7

#---- Different lags for sensitivity
sens_pars <- list(
  lag3 = list(varfun = varfun, vardegree = vardegree, varper = varper,
    maxlag = 3, lagfun = lagfun, lagknots = 1),
  lag21 = list(varfun = varfun, vardegree = vardegree, varper = varper,
    maxlag = 21, lagfun = lagfun, lagknots = logknots(21, 3))
)

#---------------------------
#  Order dlist according to cities
#---------------------------

dlist <- dlist[match(names(dlist), cities$city)]

#---------------------------
#  Prepare paralelization
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
  
  # Check that all variables are there
  varlist <- c(outcome_vars, "tmean", "date")
  if (any(!varlist %in% names(dat))){
    stop(sprintf("Missing variables (%s) in city %s",
      paste(setdiff(varlist, names(dat)), collapse = ", "),
      cities$cityname[i]))
  }
  
  # Coerce date and extract month, doy, year and dow
  dat$date <- as.Date(dat$date)
  dat$month <- as.numeric(format(dat$date, "%m"))
  dat$doy <- as.numeric(format(dat$date, "%j"))
  dat$year <- as.numeric(format(dat$date, "%Y"))
  dat$dow <- as.factor(weekdays(dat$date, abb = T))
  
  # Initialize output list
  out <- list()
  
  # Temperature summary
  tsum <- summary(dat$tmean)
  tsum["Range"] <- tsum["Max."] - tsum["Min."]
  out$tsum <- tsum
  
  # Define crossbasis
  cb <- crossbasis(dat$tmean, lag = maxlag, 
    argvar = list(fun = varfun, degree = vardegree, 
      knots = quantile(dat$tmean, varper / 100, na.rm = T)),
    arglag = list(fun = lagfun, knots = lagknots))
  
  #----- Run model for each outcome
  for (outcome in outcome_vars){
    # Extract outcome
    y <- dat[, outcome]
    
    # Run the model
    res <- glm(y ~ cb + dow + ns(date, df = nkseas * length(unique(year))), 
      dat, family = quasipoisson)
    
    # Store convergence
    out[[outcome]]$conv <- res$converged
    
    # Reduction to overall cumulative
    red <- crossreduce(cb, res, cen = 15)
    
    # Store reduced coefs
    out[[outcome]]$coef <- coef(red)
    out[[outcome]]$vcov <- vcov(red)
  }
  
  #----- Run sensitivity analysis
  for (j in seq_along(sens_pars)){
    # Redefine crossbasis
    cb <- with(sens_pars[[j]], crossbasis(dat$tmean, lag = maxlag, 
      argvar = list(fun = varfun, degree = vardegree, 
        knots = quantile(dat$tmean, varper / 100, na.rm = T)),
      arglag = list(fun = lagfun, knots = lagknots))
    )
    
    # Run the model and store results
    y <- dat[, outcome_vars[1]]
    res <- glm(y ~ cb + dow + ns(date, df = nkseas * length(unique(year))), 
      dat, family = quasipoisson)
    red <- crossreduce(cb, res, cen = 15)
    
    # Store results
    nam <- names(sens_pars)[j]
    out[[nam]]$conv <- res$converged
    out[[nam]]$coef <- coef(red)
    out[[nam]]$vcov <- vcov(red)
  }
  
  # Output
  return(out)
}

# Stop parallel
stopCluster(cl)

# Rename
names(stage1res) <- cities$city

# Export
# Please replace 'country' by your country
save(dlist, cities, stage1res, file = "FirstStage_country.RData")
