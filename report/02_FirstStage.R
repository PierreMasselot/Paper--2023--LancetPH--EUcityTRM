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
#     - outcomes as specified in the parameters below (vector 'outcome vars')
#
# OUPUT:
#   A nested list stage1res the same length as dlist containing:
#     - tsum: summary of temperature for the city
#     - dsum: summary of each outcome
#     - period: period of the data (excluding missings)
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
outcome_vars <- c("all_main", "cvd_main", "resp_main", "cvresp_main", 
  "all_65p", "cvd_65p", "resp_65p", "cvresp_65p", 
  "all_75p", "cvd_75p", "resp_75p", "cvresp_75p")

# Exposure-response parameters
varfun <- "bs"
varper <- c(10,75,90)
vardegree <- 2

# Lag-response parameters
# Change here for sensitivity analysis
lagfun <- "ns"
# maxlag <- 10
# lagknots <- logknots(maxlag, 2)
# Suggestions for sensitivity:
maxlag <- 3
lagknots <- 1
#
# maxlag <- 21
# lagknots <- logknots(21, 3)

# Seasonality / trend degrees of freedom
nkseas <- 7

#---------------------------
# Order dlist according to cities
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
  
  # Mortality summary
  dsumlist <- summary(dat[,outcome_vars])
  out$dsumlist <- dsumlist
  
  # Period
  out$period <- range(na.omit(dat)$year)
  
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
    
    # Store useful elements
    out[[outcome]]$conv <- res$converged
    out[[outcome]]$nobs <- length(res$model$y)
    
    # Reduction to overall cumulative
    red <- crossreduce(cb, res, cen = 15)
    
    # Store reduced coefs
    out[[outcome]]$coef <- coef(red)
    out[[outcome]]$vcov <- vcov(red)
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
save(cities, stage1res, 
  file = sprintf("results/FirstStage_UK_lag%i.RData", maxlag))
