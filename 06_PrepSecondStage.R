################################################################################
#
#                         MCC-EUcityTRM
#
#         Prepare objects for second stage & prediction
#
################################################################################

#---------------------------
# Load data
#---------------------------

if (length(ls()) == 0){
  # Source all analysis parameters
  source("00_Packages_Parameters.R") 
  
  # Read data
  metadata <- read.csv(gzfile("data/metadata.csv.gz"))
  stage1res <- read.csv(gzfile("data/stage1res.csv.gz"))
  
  # Some transformation as factors
  metadata$region <- factor(metadata$region, levels = regord)
  eurcntr <- as.data.frame(rbind(eu_countries, efta_countries, 
    data.frame(code = "UK", name = "United Kingdom", label = "United Kingdom")))
  metadata$cntr_name <- factor(eurcntr[match(metadata$CNTR_CODE, eurcntr[,1]),2],
    level = sort(eurcntr[,2]))
  
  # Read era5series /!\ a bit longer
  era5path <- "data/era5series"
  filelist <- list.files(era5path)
  era5series <- lapply(sprintf("%s/%s", era5path, filelist), function(f) 
    read.csv(f) |> mutate(date = as.Date(date)))
  names(era5series) <- sapply(strsplit(filelist, ".", fixed = T), "[", 1)
}

#---------------------------
#  Prepare stage 2 dataset
#---------------------------

# Exclude models that did not converge
stage1res <- subset(stage1res, conv)

# Get coefs and average age
coefs <- dplyr::select(stage1res, matches("b[[:digit:]]")) |> data.matrix()
agevals <- stage1res$ageval

# Get vcov matrices: lower triangle stored
vcovs <- dplyr::select(stage1res, matches("v[[:digit:]][[:digit:]]")) |> 
  # Create matrix from lower triangle for each line
  apply(1, function(x) {
    nred <- as.integer(substr(tail(names(x), 1), 2, 2))
    m <- matrix(NA, nred, nred)
    m[lower.tri(m, diag = T)] <- unlist(x)
    m[upper.tri(m)] <- t(m)[upper.tri(m)]
    m
  }, simplify = F)

# Match mcc cities to each obs of meta regression
repmcc <- match(stage1res$city, metadata$mcc_code)

# Set contrast for region
contrasts(metadata$region) <- "contr.helmert"

# Create stage 2 data.frame
stage2df <- data.frame(region = metadata$region[repmcc], age = agevals, 
  city = metadata[repmcc, "URAU_CODE"], 
  country = as.factor(metadata[repmcc, "CNTR_CODE"]))

# Store model dimensions
nc <- ncol(coefs) # Number of first-stage coefficients
nm <- length(metapreds) # Number of metapredictors

#---------------------------
# Prepare predictions
#---------------------------

#----- Common temperature for prediction

# Estimate an overall empirical distribution of temperature
tmeandist <- t(sapply(era5series, 
  function(x) quantile(x$era5landtmean, predper / 100)))
ovper <- colMeans(tmeandist)

# Create basis for overall relationship
ovknots <- ovper[sprintf("%i.0%%",varper)]
ov_basis <- onebasis(ovper, fun = varfun, degree = vardegree, knots = ovknots)

# Acceptable MMP values 
inrange <- predper >= mmprange[1] & predper <= mmprange[2]

#---------------------------
# Prepare plotting
#---------------------------

#----- For reporting ERFs

# Axis locations for plots
ovaxis <- ovper[predper %in% axisper]

#----- For maps
# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Geographical domain of cities considered
bnlon <- range(metadata$lon)
bnlat <- range(metadata$lat)
