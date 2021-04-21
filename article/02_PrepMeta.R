################################################################################
#
#                         MCC-CityEurope
#
#                     Eurostat Data preparation
#
################################################################################

library(readxl)
library(mice)
library(sf)
library(modeest)

#---------------------------
#  Parameters
#---------------------------

# Years selected. Averaged if several
year <- as.character(2005:2015)

# The priority of urban audit geographical levels
level_priority <- c("CITY", "GREATERCITY", "FUA")

# Projecttion for geo objects
geoproj <- "4326"

# Paths
path_urau <- "V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)"
path_ucd <- "V:/VolumeQ/AGteam/Urban Centre Database"
path_nuts <- "V:/VolumeQ/AGteam/Eurostat/Regional by NUTS classification (reg)"

#---------------------------
#  Link datasets
#---------------------------

#----- Load all lookup table 

# Urban audit and UCD (contains the list of Urban audit)
urau_cities <- read.table(sprintf("%s/lookup/URAU_UCD.csv", path_urau),
  sep = ",", header = T, quote = "\"")

# Lookup table between MCC and URAU
urau_mcc <- read.table(sprintf("%s/lookup/MCC_URAU.csv", path_urau),
  header = T, sep = ";")

# Lookup table between URAU and NUTS
urau_nuts <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Urban Audit (urb_cgc)/lookup/URAU_NUTS2020.csv"))

#----- Merge everything together

metadata <- Reduce(
  function(x, y) merge(x, y, by = "URAU_CODE", all.x = T, all.y = F),
  list(urau_cities,
    urau_mcc[,c("URAU_CODE", "mcc_code", "cityname")],
    urau_nuts[, c("URAU_CODE", "URAU_CATG", "NUTS3_2016", "NUTS3_2021")])
)
  
#----- Select level

# Discard functional urban areas because they are huge
metadata <- metadata[metadata$URAU_CATG != "F",]

# Reject MCC matches that are not in selected MCC countries
keepmcc <- with(metadata, is.na(mcc_code) | 
    sapply(strsplit(mcc_code, "\\."), "[", 2) %in% droplevels(countries$country))
metadata <- metadata[keepmcc,]

# Select either city or greater city if the former is unavailable
cityselect <- by(metadata, substr(metadata$URAU_CODE, 1, 5), function(x){
  csel <- x$URAU_CATG == "C"
  if(sum(csel) > 0){
    x <- x[csel,]
  } 
  x
})
metadata <- do.call(rbind, cityselect)

# Remove potential remaining duplicates
metadata <- metadata[!duplicated(metadata),]

#----- Manage specific cases

# Remove London boroughs
metadata <- metadata[!(substr(metadata$NUTS3_2021,1,3) == "UKI" & 
    metadata$URAU_NAME != "London"),]
metadata[metadata$URAU_NAME == "London","NUTS3_2021"] <- "UKI"

# Remove overseas cities (Reykjavik !?)
overseas <- c("PT004C1", "ES025C1", "ES524C1", "ES550C1", "ES029C1", "ES008C1", 
  "ES074C1", "ES072C1", "IS001C1", "FR030C1", "FR520C1", "FR521C1", 
  "PT007C1", "FR028C1")
metadata <- metadata[!metadata$URAU_CODE %in% overseas,]

#----- Add other NUTS level codes for easier merging
metadata$NUTS2_2021 <- substr(metadata$NUTS3_2021, 1, 4)
metadata$NUTS1_2021 <- substr(metadata$NUTS3_2021, 1, 3)

# Do it also for the 2016 version
metadata$NUTS2_2016 <- substr(metadata$NUTS3_2016, 1, 4)
metadata$NUTS1_2016 <- substr(metadata$NUTS3_2016, 1, 3)

#----- Order dataset
metadata <- metadata[order(metadata$URAU_CODE),]

#----- Prepare descriptive vector
metasource <- c()

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------
# No variable from urban audit will be used as more data is available in NUTS

# datasets <- c("popstr", "env")
# 
# #----- Load the necessary datasets
# 
# urb_dat <- lapply(datasets, function(dat){
#   # Load cities and greater cities dataset
#   read <- read.table(sprintf("%s/data/urb_c%s.tsv", path_urau, dat), 
#     header = T, sep = "\t", na.strings = ": ", check.names = F)
#   
#   # Separate city and variables in two columns
#   spl_city_var <- strsplit(read[[1]], ",")
#   read$var <- sapply(spl_city_var, "[", 1)
#   read$citycode <- sapply(spl_city_var, "[", 2)
#   read <- read[,-1]
#   
#   # Convert data into numeric
#   yearspl <- apply(read[,as.character(year)], 2, function(x){
#     sapply(strsplit(x, " "), "[", 1)
#   })
#   read[,as.character(year)] <- as.numeric(yearspl)
#   
#   # Mean of selected years
#   read$value <- rowMeans(read[,as.character(year)], na.rm = T)
#   
#   # Reshape data into wide
#   out <- reshape(read[,c("var", "citycode", "value")], timevar = "var", 
#     idvar = "citycode", ids = "value", direction = "wide")
#   names(out) <- gsub("^value\\.", "", names(out))
#   
#   out
# })
# names(urb_dat) <- datasets
# 
# #----- Select variables used in the analysis
# 
# # cpopstr: Sum prop of pop 65-74 and 74 above 
# urb_dat$popstr$prop65_URAU <- rowSums(urb_dat$popstr[,c("DE1028I", "DE1055I")])
# urb_dat$popstr <- urb_dat$popstr[,c("citycode", "prop65_URAU")]
# 
# # cenv : built-up area
# urb_dat$env$builtup_URAU <- rowSums(urb_dat$env[,c("EN5200V", "EN5201V",
#   "EN5202V", "EN5203V", "EN5204V")])
# 
# # cenv: climate and pollution
# urb_dat$env <- urb_dat$env[,c("citycode", "builtup_URAU", "EN1003V", "EN1004V", 
#   "EN1002V", "EN1005V", "EN2025V", "EN2026V", "EN2027V")]
# names(urb_dat$env)[-(1:2)] <- c("tmean_warm_URAU", "tmean_cold_URAU", 
#   "sunshine_URAU", "rainfall_URAU", "acc_o3_URAU", "meanno2_URAU", 
#   "meanpm10_URAU")
# 
# 
# # Merge the different urban audit datasets together
# urb_df <- Reduce(merge, urb_dat)
# 
# #----- Merge to the list of cities 
# 
# metadata <- merge(metadata, urb_df, by.x = "URAU_CODE", by.y = "citycode",
#   all.y = F, all.x = T, sort = F)
  
#---------------------------
#  Load Eurostat's regional data
#---------------------------

#----- List loaded variables
# For each variable, contains:
#   level: the NUTS level
#   file: the file in which the variable is found
#   desc: A description of the variable
#   select: the criteria of selection inside the file
#   (optional) fun: to aggregate several categories
load_desc <- list(
  prop65 = list(
    level = 3,
    file = "demo_r_pjanind3",
    desc = "Proportion of population aged 65 and older",
    select = c(indic_de = "PC_Y65_MAX", unit = "PC")
  ),
  lifexp = list(
    level = 2,
    file = "demo_r_mlifexp",
    desc = "Life expectancy at birth",
    select = c(sex = "T", age = "Y_LT1")
  ),
  gdp = list(
    level = 3,
    file = "nama_10r_3gdp",
    desc = "GDP per capita",
    select = c(unit = "EUR_HAB")
  ),
  educ = list(
    level = 2,
    file = "edat_lfse_04",
    desc = paste0("Proportion of active population (25-64)", 
      " with ISCED level 5 and above (tertiary)"),
    select = c(isced11 = "ED5-8", sex = "T", age = "Y25-64")
  ),
  unempl = list(
    level = 2,
    file = "lfst_r_lfu3rt",
    desc = "Unemployment rate in active population (20-64)",
    select = c(sex = "T", age = "Y20-64")
  ),
  depriv = list(
    level = 2,
    file = "ilc_mddd21",
    desc = "Severe material deprivation rate",
    select = c(unit = "PC")
  ),
  bedrates = list(
    level = 2,
    file = "hlth_rs_bdsrg",
    desc = "Total number of hospital beds / 100 000 hab",
    select = c(unit = "P_HTHAB", facility = "HBEDT")
  ),
  pop = list(
    level = 3, 
    file = "demo_r_pjangrp3",
    desc = "Total population",
    select = c(sex = "T", age = "TOTAL")
  ),
  popdens = list(
    level = 3,
    file = "demo_r_d3dens",
    desc = "Population density (person / km2)",
    select = c(unit = "PER_KM2")
  ),
  urbshare = list(
    level = 2,
    file = "lan_lcv_art",
    desc = "Share of land covered by artificial surface",
    select = c(unit = "PC", landcover = "LCA")
  ),
  greenshare = list(
    level = 2,
    file = "lan_lcv_ovw",
    desc = paste0("Share of land covered by green areas: cropland,",
      "woodland, shrubland or grassland"),
    select = list(unit = "PC", landcover = c("LCB", "LCC", "LCD", "LCE")),
    fun = "sum"
  ),
  blueshare = list(
    level = 2,
    file = "lan_lcv_ovw",
    desc = "Share of land covered by water",
    select = c(unit = "PC", landcover = "LCG")
  ),
  cooldegdays = list(
    level = 3,
    file = "nrg_chddr2_a",
    desc = "Cooling degree days (above 24)",
    select = c(unit = "NR", indic_nrg = "CDD")
  ),
  heatdegdays = list(
    level = 3,
    file = "nrg_chddr2_a",
    desc = "Heating degree days (below 15)",
    select = c(unit = "NR", indic_nrg = "HDD")
  )
)

#----- Load variables according to the list
nuts_vars <- lapply(load_desc, function(x){
  # Read data
  read <- read.table(sprintf("%s/data/%s.tsv", path_nuts, x$file), header = T,
    sep = "\t", na.strings = c(": ", ":"), check.names = F)
  
  # Select years
  avail_year <- which(names(read) %in% year)
  read <- read[,c(1,avail_year)] 
  
  # Coerce values into numeric (if necessary)
  yearspl <- apply(read[,-1, drop = F], 2, function(y){
    sapply(strsplit(as.character(y), " "), "[", 1)
  })
  read[,-1] <- as.numeric(yearspl)
  
  # If necessary average several years
  read$value <- rowMeans(read[,-1, drop = F], na.rm = T)
  
  # Split first column
  colspl <- do.call("rbind", strsplit(read[[1]], ","))
  colnames(colspl) <- strsplit(strsplit(names(read)[1], "\\\\")[[1]][1],
    ",")[[1]]
  read <- cbind(read, colspl)
  
  # Select variable
  conds <- Map(function(var, val) read[,var] %in% val, 
    names(x$select), x$select)
  cond <- Reduce("&", conds)
  read_sel <- read[cond,]
  
  # Eventually aggregate if several vaiables have to
  if (!is.null(x$fun)){
    res <- aggregate(value ~ geo, data = read_sel, x$fun)
  } else {
    res <- read_sel[,c("geo", "value")]
  }
  
  # Output result
  attr(res, "level") <- x$level
  res
})

#----- Merge everything together

# Merge NUTS variables with MCC
metadata <- Reduce(function(x, y){
    merge(x, y, 
      by.x = sprintf("NUTS%i_2021", attr(y, "level")), by.y = "geo",
      sort = F, all.x = T, all.y = F)
  }, 
  nuts_vars, init = metadata
)

# Add names
names(metadata)[ncol(metadata) - length(load_desc):1 + 1] <- names(load_desc)

# For NAs, search for a potential NUTS 2016 match
for (nm in names(load_desc)){
  nas <- is.na(metadata[,nm])
  nv <- nuts_vars[[nm]]
  metadata[nas,nm] <- nv[
    match(metadata[nas, sprintf("NUTS%i_2016", attr(nv, "level"))], nv$geo),
    "value"]
}

# Add source of variable
metasource[names(load_desc)] <- sprintf("NUTS%i", 
  sapply(load_desc, "[[", "level"))

#----- Add social isolation from 2011 census
read <- read.table(sprintf("%s/data/cens_11ms_r3.tsv", path_nuts), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Reshape
res_read <- reshape(read, direction = "long", v.names = "value",
  varying = list(region = 2:ncol(read)), times = names(read)[-1], 
  timevar = "region", idvar = 1, ids = read[,1])

# Coerce value variable into numeric
valspl <- apply(res_read[,3, drop = F], 2, function(x){
  sapply(strsplit(as.character(x), " "), "[", 1)
})
res_read[,3] <- as.numeric(sapply(strsplit(res_read[,3], " "), "[", 1))

# Split first variable
colspl <- do.call("rbind", strsplit(res_read[[1]], ","))
colnames(colspl) <- c("age", "sex", "marsta", "unit", "time")
prep_read <- cbind(res_read[,-1], colspl)

# Select total sex and age
select <- prep_read[prep_read$age == "TOTAL" & prep_read$sex == "T", 
  -c(3, 4, 6, 7)]

# Compute proportions of each marital status
#   and select single, divorced and widowed
select_prop <- by(select[,c("marsta", "value")], select$region, simplify = F, 
  function(x) {
    x$value <- x$value / x$value[x$marsta == "TOTAL"]
    sum(x$value[x$marsta %in% c("DIV", "SIN", "WID")])
})
select_prop <- data.frame(region = names(select_prop), 
  isol = unlist(select_prop))

# Merge with dataset
metadata <- merge(metadata, select_prop, by.x = "NUTS3_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

metasource["isol"] <- "NUTS3"

#----- Add regional typology

# Load typology
read_typo <- read.table(sprintf("%s/metadata/NUTS_AT_2021.csv", path_nuts), 
  header = T, sep = ",", quote = "\"", na.strings = "0")
type_vars <- grep("TYPE", names(read_typo), value = T)

# Fill NUTS1 and NUTS2 levels with modal value (useful for London)
# Compute modes
nuts2_modes <- aggregate(read_typo[,type_vars], 
  by = list(nuts2 = substr(read_typo[,"NUTS_ID"], 1, 4)), 
  function(x) mfv(x, na_rm = T)[1])
nuts1_modes <- aggregate(read_typo[,type_vars], 
  by = list(nuts1 = substr(read_typo[,"NUTS_ID"], 1, 3)), 
  function(x) mfv(x, na_rm = T)[1])
# Fill
nas <- apply(is.na(read_typo[,type_vars]), 1, any)
nuts1inds <- nchar(read_typo[,"NUTS_ID"]) == 3
nuts2inds <- nchar(read_typo[,"NUTS_ID"]) == 4
read_typo[nas & nuts1inds, type_vars] <- nuts1_modes[
  match(read_typo[nas & nuts1inds, "NUTS_ID"], nuts1_modes$nuts1), -1]
read_typo[nas & nuts2inds, type_vars] <- nuts2_modes[
  match(read_typo[nas & nuts2inds, "NUTS_ID"], nuts2_modes$nuts2), -1]

# Merge with dataset
metadata <- merge(metadata, read_typo[, c("NUTS_ID", type_vars)],
  by.x = "NUTS3_2021", by.y = "NUTS_ID", all.x = T, all.y = F, sort = F)

metasource[type_vars] <- "NUTS3"

#---------------------------
# Add Urban Centre Database
#---------------------------

# Read database
read_ucd <- read.table(
  sprintf("%s/original/V1-2/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv", 
    path_ucd),
  sep = ",", header = T, quote = "\"")

# Select variables
var_sel <- c(tmean = "E_WR_T_14", greenness = "E_GR_AV14",
  pm25 = "E_CPM2_T14")
read_ucd <- read_ucd[,c("ID_HDC_G0", var_sel)]
names(read_ucd)[-1] <- names(var_sel)

# Merge with metadata
metadata <- merge(metadata, read_ucd, all.x = T, all.y = F, sort = F)

# Add info on the source
metasource[names(var_sel)] <- "UCD"

#---------------------------
# Missing values imputation
#---------------------------

# Keep only metavariables
vars <- names(metasource)
metavar <- metadata[,vars]

# Keep track of missings
imputed <- is.na(metavar)
nmis <- apply(imputed, 2, sum)
propmis <- apply(imputed, 2, mean)

#----- Impute

# Impute using cart because of complex interactions
meta_imp <- mice(metavar, method = "cart", seed = 12345, print = F)

# Get the imputed dataset (of iter_Sel)
iter_sel <- 5
metavar <- complete(meta_imp, iter_sel)

#---------------------------
# Load geographical data
#---------------------------

#----- Load points for each city
# Unzip in temporary directory
unzip(zipfile = sprintf("%s/geography/URAU_LB_2020_%s.shp.zip", 
    path_urau, geoproj), 
  exdir = tempdir())

# load shapefile
urau_points <- st_read(sprintf("%s/URAU_LB_2020_%s.shp", tempdir(), geoproj))

# Remove temporary file
file.remove(sprintf("%s/%s", tempdir(), 
  list.files(tempdir())[grep("URAU_LB_2020", list.files(tempdir()))]))

#----- Select geometry of retained cities
metageo <- urau_points[match(metadata$URAU_CODE, urau_points$URAU_CODE),
  c("URAU_CODE", "geometry")]

#---------------------------
#  Save data
#---------------------------

#----- Reorganize data
# Description of metadata (meta^2 data)
metadesc <- metadata[,!names(metadata) %in% vars]

# Add indicator for whther it is in MCC
metadesc$inmcc <- !is.na(metadata$mcc_code)

# Description of metavariables
metavardesc <- data.frame(source = metasource, nmis = nmis, propmis = propmis)

# Reorder and select mcc time series
reord <- match(subset(metadesc, inmcc)$mcc_code, names(dlist))
dlist <- dlist[reord]
cities <- cities[reord,]

#----- Export
save(dlist, cities, countries, metavar, metadesc, imputed, metageo, metasource,
  metavardesc, meta_imp,
  file = "data/Alldata.RData")