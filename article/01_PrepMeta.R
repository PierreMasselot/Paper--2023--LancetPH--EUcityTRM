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
library(eurostat)

#---------------------------
#  Parameters
#---------------------------

# Years selected. Averaged if several
year <- as.character(2005:2015)

# Projecttion for geo objects
geoproj <- "4326"

# Paths
path_urau <- "V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)"
path_ucd <- "V:/VolumeQ/AGteam/Urban Centre Database"
path_nuts <- "V:/VolumeQ/AGteam/Eurostat/Regional by NUTS classification (reg)"

# MCC country datasets
mcc_countries <- c('cze9415', 'est9718', 'fnl9411', 'fra0014',  
  'grc0110', 'irl8407', 'ita0110', 'nor6916', 'por8012', 
  'spa0913', 'sui9513', 'swe9016', 'uk9016',
  'ger9315', 'net9516c', 'rom9416') # Last line is MCC_all

#---------------------------
#  Link datasets
#---------------------------

#----- Load all lookup table 

# Urban audit and UCD (contains the list of Urban audit)
urau_cities <- read.table(sprintf("%s/lookup/URAU_UCD.csv", path_urau),
  sep = ",", header = T, quote = "\"")

# Lookup table between MCC and URAU
urau_mcc_age <- read.table(sprintf("%s/lookup/URAU_MCC_AgeCause_20200907.csv", 
    path_urau),
  header = T, sep = ";", quote = "\"")
urau_mcc_all <- read.table(sprintf("%s/lookup/URAU_MCCdata_20210407.csv", 
    path_urau),
  header = T, sep = ";", quote = "\"")

# Lookup table between URAU and NUTS
urau_nuts <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Urban Audit (urb_cgc)/lookup/URAU_NUTS2020.csv"))

#----- Merge everything together

# merge URAU and NUTS
metadata <- merge(urau_cities,
  urau_nuts[, c("URAU_CODE", "URAU_CATG", "NUTS3_2016", "NUTS3_2021", 
      "AREA_SQM")],
  by = "URAU_CODE", all.x = T, all.y = F)

# merge with MCC age dataset
metadata <- merge(metadata, 
  urau_mcc_age[,c("URAU_CODE", "mcc_code", "cityname")],
  by = "URAU_CODE", all.x = T, all.y = F)

# When there is no match with MCC age dataset, search in MCC all
matchall <- match(subset(metadata, is.na(mcc_code), URAU_CODE, drop = T),
  urau_mcc_all$URAU_CODE)
metadata[is.na(metadata$mcc_code), c("mcc_code", "cityname")] <- 
  urau_mcc_all[matchall, c("mcc_code", "cityname")]
  
#----- Select level

# Discard functional urban areas because they are huge
metadata <- metadata[metadata$URAU_CATG != "F",]

# Reject MCC matches that are not in selected MCC countries
metadata <- subset(metadata, is.na(mcc_code) | 
    (sapply(strsplit(mcc_code, "\\."), "[", 2) %in% mcc_countries))

# Select greater city or city if unavailable
cityselect <- by(metadata, substr(metadata$URAU_CODE, 1, 5), function(x){
  csel <- x$URAU_CATG == "K"
  if(sum(csel) > 0){
    x <- x[csel,]
  } 
  x
})
metadata <- do.call(rbind, cityselect)

# Remove potential remaining duplicates
metadata <- metadata[!duplicated(metadata),]

# Duplicates on the NUTS3 level: select the largest city
nnuts3 <- tapply(metadata$NUTS3_2021, metadata$NUTS3_2021, length)
nutsdup <- subset(metadata, NUTS3_2021 %in% names(nnuts3[nnuts3 > 1]))
toremove <- by(nutsdup, nutsdup$NUTS3_2021, function(x){ 
  ksel <- x$URAU_CATG == "K"
  sel <- if (sum(ksel) > 0) x[ksel,] else x
  sel <- sel[which.max(sel$AREA_SQM), "URAU_CODE"]
  x$URAU_CODE[x$URAU_CODE != sel]
})
metadata <- subset(metadata, !URAU_CODE %in% unlist(toremove))

#----- Manage specific cases

# Remove London boroughs
metadata <- metadata[!(substr(metadata$NUTS3_2021,1,3) == "UKI" & 
    metadata$URAU_NAME != "London"),]
metadata[metadata$URAU_NAME == "London","NUTS3_2021"] <- "UKI"

# Remove mismatch between large UK conurbation and smaller cities
#   Manchester/Wigan, Liverpool/Birkenhead
torm <- c(
  with(metadata, which(URAU_CODE == "UK008K1" & mcc_code == "wign.uk9016")),
  with(metadata, which(URAU_CODE == "UK006K2" & mcc_code == "brkn.uk9016"))
)
metadata <- metadata[-torm,]

# Remove overseas cities 
overseas <- c("PT004C1", "PT007C1", 
  "ES025K1", "ES524C1", "ES550K1", "ES029C1", "ES008C1", "ES074C1", "ES072C1",
  "ES055C1", "ES045C1", # Spanish cities in Morocco
  "IS001C1", # Reykjavik
  "FR030C1", "FR520C1", "FR521C1", "FR028C1", "FR522C1"
)
metadata <- metadata[!metadata$URAU_CODE %in% overseas,]

# Remove some duplicates due to older versions of city
older <- c("ES552", "ES550")
metadata <- metadata[!rownames(metadata) %in% older,]

#----- Add other NUTS level codes for easier merging
metadata$NUTS2_2021 <- substr(metadata$NUTS3_2021, 1, 4)
metadata$NUTS1_2021 <- substr(metadata$NUTS3_2021, 1, 3)

# Do it also for the 2016 version
metadata$NUTS2_2016 <- substr(metadata$NUTS3_2016, 1, 4)
metadata$NUTS1_2016 <- substr(metadata$NUTS3_2016, 1, 3)

#----- Order dataset
metadata <- metadata[order(metadata$URAU_CODE),]

# List of description variables
desc_vars <- names(metadata)

# Add indicator for whether it is in MCC
metadata$inmcc <- !is.na(metadata$mcc_code)

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------

# # List of datasets that should be downloaded
# datasets <- list(pop1 = c(pop_tot = "DE1001V", pop_0004 = "DE1040V", 
#   pop_0509 = "DE1074V", pop_1014 = "DE1077V", pop_1519 = "DE1046V", 
#   pop_2024 = "DE1049V", pop_2534 = "DE1058V", pop_3544 = "DE1061V", 
#   pop_4554 = "DE1064V", pop_5564 = "DE1025V", pop_6574 = "DE1028V", 
#   pop75p = "DE1055V"))
#  
# #----- Load the necessary datasets
# urb_dat <- lapply(sprintf("urb_c%s", names(datasets)), get_eurostat, 
#   time_format = "num")
# names(urb_dat) <- names(datasets)
# 
# # Keep only cities selected above
# urb_dat <- lapply(urb_dat, subset, cities %in% metadata$URAU_CODE)
# 
# # Keep only selected variables
# urb_dat <- Map(function(x, y) subset(x, indic_ur %in% y),
#   urb_dat, datasets)
# 
# # Keep only selected years and aggregate
# urb_dat <- lapply(urb_dat, subset, time %in% year)
# agg_dat <- lapply(urb_dat, function(x){
#   aggregate(values ~ indic_ur + cities, data = x, mean, na.rm = T)
# })
# 
# # Reshape as wide
# res_dat <- lapply(agg_dat, reshape, timevar = "indic_ur", idvar = "cities",
#   ids = "values", direction = "wide")
# 
# # Merge
# urb_df <- Reduce(function(x, y) merge(x, y, all = T, by = "cities"), 
#   res_dat)
# 
# # Rename
# namevec <- unlist(unname(datasets))
# names(urb_df) <- gsub("values\\.", "", names(urb_df))
# names(urb_df)[match(namevec, names(urb_df))] <- names(namevec)
# 
# #----- Create additional variables
# 
# # Prop or population 65+
# urb_df$prop65_URAU <- rowSums(urb_df[,c("pop_6574", "pop75p")]) / 
#   urb_df[,"pop_tot"]
# 
# #----- Merge to the list of cities
# 
# metadata <- merge(metadata, urb_df, by.x = "URAU_CODE", by.y = "cities",
#   all.x = T, all.y = F, sort = F)
  
#---------------------------
#  Load Eurostat's regional data
#---------------------------

#----- Population structure
indicde_list <- c("PC_Y0_4", "PC_Y5_9", "PC_Y10_14", "PC_Y15_19", 
  "PC_Y20_24", "PC_Y25_29", "PC_Y30_34", "PC_Y35_39", "PC_Y40_44", 
  "PC_Y45_49", "PC_Y50_54", "PC_Y55_59", "PC_Y60_64", "PC_Y65_69", 
  "PC_Y70_74", "PC_Y75_79", "PC_Y80_84", "PC_Y85_MAX", "PC_Y65_MAX")
varnames <- c("prop_0004", "prop_0509", "prop_1014", "prop_1519", 
  "prop_2024", "prop_2529", "prop_3034", "prop_3539", "prop_4044", 
  "prop_4549", "prop_5054", "prop_5559", "prop_6064", "prop_6569", 
  "prop_7074", "prop_7579", "prop_8084", "prop_8599", "prop_65p")

# Load variables from eurostat
popstr <- get_eurostat("demo_r_pjanind3", time_format = "num",
  filters = list(indic_de = indicde_list, time = year))

# Average years
popstr <- aggregate(values ~ indic_de + geo, data = popstr, mean)

# Reshape
popstr <- reshape(popstr, timevar = "indic_de", idvar = "geo",
  ids = "values", direction = "wide")
names(popstr)[match(sprintf("values.%s", indicde_list), names(popstr))] <- 
  varnames

# Merge with metadata
metadata <- merge(metadata, popstr, by.x = "NUTS3_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Total population
# Load variables from eurostat
totpop <- get_eurostat("demo_r_pjangrp3", time_format = "num",
  filters = list(sex = "T", age = "TOTAL", time = year)
)

# Average years
totpop <- aggregate(values ~ geo, data = totpop, mean)
names(totpop)[-1] <- "pop"

# Merge with metadata
metadata <- merge(metadata, totpop, by.x = "NUTS3_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Population density
popdens <- get_eurostat("demo_r_d3dens", time_format = "num",
  filters = list(unit = "PER_KM2", time = year)
)

# Average years
popdens <- aggregate(values ~ geo, data = popdens, mean)
names(popdens)[-1] <- "popdens"

# Merge with metadata
metadata <- merge(metadata, popdens, by.x = "NUTS3_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Life expectancy
lifexp <- get_eurostat("demo_r_mlifexp", time_format = "num",
  filters = list(sex = "T", age = "Y_LT1", time = year)
)

# Average years
lifexp <- aggregate(values ~ geo, data = lifexp, mean)
names(lifexp)[-1] <- "lifexp"

# Merge with metadata
metadata <- merge(metadata, lifexp, by.x = "NUTS2_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- GDP per capita
gdp <- get_eurostat("nama_10r_3gdp", time_format = "num",
  filters = list(unit = "EUR_HAB", time = year)
)

# Average years
gdp <- aggregate(values ~ geo, data = gdp, mean)
names(gdp)[-1] <- "gdp"

# Merge with metadata
metadata <- merge(metadata, gdp, by.x = "NUTS3_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Proportion of active population (25-64) with ISCED level 5 and above
educ <- get_eurostat("edat_lfse_04", time_format = "num",
  filters = list(isced11 = "ED5-8", sex = "T", age = "Y25-64", time = year)
)

# Average years
educ <- aggregate(values ~ geo, data = educ, mean)
names(educ)[-1] <- "educ"

# Merge with metadata
metadata <- merge(metadata, educ, by.x = "NUTS2_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Unemployment rate
unempl <- get_eurostat("lfst_r_lfu3rt", time_format = "num",
  filters = list(isced11 = "TOTAL", sex = "T", age = "Y20-64", time = year)
)

# Average years
unempl <- aggregate(values ~ geo, data = unempl, mean)
names(unempl)[-1] <- "unempl"

# Merge with metadata
metadata <- merge(metadata, unempl, by.x = "NUTS2_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Severe material deprivation rate
depriv <- get_eurostat("ilc_mddd21", time_format = "num",
  filters = list(unit = "PC", time = year)
)

# Average years
depriv <- aggregate(values ~ geo, data = depriv, mean)
names(depriv)[-1] <- "depriv"

# Merge with metadata
metadata <- merge(metadata, depriv, by.x = "NUTS2_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Hospital bed rates
bedrates <- get_eurostat("hlth_rs_bdsrg", time_format = "num",
  filters = list(unit = "P_HTHAB", facility = "HBEDT", time = year)
)

# Average years
bedrates <- aggregate(values ~ geo, data = bedrates, mean)
names(bedrates)[-1] <- "bedrates"

# Merge with metadata
metadata <- merge(metadata, bedrates, by.x = "NUTS2_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Share of urbanised land
urbshare <- get_eurostat("lan_lcv_art", time_format = "num",
  filters = list(unit = "PC", landcover = "LCA", time = year)
)

# Average years
urbshare <- aggregate(values ~ geo, data = urbshare, mean)
names(urbshare)[-1] <- "urbshare"

# Merge with metadata
metadata <- merge(metadata, urbshare, by.x = "NUTS2_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Landcover
landshare <- get_eurostat("lan_lcv_ovw", time_format = "num",
  filters = list(unit = "PC", landcover = c("LCB", "LCC", "LCD", "LCE", "LCG"), 
    time = year)
)

# Average years
landshare <- aggregate(values ~ geo + landcover, data = landshare, mean)

# Reshape
landshare <- reshape(landshare, timevar = "landcover", idvar = "geo",
  ids = "values", direction = "wide")
names(landshare)[6] <- "blueshare"

# Sum components of greenshare
landshare$greenshare <- rowSums(landshare[,2:5])
landshare <- landshare[,-(2:5)]

# Merge with metadata
metadata <- merge(metadata, landshare, by.x = "NUTS2_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Degree days
degdays <- get_eurostat("nrg_chddr2_a", time_format = "num",
  filters = list(unit = "NR", indic_nrg = c("CDD", "HDD"), 
    time = year)
)

# Average years
degdays <- aggregate(values ~ geo + indic_nrg, data = degdays, mean)

# Reshape
degdays <- reshape(degdays, timevar = "indic_nrg", idvar = "geo",
  ids = "values", direction = "wide")
names(degdays)[-1] <- c("cooldegdays", "heatdegdays")

# Merge with metadata
metadata <- merge(metadata, degdays, by.x = "NUTS3_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

#----- Social isolation from 2011 census
isol <- get_eurostat("cens_11ms_r3", time_format = "num",
  filters = list(age = "TOTAL", sex = "T", time = year))

# Compute prop of marital status
isol <- by(isol[,c("marsta", "values")], isol$geo, simplify = F, 
  function(x) {
    x$values <- x$values / x$values[x$marsta == "TOTAL"]
    sum(x$values[x$marsta %in% c("DIV", "SIN", "WID")])
})
isol <- data.frame(region = names(isol), isol = unlist(isol))

# Merge with dataset
metadata <- merge(metadata, isol, by.x = "NUTS3_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

#----- Regional typology
# (Didn't find how to get it from eurostat R package)
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
read_typo <- read_typo[, c("NUTS_ID", type_vars)]
names(read_typo)[names(read_typo) %in% type_vars] <- tolower(type_vars)
metadata <- merge(metadata, read_typo,
  by.x = "NUTS3_2021", by.y = "NUTS_ID", all.x = T, all.y = F, sort = F)

#----- Death rate
age_list <- c("Y_LT5", "Y5-9", "Y10-14", "Y15-19", 
  "Y20-24", "Y25-29", "Y30-34", "Y35-39", "Y40-44", 
  "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", 
  "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90", 
  "TOTAL")
varnames <- c("death_0004", "death_0509", "death_1014", "death_1519", 
  "death_2024", "death_2529", "death_3034", "death_3539", "death_4044", 
  "death_4549", "death_5054", "death_5559", "death_6064", "death_6569", 
  "death_7074", "death_7579", "death_8084", "death_8589", "death_90p",
  "death_tot")

# Load
deaths <- get_eurostat("demo_r_magec3", time_format = "num",
  filters = list(sex = "T")
)

# Average years
deaths <- aggregate(values ~ age + geo, data = deaths, mean)

# Reshape
deaths <- reshape(deaths, timevar = "age", idvar = "geo",
  ids = "values", direction = "wide")
names(deaths)[match(sprintf("values.%s", age_list), names(deaths))] <- 
  varnames

# Sum 85-89 and ge90 to match population
deaths$death_8599 <- deaths$death_8589 + deaths$death_90p

# Remove variables
deaths[c("values.UNK", "death_8589", "death_90p")] <- NULL

# Merge with metadata
metadata <- merge(metadata, deaths, by.x = "NUTS3_2021", by.y = "geo",
  all.x = T, all.y = F, sort = F)

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

#---------------------------
# Missing values imputation
#---------------------------

# Keep only metavariables
metavar <- metadata[,!names(metadata) %in% desc_vars]

# Keep track of missings
imputed <- is.na(metavar)
nmis <- apply(imputed, 2, sum)
propmis <- apply(imputed, 2, mean)

#----- Impute

# Impute using cart because of complex interactions
meta_imp <- mice(metavar, method = "cart", seed = 12345, print = F)

# Get the imputed dataset (of iter_Sel)
iter_sel <- 5
metadata[,!names(metadata) %in% desc_vars] <- complete(meta_imp, iter_sel)

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
# Load ERA5land data
#---------------------------

# Load ERA5land data for Urban Audit
load(sprintf("%s/data/URAU_LB_2020_ERA5_land_1981_2019_daily_Tmean_deg_C.RData", 
  path_urau))
names(URAU_LB_2020_ERA5_land_df)[4:5] <- c("date", "era5landtmean")

# Select only records for cities in metadata
URAU_LB_2020_ERA5_land_df <- subset(URAU_LB_2020_ERA5_land_df,
  Locn_code %in% metadata$URAU_CODE)

# Transform date
URAU_LB_2020_ERA5_land_df$date <- as.Date(URAU_LB_2020_ERA5_land_df$date)

# Split by location
era5series <- split(URAU_LB_2020_ERA5_land_df[,c("date", "era5landtmean")],
  URAU_LB_2020_ERA5_land_df$Locn_code)

# Reorder
era5series <- era5series[match(metadata$URAU_CODE, names(era5series))]
