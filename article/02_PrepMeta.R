################################################################################
#
#                         MCC-CityEurope
#
#                     Meta variables preparation
#
################################################################################

#----- Prepare storing objects

# Descriptive summary
metadesc <- data.frame(metavar = c(), label = c(), source = c())

# City-year data.frame to keep info on years of data
metacityyear <- cbind(metadata[rep(1:nrow(metadata), each = length(year)), 
  c("URAU_CODE", "URAU_CODE2", 
    sprintf("NUTS%i_2021", 0:3), sprintf("NUTS%i_2016", 0:3))],
  year = rep(year, nrow(metadata))
)

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------

# List of datasets that should be downloaded
datasets <- list(
  pop1 = c(pop = "DE1001V", pop_6574 = "DE1028V", pop75p = "DE1055V"),
  livcon = c(isol = "DE3002V")
)

#----- Load the necessary datasets
urb_dat <- lapply(sprintf("urb_c%s", names(datasets)), get_eurostat,
  time_format = "num")
names(urb_dat) <- names(datasets)

# Remove tibble class
urb_dat <- lapply(urb_dat, as.data.frame)

# Keep only selected variables
urb_dat <- Map(function(x, y) subset(x, indic_ur %in% y),
  urb_dat, datasets)

# Reshape as wide
res_dat <- lapply(urb_dat, reshape, timevar = "indic_ur", 
  idvar = c("cities", "time"), ids = "values", direction = "wide")

# Merge and rename the different datasets
urb_df <- Reduce(function(x, y) merge(x, y, all = T, by = c("cities", "time")),
  res_dat)
namevec <- unlist(unname(datasets))
names(urb_df) <- gsub("values\\.", "", names(urb_df))
names(urb_df)[match(namevec, names(urb_df))] <- names(namevec)

#----- Create final variables

# Prop or population 65+
urb_df$prop_65p <- rowSums(urb_df[,c("pop_6574", "pop75p")]) /
  urb_df[,"pop"]

# Drop variables
urb_df <- urb_df[,-(4:5)]

#----- Merge to the list of cities

# Merge with metadata
metacityyear <- merge(metacityyear, urb_df, 
  by.x = c("URAU_CODE2", "year"), by.y = c("cities", "time"),
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = colnames(urb_df)[-(1:2)], 
  label = c("Total population", "Isolation", "Population above 65"),
  source = "Urban Audit"))

#---------------------------
#  Load Eurostat's regional data
#---------------------------

#----- Merge function to be used
# Function to be used to merge NUTS variable with the metadata
# meta: the existing data.frame
# newvar: the df containing new NUTS metavariable
# level: the NUTS level of the variable
# highest: the highest NUTS level to attempt the match if the level is not found
# tryOld: logical. Should the merging should be attempting with old NUTS codes?
nuts_merge <- function(meta, newvar, level = 3, highest = level, tryOld = T)
{
  # Determine index variable
  idvar <- sprintf("NUTS%s_2021", level)
  
  # Initial merge with metadata
  meta <- merge(meta, newvar, by.x = c(idvar, "year"), by.y = c("geo", "time"),
    all.x = T, all.y = F)
  
  # Try to merge with NUTS3 of 2016
  if (tryOld){
    idold <- sprintf("NUTS%s_2016", level)
    nutmis <- is.na(meta[,colnames(newvar)[3]])
    matchind <- match(interaction(meta[nutmis, c(idold, "year")]), 
      interaction(newvar[,c("geo", "time")]))
    meta[which(nutmis)[!is.na(matchind)], colnames(newvar)[-(1:2)]] <- 
      newvar[na.omit(matchind), -(1:2)]
  }
  
  # Try to merge with higher level for missing
  if (highest < level){
    for (l in (level - 1):highest){
      idvar <- sprintf("NUTS%s_2021", l)
      nutmis <- is.na(meta[,colnames(newvar)[3]])
      matchind <- match(interaction(meta[nutmis, c(idvar, "year")]), 
        interaction(newvar[,c("geo", "time")]))
      meta[which(nutmis)[!is.na(matchind)],colnames(newvar)[-(1:2)]] <- 
        newvar[na.omit(matchind),-(1:2)]
      
      # try old NUTS code for this level as well
      if (tryOld){
        idold <- sprintf("NUTS%s_2016", l)
        nutmis <- is.na(meta[,colnames(newvar)[3]])
        matchind <- match(interaction(meta[nutmis, c(idold, "year")]), 
          interaction(newvar[,c("geo", "time")]))
        meta[which(nutmis)[!is.na(matchind)],colnames(newvar)[-(1:2)]] <- 
          newvar[na.omit(matchind),-(1:2)]
      }
    }
  }
  
  # Output
  return(meta)
}

#----- Population structure
indicde_list <- c("PC_Y0_4", "PC_Y5_9", "PC_Y10_14", "PC_Y15_19", 
  "PC_Y20_24", "PC_Y25_29", "PC_Y30_34", "PC_Y35_39", "PC_Y40_44", 
  "PC_Y45_49", "PC_Y50_54", "PC_Y55_59", "PC_Y60_64", "PC_Y65_69", 
  "PC_Y70_74", "PC_Y75_79", "PC_Y80_84", "PC_Y85_MAX")
varnames <- c("prop_0004", "prop_0509", "prop_1014", "prop_1519", 
  "prop_2024", "prop_2529", "prop_3034", "prop_3539", "prop_4044", 
  "prop_4549", "prop_5054", "prop_5559", "prop_6064", "prop_6569", 
  "prop_7074", "prop_7579", "prop_8084", "prop_8599")

# Load variables from eurostat
popstr <- get_eurostat("demo_r_pjanind3", time_format = "num",
  filters = list(indic_de = indicde_list, time = year))

# Reshape
popstr <- reshape(as.data.frame(popstr), timevar = "indic_de", 
  idvar = c("geo", "time"), ids = "values", direction = "wide",
  drop = "unit")
names(popstr)[match(sprintf("values.%s", indicde_list), names(popstr))] <- 
  varnames

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, popstr, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = varnames, 
  label = sprintf("Proportion of population in ages %s", 
    substr(varnames, 6, 10)),
  source = "NUTS3"))

#----- Population density

# Load
popdens <- get_eurostat("demo_r_d3dens", time_format = "num",
  filters = list(unit = "PER_KM2", time = year)
)
popdens <- popdens[,-1]
names(popdens)[3] <- "popdens"

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, popdens, level = 3, highest = 2)
  
# Add description
metadesc <- rbind(metadesc, cbind(metavar = "popdens", 
  label = "Population density", source = "NUTS3"))

#----- Life expectancy

# Selected ages
ages <- c("Y_LT1", sprintf("Y%i", seq(5, 80, by = 5)), "Y_GE85")
varnames <- sprintf("lifexp_%02i", c(0, seq(5, 80, by = 5), 85))

# Download data
lifexp <- get_eurostat("demo_r_mlifexp", time_format = "num",
  filters = list(sex = "T", age = ages, time = year)
)

# Reshape
lifexp <- reshape(as.data.frame(lifexp), timevar = "age", 
  idvar = c("geo", "time"), ids = "values", direction = "wide",
  drop = c("unit", "sex"))
names(lifexp)[match(sprintf("values.%s", ages), names(lifexp))] <- 
  varnames

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, lifexp, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = varnames, 
  label = sprintf("Life expectancy %s", gsub("Y_LT1", "", ages)), 
  source = "NUTS2"))

#----- GDP per capita
gdp <- get_eurostat("nama_10r_3gdp", time_format = "num",
  filters = list(unit = "EUR_HAB", time = year)
)
gdp <- gdp[,-1]
names(gdp)[3] <- "gdp"

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, gdp, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "gdp", 
  label = "GDP", source = "NUTS3"))

#----- Proportion of active population (25-64) with ISCED level 5 and above
educ <- get_eurostat("edat_lfse_04", time_format = "num",
  filters = list(isced11 = "ED5-8", sex = "T", age = "Y25-64", time = year)
)
educ <- educ[,-(1:4)]
names(educ)[3] <- "educ"

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, educ, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "educ",
  label = "Education level", source = "NUTS2"))

#----- Unemployment rate
unempl <- get_eurostat("lfst_r_lfu3rt", time_format = "num",
  filters = list(isced11 = "TOTAL", sex = "T", age = "Y20-64", time = year)
)
unempl <- unempl[,-(1:4)]
names(unempl)[3] <- "unempl"

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, unempl, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "unempl", 
  label = "Unemployment rate", source = "NUTS2"))

#----- Severe material deprivation rate
depriv <- get_eurostat("ilc_mddd21", time_format = "num",
  filters = list(unit = "PC", time = year)
)
depriv <- depriv[,-1]
names(depriv)[3] <- "depriv"

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, depriv, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "depriv", 
  label = "Deprivation rate", source = "NUTS2"))

#----- Hospital bed rates
bedrates <- get_eurostat("hlth_rs_bdsrg", time_format = "num",
  filters = list(unit = "P_HTHAB", facility = "HBEDT", time = year)
)
bedrates <- bedrates[,-(1:2)]
names(bedrates)[3] <- "bedrates"

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, bedrates, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "bedrates", 
  label = "Hospital bed rates", source = "NUTS2"))

#----- Share of urbanised land
urbshare <- get_eurostat("lan_lcv_art", time_format = "num",
  filters = list(unit = "PC", landcover = "LCA", time = year)
)
urbshare <- urbshare[,-(1:2)]
names(urbshare)[3] <- "urbshare"

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, urbshare, level = 2)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "urbshare", 
  label = "Share of urban area", source = "NUTS2"))

#----- Landcover
landshare <- get_eurostat("lan_lcv_ovw", time_format = "num",
  filters = list(unit = "PC", landcover = c("LCB", "LCC", "LCD", "LCE", "LCG"), 
    time = year)
)

# Reshape
landshare <- reshape(as.data.frame(landshare), timevar = "landcover", 
  idvar = c("geo", "time"), ids = "values", direction = "wide", drop = "unit")
names(landshare)[7] <- "blueshare"

# Sum components of greenshare
landshare$greenshare <- rowSums(landshare[,3:6])
landshare <- landshare[,-(3:6)]

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, landshare, level = 2)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = c("blueshare", "greenshare"), 
  label = sprintf("Share of %s", c("water", "green area")), source = "NUTS2"))

#----- Regional typology
# (Didn't find how to get it from eurostat R package)
# Load typology
read_typo <- read.table(sprintf("%s/metadata/NUTS_AT_2021.csv", path_nuts), 
  header = T, sep = ",", quote = "\"", na.strings = "0")
type_vars <- grep("TYPE", names(read_typo), value = T)

# Reverse the scale of typologies
for (var in type_vars) read_typo[,var] <- max(read_typo[,var], na.rm = T) - 
  read_typo[,var] + 1

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

# Reorganize
read_typo <- read_typo[, c("NUTS_ID", type_vars)]
colnames(read_typo)[1] <- "geo"
names(read_typo)[names(read_typo) %in% type_vars] <- tolower(type_vars)

# Merge
typo_ext <- cbind(time = rep(year, nrow(read_typo)),
  read_typo[rep(1:nrow(read_typo), each = length(year)),])
metacityyear <- nuts_merge(metacityyear, typo_ext, level = 3)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = tolower(type_vars), 
  label = sprintf("%s region type", c("Mountain", "Urban", "Coastal")), 
  source = "NUTS3"))

#----- Death rate
age_list <- c("Y_LT5", "Y5-9", "Y10-14", "Y15-19", 
  "Y20-24", "Y25-29", "Y30-34", "Y35-39", "Y40-44", 
  "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", 
  "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90", 
  "TOTAL")
varnames <- c("deathrate_0004", "deathrate_0509", "deathrate_1014", 
  "deathrate_1519", "deathrate_2024", "deathrate_2529", "deathrate_3034", 
  "deathrate_3539", "deathrate_4044", "deathrate_4549", "deathrate_5054", 
  "deathrate_5559", "deathrate_6064", "deathrate_6569", "deathrate_7074", 
  "deathrate_7579", "deathrate_8084", "deathrate_8589", "deathrate_90p",
  "deathrate_tot")

# Load total deaths
deaths <- get_eurostat("demo_r_magec3", time_format = "num",
  filters = list(sex = "T", time = year)
)
colnames(deaths)[colnames(deaths) == "values"] <- "deaths"

# Load population for NUTS3
pop <- get_eurostat("demo_r_pjangrp3", time_format = "num",
  filters = list(sex = "T", time = year)
)
colnames(pop)[colnames(pop) == "values"] <- "pop"

# Merge them
popdeaths <- merge(deaths, pop)

# Compute rates
popdeaths$deathrate <- with(popdeaths, deaths / pop)

# Reshape
popdeaths <- reshape(popdeaths, timevar = "age", idvar = c("geo", "time"),
  ids = "deathrate", direction = "wide", 
  drop = c("sex", "unit", "deaths", "pop"))
names(popdeaths)[match(sprintf("deathrate.%s", age_list), 
  names(popdeaths))] <- varnames

# Sum 85-89 and ge90 to match population
popdeaths$deathrate_8599 <- popdeaths$deathrate_8589 + popdeaths$deathrate_90p

# Remove variables
popdeaths[c("deathrate.UNK", "deathrate_8589", "deathrate_90p")] <- NULL

# Merge with metadata
metacityyear <- nuts_merge(metacityyear, popdeaths, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = colnames(popdeaths)[-(1:2)], 
  label = c("Death rate", sprintf("Death rate in ages %s to %s", 
    substr(colnames(popdeaths)[-(1:3)], 11, 12), 
    substr(colnames(popdeaths)[-(1:3)], 13, 14))),
  source = "NUTS3"))

#---------------------------
# Additional remote sensing data
#---------------------------

#----- NDVI

# Load data
load(paste0(path_urau, "/data/MODIS_NDVI/city_shapes",
  "/MODIS_NDVI_250m_URAU_LB_2000_2020_city_shapes.RData"))

# Select years and aggregate
ndvi_sel <- modis_250m_annual_median_NDVI_URAU_LB_2020[,c(1, 
  which(substr(colnames(modis_250m_annual_median_NDVI_URAU_LB_2020), 6, 9) %in% 
      year))]
st_geometry(ndvi_sel) <- NULL

# Remove duplicates
ndvi_sel <- subset(ndvi_sel, !duplicated(URAU_CODE))

# Reshape and merge
ndvi_long <- reshape(ndvi_sel, direction = "long", 
  varying = names(ndvi_sel)[-1], v.names = "ndvi", 
  idvar = "URAU_CODE", times = year)
metacityyear <- merge(metacityyear, ndvi_long, 
  by.x = c("URAU_CODE", "year"), by.y = c("URAU_CODE", "time"),
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "ndvi", 
  label = "NDVI", source = "GEE MODIS"))

#----- PM2.5

# Load data
load(paste0(path_urau, "/data/air_pollutants/city_shapes",
  "/PM25_URAU_LB_2001_2018_city_shapes.RData"))

# Select years
pm25_sel <- pm2p5_URAU_LB_2020_DF[,c(1, 
  which(substr(colnames(pm2p5_URAU_LB_2020_DF), 6, 9) %in% year))]

# Remove duplicates
pm25_sel <- subset(pm25_sel, !duplicated(URAU_CODE))

# Reshape and merge
pm25_long <- reshape(pm25_sel, direction = "long", 
  varying = names(pm25_sel)[-1], v.names = "pm25", 
  idvar = "URAU_CODE", times = substr(colnames(pm25_sel)[-1], 6, 9))
metacityyear <- merge(metacityyear, pm25_long, 
  by.x = c("URAU_CODE", "year"), by.y = c("URAU_CODE", "time"),
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "pm25", 
  label = "PM2.5", source = "Atmospheric Composition Analysis Group"))

#----- NO2

# Load data
load(paste0(path_urau, "/data/air_pollutants/city_shapes",
  "/NO2_URAU_LB_1996_2012_city_shapes.RData"))
st_geometry(no2_URAU_LB_2020_sf) <- NULL

# Select years
no2_sel <- no2_URAU_LB_2020_sf[,c(1, 
  which(substr(colnames(no2_URAU_LB_2020_sf), 5, 8) %in% year))]

# Remove duplicates
no2_sel <- subset(no2_sel, !duplicated(URAU_CODE))

# Reshape and merge
no2_long <- reshape(no2_sel, direction = "long", 
  varying = names(no2_sel)[-1], v.names = "no2", 
  idvar = "URAU_CODE", times = substr(colnames(no2_sel)[-1], 5, 8))
metacityyear <- merge(metacityyear, no2_long, 
  by.x = c("URAU_CODE", "year"), by.y = c("URAU_CODE", "time"),
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "no2", 
  label = "NO2", source = "Atmospheric Composition Analysis Group"))

#---------------------------
# Load ERA5land data
#---------------------------

#----- Create time series

# Load ERA5land data for Urban Audit
load(sprintf("%s/data/URAU_LB_2020_ERA5_land_1981_2019_daily_Tmean_deg_C.RData", 
  path_urau))
names(URAU_LB_2020_ERA5_land_df)[4:5] <- c("date", "era5landtmean")

# Select only records for cities in metadata
URAU_LB_2020_ERA5_land_df <- subset(URAU_LB_2020_ERA5_land_df,
  Locn_code %in% metadata$URAU_CODE)

# Transform date and select period
URAU_LB_2020_ERA5_land_df$date <- as.Date(URAU_LB_2020_ERA5_land_df$date)
URAU_LB_2020_ERA5_land_df <- subset(URAU_LB_2020_ERA5_land_df, 
  format(date, "%Y") >= yearstart)

# Split by location
era5series <- split(URAU_LB_2020_ERA5_land_df[,c("date", "era5landtmean")],
  URAU_LB_2020_ERA5_land_df$Locn_code)

# Reorder
era5series <- era5series[match(metadata$URAU_CODE, names(era5series))]

#----- Temperature-based metadata

# Bind all series and select years
longseries <- do.call(rbind, era5series)
longseries$URAU_CODE <- rep(names(era5series), sapply(era5series, nrow))

# Select years
longseries$year <- format(longseries$date, "%Y")
longseries <- longseries[longseries$year %in% as.character(year),]

# Compute annual average temperatures and degree days
annualEra5 <- aggregate(era5landtmean ~ URAU_CODE + year, 
  longseries, function(x) c(tmean = mean(x), cooldegdays = sum(x[x > 24] - 21), 
    heatdegdays = sum(18 - x[x < 15])))
annualEra5 <- data.frame(annualEra5[,1:2], annualEra5[[3]])

# Merge to other variables
metacityyear <- merge(metacityyear, annualEra5, 
  by= c("URAU_CODE", "year"), all.x = T, all.y = F, sort = F)

# Add description of metadata
metadesc <- rbind(metadesc, 
  cbind(metavar = c("tmean", "cooldegdays", "heatdegdays"), 
    label = c("Mean temperature", "Cooling degree days", "Heating degree days"), 
    source = "Copernicus"))

#---------------------------
# Alternative sources to fill missings
#---------------------------

#----- Add GDP for UK

# Load GDP values at NUTS3 level
gdppath <- paste0("V:/VolumeQ/AGteam/ONS/gdp",
  "/regionalgrossdomesticproductgdpallnutslevelregions.xlsx")
ukgdp <- as.data.frame(read_excel(gdppath, sheet = 8, range = "A2:X238",
  na = "-"))

# Load exchange rate
exchratepath <- "V:/VolumeQ/AGteam/ONS/gdp/euroexchangerate.csv"
exchrate <- read.table(exchratepath, skip = 8, header = F, sep = ",")
names(exchrate) <- c("Date", "Rate")

# Convert GDP to euros
commonyears <- Reduce(intersect, list(year, names(ukgdp), exchrate$Date))
eurogdp <- as.data.frame(mapply(function(gdp, rt) gdp * rt,
  ukgdp[,match(commonyears, names(ukgdp))], 
  exchrate[match(commonyears, exchrate$Date),"Rate"]))
eurogdp$NUTS <- ukgdp$`NUTS code`

# Add to metadata
gdplong <- reshape(eurogdp[-1,], direction = "long", 
  varying = names(eurogdp)[-ncol(eurogdp)], v.names = "gdp", 
  idvar = "NUTS", times = names(eurogdp)[-ncol(eurogdp)])
matchind <- match(interaction(metacityyear[,c("NUTS3_2016", "year")]),
  interaction(gdplong[, c("NUTS", "time")]))
metacityyear[!is.na(matchind), "gdp"] <- gdplong[na.omit(matchind), "gdp"]

#----- Missing population

# Populations in Wikipedia (different years)
wikipop <- c(CY501C1.2011 = 101000, EL014C1.2011 = 108642, EL013C1.2011 = 65133, 
  EL012C1.2011 = 85851, EL011C1.2011 = 76817, EL010C1.2011 = 81355)

# Add to metadata
matchind <- match(names(wikipop), 
  interaction(metacityyear[,c("URAU_CODE", "year")]))
metacityyear[matchind, "pop"] <- wikipop

#---------------------------
# Missing values imputation
#---------------------------

#----- Compute final variables

# Average years
meta_means <- aggregate(metacityyear[,metadesc$metavar], 
  metacityyear["URAU_CODE"], mean, na.rm = T)

# Merge to metadata
metadata <- merge(metadata, meta_means, by = "URAU_CODE", all.x = T)

#----- Keep track of missing

# Missings
imputed <- is.na(subset(metadata, select = metadesc$metavar))
rownames(imputed) <- metadata$URAU_CODE

# Number of missings per variables
metadesc$nmis <- apply(imputed, 2, sum)
metadesc$propmis <- round(apply(imputed, 2, mean) * 100)

# Number of missings per city
metadata$nmiss <- apply(imputed, 1, sum)

#----- Impute

# Variables used in imputation
imputvars <- c(metadesc$metavar, "CNTR_CODE", "AREA_SQM", "region")

# Impute using cart because of complex interactions
meta_imp <- mice(metadata[,imputvars], method = "cart", maxit = 20,
  seed = 12345, print = F)

# Get the imputed dataset (as the median of the 5 iterations)
impdat <- complete(meta_imp, "long")
aggimp <- aggregate(impdat[,metadesc$metavar], by = impdat[".id"], median)[,-1]
metadata[,names(aggimp)] <- aggimp

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

# Add info to metadata
geobind <- do.call(rbind, metageo$geometry)
colnames(geobind) <- c("lon", "lat")
metadata <- cbind(metadata, geobind)
