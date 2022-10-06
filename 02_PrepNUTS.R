################################################################################
#
#                         MCC-CityEurope
#
#               Linkage of cities with NUTS regions
#
################################################################################

#---------------------------
# Prepare NUTS regions
#---------------------------

#----- Lookup table

# Load lookup table
nuts_lookup <- read.csv(paste0(path_euro, "/lookup/URAU_NUTS_2021.csv"))

# Select cities
nuts_meta <- subset(nuts_lookup, URAU_CODE %in% metadata$URAU_CODE)

# For missing proportion of population, replace with 1
nuts_meta$POP_PROP[is.na(nuts_meta$POP_PROP)] <- 1

#----- Prepare the annual table for NUTS

# Expand by year
meta_nutsyear <- expand.grid(NUTS3 = unique(nuts_meta$NUTS3), year = year)

# Create variables for higher NUTS level
for (i in 0:2) meta_nutsyear[[sprintf("NUTS%i", i)]] <- substr(
  meta_nutsyear$NUTS3, 1, 2 + i)

#---------------------------
# Load NUTS meta-variables
#---------------------------

#----- Merge function to be used

## Function to be used to merge NUTS variable with the metadata
# meta: the existing data.frame
# newvar: the df containing new NUTS metavariable
# level: the NUTS level of the variable
# highest: the highest NUTS level to attempt the match if the level is not found
nuts_merge <- function(meta, newvar, level = 3, highest = level)
{
  
  # Determine index variable
  idvar <- sprintf("NUTS%s", level)
  
  # Initial merge with metadata
  meta <- merge(meta, newvar, by.x = c(idvar, "year"), by.y = c("geo", "time"),
    all.x = T, all.y = F)
  
  # Try to merge with higher level for missing
  while (level > highest){
    level <- level - 1
    idvar <- sprintf("NUTS%s", level)
    nutmis <- is.na(meta[,colnames(newvar)[3]])
    matchind <- match(interaction(meta[nutmis, c(idvar, "year")]), 
      interaction(newvar[,c("geo", "time")]))
    meta[which(nutmis)[!is.na(matchind)], colnames(newvar)[-(1:2)]] <- 
      newvar[na.omit(matchind), -(1:2)]
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
meta_nutsyear <- nuts_merge(meta_nutsyear, popstr, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = varnames, 
  label = sprintf("Proportion of population in ages %s", 
    substr(varnames, 6, 10)),
  source = "NUTS3"))

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
meta_nutsyear <- nuts_merge(meta_nutsyear, lifexp, level = 2, highest = 0)

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

## Add UK
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

# Add to the existing table
gdpuklong <- reshape(eurogdp[-1,], direction = "long", 
  varying = names(eurogdp)[-ncol(eurogdp)], v.names = "gdp", 
  idvar = "NUTS", times = names(eurogdp)[-ncol(eurogdp)])
names(gdpuklong) <- names(gdp)
gdp <- rbind(gdp, gdpuklong)

## Merge with metadata
meta_nutsyear <- nuts_merge(meta_nutsyear, gdp, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "gdp", 
  label = "GDP", source = "NUTS3 / ONS"))

#----- Proportion of active population (25-64) with ISCED level 5 and above
educ <- get_eurostat("edat_lfse_04", time_format = "num",
  filters = list(isced11 = "ED5-8", sex = "T", age = "Y25-64", time = year)
)
educ <- educ[,-(1:4)]
names(educ)[3] <- "educ"

# Merge with metadata
meta_nutsyear <- nuts_merge(meta_nutsyear, educ, level = 2, highest = 0)

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
meta_nutsyear <- nuts_merge(meta_nutsyear, unempl, level = 2, highest = 0)

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
meta_nutsyear <- nuts_merge(meta_nutsyear, depriv, level = 2, highest = 0)

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
meta_nutsyear <- nuts_merge(meta_nutsyear, bedrates, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "bedrates", 
  label = "Hospital bed rates", source = "NUTS2"))

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
meta_nutsyear <- nuts_merge(meta_nutsyear, popdeaths, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = colnames(popdeaths)[-(1:2)], 
  label = c("Death rate", sprintf("Death rate in ages %s to %s", 
    substr(colnames(popdeaths)[-(1:3)], 11, 12), 
    substr(colnames(popdeaths)[-(1:3)], 13, 14))),
  source = "NUTS3"))

#----- Proportion of lone person households
householdcens <- get_eurostat("cens_11htts_r2", time_format = "num",
  filters = list(tenure = "TOTAL"))

# Compute proportion
lonehousehold <- by(householdcens, householdcens[,c("time", "geo")], 
  function(x) cbind(x[1,c("geo", "time")], 
    subset(x, hhcomp == "P1", "values") / 
      subset(x, hhcomp == "TOTAL", "values")))
lonedf <- do.call(rbind, lonehousehold)
names(lonedf)[3] <- "isol2"

# Merge with metadata
meta_nutsyear <- nuts_merge(meta_nutsyear, lonedf, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "isol2", 
  label = "Isolation", source = "NUTS2"))

#---------------------------
# Aggregate and add to metadata
#---------------------------

#----- Aggregate by city

# Merge with city codes
meta_all <- merge(nuts_meta, meta_nutsyear, all.x = T)

# Aggregate by city and year
cityyear_means <- as.data.table(meta_all)[, lapply(.SD, weighted.mean, 
  w = POP_PROP, na.rm = T), by = list(URAU_CODE, year), 
  .SDcols = intersect(colnames(meta_all), metadesc$metavar)]

# Add to metadata
metacityyear <- merge(metacityyear, cityyear_means, all.x = T)
