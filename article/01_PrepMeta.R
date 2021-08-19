################################################################################
#
#                         MCC-CityEurope
#
#                     Eurostat Data preparation
#
################################################################################

source("00_Packages_Parameters.R")

# Paths
path_urau <- "V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)"
# path_ucd <- "V:/VolumeQ/AGteam/Urban Centre Database"
path_nuts <- "V:/VolumeQ/AGteam/Eurostat/Regional by NUTS classification (reg)"

#---------------------------
#  Link datasets
#---------------------------

#----- Load all lookup table 

# Lookup table between URAU and NUTS
urau_nuts <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Urban Audit (urb_cgc)/lookup/URAU_NUTS2020.csv"))

# Lookup table between MCC and URAU
urau_mcc_age <- read.table(sprintf("%s/lookup/URAU_MCC_AgeCause_20200907.csv", 
    path_urau),
  header = T, sep = ";", quote = "\"")
urau_mcc_all <- read.table(sprintf("%s/lookup/URAU_MCCdata_20210407.csv", 
    path_urau),
  header = T, sep = ";", quote = "\"")

# Lookup table between Cities and greater city
urau_greater <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Urban Audit (urb_cgc)/lookup/CitiesToGreater.csv"))

#----- Merge everything together

# Merge with MCC age dataset
metadata <- merge(urau_nuts[,-(5:7)], 
  urau_mcc_age[,c("URAU_CODE", "mcc_code", "cityname")],
  by = "URAU_CODE", all.x = T, all.y = F)

# When there is no match with MCC age dataset, search in MCC all
matchall <- match(subset(metadata, is.na(mcc_code), URAU_CODE, drop = T),
  urau_mcc_all$URAU_CODE)
metadata[is.na(metadata$mcc_code), c("mcc_code", "cityname")] <- 
  urau_mcc_all[matchall, c("mcc_code", "cityname")]

#----- Select URAU level

# Discard functional urban areas because they are huge
metadata <- metadata[metadata$URAU_CATG != "F",]

# Remove all cities inside greater city
metadata <- subset(metadata, !URAU_CODE %in% urau_greater$city_code)

# Remove potential remaining level duplicates
metadata <- metadata[!duplicated(metadata),]

#----- Treat specific cases

# Reject MCC matches that are not in selected MCC country datasets
metadata <- subset(metadata, is.na(mcc_code) | 
    (sapply(strsplit(mcc_code, "\\."), "[", 2) %in% mcc_countries))

# Remove mismatch between large UK conurbation and smaller cities
#   Manchester/Wigan, Liverpool/Birkenhead
torm <- c(
  with(metadata, which(URAU_CODE == "UK008K1" & mcc_code == "wign.uk9016")),
  with(metadata, which(URAU_CODE == "UK006K2" & mcc_code == "brkn.uk9016"))
)
metadata <- metadata[-torm,]

# # Duplicates on the NUTS3 level: select the largest city
# nnuts3 <- tapply(metadata$NUTS3_2021, metadata$NUTS3_2021, length)
# nutsdup <- subset(metadata, NUTS3_2021 %in% names(nnuts3[nnuts3 > 1]))
# toremove <- by(nutsdup, nutsdup$NUTS3_2021, function(x){ 
#   ksel <- x$URAU_CATG == "K"
#   sel <- if (sum(ksel) > 0) x[ksel,] else x
#   sel <- sel[which.max(sel$AREA_SQM), "URAU_CODE"]
#   x$URAU_CODE[x$URAU_CODE != sel]
# })
# metadata <- subset(metadata, !URAU_CODE %in% unlist(toremove))

# Change London NUTS3 code for easier linkage later
metadata[metadata$URAU_NAME == "London","NUTS3_2021"] <- "UKI"

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

# # Add a seventh character to Polish cities that was missing for some reason
# incomplete <- nchar(metadata$URAU_CODE) == 6
# metadata$URAU_CODE[incomplete] <- paste0(metadata$URAU_CODE[incomplete], "1")
# 
# # Change some codes that don't match between lookup tables and datasets
# metadata$URAU_CODE[metadata$URAU_CODE == "DK001K1"] <- "DK001C1"

#----- Add other information

# Add region
metadata$region <- as.factor(regionlist[metadata$CNTR_CODE])

# NUTS codes for easier merging
metadata$NUTS2_2021 <- substr(metadata$NUTS3_2021, 1, 4)
metadata$NUTS1_2021 <- substr(metadata$NUTS3_2021, 1, 3)
metadata$NUTS0_2021 <- substr(metadata$NUTS3_2021, 1, 2)

# Do it also for the 2016 version
metadata$NUTS2_2016 <- substr(metadata$NUTS3_2016, 1, 4)
metadata$NUTS1_2016 <- substr(metadata$NUTS3_2016, 1, 3)
metadata$NUTS0_2016 <- substr(metadata$NUTS3_2016, 1, 2)

#----- Tidy dataset

# Order it
metadata <- metadata[order(metadata$URAU_CODE),]

# List of description variables
desc_vars <- names(metadata)

# Add indicator for whether it is in MCC
metadata$inmcc <- !is.na(metadata$mcc_code)

#----- Label cities

# Load some labels
labpath <- paste0("V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)/metadata",
  "/URAU_DisplayNames.csv")
uraulabs <- read.csv(labpath)

# Merge
metadata <- merge(metadata, uraulabs, all.x = T, all.y = F)

# Add URAU names for missing labels
metadata$LABEL[is.na(metadata$LABEL)] <- metadata$URAU_NAME[
  is.na(metadata$LABEL)]

# Remove all mention to city words
metadata$LABEL <- gsub("City of", "", metadata$LABEL)
metadata$LABEL <- gsub("Greater", "", metadata$LABEL)

# Remove all that is in parenthesis
metadata$LABEL <- gsub("\\(.*\\)", "", metadata$LABEL)

# The Case of Romania
metadata$LABEL <- gsub("MUNICIPIUL", "", metadata$LABEL)

# The Case of Poland
metadata$LABEL <- gsub("M\\. ", "", metadata$LABEL)

# Remove spaces at the start or end of label
metadata$LABEL <- gsub("^[[:blank:][:punct:]]*", "", metadata$LABEL)
metadata$LABEL <- gsub("[[:blank:][:punct:]]*$", "", metadata$LABEL)

# First letter upper case
metadata$LABEL <- str_to_title(metadata$LABEL)

#----- Prepare meta-metadata
metadesc <- data.frame(metavar = c(), label = c(), source = c())

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------

# List of datasets that should be downloaded
datasets <- list(
  pop1 = c(pop = "DE1001V", pop_6574 = "DE1028V", pop75p = "DE1055V"),
  livcon = c(isol = "DE3002V")
)
# , pop_0004 = "DE1040V",
#   pop_0509 = "DE1074V", pop_1014 = "DE1077V", pop_1519 = "DE1046V",
#   pop_2024 = "DE1049V", pop_2534 = "DE1058V", pop_3544 = "DE1061V",
#   pop_4554 = "DE1064V", pop_5564 = "DE1025V"
  # educ = c(educ = "TE2031I"),
#   lma = c(unempl = "EC1020I"),
#   env = c(o3 = "EN2025V", urban = "EN5200V")

#----- Load the necessary datasets
urb_dat <- lapply(sprintf("urb_c%s", names(datasets)), get_eurostat,
  time_format = "num")
names(urb_dat) <- names(datasets)

# Keep only selected variables
urb_dat <- Map(function(x, y) subset(x, indic_ur %in% y),
  urb_dat, datasets)

# Keep only selected years and aggregate
urb_dat <- lapply(urb_dat, subset, time %in% year)
agg_dat <- lapply(urb_dat, function(x){
  aggregate(values ~ indic_ur + cities, data = x, mean, na.rm = T)
})

# Reshape as wide
res_dat <- lapply(agg_dat, reshape, timevar = "indic_ur", idvar = "cities",
  ids = "values", direction = "wide")

# Merge
urb_df <- Reduce(function(x, y) merge(x, y, all = T, by = "cities"),
  res_dat)

# Rename
namevec <- unlist(unname(datasets))
names(urb_df) <- gsub("values\\.", "", names(urb_df))
names(urb_df)[match(namevec, names(urb_df))] <- names(namevec)

#----- Create additional variables

# Prop or population 65+
urb_df$prop_65p <- rowSums(urb_df[,c("pop_6574", "pop75p")]) /
  urb_df[,"pop"]

#----- Merge to the list of cities

# Drop variables
urb_df <- urb_df[,-c(3:4)]

# Merge with metadata
metadata <- merge(metadata, urb_df, by.x = "URAU_CODE", by.y = "cities",
  all.x = T, all.y = F, sort = F)

#----- For some missings try to attribute differently

# loop on variables
for (v in colnames(urb_df)[-1]){
  # Find missings for the variable
  urbmis <- is.na(metadata[,v])
  
  # Try to match with 6 character URAU CODE (Poland)
  matchind <- match(metadata$URAU_CODE[urbmis], substr(urb_df$cities, 1, 6))
  metadata[which(urbmis)[!is.na(matchind)], v] <- 
    urb_df[na.omit(matchind),v]
  
  # Try to match city to greater and conversely (some mixing in Belgium codes)
  altercodes <- metadata[urbmis, "URAU_CODE"]
  substr(altercodes[metadata[urbmis, "URAU_CATG"] == "C"], 6, 6) <- "K"
  substr(altercodes[metadata[urbmis, "URAU_CATG"] == "K"], 6, 6) <- "C"
  matchind <- match(altercodes, urb_df$cities)
  metadata[which(urbmis)[!is.na(matchind)], v] <- 
    urb_df[na.omit(matchind),v]
  
}
# For some missings check if we can merge by the 6 first character
#   Poland and Hungary


# Add description
metadesc <- rbind(metadesc, cbind(metavar = colnames(urb_df)[-1], 
  label = c("Total population", "Isolation", "Population above 65"),
  source = "Urban Audit"))
 
#---------------------------
#  Load Eurostat's regional data
#---------------------------

#----- Merge function to be used
# Function to be used to merge NUTS variable with the metadata
# metadata: the existing data.frame
# newvar: the df containing new NUTS metavariable
# level: the NUTS level of the variable
# highest: the highest NUTS level to attempt the match if the level is not found
# tryOld: logical. Should the merging should be attempting with old NUTS codes?
nuts_merge <- function(metadata, newvar, level = 3, highest = level, tryOld = T)
{
  # Determine index variable
  idvar <- sprintf("NUTS%s_2021", level)
  
  # Initial merge with metadata
  metadata <- merge(metadata, newvar, by.x = idvar, by.y = "geo",
    all.x = T, all.y = F)
  
  # Try to merge with NUTS3 of 2016
  if (tryOld){
    idold <- sprintf("NUTS%s_2016", level)
    nutmis <- is.na(metadata[,colnames(newvar)[2]])
    matchind <- match(metadata[nutmis, idold], newvar$geo)
    metadata[which(nutmis)[!is.na(matchind)],colnames(newvar)[-1]] <- 
      newvar[na.omit(matchind),-1]
  }
  
  # Try to merge with higher level for missing
  if (highest < level){
    for (l in (level - 1):highest){
      idvar <- sprintf("NUTS%s_2021", l)
      nutmis <- is.na(metadata[,colnames(newvar)[2]])
      matchind <- match(metadata[nutmis, idvar], newvar$geo)
      metadata[which(nutmis)[!is.na(matchind)],colnames(newvar)[-1]] <- 
        newvar[na.omit(matchind),-1]
      
      # try old NUTS code for this level as well
      if (tryOld){
        idold <- sprintf("NUTS%s_2016", l)
        nutmis <- is.na(metadata[,colnames(newvar)[2]])
        matchind <- match(metadata[nutmis, idold], newvar$geo)
        metadata[which(nutmis)[!is.na(matchind)],colnames(newvar)[-1]] <- 
          newvar[na.omit(matchind),-1]
      }
    }
  }
  
  # Output
  return(metadata)
}

#----- Population structure
indicde_list <- c("PC_Y0_4", "PC_Y5_9", "PC_Y10_14", "PC_Y15_19", 
  "PC_Y20_24", "PC_Y25_29", "PC_Y30_34", "PC_Y35_39", "PC_Y40_44", 
  "PC_Y45_49", "PC_Y50_54", "PC_Y55_59", "PC_Y60_64", "PC_Y65_69", 
  "PC_Y70_74", "PC_Y75_79", "PC_Y80_84", "PC_Y85_MAX")#, "PC_Y65_MAX")
varnames <- c("prop_0004", "prop_0509", "prop_1014", "prop_1519", 
  "prop_2024", "prop_2529", "prop_3034", "prop_3539", "prop_4044", 
  "prop_4549", "prop_5054", "prop_5559", "prop_6064", "prop_6569", 
  "prop_7074", "prop_7579", "prop_8084", "prop_8599")#, "prop_65p")

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
metadata <- nuts_merge(metadata, popstr, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = varnames, 
  label = sprintf("Proportion of population in ages %s", 
    substr(varnames, 6, 10)),
  source = "NUTS3"))

# #----- Total population
# # Load variables from eurostat
# totpop <- get_eurostat("demo_r_pjangrp3", time_format = "num",
#   filters = list(sex = "T", age = "TOTAL", time = year)
# )
# 
# # Average years
# totpop <- aggregate(values ~ geo, data = totpop, mean)
# names(totpop)[-1] <- "pop"
# 
# # Merge with metadata
# metadata <- merge(metadata, totpop, by.x = "NUTS3_2021", by.y = "geo",
#   all.x = T, all.y = F, sort = F)
# 
# # Add description
# metadesc <- rbind(metadesc, cbind(metavar = "pop", 
#   label = "Population", source = "NUTS3"))

#----- Population density
popdens <- get_eurostat("demo_r_d3dens", time_format = "num",
  filters = list(unit = "PER_KM2", time = year)
)

# Average years
popdens <- aggregate(values ~ geo, data = popdens, mean)
names(popdens)[-1] <- "popdens"

# Merge with metadata
metadata <- nuts_merge(metadata, popdens, level = 3, highest = 2)
  
# Add description
metadesc <- rbind(metadesc, cbind(metavar = "popdens", 
  label = "Population density", source = "NUTS3"))

#----- Life expectancy
lifexp <- get_eurostat("demo_r_mlifexp", time_format = "num",
  filters = list(sex = "T", age = "Y_LT1", time = year)
)

# Average years
lifexp <- aggregate(values ~ geo, data = lifexp, mean)
names(lifexp)[-1] <- "lifexp"

# Merge with metadata
metadata <- nuts_merge(metadata, lifexp, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "lifexp", 
  label = "Life expectancy", source = "NUTS2"))

#----- GDP per capita
gdp <- get_eurostat("nama_10r_3gdp", time_format = "num",
  filters = list(unit = "EUR_HAB", time = year)
)

# Average years
gdp <- aggregate(values ~ geo, data = gdp, mean)
names(gdp)[-1] <- "gdp"

# Merge with metadata
metadata <- nuts_merge(metadata, gdp, level = 3, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "gdp", 
  label = "GDP", source = "NUTS3"))

#----- Proportion of active population (25-64) with ISCED level 5 and above
educ <- get_eurostat("edat_lfse_04", time_format = "num",
  filters = list(isced11 = "ED5-8", sex = "T", age = "Y25-64", time = year)
)

# Average years
educ <- aggregate(values ~ geo, data = educ, mean)
names(educ)[-1] <- "educ"

# Merge with metadata
metadata <- nuts_merge(metadata, educ, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "educ",
  label = "Education level", source = "NUTS2"))

#----- Unemployment rate
unempl <- get_eurostat("lfst_r_lfu3rt", time_format = "num",
  filters = list(isced11 = "TOTAL", sex = "T", age = "Y20-64", time = year)
)

# Average years
unempl <- aggregate(values ~ geo, data = unempl, mean)
names(unempl)[-1] <- "unempl"

# Merge with metadata
metadata <- nuts_merge(metadata, unempl, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "unempl", 
  label = "Unemployment rate", source = "NUTS2"))

#----- Severe material deprivation rate
depriv <- get_eurostat("ilc_mddd21", time_format = "num",
  filters = list(unit = "PC", time = year)
)

# Average years
depriv <- aggregate(values ~ geo, data = depriv, mean)
names(depriv)[-1] <- "depriv"

# Merge with metadata
metadata <- nuts_merge(metadata, depriv, level = 2, highest = 0)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "depriv", 
  label = "Deprivation rate", source = "NUTS2"))

#----- Hospital bed rates
bedrates <- get_eurostat("hlth_rs_bdsrg", time_format = "num",
  filters = list(unit = "P_HTHAB", facility = "HBEDT", time = year)
)

# Average years
bedrates <- aggregate(values ~ geo, data = bedrates, mean)
names(bedrates)[-1] <- "bedrates"

# Merge with metadata
metadata <- nuts_merge(metadata, bedrates, level = 2, highest = 1)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "bedrates", 
  label = "Hospital bed rates", source = "NUTS2"))

#----- Share of urbanised land
urbshare <- get_eurostat("lan_lcv_art", time_format = "num",
  filters = list(unit = "PC", landcover = "LCA", time = year)
)

# Average years
urbshare <- aggregate(values ~ geo, data = urbshare, mean)
names(urbshare)[-1] <- "urbshare"

# Merge with metadata
metadata <- nuts_merge(metadata, urbshare, level = 2)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "urbshare", 
  label = "Share of urban area", source = "NUTS2"))

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
metadata <- nuts_merge(metadata, landshare, level = 2)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = c("blueshare", "greenshare"), 
  label = sprintf("Share of %s", c("water", "green area")), source = "NUTS2"))

# #----- Degree days
# degdays <- get_eurostat("nrg_chddr2_a", time_format = "num",
#   filters = list(unit = "NR", indic_nrg = c("CDD", "HDD"), 
#     time = year)
# )
# 
# # Average years
# degdays <- aggregate(values ~ geo + indic_nrg, data = degdays, mean)
# 
# # Reshape
# degdays <- reshape(degdays, timevar = "indic_nrg", idvar = "geo",
#   ids = "values", direction = "wide")
# names(degdays)[-1] <- c("cooldegdays", "heatdegdays")
# 
# # Merge with metadata
# metadata <- merge(metadata, degdays, by.x = "NUTS3_2021", by.y = "geo",
#   all.x = T, all.y = F, sort = F)
# 
# # Add description
# metadesc <- rbind(metadesc, cbind(metavar = c("cooldegdays", "heatdegdays"), 
#   label = sprintf("%s degree days", c("Cooling", "Heating")), 
#   source = "NUTS3"))

# #----- Social isolation from 2011 census
# isol <- get_eurostat("cens_11ms_r3", time_format = "num",
#   filters = list(age = "TOTAL", sex = "T", time = year))
# 
# # Compute prop of marital status
# isol <- by(isol[,c("marsta", "values")], isol$geo, simplify = F, 
#   function(x) {
#     x$values <- x$values / x$values[x$marsta == "TOTAL"]
#     sum(x$values[x$marsta %in% c("DIV", "SIN", "WID")])
# })
# isol <- data.frame(region = names(isol), isol = unlist(isol))
# 
# # Merge with dataset
# metadata <- merge(metadata, isol, by.x = "NUTS3_2021", by.y = "region",
#   all.x = T, all.y = F, sort = F)
# 
# # Add description
# metadesc <- rbind(metadesc, cbind(metavar = "isol", 
#   label = "Isolation rate", source = "NUTS3 2011 census"))

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

# Reorganize
read_typo <- read_typo[, c("NUTS_ID", type_vars)]
colnames(read_typo)[1] <- "geo"
names(read_typo)[names(read_typo) %in% type_vars] <- tolower(type_vars)

# Merge
metadata <- nuts_merge(metadata, read_typo, level = 3)

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
metadata <- nuts_merge(metadata, deaths, level = 3)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = colnames(deaths)[-1], 
  label = c("Total deaths", sprintf("Deaths in ages %s to %s", 
    substr(colnames(deaths)[-(1:2)], 7, 8), 
    substr(colnames(deaths)[-(1:2)], 9, 10))),
  source = "NUTS3"))

# #---------------------------
# # Add Urban Centre Database
# #---------------------------
# 
# # Read database
# read_ucd <- read.table(
#   sprintf("%s/original/V1-2/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv",
#     path_ucd),
#   sep = ",", header = T, quote = "\"")
# 
# # Select variables
# var_sel <- c(tmean = "E_WR_T_14", greenness = "E_GR_AV14",
#   poll25 = "E_CPM2_T14")
# read_ucd <- read_ucd[,c("ID_HDC_G0", var_sel)]
# names(read_ucd)[-1] <- names(var_sel)
# 
# # Merge with metadata
# metadata2 <- merge(metadata2, read_ucd, by.x = "ID_HDC_G0", by.y = "ID_HDC_G0",
#   all.x = T, all.y = F, sort = F)
# 
# # Add description
# metadesc <- rbind(metadesc, cbind(metavar = names(var_sel),
#   label = c("Mean temperature", "NDVI", "PM25"),
#   source = "Urban Center Databse"))

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
ndvi_sel$ndvi <- rowMeans(ndvi_sel[,-1])

# Remove duplicates
ndvi_sel <- subset(ndvi_sel, !duplicated(URAU_CODE))

# Merge with metadata
metadata <- merge(metadata, ndvi_sel[,c("URAU_CODE", "ndvi")], 
  by.x = "URAU_CODE", by.y = "URAU_CODE",
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "ndvi", 
  label = "NDVI", source = "GEE MODIS"))

#----- PM2.5

# Load data
load(paste0(path_urau, "/data/air_pollutants/city_shapes",
  "/PM25_URAU_LB_2001_2018_city_shapes.RData"))

# Select years and aggregate
pm25_sel <- pm2p5_URAU_LB_2020_DF[,c(1, 
  which(substr(colnames(pm2p5_URAU_LB_2020_DF), 6, 9) %in% year))]
pm25_sel$pm25 <- rowMeans(pm25_sel[,-1])

# Remove duplicates
pm25_sel <- subset(pm25_sel, !duplicated(URAU_CODE))

# Merge with metadata
metadata <- merge(metadata, pm25_sel[,c("URAU_CODE", "pm25")], 
  by.x = "URAU_CODE", by.y = "URAU_CODE",
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "pm25", 
  label = "PM2.5", source = "Atmospheric Composition Analysis Group"))

#----- NO2

# Load data
load(paste0(path_urau, "/data/air_pollutants/city_shapes",
  "/NO2_URAU_LB_1996_2012_city_shapes.RData"))
st_geometry(no2_URAU_LB_2020_sf) <- NULL

# Select years and aggregate
no2_sel <- no2_URAU_LB_2020_sf[,c(1, 
  which(substr(colnames(no2_URAU_LB_2020_sf), 5, 8) %in% year))]
no2_sel$no2 <- rowMeans(no2_sel[,-1])

# Remove duplicates
no2_sel <- subset(no2_sel, !duplicated(URAU_CODE))

# Merge with metadata
metadata <- merge(metadata, no2_sel[,c("URAU_CODE", "no2")], 
  by.x = "URAU_CODE", by.y = "URAU_CODE",
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

# Select years in each series
metaseries <- lapply(era5series, 
  function(x) subset(x, format(date, "%Y") %in% year))

# Compute average temperature
metadata$tmean <- sapply(metaseries, function(x) mean(x$era5landtmean))

# Compute cooling degree days
metadata$cooldegdays <- sapply(metaseries, 
  function(x) sum(unlist(subset(x, era5landtmean > 24, era5landtmean)) - 21)) / 
  length(year)

# Compute heating degree days
metadata$heatdegdays <- sapply(metaseries, 
  function(x) sum(18 - unlist(subset(x, era5landtmean < 15, era5landtmean)))) / 
  length(year)

# Add description of metadata
metadesc <- rbind(metadesc, 
  cbind(metavar = c("tmean", "cooldegdays", "heatdegdays"), 
    label = c("Mean temperature", "Cooling degree days", "Heating degree days"), 
    source = "ERA5land"))

#---------------------------
# Missing values imputation
#---------------------------

#----- Final tidying of data

# Extract number of missings
citymis <- apply(is.na(metadata[,names(metadata) %in% metadesc$metavar]), 
  1, sum)

# # Remove cities with more than 10 missings
# metadata <- metadata[citymis < 10,]

# Reorder data
metadata <- metadata[order(metadata$URAU_CODE),]

#----- Extract variables

# Keep only metavariables
metavar <- metadata[,names(metadata) %in% metadesc$metavar]

# Keep track of missings
imputed <- is.na(metavar)
metadesc$nmis <- apply(imputed, 2, sum)
metadesc$propmis <- round(apply(imputed, 2, mean) * 100)

#----- Impute

# Impute using cart because of complex interactions
meta_imp <- mice(metavar, method = "cart", seed = 12345, print = F)

# Get the imputed dataset (of iter_Sel)
iter_sel <- 5
metadata[,match(metadesc$metavar, names(metadata))] <- 
  complete(meta_imp, iter_sel)

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


