################################################################################
#
#                         MCC-CityEurope
#
#                     Eurostat Data preparation
#
################################################################################

#---------------------------
#  Parameters
#---------------------------

# Years selected. Averaged if several
year <- as.character(2005:2015)

# The priority of urban audit geographical levels
level_priority <- c("CITY", "GREATERCITY", "FUA")

#---------------------------
#  Link datasets
#---------------------------

#----- Link Urban audit to MCC

# Lookup table between MCC and URAU
lookup <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)/",
  "lookup/MCC_URAU.csv"))

# Merge with cities
mcc_urau <- merge(cities, lookup, sort = F, all = F)

# Select code according to level of priority
euro_meta <- mcc_urau[,c("city", "cityname")]
euro_meta[,c("urau_code", "urau_name")] <- NA

for (i in seq_along(level_priority)){
  # Missing values
  missings <- is.na(euro_meta[,c("urau_code", "urau_name")])
  
  # Replace missings
  euro_meta[,c("urau_code", "urau_name")][missings] <- 
    mcc_urau[,c(sprintf("URAU_%s_CODE", level_priority[i]), 
      sprintf("URAU_%s_NAME", level_priority[i]))][missings]
}

# Exclude cities with no match in the Urban audit dataset
nomatch <- is.na(euro_meta$urau_code)
dlist <- dlist[!nomatch]
cities <- cities[!nomatch,]
euro_meta <- euro_meta[!nomatch,]

#----- Link NUTS to MCC

# Lookup table between urban audit and NUTS
lookup_urau_nuts <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Urban Audit (urb_cgc)/lookup/URAU_NUTS2020.csv"))

# Merge with cities
euro_meta <- merge(euro_meta, lookup_urau_nuts[,c("URAU_CODE", "NUTS3_2021")],
  by.x = "urau_code", by.y = "URAU_CODE", sort = F, all.y = F)

# Add NUTS3 description
nuts_desc <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Regional by NUTS classification (reg)/metadata/NUTS_AT_2021.csv"))
euro_meta <- merge(euro_meta, 
  nuts_desc[,c("NUTS_ID", "NAME_LATN", "URBN_TYPE", "COAST_TYPE")], 
  by.x = "NUTS3_2021", by.y = "NUTS_ID", sort = F, all.y = F)

# Managing specific cases
euro_meta[euro_meta$cityname == "London","NUTS3_2021"] <- 
  substr(euro_meta[euro_meta$cityname == "London","NUTS3_2021"], 1, 3)

# Add codes for NUTS2 and NUTS1
euro_meta$NUTS2_2021 <- substr(euro_meta$NUTS3_2021, 1, 4)
euro_meta$NUTS1_2021 <- substr(euro_meta$NUTS3_2021, 1, 3)

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------

path <- "V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)"
datasets <- c("popstr", "env")

#----- Load the necessary datasets

urb_dat <- lapply(datasets, function(dat){
  # Load cities and greater cities dataset
  readcgc <- read.table(sprintf("%s/data/urb_c%s.tsv", path, dat), header = T,
    sep = "\t", na.strings = ": ", check.names = F)
  
  # Load FUA dataset
  readfua <- read.table(sprintf("%s/data/urb_l%s.tsv", path, dat), header = T,
    sep = "\t", na.strings = ": ", check.names = F)
  
  # Put them together
  read <- rbind(readcgc[,c(names(readcgc)[1], year)], 
    readfua[,c(names(readfua)[1], year)])
  
  # Separate city and variables in two columns
  spl_city_var <- strsplit(read[[1]], ",")
  read$var <- sapply(spl_city_var, "[", 1)
  read$citycode <- sapply(spl_city_var, "[", 2)
  read <- read[,-1]
  
  # Convert data into numeric
  yearspl <- apply(read[,as.character(year)], 2, function(x){
    sapply(strsplit(x, " "), "[", 1)
  })
  read[,as.character(year)] <- as.numeric(yearspl)
  
  # Mean of selected years
  read$value <- rowMeans(read[,as.character(year)], na.rm = T)
  
  # Reshape data into wide
  out <- reshape(read[,c("var", "citycode", "value")], timevar = "var", 
    idvar = "citycode", ids = "value", direction = "wide")
  names(out) <- gsub("^value\\.", "", names(out))
  
  out
})
names(urb_dat) <- datasets

#----- Select variables used in the analysis

# cpopstr: Sum prop of pop 65-74 and 74 above 
urb_dat$popstr$prop65_URAU <- rowSums(urb_dat$popstr[,c("DE1028I", "DE1055I")])
urb_dat$popstr <- urb_dat$popstr[,c("citycode", "prop65_URAU")]

# cenv : built-up area
urb_dat$env$builtup_URAU <- rowSums(urb_dat$env[,c("EN5200V", "EN5201V",
  "EN5202V", "EN5203V", "EN5204V")])

# cenv: climate and pollution
urb_dat$env <- urb_dat$env[,c("citycode", "builtup_URAU", "EN1003V", "EN1004V", 
  "EN1002V", "EN1005V", "EN2025V", "EN2026V", "EN2027V")]
names(urb_dat$env)[-(1:2)] <- c("tmean_warm_URAU", "tmean_cold_URAU", 
  "sunshine_URAU", "rainfall_URAU", "acc_o3_URAU", "meanno2_URAU", 
  "meanpm10_URAU")


# Merge the different urban audit datasets together
urb_df <- Reduce(merge, urb_dat)

#----- Link MCC cities 

euro_meta <- merge(euro_meta, urb_df, by.x = "urau_code", by.y = "citycode",
  sort = F, all.y = F, all.x = T)

# Add meta description
sources <- list(prop65_URAU = "cpopstr", builtup_URAU = "cenv",
  tmean_warm_URAU = "cenv", tmean_cold_URAU = "cenv", 
  sunshine_URAU = "cenv", rainfall_URAU = "cenv", acc_o3_URAU = "cenv", 
  meanno2_URAU = "cenv", meanpm10_URAU = "cenv")

descriptions <- list(
  prop65_URAU = "Proportion of population aged 65 and older",
  builtup_URAU = paste0("Share of built-up area:", 
    "residential urban fabric + industrial/commercial/public infrastructure +", 
    "transport infrastructure + other art. area"
  ),
  tmean_warm_URAU = "Mean temperature of warmest month", 
  tmean_cold_URAU = "Mean temperature of coldest month", 
  sunshine_URAU = "Numbers of hours of sunshine per day", 
  rainfall_URAU = "Rainfall (litre / m2)", 
  acc_o3_URAU = "Accumulated ozone concentration in excess 70 µg/m3", 
  meanno2_URAU = "Average concentration of NO2", 
  meanpm10_URAU = "Average concentration of PM10"
)
  

#---------------------------
#  Load Eurostat's regional data
#---------------------------

# Data path
path <- "V:/VolumeQ/AGteam/Eurostat/Regional by NUTS classification (reg)"

# Dataset preparation function
prep_nuts <- function(d, year, varname = "res", firstcol.names = NULL){
  # Select years
  avail_year <- which(names(d) %in% year)
  d <- d[,c(1,avail_year)]
  
  # Coerce values into numeric (if necessary)
  yearspl <- apply(d[,-1, drop = F], 2, function(x){
    sapply(strsplit(as.character(x), " "), "[", 1)
  })
  d[,-1] <- as.numeric(yearspl)
  
  # If necessary average several years
  d[[varname]] <- rowMeans(d[,-1, drop = F], na.rm = T)
  
  # Split first column
  colspl <- do.call("rbind", strsplit(d[[1]], ","))
  colnames(colspl) <- firstcol.names
  
  # Output
  cbind(d[-1], colspl)
}


#----- 65 years old
read <- read.table(sprintf("%s/data/demo_r_pjanind3.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "prop65_NUTS3", 
  firstcol.names = c("Var", "Unit", "Region"))

# Select 65 year old percentage
select <- prep_read[prep_read$Var == "PC_Y65_MAX" & 
    prep_read$Unit == "PC", 
  c("Region", "prop65_NUTS3")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS3_2021", by.y = "Region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$prop65_NUTS3 <- "demo_r_pjanind3"
descriptions$prop65_NUTS3 <- 
  "Proportion of population aged 65 and older"

#----- Life expectancy
read <- read.table(sprintf("%s/data/demo_r_mlifexp.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "lifexp_NUTS2", 
  firstcol.names = c("unit", "sex", "age", "region"))

# Select variable
# here I take Life expectancy at birth ("Y-LT1") for sex total
select <- prep_read[prep_read$sex == "T" & prep_read$age == "Y_LT1",
  c("region", "lifexp_NUTS2")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS2_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$lifexp_NUTS2 <- "demo_r_mlifexp"
descriptions$lifexp_NUTS2 <- 
  "Life expectancy at birth"

#----- GDP
read <- read.table(sprintf("%s/data/nama_10r_3gdp.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "gdp_NUTS3", 
  firstcol.names = c("unit", "region"))

# Select variable: euro / inhabitant
select <- prep_read[prep_read$unit == "EUR_HAB", c("region", "gdp_NUTS3")]

# Merge with dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS3_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$gdp_NUTS3 <- "nama_10r_3gdp"
descriptions$gdp_NUTS3 <- "GDP per capita"

#----- Education level
read <- read.table(sprintf("%s/data/edat_lfse_04.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "educ_NUTS2", 
  firstcol.names = c("sex", "isced11", "age", "unit", "region"))

# Select variable: proportion of active population with ISCED level >=5 (tertiary)
select <- prep_read[prep_read$isced11 == "ED5-8" & prep_read$sex == "T" & 
    prep_read$age == "Y25-64", 
  c("region", "educ_NUTS2")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS2_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$educ_NUTS2 <- "edat_lfse_04"
descriptions$educ_NUTS2 <- 
  "Proportion of active population (25-64) with ISCED level 5 and above (tertiary)"

#----- Unemployment rate
read <- read.table(sprintf("%s/data/lfst_r_lfu3rt.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "unempl_NUTS2", 
  firstcol.names = c("unit", "age", "sex", "region"))

# Select variable: unemployment rate for active pop (20-64)
select <- prep_read[prep_read$sex == "T" & prep_read$age == "Y20-64",
  c("region", "unempl_NUTS2")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS2_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$unempl_NUTS2 <- "lfst_r_lfu3rt"
descriptions$unempl_NUTS2 <- 
  "Unemployment rate in active population (20-64)"

#----- Severe material deprivation
read <- read.table(sprintf("%s/data/ilc_mddd21.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "depriv_NUTS1", 
  firstcol.names = c("unit", "region"))

# Select variable: percentage of severe material deprivation
select <- prep_read[prep_read$unit == "PC", c("region", "depriv_NUTS1")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS1_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$depriv_NUTS1 <- "ilc_mddd21"
descriptions$depriv_NUTS1 <- 
  "Severe material deprivation rate"

#----- Hospital bed rates
read <- read.table(sprintf("%s/data/hlth_rs_bdsrg.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "bedrates_NUTS2", 
  firstcol.names = c("unit", "facility", "region"))

# Select variable: Number of hospital bed / 100 000 inhabitants
select <- prep_read[prep_read$unit == "P_HTHAB" & 
    prep_read$facility == "HBEDT",
  c("region", "bedrates_NUTS2")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS2_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$bedrates_NUTS2 <- "hlth_rs_bdsrg"
descriptions$bedrates_NUTS2 <- 
  "Total number of hospital beds / 100 000 hab"

#----- Degree days
read <- read.table(sprintf("%s/data/nrg_chddr2_a.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "degreedays_NUTS2", 
  firstcol.names = c("unit", "indic", "region"))

# Reshape to separate cooling and heating degree days
select <- reshape(prep_read[,c("region", "indic", "degreedays_NUTS2")],
  direction = "wide", idvar = "region", timevar = "indic")
colnames(select)[-1] <- c("cooldegdays_NUTS2", "heatdegdays_NUTS2")

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS2_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources[c("cooldegdays_NUTS2", "heatdegdays_NUTS2")] <- 
  rep(list("nrg_chddr2_a"), 2)
descriptions[c("cooldegdays_NUTS2", "heatdegdays_NUTS2")] <- 
  sprintf("Total number of %s degree days (%i C)", c("cooling", "heating"),
    c(24, 15))

#----- Total population
read <- read.table(sprintf("%s/data/demo_r_pjangrp3.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "pop_NUTS3", 
  firstcol.names = c("sex", "unit", "age", "region"))

# Select variable : total population
select <- prep_read[prep_read$sex == "T" & prep_read$age == "TOTAL",
  c("region", "pop_NUTS3")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS3_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$pop_NUTS3 <- "demo_r_pjangrp3"
descriptions$pop_NUTS3 <- "Total population"

#----- Population density
read <- read.table(sprintf("%s/data/demo_r_d3dens.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "popdens_NUTS3", 
  firstcol.names = c("unit", "region"))

# Merge with the dataset
euro_meta <- merge(euro_meta, prep_read[,c("region", "popdens_NUTS3")], 
  by.x = "NUTS3_2021", by.y = "region", all.x = T, all.y = F, sort = F)

# Add meta description
sources$popdens_NUTS3 <- "demo_r_d3dens"
descriptions$popdens_NUTS3 <- 
  "Population density (person / km2)"

#----- Urbanised area
read <- read.table(sprintf("%s/data/lan_lcv_art.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "urbshare_NUTS2", 
  firstcol.names = c("unit", "landcover", "region"))

# Select variable: artifical land cover
select <- prep_read[prep_read$unit == "PC" & prep_read$landcover == "LCA",
  c("region", "urbshare_NUTS2")]

# Merge with the dataset
euro_meta <- merge(euro_meta, select, by.x = "NUTS2_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$urbshare_NUTS2 <- "lan_lcv_art"
descriptions$urbshare_NUTS2 <- 
  "Share of land covered by artificial surface"

#----- Green / blue areas
read <- read.table(sprintf("%s/data/lan_lcv_ovw.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Prepare dataset
prep_read <- prep_nuts(read, year = year, varname = "lcov_NUTS2",
  firstcol.names = c("unit", "landcover", "region"))

# Select variable: green area share (cropland, woodland, shrubland, grassland)
selectgr <- prep_read[prep_read$unit == "PC" & 
    prep_read$landcover %in% c("LCB", "LCC", "LCD", "LCE"),
  c("region", "lcov_NUTS2")]
selectgr <- aggregate(lcov_NUTS2 ~ region, data = selectgr, sum)

# Select variable: blue area share (water)
selectbl <- prep_read[prep_read$unit == "PC" & 
    prep_read$landcover %in% c("LCG"),
  c("region", "lcov_NUTS2")]

# Merge with the dataset
select <- merge(selectgr, selectbl, by = "region", all = T)
colnames(select)[-1] <- c("greenshare_NUTS2", "blueshare_NUTS2")

euro_meta <- merge(euro_meta, select, by.x = "NUTS2_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources[c("greenshare_NUTS2", "blueshare_NUTS2")] <- 
  rep(list("lan_lcv_ovw"), 2)
descriptions[c("greenshare_NUTS2", "blueshare_NUTS2")] <- 
  c(paste0("Share of land covered by green areas: cropland,",
      "woodland, shrubland or grassland"),
    "Share of land covered by water")

#----- Social isolation: 2011
read <- read.table(sprintf("%s/data/cens_11ms_r3.tsv", path), header = T,
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
  isol_NUTS3 = unlist(select_prop))

# Merge with dataset
euro_meta <- merge(euro_meta, select_prop, by.x = "NUTS3_2021", by.y = "region",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources$isol_NUTS3 <- "cens_11ms_r3"
descriptions$isol_NUTS3 <- 
  paste0("Proportion of persons with marital status (from 2011 census):",
    "single, divorced or widowed")

#---------------------------
# Meta descriptions
#---------------------------

# Unlist description and source
attr(euro_meta, "source") <- unlist(sources)
attr(euro_meta, "description") <- unlist(descriptions)

# Missings for each variable
attr(euro_meta, "miss_vars") <- apply(euro_meta[,-(1:10)], 2, function(x) sum(is.na(x)))

# Missings for each city
attr(euro_meta, "miss_city") <- apply(euro_meta[,-(1:10)], 1, function(x) sum(is.na(x)))
names(attr(euro_meta, "miss_city")) <- euro_meta$city

# Sort meta data as cities
ord <- match(cities$city, euro_meta$city)
euro_meta <- euro_meta[ord,]
rownames(euro_meta) <- NULL

#---------------------------
#  Save data
#---------------------------

save(dlist, cities, countries, euro_meta, 
  file = "Data/prep_data.RData")