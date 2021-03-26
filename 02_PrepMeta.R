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
year <- as.character(1990:2018)

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
sources <- c(prop65_URAU = "cpopstr", builtup_URAU = "cenv",
  tmean_warm_URAU = "cenv", tmean_cold_URAU = "cenv", 
  sunshine_URAU = "cenv", rainfall_URAU = "cenv", acc_o3_URAU = "cenv", 
  meanno2_URAU = "cenv", meanpm10_URAU = "cenv")

descriptions <- c(
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

path_nuts <- "V:/VolumeQ/AGteam/Eurostat/Regional by NUTS classification (reg)"

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
euro_meta <- Reduce(function(x, y){
    merge(x, y, 
      by.x = sprintf("NUTS%i_2021", attr(y, "level")), by.y = "geo",
      sort = F, all.x = T, all.y = F)
  }, 
  nuts_vars, init = euro_meta, 
  accumulate = T
)[[length(load_desc) + 1]]

# Add names
namvec <- sprintf("%s_NUTS%i", names(load_desc), 
  sapply(load_desc, "[[", "level"))
names(euro_meta)[ncol(euro_meta) - length(load_desc):1 + 1] <- 

# Add descriptions and source
sources[namvec] <- sapply(load_desc, "[[", "file")
descriptions[namvec] <- sapply(load_desc, "[[", "desc")


#----- Add social isolation from 2011 census
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
sources["isol_NUTS3"] <- "cens_11ms_r3"
descriptions["isol_NUTS3"] <- 
  paste0("Proportion of persons with marital status (from 2011 census):",
    "single, divorced or widowed")
  
#---------------------------
# Temperature indicators
#---------------------------

# Compute temperature indicators for MCC cities
temp_meta <- sapply(dlist, function(d){
  c(tmean = mean(d$era5tmean), 
    trange = diff(range(d$era5tmean, na.rm = T)),
    cooldegdays = sum(pmax(0, d$era5tmean - 24), na.rm = T),
    heatdegdays = sum(pmax(0, 15 - d$era5tmean), na.rm = T)
  )}
)

# Coerce it as a proper data.frame
temp_meta_df <- as.data.frame(t(temp_meta))
names(temp_meta_df) <- sprintf("%s_MCC", names(temp_meta_df))
temp_meta_df$city <- rownames(temp_meta_df)
rownames(temp_meta_df) <- NULL

# Merge it with the euro_meta data.frame
euro_meta <- merge(euro_meta, temp_meta_df, by = "city",
  all.x = T, all.y = F, sort = F)

# Add meta description
sources[names(temp_meta_df)[-ncol(temp_meta_df)]] <- "dlist"
descriptions[names(temp_meta_df)[-ncol(temp_meta_df)]] <- c(
  "Average temperature", "Temperature range", 
  "Total number of cooling degree days (24 C)",
  "Total number of heating degree days (15 C)"
)

#---------------------------
# Meta descriptions
#---------------------------

# Unlist description and source
attr(euro_meta, "source") <- unlist(sources)
attr(euro_meta, "description") <- unlist(descriptions)

# Missings for each variable
attr(euro_meta, "miss_vars") <- apply(euro_meta[,-(1:10)], 2, 
  function(x) sum(is.na(x)))

# Missings for each city
attr(euro_meta, "miss_city") <- apply(euro_meta[,-(1:10)], 1, 
  function(x) sum(is.na(x)))
names(attr(euro_meta, "miss_city")) <- euro_meta$city

# Sort meta data as cities
ord <- match(cities$city, euro_meta$city)
euro_meta <- euro_meta[ord,]
rownames(euro_meta) <- NULL

#---------------------------
#  Save data
#---------------------------

# save(dlist, cities, countries, euro_meta, 
#   file = "Data/prep_data.RData")