################################################################################
#
#                         MCC-CityEurope
#
#                     Eurostat Data preparation
#
################################################################################

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------

path <- "V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)"
year <- as.character(2000:2019)
datasets <- c("popstr", "env")

#----- Lookup table
lookup <- read.csv(sprintf("%s/lookup/MCC_URAU.csv", path))

# Merge with cities
cities <- merge(cities, lookup, sort = F, all = F)

# Exclude cities with no match in the Urban audit dataset
nomatch <- is.na(cities$URAU_CITY_CODE) & is.na(cities$URAU_FUA_CODE) &
  is.na(cities$URAU_GREATERCITY_CODE)
dlist <- dlist[!nomatch]
cities <- cities[!nomatch,]

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
urb_dat$popstr$prop65 <- rowSums(urb_dat$popstr[,c("DE1028I", "DE1055I")])
urb_dat$popstr <- urb_dat$popstr[,c("citycode", "prop65")]

# cenv : temperature / pollution / green space/ built-up
urb_dat$env$builtup <- rowSums(urb_dat$env[,c("EN5200V", "EN5201V",
  "EN5202V", "EN5203V", "EN5204V")])
urb_dat$env <- urb_dat$env[,c("citycode", "builtup", "EN1003V", "EN1004V", 
  "EN1002V", "EN1005V", "EN2025V", "EN2026V", "EN2027V")]
names(urb_dat$env)[-(1:2)] <- c("tmean_warm", "tmean_cold", "sunshine", 
  "rainfall", "acc_o3", "meanno2", "meanpm10")


#----- Merge everything together and link with MCC
# Merge the different urban audit datasets together
urb_df <- Reduce(merge, urb_dat)

# Link MCC cities with city
mcc_link <- merge(cities, urb_df, 
  by.x = "URAU_CITY_CODE", by.y = "citycode", 
  all.x = T, all.y = F, sort = F)
# Link with FUA
mcc_link <- merge(mcc_link, urb_df, 
  by.x = "URAU_FUA_CODE", by.y = "citycode", 
  all.x = T, all.y = F, sort = F, suffixes = c("", ".fua"))
# Link with greater city
mcc_link <- merge(mcc_link, urb_df, 
  by.x = "URAU_GREATERCITY_CODE", by.y = "citycode", 
  all.x = T, all.y = F, sort = F, suffixes = c("", ".gc"))

# Now we take city-level data
mcc_urb <- mcc_link[,c("city", "cityname", names(urb_df)[-1])]
citymiss <- apply(is.na(mcc_urb[,-(1:2)]), 2, sum)

# When we have NA for the variable, we try to take at greater city level
for (i in 1:(ncol(urb_df) - 1)){ 
  missing <- is.na(mcc_urb[,i + 2])
  mcc_urb[missing,i + 2] <- 
    mcc_link[missing, names(mcc_link)[grep("\\.gc", names(mcc_link))][i]]
  metadat[i, "city"] <- sum(!missing)
}
greatcitymiss <- apply(is.na(mcc_urb[,-(1:2)]), 2, sum)

# And ultimately the FUA level
for (i in 1:(ncol(urb_df) - 1)){ 
  missing <- is.na(mcc_urb[,i + 2])
  mcc_urb[missing,i + 2] <- 
    mcc_link[missing, names(mcc_link)[grep("\\.fua", names(mcc_link))][i]]
  metadat[i, "greatercity"] <- sum(!missing)
}
fuamiss <- apply(is.na(mcc_urb[,-(1:2)]), 2, sum)

#----- Meta metadata (meta^2 data?)
metacount <- data.frame(row.names = names(mcc_urb)[-(1:2)], 
  city = nrow(mcc_urb) - citymiss, greatercity = citymiss - greatcitymiss,
  fua = greatcitymiss - fuamiss, missings = fuamiss)

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------

path <- "V:/VolumeQ/AGteam/Eurostat/Regional by NUTS classification (reg)"
year <- as.character(2015)
level_priority <- c("CITY", "GREATERCITY", "FUA")

#----- Lookup between MCC and NUTS
# Lookup between MCC and urban audit
lookup_MCC_urau <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Urban Audit (urb_cgc)/lookup/MCC_URAU.csv"))

# Merge with cities
cities <- merge(cities, lookup_MCC_urau, sort = F, all = F)

# Choose the geographical level
cities$urau_code <- cities[,sprintf("URAU_%s_CODE", level_priority[1])]
cities$urau_code[is.na(cities$urau_code)] <- cities[is.na(cities$urau_code),
  sprintf("URAU_%s_CODE", level_priority[2])]
cities$urau_code[is.na(cities$urau_code)] <- cities[is.na(cities$urau_code),
  sprintf("URAU_%s_CODE", level_priority[3])]

# Remove URAU variables
cities[,grep("URAU", names(cities))] <- NULL

# Exclude cities with no match in the Urban audit dataset
nomatch <- is.na(cities$urau_code)
dlist <- dlist[!nomatch]
cities <- cities[!nomatch,]

# Lookup between urban audit and NUTS
lookup_urau_nuts <- read.csv(paste0("V:/VolumeQ/AGteam/Eurostat/", 
  "Urban Audit (urb_cgc)/lookup/URAU_NUTS2020.csv"))

# Merge with cities
cities <- merge(cities, lookup_urau_nuts[,c("URAU_CODE", "NUTS3_2021")],
  by.x = "urau_code", by.y = "URAU_CODE", sort = F, all.y = F)

# Prepare Urban dataset
urb_dat <- cities[,c("cityname", "NUTS3_2021")]

#----- 65 years old
read <- read.table(sprintf("%s/data/demo_r_pjanind3.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Split first column
colspl <- strsplit(read[[1]], ",")
read$var <- sapply(colspl, "[", 1)
read$unit <- sapply(colspl, "[", 2)
read$region <- sapply(colspl, "[", 3)

# Select variable (PC_Y65_MAX with PC unit) and years
avail_year <- year[year %in% names(read)]
read <- read[read$var == "PC_Y65_MAX" & read$unit == "PC",
  c("region", avail_year)]

# Coerce values into numeric
yearspl <- apply(read[,avail_year, drop = F], 2, function(x){
  sapply(strsplit(x, " "), "[", 1)
})
read[,avail_year] <- as.numeric(yearspl)

# If necessary average several years
read$prop65 <- rowMeans(read[,-1, drop = F])
read[, avail_year] <- NULL

# Merge with the dataset
urb_dat <- merge(urb_dat, read, by.x = "NUTS3_2021", by.y = "region",
  all.y = F)

#----- Life expectancy
read <- read.table(sprintf("%s/data/demo_r_mlifexp.tsv", path), header = T,
  sep = "\t", na.strings = c(": ", ":"), check.names = F)

# Split first column
colspl <- strsplit(read[[1]], ",")
read$unit <- sapply(colspl, "[", 1)
read$sex <- sapply(colspl, "[", 2)
read$age <- sapply(colspl, "[", 3)
read$region <- sapply(colspl, "[", 4)

# Select variable and years
# here I take Life expectancy at birth ("Y-LT1") for sex total
avail_year <- year[year %in% names(read)]
read <- read[read$sex == "T" & read$age == "Y_LT1",
  c("region", avail_year)]

# Coerce values into numeric
yearspl <- apply(read[,avail_year, drop = F], 2, function(x){
  sapply(strsplit(x, " "), "[", 1)
})
read[,avail_year] <- as.numeric(yearspl)

# If necessary average several years
read$life_expectancy <- rowMeans(read[,-1, drop = F])
read[, avail_year] <- NULL

# Merge with the dataset
urb_dat <- merge(urb_dat, read, by.x = "NUTS3_2021", by.y = "region",
  all.y = F, all.x = T)