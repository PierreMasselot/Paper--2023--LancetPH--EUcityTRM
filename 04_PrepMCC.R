################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
#
# (Non-reproducible) R Code
# Part 4: Load and prepare MCC mortality data
#
################################################################################

#-----------------------------
# Link with MCC
#-----------------------------

# Lookup table between MCC and URAU
urau_mcc_age <- read.table(sprintf("%s/lookup/URAU_MCC_AgeCause_20220505.csv", 
    path_euro),
  header = T, sep = ";", quote = "\"")
urau_mcc_all <- read.table(sprintf("%s/lookup/URAU_MCCdata_20220505.csv", 
    path_euro),
  header = T, sep = ";", quote = "\"")

# Subset countries
urau_mcc_age <- subset(urau_mcc_age, country %in% mcc_countries)
urau_mcc_all <- subset(urau_mcc_all, country %in% mcc_countries)

# Merge MCC age dataset to list of cities
metadata <- merge(metadata,
  urau_mcc_age[,c("URAU_CODE", "mcc_code", "cityname", "country")],
  by = "URAU_CODE", all.x = T, all.y = F)

# When there is no match with MCC age dataset, search in MCC all
matchall <- match(subset(metadata, is.na(mcc_code), URAU_CODE, drop = T),
  urau_mcc_all$URAU_CODE)
metadata[is.na(metadata$mcc_code), c("mcc_code", "cityname")] <- 
  urau_mcc_all[matchall, c("mcc_code", "cityname")]

# Add indicator for whether it is in MCC
metadata$inmcc <- !is.na(metadata$mcc_code)

# Remove some double matches
metadata <- subset(metadata, !mcc_code %in% c("sttc.uk9020", "pool.uk9020", 
  "glln.uk9020"))

#---------------------------
#  Load MCC data
#---------------------------

path <- "V:/VolumeQ/AGteam/MCCdata/data"

#----- Load age causes MCC dataset
load(sprintf("%s/MCC_age_classes/MCC_AgeCause_20220505.RData", path))

# Select cities in the meta dataset constructed
dlist_eu <- dlist[cities$city %in% na.omit(metadata$mcc_code)]
cities_eu <- cities[cities$city %in% na.omit(metadata$mcc_code),]

#----- Add countries with all age group
load(sprintf("%s/MCC_all/MCCdata_20220505.RData", path))

# Select cities 
citylistnoage <- setdiff(na.omit(metadata$mcc_code), cities_eu$city)
dlist_noage <- dlist[cities$city %in% citylistnoage]
cities_noage <- cities[cities$city %in% citylistnoage,]

# Put everything together
dlist <- c(dlist_eu, dlist_noage)
cities <- rbind(cities_eu, cities_noage[,names(cities_eu)])

# Keep only those inspecified countries
dlist <- dlist[cities$country %in% mcc_countries]
cities <- cities[cities$country %in% mcc_countries,]

#----- Tidy data

for(i in seq(dlist)) {
  # Exclude everything before starting year
  dlist[[i]] <- dlist[[i]][dlist[[i]]$year %in% yearanalysis,]
  
  # Reorder by date
  dlist[[i]] <- dlist[[i]][order(dlist[[i]]$date),]
}

#---------------------------
#  Link ERA5land data
#---------------------------

for(nm in cities$city) {
  uraucd <- subset(metadata, mcc_code == nm, URAU_CODE, drop = T)
  dlist[[nm]] <- merge(dlist[[nm]],
    era5series[[uraucd]], by = "date")
}

#---------------------------
#  Save metadata
#---------------------------

# Reorder everything by URAU_CODE
metadata <- metadata[order(metadata$URAU_CODE),]
imputed <- imputed[order(rownames(imputed)),]
metageo <- metageo[order(metageo$URAU_CODE),]
era5series <- era5series[order(names(era5series))]

# Reorder MCC objects accordingly
ord <- match(na.omit(metadata$mcc_code), cities$city)
cities <- cities[ord,]
dlist <- dlist[ord]

# Save metadata
write.csv(metadata, file = gzfile("data/metadata.csv.gz"), 
  row.names = F)
write.csv(metacityyear, file = gzfile("data/metacityyear.csv.gz"), 
  row.names = F)
write.csv(metadesc, file = gzfile("data/metadesc.csv.gz"), 
  row.names = F)

# Save era5 series
era5out <- era5_df |> arrange(URAU_CODE, date) |> subset(select = -Year)
write.csv(era5out, file = gzfile("data/era5series.csv.gz"), row.names = F)

