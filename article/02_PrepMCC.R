################################################################################
#
#                         MCC-CityEurope
#
#                       MCC data preparation
#
################################################################################

#---------------------------
#  Load MCC data
#---------------------------

path <- "V:/VolumeQ/AGteam/MCCdata/data"

#----- Load age causes MCC dataset
load(sprintf("%s/MCC_age_classes/MCC_AgeCause_20200907.RData", path))

# Select cities in the meta dataset constructed
dlist_eu <- dlist[cities$city %in% na.omit(metadata$mcc_code)]
cities_eu <- cities[cities$city %in% na.omit(metadata$mcc_code),]

#----- Add countries with all age group
load(sprintf("%s/MCC_all/MCCdata_20210407.RData", path))

# Select cities 
citylistnoage <- setdiff(na.omit(metadata$mcc_code), cities_eu$city)
dlist_noage <- dlist[cities$city %in% citylistnoage]
cities_noage <- cities[cities$city %in% citylistnoage,]

# Put everything together
dlist <- c(dlist_eu, dlist_noage)
cities <- rbind(cities_eu, cities_noage[,names(cities_eu)])

#----- Tidy data

# Exclude everything before starting year
for(i in seq(dlist)) dlist[[i]] <- dlist[[i]][dlist[[i]]$year >= yearstart,]

# Reorder as in metadata
ord <- match(na.omit(metadata$mcc_code), cities$city)

cities <- cities[ord,]
dlist <- dlist[ord]

# Remove unused levels in factors
ind1 <- c("cityname","country","countryname")
cities[ind1] <- lapply(cities[ind1], droplevels)

#---------------------------
#  Link ERA5land data
#---------------------------

# # Path of reanalysis data
# path <- "V:/VolumeQ/AGteam/MCCdata/reanalysis"
# # path <- paste0("C:/Users/PierreMasselot/Filr/Net Folders/",
# #   "StorageOnDemand Q/AGteam/MCCdata/reanalysis")
# 
# # Load data
# era5 <- readRDS(paste0(path, 
#   "/processed/ERA5_daily_1979_202007_mcc20200907.Rds"))
# era5 <- subset(era5, select = c("era5_code","date","temp_mean"))
# 
# # Load Look-up table to link with MCC
# era5lookup <- readRDS(paste0(path, 
#   "/lookup_table/lookup_ERA5_MCCdata_20200907.RDS"))
# # Merge with lookup table
# era5 <- merge(era5, era5lookup[,c("city","era5_code")], by = "era5_code",
#   allow.cartesian = TRUE)
# names(era5)[names(era5)=="temp_mean"] <- "era5tmean"
# 
# #----- Merge with MCC data
# for(nm in cities$city) {
#   dlist[[nm]] <- merge(dlist[[nm]], 
#     subset(era5, city == nm, c("date", "era5tmean")), by = "date")
# }

for(nm in cities$city) {
  uraucd <- subset(metadata, mcc_code == nm, URAU_CODE, drop = T)
  dlist[[nm]] <- merge(dlist[[nm]],
    era5series[[uraucd]], by = "date")
}

#---------------------------
#  Save all data
#---------------------------

save(dlist, cities, metadata, metadesc, imputed, metageo, meta_imp, era5series,
  file = "data/Alldata.RData")