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

path <- "V:/VolumeQ/AGteam/MCCdata/data/MCC_all"
# path <- paste0("C:/Users/PierreMasselot/Filr/Net Folders/",
#   "StorageOnDemand Q/AGteam/MCCdata/data/MCC_all")

load(sprintf("%s/MCCdata_20200907.RData", path))

#----- Select european countries -----
# We do not select mld0110 because absent in eurostat data
# We use only cities 
sel_country <- sort(c('cze9415', 'est9718', 'fnl9414', 'fra0014', 'ger9315', 
  'grc0110', 'irl8407', 'ita0615', 'net9516c', 'nor6918', 'por8018', 
  'rom9416', 'spa9014', 'sui9513', 'swe9016', 'uk9016'))

dlist <- dlist[cities$country %in% sel_country]
cities <- cities[cities$country %in% sel_country,]
countries <- countries[countries$country %in% sel_country,]

#----- Exclusions

# Exclude South Bohemia region
dlist[cities$city == 'sthb.cze9415'] <- NULL
cities <- cities[-which(cities$city == "sthb.cze9415"),]

# Include only 1985 on
for(i in seq(dlist)) dlist[[i]] <- dlist[[i]][dlist[[i]]$year >= 1985,]

#----- Reorder and clean factors

# Reorder by country name
ord <- with(cities, order(as.character(countryname), 
  as.character(cityname)))

cities <- cities[ord,]
dlist <- dlist[ord]
countries <- countries[order(as.character(countries$countryname)),]

# Remove unused levels in factors
ind1 <- c("cityname","country","countryname", "kgclzone","kgclzone1","kgclzone2")
cities[ind1] <- lapply(cities[ind1], droplevels)

#---------------------------
#  Load ERA data
#---------------------------

# Path of reanalysis data
path <- "V:/VolumeQ/AGteam/MCCdata/reanalysis"
# path <- paste0("C:/Users/PierreMasselot/Filr/Net Folders/",
#   "StorageOnDemand Q/AGteam/MCCdata/reanalysis")

# Load data
era5 <- readRDS(paste0(path, "/processed/ERA5_daily_1979_202007_mcc20200907.Rds"))
era5 <- subset(era5, select = c("era5_code","date","temp_mean"))

# Load Look-up table to link with MCC
era5lookup <- readRDS(paste0(path, 
  "/lookup_table/lookup_ERA5_MCCdata_20200907.RDS"))
# Merge with lookup table
era5 <- merge(era5, era5lookup[,c("city","era5_code")], by = "era5_code",
  allow.cartesian = TRUE)
names(era5)[names(era5)=="temp_mean"] <- "era5tmean"

#----- Merge with MCC data
for(nm in cities$city) {
  dlist[[nm]] <- merge(dlist[[nm]], 
    subset(era5, city == nm, c("date", "era5tmean")), by = "date")
}


#---------------------------
#  Save
#---------------------------
save(dlist, cities, countries, file = "Data/01_MCCdata.RData")