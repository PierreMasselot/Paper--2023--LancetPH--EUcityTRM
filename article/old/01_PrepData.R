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
# path <- paste0("C:/Users/PierreMasselot/Filr/Net Folders/",
#   "StorageOnDemand Q/AGteam/MCCdata/data/MCC_all")

#----- Load age causes MCC dataset
load(sprintf("%s/MCC_age_classes/MCC_AgeCause_20200907.RData", path))

# Select countries
sel_country <- sort(c('cze9415', 'est9718', 'fnl9411', 'fra0014',  
  'grc0110', 'irl8407', 'ita0110', 'nor6916', 'por8012', 
  'spa0913', 'sui9513', 'swe9016', 'uk9016'))

dlist_eu <- dlist[cities$country %in% sel_country]
cities_eu <- cities[cities$country %in% sel_country,]
countries_eu <- countries[countries$country %in% sel_country,]

#----- Add countries with all age group
load(sprintf("%s/MCC_all/MCCdata_20210407.RData", path))

# We do not select mld0110 because absent in eurostat data
sel_country <- sort(c('ger9315', 'net9516c', 'rom9416'))

dlist_noage <- dlist[cities$country %in% sel_country]
cities_noage <- cities[cities$country %in% sel_country,]
countries_noage <- countries[countries$country %in% sel_country,]

# Put everything together
dlist <- c(dlist_eu, dlist_noage)
cities <- rbind(cities_eu, cities_noage[,names(cities_eu)])
countries <- rbind(countries_eu, countries_noage[,names(countries_eu)])

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
ind1 <- c("cityname","country","countryname")
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
