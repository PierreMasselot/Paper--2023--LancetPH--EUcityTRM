################################################################################
#
#                       Exhaustion Level 1
#
#                         Prep UK data
#
################################################################################

library(data.table)
library(sf)

#--------------------------
#  Parameters
#--------------------------

# Dates for analysis
datestart <- as.Date("1990/01/01")
dateend <- as.Date("2016/12/31")

# Define age groups (nested)
age_cut <- c(0, 65, 75)
age_labs <- c("main", "65p", "75p")

#--------------------------
#  List of BUAs
#--------------------------

# Lookup between Built-Up Areas and LSOAs
buapath <- "V:/VolumeQ/AGteam/ONS/geography/lookup"
# buapath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/ONS/geography/lookup"

bualookup <- read.table(paste0(buapath, "/Lower_Layer_Super_Output_Area__2011_",
  "_to_Built-up_Area_Sub-division_to_Built-up_Area_to_Local_Authority",  
  "_District_to_Region__December_2011__Lookup_in_England_and_Wales.csv"),
  header = T, sep = ",", quote = "\"", fileEncoding = "UTF-8-BOM")

# Load LSOA population
poppath <- "V:/VolumeQ/AGteam/NOMIS/census2011/KeyStatistics/"
# poppath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/NOMIS/census2011/KeyStatistics/"
pop <- read.csv(paste0(poppath, "KS102EW.csv"))
names(pop)[5] <- "Total"

# Merge them
pop <- merge(pop[,c(3,5)], bualookup[,c(1, 5:6)], 
  by.x = "geography.code", by.y = "LSOA11CD")
pop <- subset(pop, BUA11NM != "")

# Total population by Built-Up Area
buapop <- aggregate(Total ~ BUA11CD + BUA11NM, data = pop, sum)

# Keep BUAs with > 100,000 residents
bualist <- subset(buapop, Total > 100000)
bualookup <- subset(bualookup, BUA11CD %in% bualist$BUA11CD)

#--------------------------
#  Load ONS mortality data
#--------------------------

# Path of mortality data
deathpath <- "V:/VolumeQ/AGteam/ONS/mortality/data"
# deathpath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/ONS/mortality/data"

onsdeath <- as.data.table(readRDS(paste0(deathpath, "/ONSmortality.RDS")))

# Remove records too old, recent or of too young subjects
onsdeath <- subset(onsdeath, DOD >= datestart & DOD <= dateend)
onsdeath <- subset(onsdeath, ageinyrs >= min(age_cut))

# Merg BUA info and keep only those in selected list
onsdeath <- merge(onsdeath, bualookup[,c("LSOA11CD", "BUA11CD", "BUA11NM")],
  by.x = "lsoa", by.y = "LSOA11CD")
onsdeath <- subset(onsdeath, BUA11CD %in% bualist$BUA11CD)

# Aggregate by BUA, date, cause and age class 
outcome_list <- list()
for (a in seq_along(age_labs)){
  # All causes
  vname <- sprintf("all_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a], .N, 
    by = list(city = BUA11CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Cardiovascular diseases
  vname <- sprintf("cvd_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a] & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 39 & 
        as.numeric(substr(cause, 1, 2)) < 46) |
        (dodyr > 2000 & substr(cause, 1, 1) == "I")), 
    .N, by = list(city = BUA11CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Respiratory diseases
  vname <- sprintf("resp_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a] & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 46 & 
          as.numeric(substr(cause, 1, 2)) < 52) |
          (dodyr > 2000 & substr(cause, 1, 1) == "J")), 
    .N, by = list(city = BUA11CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Cardiorespiratory diseases
  vname <- sprintf("cvresp_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a] & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 39 & 
          as.numeric(substr(cause, 1, 2)) < 52) |
          (dodyr > 2000 & substr(cause, 1, 1) %in% c("I", "J"))), 
    .N, by = list(city = BUA11CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
}

# Merge all series with all possible combinations of date / city
citydatgrid <- as.data.table(expand.grid(
  date = seq.Date(datestart, dateend, "days"),
  city = bualist$BUA11CD))
alloutcomes <- Reduce(function(x,y) merge(x, y, all = T), 
  outcome_list, init = citydatgrid)

# Fill NAs (no record) with 0s
alloutcomes[is.na(alloutcomes)] <- 0

# Split the data by city
dlist <- split(alloutcomes[, -"city"], 
  alloutcomes[,"city"])

#--------------------------
#  Load temperature data
#--------------------------

# Load LSOA level temp data
tmeanpath <- "V:/VolumeQ/AGteam/ONSmortality/processed/LSOA19812018/"
# tmeanpath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/ONSmortality/processed/LSOA19812018/"

# Dates of tmean data
tmeandates <- seq(as.Date("1981/1/1"), as.Date("2018/12/31"), "days")

#----- Load LSOA temp data for each BUA
for (i in seq_along(dlist)){
  # LSOAs inside the BUA
  lsoalist <- unlist(subset(bualookup, BUA11CD == names(dlist)[i], LSOA11CD))
  
  # Load and append them
  templist <- lapply(lsoalist, function(lsoa) {
    temp <- data.table(readRDS(paste0(tmeanpath, lsoa, ".Rds")))
    cbind(data.table(LSOA11CD = lsoa), date = tmeandates, temp)
  }) 
  alltemp <- do.call(rbind, templist)
  
  # Average by date
  tmean <- alltemp[,.(tmean = mean(tmean)), by = date]
  
  # Merge to dlist
  dlist[[i]] <- merge(dlist[[i]], tmean, all.x = T, all.y = F)
}

#--------------------------
#  City characteristics
#--------------------------

#----- Load geographical information

# Read BUA polygon shapefiles
source <- "V:/VolumeQ/AGteam/ONS/geography/shapefiles/BUA"
file <- "Built-up_Areas_(December_2011)_Boundaries_V2"
file.copy(paste0(source, "/", file, ".zip"), getwd())
unzip(zipfile = paste0(file,".zip"), exdir = getwd())
buashp <- st_read(paste0(file, ".shp"))[2]
file.remove(list.files()[grep(file, list.files(), fixed = T)])

# Keep only the BUA above
buashp <- subset(buashp, bua11cd %in% bualist$BUA11CD)

# Compute centroid of each BUA
buapt <- st_centroid(buashp)

# Transofrm in lon/lat
buapt <- st_transform(buapt, 4326)
bualonlat <- st_coordinates(buapt)
colnames(bualonlat) <- c("lon", "lat")

#----- Create city meta object

cities <- data.frame(city = names(dlist), 
  cityname = bualist$BUA11NM[match(names(dlist), bualist$BUA11CD)],
  bualonlat[match(names(dlist), buapt$bua11cd),])


#--------------------------
#  Save
#--------------------------

# Remove data.table classes for consistency
dlist <- lapply(dlist, as.data.frame)

# Save
save(dlist, cities, file = "data/ukdata.RData")

