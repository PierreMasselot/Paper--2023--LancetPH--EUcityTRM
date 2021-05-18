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

# Define sex labs
sex_labs <- c("m", "f")

#--------------------------
#  List of BUAs
#--------------------------

# Lookup between major cities and LSOAs
citypath <- "V:/VolumeQ/AGteam/ONS/geography/lookup"
# citypath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/ONS/geography/lookup"

citylookup <- read.table(paste0(citypath, "/Lower_Layer_Super_Output_Area", 
  "_(2011)_to_Major_Towns_and_Cities_(December_2015)_Lookup_in", 
  "_England_and_Wales.csv"),
  header = T, sep = ",", quote = "\"", fileEncoding = "UTF-8-BOM")

# Load LSOA population
poppath <- "V:/VolumeQ/AGteam/NOMIS/census2011/KeyStatistics/"
# poppath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/NOMIS/census2011/KeyStatistics/"
pop <- read.csv(paste0(poppath, "KS102EW.csv"))
names(pop)[5] <- "Total"

# Merge them
pop <- merge(pop[,c(3,5)], citylookup[,c(1, 3:4)], 
  by.x = "geography.code", by.y = "LSOA11CD")
pop <- subset(pop, TCITY15NM != "")

# Total population by Major city
citypop <- aggregate(Total ~ TCITY15CD + TCITY15NM, data = pop, sum)

# Keep BUAs with > 100,000 residents
citylist <- subset(citypop, Total > 100000)
citylookup <- subset(citylookup, TCITY15CD %in% citylist$TCITY15CD)

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

# Merge city info and keep only those in selected list
onsdeath <- merge(onsdeath, 
  citylookup[,c("LSOA11CD", "TCITY15CD", "TCITY15NM")],
  by.x = "lsoa", by.y = "LSOA11CD")
onsdeath <- subset(onsdeath, TCITY15CD %in% citylist$TCITY15CD)

# Aggregate by city, date, cause and age class 
outcome_list <- list()
for (a in seq_along(age_labs)){
  # All causes
  vname <- sprintf("all_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a], .N, 
    by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Natural causes
  vname <- sprintf("nat_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a] & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 1)) >= 0 & 
          as.numeric(substr(cause, 1, 1)) < 8) |
          (dodyr > 2000 & substr(cause, 1, 1) %in% LETTERS[1:18])), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Cardiovascular diseases
  vname <- sprintf("cvd_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a] & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 39 & 
        as.numeric(substr(cause, 1, 2)) < 46) |
        (dodyr > 2000 & substr(cause, 1, 1) == "I")), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Respiratory diseases
  vname <- sprintf("resp_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a] & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 46 & 
          as.numeric(substr(cause, 1, 2)) < 52) |
          (dodyr > 2000 & substr(cause, 1, 1) == "J")), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Cardiorespiratory diseases
  vname <- sprintf("cvresp_%s", age_labs[a])
  outcome_list[[vname]] <- onsdeath[
    ageinyrs >= age_cut[a] & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 39 & 
          as.numeric(substr(cause, 1, 2)) < 52) |
          (dodyr > 2000 & substr(cause, 1, 1) %in% c("I", "J"))), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
}

# Aggregate by city, date, cause and sex
for (s in 1:2){
  # All causes
  vname <- sprintf("all_%s", sex_labs[s])
  outcome_list[[vname]] <- onsdeath[
    sex == s, .N, 
    by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Natural causes
  vname <- sprintf("nat_%s", sex_labs[s])
  outcome_list[[vname]] <- onsdeath[
    sex == s & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 1)) >= 0 & 
          as.numeric(substr(cause, 1, 1)) < 8) |
          (dodyr > 2000 & substr(cause, 1, 1) %in% LETTERS[1:18])), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Cardiovascular diseases
  vname <- sprintf("cvd_%s", sex_labs[s])
  outcome_list[[vname]] <- onsdeath[
    sex == s & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 39 & 
        as.numeric(substr(cause, 1, 2)) < 46) |
        (dodyr > 2000 & substr(cause, 1, 1) == "I")), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Respiratory diseases
  vname <- sprintf("resp_%s", sex_labs[s])
  outcome_list[[vname]] <- onsdeath[
    sex == s & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 46 & 
          as.numeric(substr(cause, 1, 2)) < 52) |
          (dodyr > 2000 & substr(cause, 1, 1) == "J")), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
  
  # Cardiorespiratory diseases
  vname <- sprintf("cvresp_%s", sex_labs[s])
  outcome_list[[vname]] <- onsdeath[
    sex == s & 
      ((dodyr <= 2000 & as.numeric(substr(cause, 1, 2)) >= 39 & 
          as.numeric(substr(cause, 1, 2)) < 52) |
          (dodyr > 2000 & substr(cause, 1, 1) %in% c("I", "J"))), 
    .N, by = list(city = TCITY15CD, date = DOD)]
  setnames(outcome_list[[vname]], "N", vname)
}

# Merge all series with all possible combinations of date / city
citydatgrid <- as.data.table(expand.grid(
  date = seq.Date(datestart, dateend, "days"),
  city = citylist$TCITY15CD))
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

#----- Load LSOA temp data for each city
for (i in seq_along(dlist)){
  # LSOAs inside the BUA
  lsoalist <- unlist(subset(citylookup, TCITY15CD == names(dlist)[i], LSOA11CD))
  
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
source <- "V:/VolumeQ/AGteam/ONS/geography/shapefiles/MajorTownCities"
file <- "Major_Towns_and_Cities_(December_2015)_Boundaries_V2"
file.copy(paste0(source, "/", file, ".zip"), getwd())
unzip(zipfile = paste0(file,".zip"), exdir = getwd())
cityshp <- st_read(paste0(file, ".shp"))[2]
file.remove(list.files()[grep(file, list.files(), fixed = T)])

# Keep only the BUA above
cityshp <- subset(cityshp, TCITY15CD %in% citylist$TCITY15CD)

# Compute centroid of each BUA
citypt <- st_centroid(cityshp)

# Transofrm in lon/lat
citypt <- st_transform(citypt, 4326)
citylonlat <- st_coordinates(citypt)
colnames(citylonlat) <- c("lon", "lat")

#----- Create city meta object

cities <- data.frame(city = names(dlist), 
  cityname = citylist$TCITY15NM[match(names(dlist), citylist$TCITY15CD)],
  citylonlat[match(names(dlist), citypt$TCITY15CD),])


#--------------------------
#  Save
#--------------------------

# Remove data.table classes for consistency
dlist <- lapply(dlist, as.data.frame)

# Save
save(dlist, cities, file = "data/ukdata.RData")

