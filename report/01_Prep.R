################################################################################
#
#                       Exhaustion Level 1
#
#                         Prep UK data
#
################################################################################

library(data.table)

#--------------------------
#  Parameters
#--------------------------

# Dates for analysis
datestart <- as.Date("1981/01/01")
dateend <- as.Date("2020/12/31")

# Define age groups (nested)
age_cut <- c(0, 65, 75)
age_labs <- c("main", "65p", "75p")

#--------------------------
#  List of BUAs
#--------------------------

# Lookup between Built-Up Areas and LSOAs
buapath <- "V:/VolumeQ/AGteam/ONS/geography/lookup"
buapath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/ONS/geography/lookup"

bualookup <- read.table(paste0(buapath, "/Lower_Layer_Super_Output_Area__2011_",
  "_to_Built-up_Area_Sub-division_to_Built-up_Area_to_Local_Authority",  
  "_District_to_Region__December_2011__Lookup_in_England_and_Wales.csv"),
  header = T, sep = ",", quote = "\"", fileEncoding = "UTF-8-BOM")

# Load LSOA population
poppath <- "V:/VolumeQ/AGteam/NOMIS/census2011/KeyStatistics/"
poppath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/NOMIS/census2011/KeyStatistics/"
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
deathpath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/ONS/mortality/data"

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

# Merge all series
alloutcomes <- Reduce(merge, outcome_list)

# Split the data by city
dlist <- split(alloutcomes[, -1], alloutcomes[,"city"])
names(dlist) <- gsub(" BUA", "", 
  bualist$BUA11NM[match(names(dlist), bualist$BUA11CD)])

# Fill all dates between start and end dates
dateseq <- seq.Date(datestart, dateend, "days")
dlist <- lapply(dlist, function(x) merge(x, data.frame(date = dateseq), 
  all = T))

#--------------------------
#  Load temperature data
#--------------------------

# Load LSOA level temp data
tmeanpath <- "V:/VolumeQ/AGteam/ONSmortality/processed/LSOA19812018/"
tmeanpath <- "C:/Users/PierreMasselot/Filr/Net Folders/StorageOnDemand Q/AGteam/ONSmortality/processed/LSOA19812018/"

# Dates of tmean data
tmeandates <- seq(as.Date("1981/1/1"), as.Date("2018/12/31"), "days")

# Load LSOA temp data for each BUA
for (i in seq_len(nrow(bualist))){
  lsoalist <- subset(bualookup, BUA11CD == bualist$BUA11CD[i], LSOA11CD)
  tmeanlist <- lapply(lsoalist, function(lsoa) {
    temp <- data.table(readRDS(paste0(tmeanpath, lsoa, ".Rds")))
    cbind(data.table(LSOA11CD = lsoa), date = tmeandates, temp)
  }) 
  alltmean <- do.call(rbind, tmeanlist)
}
