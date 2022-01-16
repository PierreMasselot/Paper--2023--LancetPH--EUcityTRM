################################################################################
#
#                         MCC-CityEurope
#
#                          City selection
#
################################################################################

source("00_Packages_Parameters.R")

# Paths
path_urau <- "V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)"
path_nuts <- "V:/VolumeQ/AGteam/Eurostat/Regional by NUTS classification (reg)"

#---------------------------
#  Link datasets
#---------------------------

#----- Load all lookup tables 

# Lookup table between URAU and NUTS
urau_nuts <- read.csv(paste0(path_urau, "/lookup/URAU_NUTS2020.csv"), 
  encoding = "UTF-8")

# Lookup table between MCC and URAU
urau_mcc_age <- read.table(sprintf("%s/lookup/URAU_MCC_AgeCause_20211007.csv", 
    path_urau),
  header = T, sep = ";", quote = "\"")
urau_mcc_all <- read.table(sprintf("%s/lookup/URAU_MCCdata_20211007.csv", 
    path_urau),
  header = T, sep = ";", quote = "\"")

# Lookup table between Cities and greater city
urau_greater <- read.csv(paste0(path_urau, "/lookup/CitiesToGreater.csv"))

#----- Merge everything together

# Merge with MCC age dataset
metadata <- merge(urau_nuts[,-(5:7)],
  urau_mcc_age[,c("URAU_CODE", "mcc_code", "cityname")],
  by = "URAU_CODE", all.x = T, all.y = F)

# When there is no match with MCC age dataset, search in MCC all
matchall <- match(subset(metadata, is.na(mcc_code), URAU_CODE, drop = T),
  urau_mcc_all$URAU_CODE)
metadata[is.na(metadata$mcc_code), c("mcc_code", "cityname")] <- 
  urau_mcc_all[matchall, c("mcc_code", "cityname")]

#----- Manage codes

# Create a second URAU code variable as there are mismatches in different files
metadata$URAU_CODE2 <- metadata$URAU_CODE

# Add numbers to Polish codes
misnum <- nchar(metadata$URAU_CODE2) == 6
metadata$URAU_CODE2[misnum] <- paste(metadata$URAU_CODE2[misnum], "1", sep = "")

# Change Belgian codes
bek <- with(metadata, URAU_CATG == "K" & CNTR_CODE == "BE")
metadata$URAU_CODE2[bek] <- gsub("K", "C", metadata$URAU_CODE2[bek])

# Thessaloniki
thessa <- metadata$URAU_CODE2 == "EL002K1"
metadata$URAU_CODE2[thessa] <- "EL002C1"

# Change London NUTS3 code for easier linkage later
metadata[metadata$URAU_NAME == "London", "NUTS3_2021"] <- "UKI"

#----- City selection

# Load 'official' list of cities
citylist <- as.data.frame(read_excel(
  paste0(path_urau, "/metadata/cities (urb_esms_an4).xls"), sheet = 1)
)

# Remove spaces in codes in citylist
citylist$CODE <- substr(citylist$CODE, 1, 7)

# Keep only cities in list
metadata <- subset(metadata, URAU_CODE2 %in% citylist$CODE)

# Discard functional urban areas
metadata <- subset(metadata, URAU_CATG != "F")

# Remove all cities inside greater city
ingreater <- merge(metadata["URAU_CODE"], urau_greater, 
  by.x = "URAU_CODE", by.y = "greater_code")
metadata <- subset(metadata, !URAU_CODE %in% ingreater$city_code)

# Reject MCC matches that are not in selected MCC country datasets
metadata <- subset(metadata, is.na(mcc_code) | 
    (sapply(strsplit(mcc_code, "\\."), "[", 2) %in% mcc_countries))

# Remove mismatch between large UK conurbation and smaller cities
#   Manchester/Wigan, Liverpool/Birkenhead
torm <- c(
  with(metadata, which(URAU_CODE == "UK008K1" & mcc_code == "wign.uk9016")),
  with(metadata, which(URAU_CODE == "UK006K2" & mcc_code == "brkn.uk9016"))
)
metadata <- metadata[-torm,]

# Remove overseas cities 
overseas <- c("PT004C1", "PT007C1", 
  "ES025K1", "ES524C1", "ES550K1", "ES029C1", "ES008C1", "ES074C1", "ES072C1",
  "ES055C1", "ES045C1", # Spanish cities in Morocco
  "IS001C1", # Reykjavik
  "FR030C1", "FR520C1", "FR521C1", "FR028C1", "FR522C1"
)
metadata <- metadata[!metadata$URAU_CODE %in% overseas,]

# Remove some duplicates due to older versions of city
older <- c("ES552", "ES550")
metadata <- metadata[!rownames(metadata) %in% older,]

# Remove potential remaining level duplicates
metadata <- metadata[!duplicated(metadata),]

#----- Add other information

# Add region
metadata$region <- factor(regionlist[metadata$CNTR_CODE], levels = regord)

# Add indicator for whether it is in MCC
metadata$inmcc <- !is.na(metadata$mcc_code)

# NUTS codes for easier merging
metadata$NUTS2_2021 <- substr(metadata$NUTS3_2021, 1, 4)
metadata$NUTS1_2021 <- substr(metadata$NUTS3_2021, 1, 3)
metadata$NUTS0_2021 <- substr(metadata$NUTS3_2021, 1, 2)

# Do it also for the 2016 version
metadata$NUTS2_2016 <- substr(metadata$NUTS3_2016, 1, 4)
metadata$NUTS1_2016 <- substr(metadata$NUTS3_2016, 1, 3)
metadata$NUTS0_2016 <- substr(metadata$NUTS3_2016, 1, 2)

#----- Label cities

# Load some labels
labpath <- paste0("V:/VolumeQ/AGteam/Eurostat/Urban Audit (urb_cgc)/metadata",
  "/URAU_DisplayNames.csv")
uraulabs <- read.csv(labpath)

# Merge
metadata <- merge(metadata, uraulabs, all.x = T, all.y = F)

# Add URAU names for missing labels
metadata$LABEL[is.na(metadata$LABEL)] <- metadata$URAU_NAME[
  is.na(metadata$LABEL)]

# Remove all mention to city words
metadata$LABEL <- gsub("City of", "", metadata$LABEL)
metadata$LABEL <- gsub("Greater", "", metadata$LABEL)

# Remove all that is in parenthesis
metadata$LABEL <- gsub("\\(.*\\)", "", metadata$LABEL)

# The Case of Romania
metadata$LABEL <- gsub("MUNICIPIUL", "", metadata$LABEL)

# The Case of Poland
metadata$LABEL <- gsub("M\\. ", "", metadata$LABEL)

# Remove spaces at the start or end of label
metadata$LABEL <- gsub("^[[:blank:][:punct:]]*", "", metadata$LABEL)
metadata$LABEL <- gsub("[[:blank:][:punct:]]*$", "", metadata$LABEL)

# First letter upper case
metadata$LABEL <- str_to_title(metadata$LABEL)

# Label country
eurcntr <- rbind(eu_countries, efta_countries)
metadata$cntr_name <- factor(eurcntr[match(metadata$CNTR_CODE, eurcntr[,1]),2],
  level = sort(eurcntr[,2]))