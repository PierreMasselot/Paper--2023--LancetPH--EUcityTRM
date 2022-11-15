################################################################################
#
#                         MCC-CityEurope
#
#               City selection and data preparation
#
################################################################################

#---------------------------
#  Select cities
#---------------------------

#----- Unique list of cities

# Load list of cities (lookup between new and old)
lookup_cities <- read_excel(paste0(path_euro, "/lookup/CITIES.xlsx"))

# Select unique list
metadata <- na.omit(unique(lookup_cities[,c("CC", "NEW_CODE", "NEW_NAME")]))
metadata <- arrange(metadata, NEW_CODE)

# Rename
metadata <- rename(metadata, CNTR_CODE = "CC", URAU_CODE = "NEW_CODE",
  URAU_NAME = "NEW_NAME")

# Remove overseas cities 
overseas <- c("PT004C", "PT007C", 
  "ES008C", "ES024C", "ES027C", "ES096C", "ES057C", "ES055C",
  "ES045C", "ES039C", # Spanish cities in Morocco
  "IS001C", # Reykjavik Iceland
  "FR046C", "FR030C", "FR062C", "FR072C", "FR074C" # French overseas
)
metadata <- metadata[!metadata$URAU_CODE %in% overseas,]

#----- Point locations

# Load points for each city
unzip(zipfile = sprintf("%s/geography/URAU/URAU_PT_2020_%s.shp.zip", 
    path_euro, geoproj), 
  exdir = tempdir())
urau_points <- st_read(sprintf("%s/URAU_PT_2020_%s.shp", tempdir(), geoproj))
file.remove(sprintf("%s/%s", tempdir(), 
  list.files(tempdir())[grep("URAU_PT_2020", list.files(tempdir()))]))

# Select geometry of retained cities
metageo <- urau_points[match(metadata$URAU_CODE, urau_points$URAU_CODE),
  c("URAU_CODE", "geometry")]

# Add info to metadata
geobind <- do.call(rbind, metageo$geometry)
colnames(geobind) <- c("lon", "lat")
metadata <- cbind(metadata, geobind)

#----- Add other information

# Add region
metadata$region <- factor(regionlist[metadata$CNTR_CODE], levels = regord)

# Load some labels
labpath <- paste0(path_euro, "/lookup/URAU_DisplayNames.csv")
uraulabs <- read.csv(labpath)

# Merge
metadata <- merge(metadata, uraulabs, all.x = T, all.y = F)

# Add URAU names for missing labels
metadata$LABEL[is.na(metadata$LABEL)] <- metadata$URAU_NAME[
  is.na(metadata$LABEL)]

# Remove all mention to city words
metadata$LABEL <- gsub("Greater", "", metadata$LABEL)

# Relabel whent here is a comma
commaind <- grep("\\,", metadata$LABEL)
metadata$LABEL[commaind] <- sapply(strsplit(metadata$LABEL[commaind], ", "), 
  function(x) paste(rev(x), collapse = " "))

# Remove all that is in parenthesis
metadata$LABEL <- gsub("\\(.*\\)", "", metadata$LABEL)

# Remove spaces at the start or end of label
metadata$LABEL <- gsub("^[[:blank:][:punct:]]*", "", metadata$LABEL)
metadata$LABEL <- gsub("[[:blank:][:punct:]]*$", "", metadata$LABEL)

# First letter upper case
metadata$LABEL <- str_to_title(metadata$LABEL)

# Label country
eurcntr <- rbind(eu_countries, efta_countries)
metadata$cntr_name <- factor(eurcntr[match(metadata$CNTR_CODE, eurcntr[,1]),2],
  level = sort(eurcntr[,2]))

#----- Prepare storing objects

# Descriptive summary
metadesc <- data.frame(metavar = c(), label = c(), source = c())

# City-year data.frame to keep info on years of data
metacityyear <- expand.grid(URAU_CODE = metadata$URAU_CODE, year = year)

#---------------------------
#  Load Eurostat's Urban Audit data
#---------------------------

# List of datasets that should be downloaded
datasets <- list(
  pop1 = c(pop = "DE1001V", pop_6574 = "DE1028V", pop75p = "DE1055V"),
  livcon = c(isol = "DE3002I")
)

#----- Load the necessary datasets
urb_dat <- lapply(sprintf("urb_c%s", names(datasets)), get_eurostat,
  time_format = "num")
names(urb_dat) <- names(datasets)

# Remove tibble class
urb_dat <- lapply(urb_dat, as.data.frame)

# Keep only selected variables
urb_dat <- Map(function(x, y) subset(x, indic_ur %in% y),
  urb_dat, datasets)

# Reshape as wide
res_dat <- lapply(urb_dat, reshape, timevar = "indic_ur", 
  idvar = c("cities", "time"), ids = "values", direction = "wide")

# Merge and rename the different datasets
urb_df <- Reduce(function(x, y) merge(x, y, all = T, by = c("cities", "time")),
  res_dat)
namevec <- unlist(unname(datasets))
names(urb_df) <- gsub("values\\.", "", names(urb_df))
names(urb_df)[match(namevec, names(urb_df))] <- names(namevec)

#----- Create final variables

# Prop or population 65+
urb_df$prop_65p <- rowSums(urb_df[,c("pop_6574", "pop75p")]) /
  urb_df[,"pop"]

# Drop variables
urb_df <- urb_df[,-(4:5)]

#----- Merge to the list of cities

# Merge with metadata
metacityyear <- merge(metacityyear, urb_df, 
  by.x = c("URAU_CODE", "year"), by.y = c("cities", "time"),
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = colnames(urb_df)[-(1:2)], 
  label = c("Total population", "Isolation", "Population above 65"),
  source = "Urban Audit"))

#----- Add data from LAUs

# Lookup table between URAU and NUTS (through LAUs)
lau_tab <- read_excel(paste0(path_euro, "/lookup/CITY-LAU-2021.xlsx"))
lau_tab <- rename(lau_tab, CNTR_CODE = "CC", NUTS3 = "NUTS 3 CODE", 
  URAU_CODE = "CITY CODE", LAU_CODE = "LAU CODE")

# Estonia
lau_tab$LAU_CODE[lau_tab$CNTR_CODE == "EE"] <- gsub("_", "_0",
  lau_tab$LAU_CODE[lau_tab$CNTR_CODE == "EE"])

# Add UK
lau_UK <- read_excel(
  paste0(path_euro, "/lookup/EU-27-LAU-2020-NUTS-2021-NUTS-2016.xlsx"),
  sheet = "UK NUTS 2021 and NUTS 2016")
lau_UK <- rename(lau_UK, NUTS3 = "NUTS 3 CODE 2021", LAU_CODE = "LAU CODE")
lau_UK$CITY_ID[is.na(lau_UK$CITY_ID)] <- lau_UK$GREATER_CITY_ID[
  is.na(lau_UK$CITY_ID)]
linkUK <- merge(lau_UK, na.omit(lookup_cities), 
  by.x = "CITY_ID", by.y = "OLD_CODE", all.x = T, all.y = F)
linkUK <- rename(linkUK, URAU_CODE = "NEW_CODE", `CITY NAME` = "NEW_NAME",
  CNTR_CODE = "CC", FUA = "FUA_ID")
linkUK$LAU_CODE <- sprintf("UK_%s", linkUK$LAU_CODE)
lau_tab <- rbind(lau_tab, subset(linkUK, !is.na(URAU_CODE), names(lau_tab)))

# Loop on available years (2011:2020 are the available years in giscoR, 20220929)
# to get pop / popdens
laupop <- foreach(y = intersect(2011:2020, year), .combine = rbind) %do% {
  lauinfo <- gisco_get_lau(year = y, epsg = geoproj)
  lauinfo <- rename(lauinfo, POP = sprintf("POP_%i", y))
  st_drop_geometry(subset(lauinfo, YEAR %in% year & POP > 0, 
    c("GISCO_ID", "LAU_NAME", "YEAR", "POP", "AREA_KM2")))
}

# Link LAUs and cities
lautab <- merge(lau_tab, unique(laupop), by.x = "LAU_CODE", by.y = "GISCO_ID")

# Deal with Latvia
lvtab <- merge(subset(lau_tab, CNTR_CODE == "LV"), unique(laupop), 
  by.x = "LAU NAME NATIONAL", by.y = "LAU_NAME")
lvtab$LAU_NAME <- lvtab$`LAU NAME NATIONAL`
lautab <- rbind(lautab, lvtab[,names(lautab)])

# Sum by city
lautab$POP <- as.numeric(lautab$POP)
citypop <- aggregate(cbind(POP, AREA_KM2) ~ URAU_CODE + YEAR, 
  lautab, sum)

# Compute population density
citypop <- mutate(citypop, popdens = POP / AREA_KM2)

# Add pop to missing cities
nopop <- setdiff(metadata$URAU_CODE, urb_dat$pop1$cities)
ind <- match(
  interaction(subset(citypop, URAU_CODE %in% nopop, c("URAU_CODE", "YEAR"))),
  interaction(metacityyear[,c("URAU_CODE", "year")]))
metacityyear[ind, "pop"] <- subset(citypop, URAU_CODE %in% nopop, "POP")

# Add popdens to metadata
metacityyear <- merge(metacityyear, citypop[,c("URAU_CODE", "YEAR", "popdens")], 
  by.x = c("URAU_CODE", "year"), by.y = c("URAU_CODE", "YEAR"),
  all.x = T, all.y = F, sort = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "popdens", 
  label = "Population density", source = "Urban Audit"))

#---------------------------
# Remote sensing data
#---------------------------

path_rem <- paste0(path_euro, "/data/MNM/RData")

#----- NDVI

# Load data
load(paste0(path_rem, "/MODIS_NDVI_250m_URAU_LB_2000_2021_boundaries.RData"))

# Reshape into long
yrs <- grep("NDVI_[[:digit:]]{4}", 
  names(modis_250m_annual_median_NDVI_URAU_LB_2020), value = T)
ndvi_long <- reshape(modis_250m_annual_median_NDVI_URAU_LB_2020, 
  direction = "long", varying = yrs, v.names = "ndvi", 
  idvar = "URAU_CODE", times = substr(yrs, 6, 9), timevar = "year", 
  drop = c("POP_2020", "AREA_KM2", "CC", "URAU_NAME", "URAU_CATG", "geometry"))

# Merge with other metadata
metacityyear <- merge(metacityyear, ndvi_long, 
  all.x = T, all.y = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "ndvi", 
  label = "NDVI", source = "GEE MODIS"))

#----- PM2.5

# Load data
load(paste0(path_rem, "/PM25_URAU_LB_1998_2020_boundaries.RData"))

# Reshape into long
yrs <- grep("PM25_[[:digit:]]{4}", names(pm2p5_URAU_RG_2020_4326_DF), value = T)
pm25_long <- reshape(pm2p5_URAU_RG_2020_4326_DF, 
  direction = "long", varying = yrs, v.names = "pm25", 
  idvar = "URAU_CODE", times = substr(yrs, 6, 9), timevar = "year")

# Merge with other metadata
metacityyear <- merge(metacityyear, pm25_long, by = c("URAU_CODE", "year"),
  all.x = T, all.y = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "pm25", 
  label = "PM2.5", source = "Atmospheric Composition Analysis Group"))

#----- NO2

# Load data
load(paste0(path_rem, "/NO2_URAU_LB_2005_2019_boundaries.RData"))

# Reshape into long
yrs <- grep("NO2_[[:digit:]]{4}", names(no2_URAU_RG_2020_4326_DF), value = T)
no2_long <- reshape(no2_URAU_RG_2020_4326_DF, 
  direction = "long", varying = yrs, v.names = "no2", 
  idvar = "URAU_CODE", times = substr(yrs, 5, 9), timevar = "year")

# Merge with other metadata
metacityyear <- merge(metacityyear, no2_long, by = c("URAU_CODE", "year"),
  all.x = T, all.y = F)

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "no2", 
  label = "NO2", source = "Atmospheric Composition Analysis Group"))

#----- Copernicus High-Resolution-Layers

# Load data
load(paste0(path_rem, "/Corine_LC_URAU_LB_2015_boundaries.RData"))

# Reshape to merge
corine_df <- st_drop_geometry(Corine_LC_URAU_RG_2020_4326_sf[,-c(2:6)])
names(corine_df)[-1] <- c("imperv", "tree", "grass", "water", "woody")
corine_df$year <- 2015

# Merge with other metadata
metacityyear <- merge(metacityyear, corine_df, all.x = T, all.y = F)

# Add description
metadesc <- rbind(metadesc, cbind(
  metavar = c("imperv", "tree", "grass", "water", "woody"), 
  label = c("Imperviousness", "Tree Cover Density", "Grassland", 
    "Wetness and Water", "Small Woody Features"), 
  source = "Copernicus / HRL"))

#---------------------------
# Additional city-level data
#---------------------------

#----- Elevation

# extract elevation
elev <- get_elev_point(metageo$geometry, src = "aws")

# Add to metadata
inds <- match(metacityyear$URAU_CODE, metageo$URAU_CODE)
metacityyear$elevation <- elev$elevation[inds]

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "elevation", 
  label = "Elevation", source = "Amazon Web Services Terrain Tiles"))

#----- Distance to coast

# Get distance to coast
dists <- st_distance(st_transform(st_as_sf(coastline50), 3035), 
  st_transform(metageo, 3035))
mindist <- apply(dists, 2, min) / 1000

# Add to metadata
inds <- match(metacityyear$URAU_CODE, metageo$URAU_CODE)
metacityyear$coast_dist <- mindist[inds]

# Add description
metadesc <- rbind(metadesc, cbind(metavar = "coast_dist", 
  label = "Distance to coast", source = "Natural Earth"))

#---------------------------
# Load ERA5land data
#---------------------------

#----- Create time series

# Load ERA5land data for Urban Audit
load(paste0(path_rem, 
  "/URAU_LB_2020_ERA5_land_1981_2021_daily_Tmean_deg_C_boundaries.RData"))
names(URAU_RG_2020_4326_era5_land_df)[1:3] <- c("URAU_CODE", "date", 
  "era5landtmean")

# Select only records for cities in metadata
era5_df <- subset(URAU_RG_2020_4326_era5_land_df,
  URAU_CODE %in% metadata$URAU_CODE)

# Select period
era5_df <- subset(era5_df, format(date, "%Y") %in% yearanalysis)

# Split by location
era5series <- split(era5_df[,c("date", "era5landtmean")], era5_df$URAU_CODE)

# Reorder
era5series <- era5series[match(metadata$URAU_CODE, names(era5series))]

#----- Temperature-based metadata

# Select years
era5_df$year <- format(era5_df$date, "%Y")
era5_df <- era5_df[era5_df$year %in% as.character(year),]

# Compute annual average temperatures and degree days
annualEra5 <- aggregate(era5landtmean ~ URAU_CODE + year, era5_df, 
  function(x) c(tmean = mean(x), trange = diff(range(x))))
annualEra5 <- cbind(annualEra5[1:2], annualEra5[[3]])

# Merge to other variables
metacityyear <- merge(metacityyear, annualEra5, 
  by= c("URAU_CODE", "year"), all.x = T, all.y = F)

# Add description of metadata
metadesc <- rbind(metadesc, 
  cbind(metavar = c("tmean", "trange"), 
    label = c("Mean temperature", "Temperature range"), 
    source = "Copernicus"))
