################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Non-reproducible) R Code
# Part 3: Manual cleaning and tidying
#
################################################################################

#---------------------------
# Some filling with additional sources
#---------------------------

#----- Add missing population 

# Valmiera (https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__IR__IRS/IRD060/)
metacityyear[metacityyear$URAU_CODE == "LV008C" & metacityyear$year == 2018,
  c("pop", "popdens")] <- c(23024, 1189)

# Mechelen (https://en.wikipedia.org/wiki/Mechelen)
metacityyear[metacityyear$URAU_CODE == "BE012C" & metacityyear$year == 2018,
  c("pop", "popdens")] <- c(86304, 2600)

# Mouscron (https://en.wikipedia.org/wiki/Mouscron)
metacityyear[metacityyear$URAU_CODE == "BE013C" & metacityyear$year == 2018,
  c("pop", "popdens")] <- c(58234, 1500)

# La Louvière (https://en.wikipedia.org/wiki/La_Louvière)
metacityyear[metacityyear$URAU_CODE == "BE014C" & metacityyear$year == 2018,
  c("pop", "popdens")] <- c(80637, 1255)

# Verviers (https://fr.wikipedia.org/wiki/Verviers)
metacityyear[metacityyear$URAU_CODE == "BE015C" & metacityyear$year == 2018,
  c("pop", "popdens")] <- c(55198, 1669)

#----- Fill missings in 65p by NUTS3 structure

# Fill missing
urauna <- tapply(metacityyear$prop_65p, metacityyear$URAU_CODE, mean, na.rm = T)
indmis <- metacityyear$URAU_CODE %in% names(urauna)[is.na(urauna)]
metacityyear$prop_65p[indmis] <- rowSums(metacityyear[indmis, 
  c("prop_6569", "prop_7074", "prop_7579", "prop_8084", "prop_8599")]) / 
  100

# Change description of metavariable
metadesc[metadesc$metavar == "prop_65p", "source"] <- "Urban Audit / NUTS3"

#----- Fill missings in isol by NUTS2 household census

# Fill missing
metacityyear$isol[is.na(metacityyear$isol)] <- 
  metacityyear$isol2[is.na(metacityyear$isol)]

# Change description in metavariable and remove isol at NUTS2
metadesc[metadesc$metavar == "isol", "source"] <- "Urban Audit / NUTS2"
metadesc <- metadesc[metadesc$metavar != "isol2",]

#---------------------------
# Aggregate city-level variables
#---------------------------

# Average years
meta_means <- aggregate(metacityyear[,metadesc$metavar], 
  metacityyear["URAU_CODE"], mean, na.rm = T)

# Merge to metadata
metadata <- merge(metadata, meta_means, by = "URAU_CODE", all.x = T)

# Round the proportions to 100
propvars <- grep("prop_[[:digit:]]{4}", names(metadata), value = T)
metadata[,propvars] <- 100 * metadata[,propvars] / 
  matrix(rowSums(metadata[,propvars]), 
    nrow = nrow(metadata), ncol = length(propvars), byrow = F)

#---------------------------
# Keep track of missing values
#---------------------------

# Missings
imputed <- is.na(subset(metadata, select = metadesc$metavar))
rownames(imputed) <- metadata$URAU_CODE

# Number of missings per variables
metadesc$nmis <- apply(imputed, 2, sum)
metadesc$propmis <- round(apply(imputed, 2, mean) * 100)

# Number of missings per city
metadata$nmiss <- apply(imputed, 1, sum)
