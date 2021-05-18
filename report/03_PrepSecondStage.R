################################################################################
#
#                       Exhaustion Level 1
#
#                       Putting all country results together
#
################################################################################

# List all result files
fillist <- list.files("results") 

# Lag list
lags <- c(10, 3, 21)

# Prepare objects storing all data
allages <- vector("list", 5)
names(allages) <- c("main", "65p", "75p", "m", "f")

allouts <- rep(list(allages), 4)
names(allouts) <- c("all", "cvd", "resp", "cvresp")

allstage1 <- rep(list(allouts), 3)
names(allstage1) <- lags


#-------------------------------
# Load results for Germany
#-------------------------------

for(i in 1){
  # Get right file
  fil <- grep(sprintf("Germany_lag%i", lags[i]), fillist, value = T)
  
  # Load it
  load(sprintf("results/%s", fil[1]))
  load(sprintf("results/%s", fil[2]))
  
  #----- Extract results
  # All cause
  allstage1[[i]][["all"]][["main"]] <- c(allstage1[[i]][["all"]][["main"]],
    lapply(stage1res, "[[", "all_main"))
  allstage1[[i]][["all"]][["65p"]] <- c(allstage1[[i]][["all"]][["65p"]],
    lapply(stage2res, "[[", "all_65p"))
  allstage1[[i]][["all"]][["75p"]] <- c(allstage1[[i]][["all"]][["75p"]],
    lapply(stage2res, "[[", "all_75p"))
  allstage1[[i]][["all"]][["m"]] <- c(allstage1[[i]][["all"]][["m"]],
    lapply(stage2res, "[[", "all_m"))
  allstage1[[i]][["all"]][["f"]] <- c(allstage1[[i]][["all"]][["f"]],
    lapply(stage2res, "[[", "all_f"))
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "cvd_main"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage2res, "[[", "cvd_65p"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage2res, "[[", "cvd_75p"))
  allstage1[[i]][["cvd"]][["m"]] <- c(allstage1[[i]][["cvd"]][["m"]],
    lapply(stage2res, "[[", "cvd_m"))
  allstage1[[i]][["cvd"]][["f"]] <- c(allstage1[[i]][["cvd"]][["f"]],
    lapply(stage2res, "[[", "cvd_f"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "resp_main"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage2res, "[[", "resp_65p"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage2res, "[[", "resp_75p"))
  allstage1[[i]][["resp"]][["m"]] <- c(allstage1[[i]][["resp"]][["m"]],
    lapply(stage2res, "[[", "resp_m"))
  allstage1[[i]][["resp"]][["f"]] <- c(allstage1[[i]][["resp"]][["f"]],
    lapply(stage2res, "[[", "resp_f"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "cvresp_main"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage2res, "[[", "cvresp_65p"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage2res, "[[", "cvresp_75p"))
  allstage1[[i]][["cvresp"]][["m"]] <- c(allstage1[[i]][["cvresp"]][["m"]],
    lapply(stage2res, "[[", "cvresp_m"))
  allstage1[[i]][["cvresp"]][["f"]] <- c(allstage1[[i]][["cvresp"]][["f"]],
    lapply(stage2res, "[[", "cvresp_f"))
}

# Extract city
citydesc <- cities[,c("cityname", "countryname", "long", "lat")]
names(citydesc)[3] <- "lon"

# Age sex
for(i in 2:3){
  # Get right file
  fil <- grep(sprintf("Germany_lag%i", lags[i]), fillist, value = T)
  
  # Load it
  load(sprintf("results/%s", fil[1]))
  load(sprintf("results/%s", fil[2]))
  
  #----- Extract results
  # All cause
  allstage1[[i]][["all"]][["main"]] <- c(allstage1[[i]][["all"]][["main"]],
    lapply(stage3res, "[[", "all_main"))
  allstage1[[i]][["all"]][["65p"]] <- c(allstage1[[i]][["all"]][["65p"]],
    lapply(stage4res, "[[", "all_65p"))
  allstage1[[i]][["all"]][["75p"]] <- c(allstage1[[i]][["all"]][["75p"]],
    lapply(stage4res, "[[", "all_75p"))
  allstage1[[i]][["all"]][["m"]] <- c(allstage1[[i]][["all"]][["m"]],
    lapply(stage4res, "[[", "all_m"))
  allstage1[[i]][["all"]][["f"]] <- c(allstage1[[i]][["all"]][["f"]],
    lapply(stage4res, "[[", "all_f"))
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage3res, "[[", "cvd_main"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage4res, "[[", "cvd_65p"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage4res, "[[", "cvd_75p"))
  allstage1[[i]][["cvd"]][["m"]] <- c(allstage1[[i]][["cvd"]][["m"]],
    lapply(stage4res, "[[", "cvd_m"))
  allstage1[[i]][["cvd"]][["f"]] <- c(allstage1[[i]][["cvd"]][["f"]],
    lapply(stage4res, "[[", "cvd_f"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage3res, "[[", "resp_main"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage4res, "[[", "resp_65p"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage4res, "[[", "resp_75p"))
  allstage1[[i]][["resp"]][["m"]] <- c(allstage1[[i]][["resp"]][["m"]],
    lapply(stage4res, "[[", "resp_m"))
  allstage1[[i]][["resp"]][["f"]] <- c(allstage1[[i]][["resp"]][["f"]],
    lapply(stage4res, "[[", "resp_f"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage3res, "[[", "cvresp_main"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage4res, "[[", "cvresp_65p"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage4res, "[[", "cvresp_75p"))
  allstage1[[i]][["cvresp"]][["m"]] <- c(allstage1[[i]][["cvresp"]][["m"]],
    lapply(stage4res, "[[", "cvresp_m"))
  allstage1[[i]][["cvresp"]][["f"]] <- c(allstage1[[i]][["cvresp"]][["f"]],
    lapply(stage4res, "[[", "cvresp_f"))
}

#-------------------------------
# Load results for Greece
#-------------------------------

for(i in seq_along(lags)){
  # Get right file
  fil <- grep(sprintf("Greece_lag%i", lags[i]), fillist, value = T)
  
  # Load it
  load(sprintf("results/%s", fil))
  
  # All cause
  allstage1[[i]][["all"]][["main"]] <- c(allstage1[[i]][["all"]][["main"]],
    lapply(stage1res, "[[", "alltm"))
  allstage1[[i]][["all"]][["65p"]] <- c(allstage1[[i]][["all"]][["65p"]],
    lapply(stage1res, "[[", "tm65"))
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "allcm"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage1res, "[[", "cm65"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "allrm"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage1res, "[[", "rm65"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "allcpm"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage1res, "[[", "cpm65"))
}

# Get city description
cities$countryname <- "Greece"
citydesc <- rbind(citydesc, cities[,c("cityname", "countryname", "lon", "lat")])

#-------------------------------
# Load results for Italy
#-------------------------------

for(i in seq_along(lags)){
  # Get right file
  fil <- grep(sprintf("Italy_lag%i", lags[i]), fillist, value = T)
  
  # Load it
  load(sprintf("results/%s", fil))
  
  # All cause
  allstage1[[i]][["all"]][["main"]] <- c(allstage1[[i]][["all"]][["main"]],
    lapply(stage1res, "[[", "nat"))
  allstage1[[i]][["all"]][["65p"]] <- c(allstage1[[i]][["all"]][["65p"]],
    lapply(stage1res, "[[", "nat.65"))
  allstage1[[i]][["all"]][["75p"]] <- c(allstage1[[i]][["all"]][["75p"]],
    lapply(stage1res, "[[", "nat.75"))
  allstage1[[i]][["all"]][["m"]] <- c(allstage1[[i]][["all"]][["m"]],
    lapply(stage1res, "[[", "nat.M"))
  allstage1[[i]][["all"]][["f"]] <- c(allstage1[[i]][["all"]][["f"]],
    lapply(stage1res, "[[", "nat.F"))
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "cvd"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage1res, "[[", "cvd.65"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage1res, "[[", "cvd.75"))
  allstage1[[i]][["cvd"]][["m"]] <- c(allstage1[[i]][["cvd"]][["m"]],
    lapply(stage1res, "[[", "cvd.M"))
  allstage1[[i]][["cvd"]][["f"]] <- c(allstage1[[i]][["cvd"]][["f"]],
    lapply(stage1res, "[[", "cvd.F"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "resp"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage1res, "[[", "resp.65"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage1res, "[[", "resp.75"))
  allstage1[[i]][["resp"]][["m"]] <- c(allstage1[[i]][["resp"]][["m"]],
    lapply(stage1res, "[[", "resp.M"))
  allstage1[[i]][["resp"]][["f"]] <- c(allstage1[[i]][["resp"]][["f"]],
    lapply(stage1res, "[[", "resp.F"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "cvdresp"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage1res, "[[", "cvdresp.65"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage1res, "[[", "cvdresp.75"))
  allstage1[[i]][["cvresp"]][["m"]] <- c(allstage1[[i]][["cvresp"]][["m"]],
    lapply(stage1res, "[[", "cvdresp.M"))
  allstage1[[i]][["cvresp"]][["f"]] <- c(allstage1[[i]][["cvresp"]][["f"]],
    lapply(stage1res, "[[", "cvdresp.F"))
}

# Get city description
cities$countryname <- "Italy"
citydesc <- rbind(citydesc, cities[,c("cityname", "countryname", "lon", "lat")])

#-------------------------------
# Load results for Norway
#-------------------------------

for(i in seq_along(lags)){
  # Get right file
  fil <- grep(sprintf("Norway_lag%i", lags[i]), fillist, value = T)
  
  # Load it
  load(sprintf("results/%s", fil))
  names(stage1res) <- cities$cityname
  
  # All cause
  allstage1[[i]][["all"]][["main"]] <- c(allstage1[[i]][["all"]][["main"]],
    lapply(stage1res, "[[", "nall"))
  allstage1[[i]][["all"]][["65p"]] <- c(allstage1[[i]][["all"]][["65p"]],
    lapply(stage1res, "[[", "n65p"))
  allstage1[[i]][["all"]][["75p"]] <- c(allstage1[[i]][["all"]][["75p"]],
    lapply(stage1res, "[[", "n75p"))
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "call"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage1res, "[[", "c65p"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage1res, "[[", "c75p"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "pall"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage1res, "[[", "p65p"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage1res, "[[", "p75p"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "cpall"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage1res, "[[", "cp65p"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage1res, "[[", "cp75p"))
}

# Get city description
cities$lon <- c(10.75, 5.32, 10.4, 5.73) 
cities$lat <- c(59.91, 60.39, 63.4, 58.9)
citydesc <- rbind(citydesc, cities[,c("cityname", "countryname", "lon", "lat")])

#-------------------------------
# Load results for UK
#-------------------------------

for(i in seq_along(lags)){
  # Get right file
  fil <- grep(sprintf("UK_lag%i", lags[i]), fillist, value = T)
  
  # Load it
  load(sprintf("results/%s", fil))
  names(stage1res) <- cities$cityname
  
  # All cause
  allstage1[[i]][["all"]][["main"]] <- c(allstage1[[i]][["all"]][["main"]],
    lapply(stage1res, "[[", "nat_main"))
  allstage1[[i]][["all"]][["65p"]] <- c(allstage1[[i]][["all"]][["65p"]],
    lapply(stage1res, "[[", "nat_65p"))
  allstage1[[i]][["all"]][["75p"]] <- c(allstage1[[i]][["all"]][["75p"]],
    lapply(stage1res, "[[", "nat_75p"))
  allstage1[[i]][["all"]][["m"]] <- c(allstage1[[i]][["all"]][["m"]],
    lapply(stage1res, "[[", "nat_m"))
  allstage1[[i]][["all"]][["f"]] <- c(allstage1[[i]][["all"]][["f"]],
    lapply(stage1res, "[[", "nat_f"))
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "cvd_main"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage1res, "[[", "cvd_65p"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage1res, "[[", "cvd_75p"))
  allstage1[[i]][["cvd"]][["m"]] <- c(allstage1[[i]][["cvd"]][["m"]],
    lapply(stage1res, "[[", "cvd_m"))
  allstage1[[i]][["cvd"]][["f"]] <- c(allstage1[[i]][["cvd"]][["f"]],
    lapply(stage1res, "[[", "cvd_f"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "resp_main"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage1res, "[[", "resp_65p"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage1res, "[[", "resp_75p"))
  allstage1[[i]][["resp"]][["m"]] <- c(allstage1[[i]][["resp"]][["m"]],
    lapply(stage1res, "[[", "resp_m"))
  allstage1[[i]][["resp"]][["f"]] <- c(allstage1[[i]][["resp"]][["f"]],
    lapply(stage1res, "[[", "resp_f"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "cvresp_main"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage1res, "[[", "cvresp_65p"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage1res, "[[", "cvresp_75p"))
  allstage1[[i]][["cvresp"]][["m"]] <- c(allstage1[[i]][["cvresp"]][["m"]],
    lapply(stage1res, "[[", "cvresp_m"))
  allstage1[[i]][["cvresp"]][["f"]] <- c(allstage1[[i]][["cvresp"]][["f"]],
    lapply(stage1res, "[[", "cvresp_f"))
}

# Get city description
cities$countryname <- "UK"
citydesc <- rbind(citydesc, cities[,c("cityname", "countryname", "lon", "lat")])
citydesc$pop <- c(3.52, 0.55, 0.54, 0.58, 0.61, 0.73, 1.78, 0.53, 1.06, 0.56, 
  1.45, 0.62, 0.49, 0.58, 0.51, 0.06, 0.15, 0.06, 0.01, 0.17, 0.32, 0.09, 0.33, 
  0.31, 0.05, 0.13, 0.67, 0.05, 2.86, 0.67, 0.67, 0.28, 0.2, 0.14, 0.11, 0.11, 
  1.14, 0.12, 0.15, 0.18, 0.2, 0.35, 0.24, 0.57, 0.15, 0.35, 0.16, 0.19, 0.13, 
  0.36, 0.11, 0.26, 0.11, 0.11, 0.12, 0.12, 0.11, 0.15, 0.11, 0.17, 0.15, 0.29, 
  0.5, 0.41, 0.57, 8.73, 0.22, 0.12, 0.55, 0.18, 0.18, 0.28, 0.13, 0.23, 0.19, 
  0.31, 0.12, 0.16, 0.17, 0.27, 0.16, 0.23, 0.13, 0.26, 0.11, 0.11, 0.54, 0.16, 
  0.11, 0.27, 0.18, 0.1, 0.11, 0.28, 0.18, 0.11, 0.18, 0.19, 0.15, 0.17, 0.14, 
  0.1, 0.24, 0.1, 0.11, 0.16) * 10^6
  
  
  
