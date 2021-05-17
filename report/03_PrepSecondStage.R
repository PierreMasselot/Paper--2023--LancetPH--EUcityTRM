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
allages <- vector("list", 3)
names(allages) <- c("main", "65p", "75p")

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
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "cvd_main"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage2res, "[[", "cvd_65p"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage2res, "[[", "cvd_75p"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "resp_main"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage2res, "[[", "resp_65p"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage2res, "[[", "resp_75p"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "cvresp_main"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage2res, "[[", "cvresp_65p"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage2res, "[[", "cvresp_75p"))
}

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
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage3res, "[[", "cvd_main"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage4res, "[[", "cvd_65p"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage4res, "[[", "cvd_75p"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage3res, "[[", "resp_main"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage4res, "[[", "resp_65p"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage4res, "[[", "resp_75p"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage3res, "[[", "cvresp_main"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage4res, "[[", "cvresp_65p"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage4res, "[[", "cvresp_75p"))
}

# Extract city
citydesc <- cities[,c("cityname", "countryname", "long", "lat")]
names(citydesc)[3] <- "lon"

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
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "cvd"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage1res, "[[", "cvd.65"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage1res, "[[", "cvd.75"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "resp"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage1res, "[[", "resp.65"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage1res, "[[", "resp.75"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "cvdresp"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage1res, "[[", "cvdresp.65"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage1res, "[[", "cvdresp.75"))
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
cities$lon <- NA; cities$lat <- NA
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
    lapply(stage1res, "[[", "all_main"))
  allstage1[[i]][["all"]][["65p"]] <- c(allstage1[[i]][["all"]][["65p"]],
    lapply(stage1res, "[[", "all_65p"))
  allstage1[[i]][["all"]][["75p"]] <- c(allstage1[[i]][["all"]][["75p"]],
    lapply(stage1res, "[[", "all_75p"))
  
  # CVD
  allstage1[[i]][["cvd"]][["main"]] <- c(allstage1[[i]][["cvd"]][["main"]],
    lapply(stage1res, "[[", "cvd_main"))
  allstage1[[i]][["cvd"]][["65p"]] <- c(allstage1[[i]][["cvd"]][["65p"]],
    lapply(stage1res, "[[", "cvd_65p"))
  allstage1[[i]][["cvd"]][["75p"]] <- c(allstage1[[i]][["cvd"]][["75p"]],
    lapply(stage1res, "[[", "cvd_75p"))
  
  # Respiratory
  allstage1[[i]][["resp"]][["main"]] <- c(allstage1[[i]][["resp"]][["main"]],
    lapply(stage1res, "[[", "resp_main"))
  allstage1[[i]][["resp"]][["65p"]] <- c(allstage1[[i]][["resp"]][["65p"]],
    lapply(stage1res, "[[", "resp_65p"))
  allstage1[[i]][["resp"]][["75p"]] <- c(allstage1[[i]][["resp"]][["75p"]],
    lapply(stage1res, "[[", "resp_75p"))
  
  # Cardiopulmonary
  allstage1[[i]][["cvresp"]][["main"]] <- c(allstage1[[i]][["cvresp"]][["main"]],
    lapply(stage1res, "[[", "cvresp_main"))
  allstage1[[i]][["cvresp"]][["65p"]] <- c(allstage1[[i]][["cvresp"]][["65p"]],
    lapply(stage1res, "[[", "cvresp_65p"))
  allstage1[[i]][["cvresp"]][["75p"]] <- c(allstage1[[i]][["cvresp"]][["75p"]],
    lapply(stage1res, "[[", "cvresp_75p"))
}

# Get city description
cities$countryname <- "UK"
citydesc <- rbind(citydesc, cities[,c("cityname", "countryname", "lon", "lat")])