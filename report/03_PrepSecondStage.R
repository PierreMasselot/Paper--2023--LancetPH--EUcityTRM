################################################################################
#
#                       Exhaustion Level 1
#
#                       Putting all country results together
#
################################################################################

# Lag list
lags <- c(10, 3, 21)

# List all result files
fillist <- list.files("results") 

# Prepare objects storing all data
allouts <- vector("list", 4)
names(allouts) <- c("all", "cvd", "resp", "cvresp")

allstage1 <- rep(list(allouts), 3)
names(allstage1) <- c(10, 3, 21)


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
  getallcause <- lapply(stage1res, function(x){
    out <- x$all_main
    out$age <- 1
    out
  })
  names(getallcause) <- sprintf("%s.%s", names(getallcause), "all_main")
  getallcause2 <- lapply(stage2res, function(x){
    out <- x[c("all_65p", "all_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 2:3)
    out
  })
  allstage1[[i]]$all <- c(allstage1[[i]]$all, getallcause, 
    unlist(getallcause2, recursive = F))
  
  # CVD
  getcvd <- lapply(stage1res, function(x){
    out <- x$cvd_main
    out$age <- 1
    out
  })
  names(getcvd) <- sprintf("%s.%s", names(getcvd), "cvd_main")
  getcvd2 <- lapply(stage2res, function(x){
    out <- x[c("cvd_65p", "cvd_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 2:3)
    out
  })
  allstage1[[i]]$cvd <- c(allstage1[[i]]$cvd, getcvd, 
    unlist(getcvd2, recursive = F))
  
  # Respiratory
  getresp <- lapply(stage1res, function(x){
    out <- x$resp_main
    out$age <- 1
    out
  })
  names(getresp) <- sprintf("%s.%s", names(getresp), "resp_main")
  getresp2 <- lapply(stage2res, function(x){
    out <- x[c("resp_65p", "resp_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 2:3)
    out
  })
  allstage1[[i]]$resp <- c(allstage1[[i]]$resp, getresp, 
    unlist(getresp2, recursive = F))
  
  # Cardiopulmonary
  getcvresp <- lapply(stage1res, function(x){
    out <- x$cvresp_main
    out$age <- 1
    out
  })
  names(getcvresp) <- sprintf("%s.%s", names(getcvresp), "cvresp_main")
  getcvresp2 <- lapply(stage2res, function(x){
    out <- x[c("cvresp_65p", "cvresp_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 2:3)
    out
  })
  allstage1[[i]]$cvresp <- c(allstage1[[i]]$cvresp, getcvresp, 
    unlist(getcvresp2, recursive = F))
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
  getallcause <- unlist(lapply(stage1res, function(x){
    out <- x[c("alltm", "tm65")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:2)
    out
  }), recursive = F)
  allstage1[[i]]$all <- c(allstage1[[i]]$all, getallcause)
  
  # Cardiovascular
  getcvd <- unlist(lapply(stage1res, function(x){
    out <- x[c("allcm", "cm65")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:2)
    out
  }), recursive = F)
  allstage1[[i]]$cvd <- c(allstage1[[i]]$cvd, getcvd)
  
  # Respiratory
  getresp <- unlist(lapply(stage1res, function(x){
    out <- x[c("allrm", "rm65")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:2)
    out
  }), recursive = F)
  allstage1[[i]]$resp <- c(allstage1[[i]]$resp, getresp)  
  
  # Cardiorespiratory
  getcpm <- unlist(lapply(stage1res, function(x){
    out <- x[c("allcpm", "cpm65")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:2)
    out
  }), recursive = F)
  allstage1[[i]]$cvresp <- c(allstage1[[i]]$cvresp, getcpm)  
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
  getallcause <- unlist(lapply(stage1res, function(x){
    out <- x[c("nat", "nat.65", "nat.75")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  }), recursive = F)
  allstage1[[i]]$all <- c(allstage1[[i]]$all, getallcause)
  
  # Cardiovascular
  getcvd <- unlist(lapply(stage1res, function(x){
    out <- x[c("cvd", "cvd.65", "cvd.75")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  }), recursive = F)
  allstage1[[i]]$cvd <- c(allstage1[[i]]$cvd, getcvd)
  
  # Respiratory
  getresp <- unlist(lapply(stage1res, function(x){
    out <- x[c("resp", "resp.65", "resp.75")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  }), recursive = F)
  allstage1[[i]]$resp <- c(allstage1[[i]]$resp, getresp)  
  
  # Cardiorespiratory
  getcpm <- unlist(lapply(stage1res, function(x){
    out <- x[c("cvdresp", "cvdresp.65", "cvdresp.75")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  }), recursive = F)
  allstage1[[i]]$cvresp <- c(allstage1[[i]]$cvresp, getcpm)  
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
  
  # All cause
  getallcause <- lapply(stage1res, function(x){
    out <- x[c("nall", "n65p", "n75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getallcause) <- cities$cityname
  allstage1[[i]]$all <- c(allstage1[[i]]$all, 
    unlist(getallcause, recursive = F))
  
  # Cardiovascular
  getcvd <- lapply(stage1res, function(x){
    out <- x[c("call", "c65p", "c75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getcvd) <- cities$cityname
  allstage1[[i]]$cvd <- c(allstage1[[i]]$cvd, 
    unlist(getcvd, recursive = F))
  
  # Respiratory
  getresp <- lapply(stage1res, function(x){
    out <- x[c("pall", "p65p", "p75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getresp) <- cities$cityname
  allstage1[[i]]$resp <- c(allstage1[[i]]$resp, 
    unlist(getresp, recursive = F))  
  
  # Cardiorespiratory
  getcpm <- lapply(stage1res, function(x){
    out <- x[c("cpall", "cp65p", "cp75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getcpm) <- cities$cityname
  allstage1[[i]]$cvresp <- c(allstage1[[i]]$cvresp, 
    unlist(getcpm, recursive = F))  
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
  
  # All cause
  getallcause <- lapply(stage1res, function(x){
    out <- x[c("all_main", "all_65p", "all_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getallcause) <- cities$cityname[match(names(getallcause), cities$city)]
  allstage1[[i]]$all <- c(allstage1[[i]]$all, 
    unlist(getallcause, recursive = F))
  
  # Cardiovascular
  getcvd <- lapply(stage1res, function(x){
    out <- x[c("cvd_main", "cvd_65p", "cvd_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getcvd) <- cities$cityname[match(names(getcvd), cities$city)]
  allstage1[[i]]$cvd <- c(allstage1[[i]]$cvd, 
    unlist(getcvd, recursive = F))
  
  # Respiratory
  getresp <- lapply(stage1res, function(x){
    out <- x[c("resp_main", "resp_65p", "resp_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getresp) <- cities$cityname[match(names(getresp), cities$city)]
  allstage1[[i]]$resp <- c(allstage1[[i]]$resp, 
    unlist(getresp, recursive = F))  
  
  # Cardiorespiratory
  getcpm <- lapply(stage1res, function(x){
    out <- x[c("cvresp_main", "cvresp_65p", "cvresp_75p")]
    out <- Map(function(o, a) {o$age <- a; o}, 
      out, 1:3)
    out
  })
  names(getcpm) <- cities$cityname[match(names(getcpm), cities$city)]
  allstage1[[i]]$cvresp <- c(allstage1[[i]]$cvresp, 
    unlist(getcpm, recursive = F))  
}

# Get city description
cities$countryname <- "UK"
citydesc <- rbind(citydesc, cities[,c("cityname", "countryname", "lon", "lat")])