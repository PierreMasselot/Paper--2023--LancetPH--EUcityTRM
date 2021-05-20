################################################################################
#
#                       Exhaustion Level 1
#
#                       Prep MCC data
#
################################################################################

# path <- "V:/VolumeQ/AGteam/MCCdata/data"
path <- paste0("C:/Users/PierreMasselot/Filr/Net Folders/",
  "StorageOnDemand Q/AGteam/MCCdata/data")

#----- Load age causes MCC dataset
load(sprintf("%s/MCC_age_classes/MCC_AgeCause_20200907.RData", path))

# Select cities in european countries
sel_countries <- c('cze9415', 'est9718', 'fnl9411', 'fra0014',  
  'grc0110', 'irl8407', 'por8012', 'spa0913', 'sui9513', 'swe9016')
dlist <- dlist[cities$country %in% sel_countries]
cities <- cities[cities$country %in% sel_countries,]

#----- Tidy data

# Include only 1985 on
for(i in seq(dlist)) dlist[[i]] <- dlist[[i]][dlist[[i]]$year >= 1990,]

# Remove unused levels in factors
ind1 <- c("cityname","country","countryname")
cities[ind1] <- lapply(cities[ind1], droplevels)

#----- Organize age data

# Czech Republic
for (i in which(cities$country %in% 'cze9415')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$nonext
  dlist[[i]]$nat_75p <- dlist[[i]]$nonext_7599
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_75p <- dlist[[i]]$cvd_7599
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$resp_75p <- dlist[[i]]$resp_7599
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_75p <- rowSums(dlist[[i]][,c("cvd_7599", "resp_7599")])
}

# Estonia
for (i in which(cities$country %in% 'est9718')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$all
  dlist[[i]]$nat_65p <- rowSums(dlist[[i]][,c("all6574", "all7584", 
    "all8599")])
  dlist[[i]]$nat_75p <- rowSums(dlist[[i]][,c("all7584", "all8599")])
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_65p <- rowSums(dlist[[i]][,c("cvd6574", "cvd7584", 
    "cvd8599")])
  dlist[[i]]$cvd_75p <- rowSums(dlist[[i]][,c("cvd7584", "cvd8599")])
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$resp_65p <- rowSums(dlist[[i]][,c("resp6574", "resp7584", 
    "resp8599")])
  dlist[[i]]$resp_75p <- rowSums(dlist[[i]][,c("resp7584", "resp8599")])
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_65p <- rowSums(dlist[[i]][,c("cvd6574", "cvd7584", 
    "cvd8599", "resp6574", "resp7584", "resp8599")])
  dlist[[i]]$cvresp_75p <- rowSums(dlist[[i]][,c("cvd7584", "cvd8599",
    "resp7584", "resp8599")])
}

# Finland
for (i in which(cities$country %in% 'fnl9411')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$all
  dlist[[i]]$nat_65p <- dlist[[i]]$all_6599
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_65p <- dlist[[i]]$cvd_6599
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$nat_65p <- dlist[[i]]$resp_6599
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_65p <- rowSums(dlist[[i]][,c("cvd_6599", "resp_6599")])

}

# France
for (i in which(cities$country %in% 'fra0014')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$nonext
  dlist[[i]]$nat_65p <- dlist[[i]]$nonext_6599
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_65p <- dlist[[i]]$cvd_6599
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$nat_65p <- dlist[[i]]$resp_6599
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_65p <- rowSums(dlist[[i]][,c("cvd_6599", "resp_6599")])
}

# Greece
for (i in which(cities$country %in% 'grc0110')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$all
  dlist[[i]]$nat_65p <- rowSums(dlist[[i]][,c("all_6574", "all_7584", 
    "all_8599")])
  dlist[[i]]$nat_75p <- rowSums(dlist[[i]][,c("all_7584", "all_8599")])
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7584", 
    "cvd_8599")])
  dlist[[i]]$cvd_75p <- rowSums(dlist[[i]][,c("cvd_7584", "cvd_8599")])
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$resp_65p <- rowSums(dlist[[i]][,c("resp_6574", "resp_7584", 
    "resp_8599")])
  dlist[[i]]$resp_75p <- rowSums(dlist[[i]][,c("resp_7584", "resp_8599")])
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7584", 
    "cvd_8599", "resp_6574", "resp_7584", "resp_8599")])
  dlist[[i]]$cvresp_75p <- rowSums(dlist[[i]][,c("cvd_7584", "cvd_8599",
    "resp_7584", "resp_8599")])
}

# Ireland
for (i in which(cities$country %in% 'irl8407')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$nonext
  dlist[[i]]$nat_65p <- rowSums(dlist[[i]][,c("nonext_6574", "nonext_7599")])
  dlist[[i]]$nat_75p <- dlist[[i]]$nonext_7599
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7599")])
  dlist[[i]]$cvd_75p <- dlist[[i]]$cvd_7599
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$resp_65p <- rowSums(dlist[[i]][,c("resp_6574", "resp_7599")])
  dlist[[i]]$resp_75p <- dlist[[i]]$resp_7599
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7599", 
    "resp_6574", "resp_7599")])
  dlist[[i]]$cvresp_75p <- rowSums(dlist[[i]][,c("cvd_7599",
    "resp_7599")])
}

# Portugal
for (i in which(cities$country %in% 'por8012')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$all
  dlist[[i]]$nat_65p <- dlist[[i]]$all_6599
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
}

# Spain
for (i in which(cities$country %in% 'spa0913')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$all
  dlist[[i]]$nat_65p <- rowSums(dlist[[i]][,c("all_6574", "all_7599")])
  dlist[[i]]$nat_75p <- dlist[[i]]$all_7599
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7599")])
  dlist[[i]]$cvd_75p <- dlist[[i]]$cvd_7599
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$resp_65p <- rowSums(dlist[[i]][,c("resp_6574", "resp_7599")])
  dlist[[i]]$resp_75p <- dlist[[i]]$resp_7599
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7599", 
    "resp_6574", "resp_7599")])
  dlist[[i]]$cvresp_75p <- rowSums(dlist[[i]][,c("cvd_7599",
    "resp_7599")])
}

# Switzerland
for (i in which(cities$country %in% 'sui9513')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$all
  dlist[[i]]$nat_65p <- rowSums(dlist[[i]][,c("all_6574", "all_7584", 
    "all_8599")])
  dlist[[i]]$nat_75p <- rowSums(dlist[[i]][,c("all_7584", "all_8599")])
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
}

# Sweden
for (i in which(cities$country %in% 'swe9016')){
  # Natural
  dlist[[i]]$nat_main <- dlist[[i]]$nonext
  dlist[[i]]$nat_65p <- rowSums(dlist[[i]][,c("nonext_6574", "nonext_7584", 
    "nonext_8599")])
  dlist[[i]]$nat_75p <- rowSums(dlist[[i]][,c("nonext_7584", "nonext_8599")])
  
  # CVD
  dlist[[i]]$cvd_main <- dlist[[i]]$cvd
  dlist[[i]]$cvd_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7584", 
    "cvd_8599")])
  dlist[[i]]$cvd_75p <- rowSums(dlist[[i]][,c("cvd_7584", "cvd_8599")])
  
  # Resp
  dlist[[i]]$resp_main <- dlist[[i]]$resp
  dlist[[i]]$resp_65p <- rowSums(dlist[[i]][,c("resp_6574", "resp_7584", 
    "resp_8599")])
  dlist[[i]]$resp_75p <- rowSums(dlist[[i]][,c("resp_7584", "resp_8599")])
  
  # Cardioresp
  dlist[[i]]$cvresp_main <- rowSums(dlist[[i]][,c("cvd", "resp")])
  dlist[[i]]$cvresp_65p <- rowSums(dlist[[i]][,c("cvd_6574", "cvd_7584", 
    "cvd_8599", "resp_6574", "resp_7584", "resp_8599")])
  dlist[[i]]$cvresp_75p <- rowSums(dlist[[i]][,c("cvd_7584", "cvd_8599",
    "resp_7584", "resp_8599")])
}

#----- Save data

save(dlist, cities, file = "data/mccdata.RData")