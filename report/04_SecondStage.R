################################################################################
#
#                       Exhaustion Level 1
#
#                       Second stage meta-analysis
#
################################################################################

library(mixmeta)

#-------------------------
# Parameters
#-------------------------

# Lags considered. The main one is at first position
lags <- c(10, 3, 21)

# Outcomes
outcomes <- list(all = c("all", "tm"),
  cardiovascular = c("cvd", "cm"),
  )

#-------------------------
# Meta-analysis by lag
#-------------------------

# List result files
fillist <- list.files("results")  

# Prepare objects to store
metaresults <- vector("list", length(lags))
names(metaresults) <- lags

#----- Loop on lags

for (i in seq_along(lags)){
  
  #----- Load results for specific lag
  lagfiles <- grep(sprintf("lag%i", lags[i]), fillist, value = T)
  loadres <- lapply(lagfiles, function(x){
    load(sprintf("results/%s", x))
    list(stage1res, cities)
  })
  
  # Get cities list
  citylist <- lapply(loadres, function(x) as.character(x[[2]]$cityname))
  countries <- sapply(strsplit(lagfiles, "_"), "[", 2)
  cities <- data.frame(cityname = unlist(citylist), 
    country = rep(coutries, sapply(citylist, length)))
  
  # 
}
