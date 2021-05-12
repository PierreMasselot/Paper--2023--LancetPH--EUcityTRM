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
  fil <- grep(sprintf("Germany_lag%i", i), fillist, value = T)
  
  # Load it
  load(sprintf("results/%s", fil))
  
  #----- Extract results
  # All cause
  allstage1[[i]]$all <- c(allstage1[[i]]$all, lapply(stage1res, function(x){
    out <- x$all_main
    out$age <- 1
    out
  }))
  
  # CVD
  allstage1[[i]]$cvd <- c(allstage1[[i]]$cvd, lapply(stage1res, function(x){
    out <- x$cvd_main
    out$age <- 1
    out
  }))
  
  # Respiratory
  allstage1[[i]]$resp <- c(allstage1[[i]]$resp, lapply(stage1res, function(x){
    out <- x$resp_main
    out$age <- 1
    out
  }))
  
  # Respiratory
  allstage1[[i]]$cvresp <- c(allstage1[[i]]$cvresp, lapply(stage1res, function(x){
    out <- x$cvresp_main
    out$age <- 1
    out
  }))
}