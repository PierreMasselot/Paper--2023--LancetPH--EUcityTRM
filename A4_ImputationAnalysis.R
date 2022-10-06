################################################################################
#
#                         MCC-CityEurope
#
#                     Appendix 1: analysis of impited values
#
################################################################################

if (length(ls()) == 0){
  source("00_Packages_Parameters.R")
  
  load("data/Alldata.RData")
}
# #-----------------------
# # Checking missing values
# #-----------------------
# 
# #----- Number of missing per city
# 
# # Extract number of missings
# citymis <- apply(imputed, 1, sum)
# 
# # Check cities with more than 10 missings
# metadata[citymis > 10, ]

#-----------------------
# Checking convergence
#-----------------------

# # Create plot
# plot(meta_imp, layout = c(2, sum(metadesc$nmis > 0)))
# 
# # Save
# dev.print(png, file = "figures/FigS_mice_convergence.png", 
#   width = 8, height = 15, units = "in")

#-----------------------
# Comparing density of observed and imputed
#-----------------------

# Create plot
densityplot(meta_imp, 
  strip = lattice::strip.custom(factor.levels = 
      subset(metadesc, nmis > 0, label, drop = T)))

dev.print(png, file = "figures/FigS_imputedDensity.png", 
  width = 10, height = 7, units = "in", res = 300)

#-----------------------
# Comparing imputed data for population
#-----------------------

# # Number of simulations
# nsim <- 50
# 
# # Check which variables will be tested
# varmiss <- apply(is.na(metavar), 2, any)
# 
# # Sample obs that will be NAs
# set.seed(12345)
# remobs <- replicate(nsim, sample.int(nrow(metavar), nrow(metavar) / 10))
# remvar <- replicate(nsim, 
#   sample(which(varmiss), nrow(metavar) / 10, replace = T))
# 
# #----- Apply PMM on each
# simres <- lapply(seq_len(nsim), function(i){
#   # Remove data
#   imeta <- metavar
#   for (j in seq_len(nrow(metavar) / 10)) imeta[remobs[j,i], remvar[j,i]] <- NA
#   
#   # Inititalize
#   resmice <- matrix(NA, nrow = nrow(metavar) / 10, 4, 
#     dimnames = list(NULL, c("PMMmedian", "PMMlast", "cartmedian", "cartlast")))
#   
#   # Apply PMM median
#   res <- mice(imeta, method = "pmm", ridge = 1e-01, print = F)
#   allres <- sapply(1:5, function(x) {
#     out <- rep(NA, nrow(metavar) / 10)
#     for (j in seq_len(nrow(metavar) / 10)){ 
#       out[j] <- complete(res, x)[remobs[j,i], remvar[j,i]]
#     }
#     out
#   })
#   resmice[,1] <- apply(allres, 1, median)
#   
#   # Apply PMM last
#   res <- mice(imeta, method = "pmm", ridge = 1e-01, print = F)
#   for (j in seq_len(nrow(metavar) / 10)){ 
#     resmice[j,2] <- complete(res, 5)[remobs[j,i], remvar[j,i]]
#   }
#   
#   # Apply CART median
#   res <- mice(imeta, method = "cart", print = F)
#   allres <- sapply(1:5, function(x) {
#     out <- rep(NA, nrow(metavar) / 10)
#     for (j in seq_len(nrow(metavar) / 10)){ 
#       out[j] <- complete(res, x)[remobs[j,i], remvar[j,i]]
#     }
#     out
#   })
#   resmice[,3] <- apply(allres, 1, median)
#   
#   # Apply CART last
#   res <- mice(imeta, method = "cart", print = F)
#   for (j in seq_len(nrow(metavar) / 10)){ 
#     resmice[j,4] <- complete(res, 5)[remobs[j,i], remvar[j,i]]
#   }
#   
#   # Output
#   resmice
# })
# 
# # Put everything together
# allres <- do.call(rbind, simres)
# 
# # Extract true values
# truevals <- rep(NA, nsim * floor(nrow(metavar) / 10))
# for (i in seq_len(nsim * floor(nrow(metavar) / 10)))
#   truevals[i] <- metavar[c(remobs)[i], c(remvar)[i]]
# 
# #----- Plots
# 
# # Error
# RMSE <- apply(allres, 2, function(d) tapply(seq_along(remvar), c(remvar), 
#   function(x) sqrt(mean((d[x] - truevals[x])^2, na.rm = T))))
# 
# barplot(scale(t(RMSE), center = F, scale = T), beside = T, col = 1:4, 
#   names.arg = names(metavar)[as.numeric(rownames(RMSE))])
# 
# # Visualization
# par(mfrow = n2mfrow(sum(varmiss)))
# 
# for (i in seq_along(which(varmiss))){
#   matplot(truevals[c(remvar) == which(varmiss)[i]], 
#     allres[c(remvar) == which(varmiss)[i],], xlab = "True", ylab = "Imputed",
#     log = "xy", pch = 15:18, main = names(metavar)[which(varmiss)[i]])
#   abline(a = 0, b = 1, lwd = 2)
#   # legend("topleft", col = 1:4, pch = 15:18,
#   #   legend = c("PMM median", "PMM last", "CART median", "CART last"))
# }

