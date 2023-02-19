################################################################################
#
#                         MCC-EUcityTRM
#
#                   Big city-level results Table
#
################################################################################

if (length(ls()) == 0) source("12_ResultsVulnerability.R")

#---------------------------
# Export full city results
#---------------------------

cityageexport <- cityageres |> 
  # Remove some uninteresting variables
  dplyr::select(!starts_with("ratetotpop")) |>
  # Rename variables
  rename_with(~ gsub("an", "excess", .x), starts_with("an")) |>
  rename_with(~ gsub("rr", "rr_", .x), starts_with("rr")) |>
  rename_with(~ gsub("rate_", "rawrate_", .x), starts_with("rate_")) |>
  # Add std rate info
  left_join(dplyr::select(cityres, URAU_CODE, starts_with("stdrate")))
  
# Save
write.csv(cityageexport, file = gzfile("results/city_results.csv.gz"), row.names = F)

#---------------------------
# Export coefs/vcov for use in projection studies
#---------------------------

#----- Export spline coefficients

# Extract
splinecoefs <- t(sapply(cityagecoefs, "[[", "fit"))

# Add city and age group info
cityageinfo <- do.call(rbind, strsplit(rownames(splinecoefs), "_"))
colnames(cityageinfo) <- c("URAU_CODE", "agegroup")
splinecoefs <- data.frame(cityageinfo, splinecoefs)

# Export
write.csv(splinecoefs, gzfile("results/coefs.csv.gz"), row.names = F)

#----- Export vcov matrices

# Extract
trimat <- lower.tri(cityagecoefs[[1]]$vcov, diag = T)
splinevcov <- t(sapply(cityagecoefs, function(x) c(x$vcov[trimat])))
colnames(splinevcov) <- sprintf("v%i%i", row(cityagecoefs[[1]]$vcov)[trimat], 
  col(cityagecoefs[[1]]$vcov)[trimat])

# Add city and age group info
cityageinfo <- do.call(rbind, strsplit(rownames(splinevcov), "_"))
colnames(cityageinfo) <- c("URAU_CODE", "agegroup")
splinevcov <- data.frame(cityageinfo, splinevcov)

# Export
write.csv(splinevcov, gzfile("results/vcov.csv.gz"), row.names = F)

#----- Export simulated coefficients

for (i in 1:nca){
  colnames(attrlist[[i]]$coefsim) <- 
    gsub(".pred", "", colnames(attrlist[[i]]$coefsim), fixed = T)
  write.csv(attrlist[[i]]$coefsim, gzfile(sprintf("results/simu/%s.csv.gz", 
    names(cityagecoefs)[i])), row.names = F)
}
