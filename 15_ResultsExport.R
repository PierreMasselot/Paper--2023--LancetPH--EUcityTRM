################################################################################
#
# Excess mortality attributed to heat and cold: 
#   a health impact assessment study in 854 cities in Europe
#
# The Lancet Planetary Health, 2023
# https://doi.org/10.1016/S2542-5196(23)00023-2
#
# (Reproducible) R Code
# Part 15: Export results
#
################################################################################

if (length(ls()) == 0) source("12_ResultsVulnerability.R")

#---------------------------
# Export full results at every geographical level
#---------------------------

# Results to export
toexport <- c("cityage", "city", "countryage", "country", "regionage", "region")

# Loop across all levels
for (d in toexport){
  export <- get(sprintf("%sres", d)) |> 
    # Remove some uninteresting variables
    dplyr::select(!starts_with("ratetotpop")) |>
    # Rename variables
    rename_with(~ gsub("an", "excess", .x), starts_with("an")) |>
    rename_with(~ gsub("rr", "rr_", .x), starts_with("rr")) |>
    rename_with(~ gsub("rate_", "rawrate_", .x), starts_with("rate_"))
  
  # Export
  write.csv(export, file = sprintf("results/%s.csv", d), row.names = F)
}

# Put everything into a zip file (requires Rtools installed)
setwd("results")
zip("results.zip", files = sprintf("%s.csv", toexport))
unlink(sprintf("%s.csv", toexport))
setwd("..")

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
write.csv(splinecoefs, "results/coefs.csv", row.names = F)

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
write.csv(splinevcov, "results/vcov.csv", row.names = F)

#----- Export simulated coefficients

# Bind everything
simu_out <- foreach(i = 1:nca, .combine = rbind, .multicombine = T) %do% {
  colnames(attrlist[[i]]$coefsim) <- 
    gsub(".pred", "", colnames(attrlist[[i]]$coefsim), fixed = T)
  data.frame(cityageres[i,c("URAU_CODE", "agegroup")], sim = 1:nsim, 
    attrlist[[i]]$coefsim)
}

# Write
write.csv(simu_out, "results/coef_simu.csv", row.names = F)

#----- Export temperature percentiles

# Compute percentiles
alltmeandist <- t(sapply(era5series, 
  function(x) quantile(x$era5landtmean, c(0, predper / 100, 1))))

# Export
out <- data.frame(URAU_CODE = rownames(alltmeandist), alltmeandist,
  check.names = F)
write.csv(out, "results/tmean_distribution.csv", row.names = F)

#---------------------------
# Export second-stage model
#---------------------------

# Save objects
save(plsres, stage2res, vgfit, file = "data/meta-model.RData")

# Add to additional_data zipfile
setwd("data")
zip("additional_data.zip", files = "meta-model.RData")
unlink("meta-model.RData")
setwd("..")