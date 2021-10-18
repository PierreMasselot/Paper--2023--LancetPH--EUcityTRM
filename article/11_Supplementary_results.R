################################################################################
#
#                         MCC-EUcityTRM
#
#                           Supplementary results
#
################################################################################

#---------------------------
# Regional effect
#---------------------------

#----- Predict coefs for each country

# Create prediction df
countrydf <- data.frame(region = regionlist[euromap$CNTR_CODE], 
  age = mean(agevals), rep(list(0), npc), 
  row.names = euromap$CNTR_CODE)
names(countrydf)[-(1:2)] <- sprintf("pls%i", seq_len(npc))

# Predict
cntrcoefs <- predict(stage2res, na.omit(countrydf), vcov = T)

#----- Summaries for each grid point

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(cntrcoefs, "[[", "fit")

# Find MMP
cntrmmt <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Predict RR at specifiec percentiles
cntrrr <- Map(function(b, mm){
  cp <- crosspred(ov_basis, coef = b$fit, vcov = b$vcov, cen = mm, 
    model.link="log", at = ovper[predper %in% resultper])
  t(rbind(RR = cp$allRRfit, low = cp$allRRlow, high = cp$allRRhigh))
}, cntrcoefs, cntrmmt)

# Summary data.frame
countrydf[!is.na(countrydf$region), "mmt"] <- cntrmmt
countrydf[!is.na(countrydf$region), "rr"] <- t(sapply(cntrrr, "[", , 1))


#------------------------
# Components interpretation
#------------------------

#----- Difference between extreme values

# Compute high and low values (of scaled meta-variables)
extmeta <- apply(scale(metavar), 2, function(x) diff(quantile(x , c(.01, .99))),
  simplify = F)
newmetax <- as.matrix(bdiag(extmeta))

#----- Screeplot
# plot(summary(pcares)$importance[3,] * 100, type = "b", pch = 16, col = 4, 
#   ylab = "Cumulative variance proportion (%)", xlab = "Principal component")
# abline(h = (summary(pcares)$importance[3, npc] * 100), lty = 2)

#----- Interpretation of PCs

# # Common scale
# ylims <- range(loads)
# 
# # Plot sequentially
# x11(height = 15, width = 10)
# par(mfrow = c(7, 1), mar = c(1, 4, 3, 2) + .1, oma = c(7, 0, 0, 0))
# for (i in seq_len(npc)) {
#   largest <- rank(-abs(loads[,i])) <= 5
#   bp <- barplot(loads[,i], names.arg = "", ylim = ylims, 
#     main = sprintf("PC%i", i), col = adjustcolor(ifelse(largest, 2, 4), .5))
#   text(bp[largest,], loads[largest,i] / 2, rownames(loads)[largest], srt = 90,
#     adj = c(0.5, 0.5), cex = .9)
#   abline(h = 0)
# }
# axis(1, at = bp, labels = rownames(loads), las = 3)
# 
# dev.print(pdf, file = "figures/PCs_bp.pdf")

#----- Coefficients associated to metapredictors

# Create overall data.frame for plotting
backdf <- data.frame(est = c(t(backcoefs)), se = sqrt(diag(backvcov)), 
  coef = rep(colnames(coefs), nm),
  var = factor(rep(metaprednames, each = nc), levels = metaprednames))

# Plot all of them
ggplot(backdf, aes(x = var)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), 
    width = .3) + 
  geom_point(aes(y = est), col = 4) + 
  facet_wrap(~ coef, ncol = 1) + 
  xlab("Metapredictor") + ylab("Coefficient") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/MetapredCoefficients.pdf", height = 15)

# By an increase of 1 SD
x11(height = 15, width = 10)
par(mfrow = c(7, 3), mar = c(4, 4, 3, 1))
for (i in seq_along(metaprednames)){
  plot(backcp[[i]], ptype = "overall", ylab = "RR increase", 
    xlab = "Temperature percentile", main = metaprednames[i],
    ylim = c(.9, 1.1), lwd = 2, col = NA, xaxt = "n")
  abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  ovcurve <- backcp[[i]]$allRRfit
  lines(ovper, ovcurve, lwd = 2, col = "darkred")
  ovcurve[ovcurve > 1] <- NA
  lines(ovper, ovcurve, lwd = 2, col = "forestgreen")
  abline(h = 1)
  text(mean(par("usr")[1:2]), par("usr")[3], 
    sprintf("p-value = %0.4f", waldres[i,2]), pos = 3, cex = 1.2)
}

dev.print(pdf, file = "figures/Fig4 - MetapredEffectSD.pdf", 
  width = 10, height = 15)

# Difference between low and large values
cols <- plasma(2, direction = -1, begin = .1, end = .9)

x11(height = 15, width = 10)
par(mfrow = c(7, 3), mar = c(4, 4, 3, 1))
for (i in seq_along(metaprednames)){
  inds <- 1:2 + 2*(i-1)
  plot(NA, ylab = "RR", xlab = "Temperature percentile", 
    main = metaprednames[i], xaxt = "n", bty = "l",
    xlim = range(ovper), ylim = c(.95, 2))
  abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  lines(extcp[[inds[1]]], ptype = "overall", col = cols[1], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(cols[1], .2)))
  lines(extcp[[inds[2]]], ptype = "overall", col = cols[2], ci = "area", 
    lwd = 2, ci.arg = list(col = adjustcolor(cols[2], .2)))
  abline(h = 1)
  legend("topleft", legend = c("low", "high"), bty = "n", ncol = 2, cex = .8,
    lwd = 2, col = cols)
}
dev.print(pdf, file = "figures/MetapredEffectDiff.pdf", width = 10, height = 15)


#------------------------
# Coefficients for all ages
#------------------------

source("00_Packages_Parameters.R")
load("results/cityResults.RData")

#----- Compute for all ages

# Prepare results dataset
cityall <- metadata[, c("URAU_CODE", "LABEL", "CNTR_CODE", "pop", "lon", "lat",
  "region")]
eurcntr <- rbind(eu_countries, efta_countries) # Objects from eurostat
cityall$cntr_name <- eurcntr[match(cityall$CNTR_CODE, eurcntr[,1]),2]

# Use life expectancy to compute the coefficients
cityall$age <- metadata$lifexp

# Add PLS information
cityall <- cbind(cityall, pcvar)

# Estimate coefficients of ERF
wholeERF <- predict(stage2res, cityall, vcov = T)

#----- Put together data with age-specific

citydesc <- rbind(cityall[,c("URAU_CODE", "LABEL", "CNTR_CODE", "cntr_name", 
    "region", "pop", "lon", "lat", "age")],
  cityres[,c("URAU_CODE", "LABEL", "CNTR_CODE", "cntr_name", 
    "region", "pop", "lon", "lat", "age")])

allcoefs <- c(cityERF, wholeERF)

dlnmpars <- list(fun = varfun, percentiles = varper, degree = vardegree)

save(citydesc, allcoefs, dlnmpars, file = "results/allCoefs.RData")
