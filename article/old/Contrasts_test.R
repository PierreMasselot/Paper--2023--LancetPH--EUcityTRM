

agepreddf <- expand.grid(age = agetot, region = unique(mccregion))
agepreddf <- cbind(agepreddf, matrix(0, nrow = nrow(agepreddf), ncol = npc, 
  dimnames = list(NULL, colnames(pcvar))))

# Predict coefficients
agecoefs <- predict(stage2res, agepreddf, vcov = T)

bgpred <- function(coef){
  firstpred <- ov_basis %*% coef$fit
  mmt <- ovper[inrange][which.min(firstpred[inrange,])]
  crosspred(ov_basis, coef = coef$fit, vcov = coef$vcov, cen = mmt, 
    model.link = "log", at = ovper)
}

agecurves <- lapply(agecoefs, bgpred)

# Grand mean
tt <- delete.response(terms(stage2res))
allmm <- model.matrix(tt, model.frame(tt, agepreddf))
ageind <- grep("age|(Intercept)", colnames(allmm))
agepred <- allmm[seq_len(length(agebreaks) + 1),ageind] %*% 
  coef(stage2res, format = "matrix")[ageind,]
grandpred <- ov_basis %*% t(agepred)
grandmmt <- ovper[inrange][apply(grandpred[inrange,], 2, which.min)]
cenvec <- do.call(onebasis, list(x = grandmmt, fun = varfun, 
  degree = vardegree, knots = ovknots))
bvarcen <- apply(cenvec, 1, scale, x = ov_basis, scale = F, simplify = F)
grandagecurves <- mapply("%*%", bvarcen, as.data.frame(t(agepred)))


plot(NA, xlim = range(ovper), ylim = c(.8, 3),
  xlab = c("Temperature percentile"), ylab = "RR", xaxt = "n")
abline(v = ovaxis, h = axTicks(2), lty = 2, col = "lightgrey")
axis(1, at = ovaxis, labels = axisper)
whichage <- 5
pal <- viridis(nlevels(mccregion))
for (i in seq_along(levels(mccregion))){
  ind <- (i - 1) * (length(agebreaks) + 1) + whichage
  lines(agecurves[[ind]], lwd = 2, col = pal[i], 
    ci = "area", ci.arg = list(col = adjustcolor(pal[i], .2)))
}
lines(ovper, exp(grandagecurves[,whichage]), lwd = 3)
