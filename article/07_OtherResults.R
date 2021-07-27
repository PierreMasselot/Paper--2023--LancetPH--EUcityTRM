################################################################################
#
#                         MCC-EUcityTRM
#
#                         Other results
#
################################################################################


#---------------------------
# Curve by age
#---------------------------

#----- Predict coefficients for different ages

# Get design matrix for age only
mixterms <- delete.response(terms(stage2res))
agemm <- model.matrix(mixterms[grep("age", attr(mixterms, "term.labels"))],
  data.frame(age = agebreaks))

# Get meta coefficients and vcov for intercept and age
allmetac <- coef(stage2res)
ageinds <- grep("age|(Intercept)", names(allmetac))
agemetac <- allmetac[ageinds]
agemetav <- vcov(stage2res)[ageinds, ageinds]

# Predict DLNM coefficients and vcov for all ages
agepreds <- apply(agemm, 1, function(x){
  xexp <- t(x) %x% diag(nc)
  coefpred <- xexp %*% agemetac
  vcovpred <- xexp %*% agemetav %*% t(xexp)
  list(fit = coefpred, vcov = vcovpred)
})

#----- Compute ERF

# Multiply to coefficients to obtain rough predicted curve
firstpred <- ov_basis %*% sapply(agepreds, "[[", "fit")

# For each find MMP
agemmp <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]

# Obtain list of ERF
agecp <- Map(crosspred, basis = list(ov_basis), 
  coef = lapply(agepreds, "[[", "fit"), vcov = lapply(agepreds, "[[", "vcov"),
  model.link = "log", cen = agemmp, at = list(ovper))


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

#---------------------------
# Interpret components
#---------------------------

#----- Backtransform coefficients to have effect of each metavariable
# Get eigenvectors
# loads <- pcares$rotation[,seq_len(npc)]
loads <- plsres$projection[,1:npc]
colnames(loads) <- sprintf("pls%i", 1:npc)

# Backtransform coefficients
st2coefs <- coef(stage2res, format = "matrix")
inds <- grep("pls", rownames(st2coefs))
plscoefs <- st2coefs[inds,]
backcoefs <- loads %*% plscoefs

# Vcov matrix
inds <- grep("pls", rownames(vcov(stage2res)))
mixvcov <- vcov(stage2res)[inds, inds]
reploads <- loads %x% diag(nc)
backvcov <- reploads %*% mixvcov %*% t(reploads)

#----- Create curves for increase in one SD for each variable

# Crosspred with backtransformed coefficients
backcp <- lapply(seq_along(metaprednames), function(i){
  inds <- (1:nc) + (i - 1) * nc
  crosspred(ov_basis, coef = backcoefs[i,], vcov = backvcov[inds,inds],
    model.link = "log", cen = median(ovper), at = ovper)
})

# Wald test
waldres <- t(sapply(seq_along(metaprednames), function(i){
  inds <- (1:nc) + (i - 1) * nc
  waldstat <- backcoefs[i,,drop = F] %*% solve(backvcov[inds,inds]) %*%
    backcoefs[i,]
  pval <- 1 - pchisq(waldstat, nc)
  return(list(waldstat = waldstat, pvalue = pval))
}))
rownames(waldres) <- metaprednames

#----- Difference between extreme values

# Compute high and low values (of scaled meta-variables)
extmeta <- apply(scale(metavar), 2, function(x) diff(quantile(x , c(.01, .99))),
  simplify = F)
newmetax <- as.matrix(bdiag(extmeta))