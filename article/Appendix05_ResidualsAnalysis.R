################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix: residuals analysis
#
################################################################################

library(ggplot2)
library(scales)
library(reshape2)
library(eurostat)

#---------------------------
# Extract residuals
#---------------------------

# Extract from stage 2 result
res <- residuals(stage2res)
yhat <- predict(stage2res)

# Create a long format DF with residuals
ncoefs <- ncol(coefs)
repind <- rep(1:nrow(stage2df), ncoefs)
st2df_long <- cbind(coef = rep(sprintf("b%i", seq_len(ncoefs)), nrow(stage2df)),
  residual = c(res), fitted = c(yhat), obs = c(coefs), 
  nmiss = apply(imputed[,names(metavar)], 1, sum)[repmcc][repind],
  stage2df[repind,], metavar[repmcc,][repind,], 
  totdeath = sapply(unlistresults, "[[", "totdeath"))

#---------------------------
# Residual plots
#---------------------------

#----- Map -----

# Country layout
euromap <- get_eurostat_geospatial(nuts_level = "0", year = "2021")

# Map
ggplot(data = st2df_long) + theme_void() + 
  geom_sf(data = euromap, fill = grey(.95)) + 
  geom_point(aes(x = lon, y = lat, fill = res), pch = 21) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)]) + 
  scale_size(trans = "log10", name = "Population", range = c(0, 5)) + 
  scale_fill_gradient2(trans = pseudo_log_trans(base = 10)) + 
  facet_wrap(~ coef)

ggsave("figures/FigS3_1_ResMap.pdf", device = pdf)

#----- Fitted vs Obs -----


ggplot(st2df_long) + theme_classic() + 
  xlim(quantile(coefs, c(.01, .99))) + ylim(quantile(coefs, c(.01, .99))) + 
  geom_point(aes(x = obs, y = fitted)) + 
  geom_abline(slope = 1, intercept = 0, col = 4) + 
  xlab("Stage 1 coefficient") + ylab("Fitted coefficient") + 
  facet_wrap(~ coef)

ggsave("figures/FigS3_2_ObsPred.pdf", device = pdf)

#----- Residuals vs country -----

ggplot(st2df_long) + theme_classic() + xlim(quantile(res, c(.01, .99))) + 
  geom_boxplot(aes(x = res, y = country, col = country), show.legend = F) +
  geom_vline(xintercept = 0) + 
  xlab("Residuals") + ylab("Country") + scale_y_discrete(limits = rev) + 
  facet_wrap(~ coef)

ggsave("figures/FigS3_3_ResCountry.pdf", device = pdf)

#----- Residuals vs age -----

ggplot(st2df_long) + theme_classic() + ylim(quantile(res, c(.01, .99))) + 
  geom_point(aes(x = age, y = res)) +
  geom_smooth(aes(x = age, y = res), col = 2) + 
  geom_hline(yintercept = 0) + 
  xlab("Age") + ylab("Residuals") + 
  facet_wrap(~ coef)

ggsave("figures/FigS3_4_ResAge.pdf", device = pdf)

#----- Residuals vs number of missings -----

ggplot(st2df_long) + theme_classic() + ylim(quantile(res, c(.01, .99))) + 
  geom_point(aes(x = nmiss, y = res)) +
  geom_smooth(aes(x = nmiss, y = res), col = 2) + 
  geom_hline(yintercept = 0) + 
  xlab("Number of imputed metavariables") + ylab("Residuals") + 
  facet_wrap(~ coef)

ggsave("figures/FigS3_5_ResNA.pdf", device = pdf)


#----- Residuals vs population -----

ggplot(st2df_long) + theme_classic() + ylim(quantile(res, c(.01, .99))) + 
  geom_point(aes(x = pop, y = res)) +
  geom_smooth(aes(x = pop, y = res), col = 2) + 
  geom_hline(yintercept = 0) + 
  xlab("Population") + ylab("Residuals") + scale_x_continuous(trans = "log10") +
  facet_wrap(~ coef)

ggsave("figures/FigS3_6_ResPop.pdf", 
  device = pdf)


#----- Residuals vs all metavariables -----

for (i in seq_along(metavar)) {
  ggplot(st2df_long) + theme_classic() + ylim(quantile(res, c(.01, .99))) + 
    geom_point(aes_string(x = names(metavar)[i], y = res)) +
    geom_smooth(aes_string(x = names(metavar)[i], y = res), col = 2) + 
    geom_hline(yintercept = 0) + 
    xlab(names(metavar)[i]) + ylab("Residuals") + 
    facet_wrap(~ coef)
  ggsave(sprintf("figures/residuals/%s.pdf", names(metavar)[i]), device = pdf)
}

#----- Residuals vs totdeath -----

ggplot(st2df_long) + theme_classic() + ylim(quantile(res, c(.01, .99))) + 
  geom_point(aes(x = totdeath, y = res)) +
  geom_smooth(aes(x = totdeath, y = res), col = 2) + 
  geom_hline(yintercept = 0) + 
  xlab("Total observed deaths") + ylab("Residuals") + 
  scale_x_continuous(trans = "log10") +
  facet_wrap(~ coef)

ggsave("figures/FigS3_7_ResDeaths.pdf", device = pdf)


#---------------------------
# Curves plots
#---------------------------

#----- Extract stage 1 and fitted curves

# Acceptable MMP values 
inrange <- predper >= mmprange[1] & predper <= mmprange[2]

# Stage 1
st1curves <- Map(function(b, vc, era5){
  tmeanper <- quantile(era5$era5landtmean, predper / 100)
  bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
    knots = quantile(era5$era5landtmean, varper / 100))
  firstpred <- bvar %*% b
  mmt <- tmeanper[inrange][which.min(firstpred[inrange])]
  crosspred(bvar, coef = b, vcov = vc, cen = mmt, 
    model.link="log", at = quantile(era5$era5landtmean, predper / 100))
}, as.data.frame(t(coefs)), vcovs, era5series[repmcc])

# Fitted
fittedcurves <- Map(function(b, era5){
  tmeanper <- quantile(era5$era5landtmean, predper / 100)
  bvar <- onebasis(tmeanper, fun = varfun, degree = vardegree, 
    knots = quantile(era5$era5landtmean, varper / 100))
  firstpred <- bvar %*% b$fit
  mmt <- tmeanper[inrange][which.min(firstpred[inrange])]
  crosspred(bvar, coef = b$fit, vcov = b$vcov, cen = mmt, 
    model.link="log", at = quantile(era5$era5landtmean, predper / 100))
}, predict(stage2res, vcov = T), era5series[repmcc])

#----- Plot
pdf("figures/FigS3_8_fittedERF.pdf", width = 9, height = 13)
layout(matrix(seq(6 * 4), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(fittedcurves)){
    # Plot cold and heat separately
  plot(st1curves[[i]], xlab = "Temperature (°C)", ylab = "RR", 
    main = rownames(coefs)[i], lwd = 2, ylim = c(.5, 3))
  lines(fittedcurves[[i]], col = 2, lwd = 2, ci = "lines")
  abline(h = 1, lty = 2)
}

dev.off()