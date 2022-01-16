################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix: residuals analysis
#
################################################################################

source("05_ResultsPrep.R")

#---------------------------
# Extract different ERFs
#---------------------------

#----- Indices to keep

# Keep only the oldest group for each location
tokeep <- by(cbind(seq_len(nrow(stage2df)), stage2df$age), stage2df$city, 
  function(x) x[which.max(x[,2]),1])

#----- Extract all ERFS

# First stage coefficients
ycoefs <- coefs[tokeep,]
yvcovs <- vcovs[tokeep]
firstpred <- ov_basis %*% t(ycoefs)
mmts <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]
fserfs <- Map(crosspred, basis = list(ov_basis), 
  coef = as.data.frame(t(ycoefs)), vcov = yvcovs, cen = mmts, 
  model.link = "log", at = list(ovper))

# Predictions
yhat <- predict(stage2res, vcov = T)[tokeep]
firstpred <- ov_basis %*% sapply(yhat, "[[", "fit")
mmts <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]
prederfs <- Map(function(x, mmt) crosspred(ov_basis, coef = x$fit, 
    vcov = x$vcov, cen = mmt, model.link = "log", at = ovper), 
  yhat, mmts)

# BLUPS
yblups <- blup(stage2res,vcov = T)[tokeep]
firstpred <- ov_basis %*% sapply(yblups, "[[", "blup")
mmts <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]
bluperfs <- Map(function(x, mmt) crosspred(ov_basis, coef = x$blup, 
  vcov = x$vcov, cen = mmt, model.link = "log", at = ovper), 
  yblups, mmts)

#----- Summary data.frame

# Extract MMTs and percentiles
fsres<- sapply(fserfs, 
  function(x) c(x$allRRfit[predper %in% resultper], x$cen))
predres <- sapply(prederfs, 
  function(x) c(x$allRRfit[predper %in% resultper], x$cen))
blupres <- sapply(bluperfs, 
  function(x) c(x$allRRfit[predper %in% resultper], x$cen))

# Put together in a data.frame
allres <- data.frame(metadata[repmcc,
    c("URAU_CODE", "region", "CNTR_CODE", "lon", "lat", "pop")][tokeep,], 
  age = stage2df$age[tokeep],
  t(fsres), t(blupres), t(predres))
names(allres)[-(1:7)] <- c(outer(c("cold", "heat", "mmt"), c("fs", "blup", "pred"), 
  paste, sep = "_"))

#---------------------------
# Residual plots
#---------------------------

#----- Map -----

# Map
basic_map <- ggplot(data = allres, aes(x = lon, y = lat, size = pop)) +
  theme_void() +
  geom_sf(data = euromap, fill = grey(.95), inherit.aes = F, col = grey(.5)) +
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  scale_size(name = "Population", range = c(2, 6), guide = "none") +
  theme(legend.position = "bottom", legend.box = "vertical") +
  geom_point(alpha = .9, pch = 21, colour = "white", stroke = .1) +
  guides(fill = guide_coloursteps(title.position = "top", title.hjust = .5,
    barwidth = 12, barheight = .8, even.steps = T))

# Plot heat and cold
rescoldmap <- basic_map + aes(fill = exp(log(cold_pred) - log(cold_blup))) +
  scale_fill_steps2(midpoint = 1, name = "Cold RR residuals")

resheatmap <- basic_map + aes(fill = exp(log(heat_pred) - log(heat_blup))) +
  scale_fill_steps2(midpoint = 1, breaks = seq(.7, 1.3, by = .1), 
    name = "Heat RR residuals")

# ggsave("figures/FigS3_1_ResMap.pdf", device = pdf)

#----- Fitted vs Obs -----

ggplot(allres) + theme_classic() + 
  geom_point(aes(x = cold_blup, y = cold_pred, col = age)) + 
  scale_colour_stepsn(breaks = agebreaks, 
    colours = mako(length(agelabs), direction = -1)) + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("BLUP") + ylab("Predicted") + ggtitle("Cold RR")

ggplot(allres) + theme_classic() + 
  xlim(c(1, 2)) + ylim(c(1, 2)) + 
  geom_point(aes(x = heat_blup, y = heat_pred, col = age)) + 
  scale_colour_stepsn(breaks = agebreaks, 
    colours = rocket(length(agelabs), direction = -1)) + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("BLUP") + ylab("Predicted") + ggtitle("Heat RR")

# ggsave("figures/FigS3_2_ObsPred.pdf", device = pdf)

#----- Residuals vs country -----

ggplot(allres) + theme_classic() + 
  geom_boxplot(aes(x = cold_pred - cold_blup, y = CNTR_CODE, fill = CNTR_CODE), 
    show.legend = F, varwidth = T) +
  geom_vline(xintercept = 0) + 
  xlab("Residuals") + ylab("Country") + ggtitle("Cold") +
  scale_y_discrete(limits = rev)

ggplot(allres) + theme_classic() + xlim(c(-3, 3)) +
  geom_boxplot(aes(x = heat_pred - heat_blup, y = CNTR_CODE, fill = CNTR_CODE), 
    show.legend = F, varwidth = T) +
  geom_vline(xintercept = 0) + 
  xlab("Residuals") + ylab("Country") + ggtitle("Heat") +
  scale_y_discrete(limits = rev)
# ggsave("figures/FigS3_3_ResCountry.pdf", device = pdf)

#----- Residuals vs region -----

ggplot(allres) + theme_classic() + 
  geom_boxplot(aes(x = cold_pred - cold_blup, y = region, fill = region), 
    show.legend = F, varwidth = T) +
  geom_vline(xintercept = 0) + 
  xlab("Residuals") + ylab("Region") + ggtitle("Cold") +
  scale_y_discrete(limits = rev)

ggplot(allres) + theme_classic() + xlim(c(-3, 3)) +
  geom_boxplot(aes(x = heat_pred - heat_blup, y = region, fill = region), 
    show.legend = F, varwidth = T) +
  geom_vline(xintercept = 0) + 
  xlab("Residuals") + ylab("Region") + ggtitle("Heat") +
  scale_y_discrete(limits = rev)
# ggsave("figures/FigS3_3_ResCountry.pdf", device = pdf)

#----- Residuals vs lat / lon -----

ggplot(allres, aes(x = lat)) + theme_classic() + 
  geom_point(aes(y = cold_pred - cold_blup), col = adjustcolor(4, .5)) +
  geom_smooth(aes(y = cold_pred - cold_blup), col = 4) +
  geom_point(aes(y = heat_pred - heat_blup), col = adjustcolor(2, .5)) +
  geom_smooth(aes(y = heat_pred - heat_blup), col = 2) +
  geom_hline(yintercept = 0) + 
  ylab("Residuals") + xlab("Latitude") + ylim(c(-2, 2))

ggsave("figures/residuals/latitude.pdf")

ggplot(allres, aes(x = lon)) + theme_classic() + 
  geom_point(aes(y = cold_pred - cold_blup), col = adjustcolor(4, .5)) +
  geom_smooth(aes(y = cold_pred - cold_blup), col = 4) +
  geom_point(aes(y = heat_pred - heat_blup), col = adjustcolor(2, .5)) +
  geom_smooth(aes(y = heat_pred - heat_blup), col = 2) +
  geom_hline(yintercept = 0) + 
  ylab("Residuals") + xlab("Longitude") + ylim(c(-2, 2))

#----- Residuals vs climatic region -----

# Add KG classif info
kgcdf <- allres[,c("URAU_CODE", "lon", "lat")]
names(kgcdf) <- c("Site", "Longitude", "Latitude")
allres$kgc <- substr(LookupCZ(kgcdf, rc = T), 1, 1)

ggplot(allres) + theme_classic() + 
  geom_boxplot(aes(x = cold_pred - cold_blup, y = kgc, fill = kgc), 
    show.legend = F, varwidth = T) +
  geom_vline(xintercept = 0) + 
  xlab("Residuals") + ylab("KGC") + ggtitle("Cold") +
  scale_y_discrete(limits = rev)

ggsave("figures/residuals/KGCcold.pdf")

ggplot(allres) + theme_classic() + xlim(c(-3, 3)) +
  geom_boxplot(aes(x = heat_pred - heat_blup, y = kgc, fill = kgc), 
    show.legend = F, varwidth = T) +
  geom_vline(xintercept = 0) + 
  xlab("Residuals") + ylab("KGC") + ggtitle("Heat") +
  scale_y_discrete(limits = rev)

ggsave("figures/residuals/KGCheat.pdf")

#----- Residuals vs temperature -----

# Add temperature info
allres$temp <- metadata$tmean[repmcc][tokeep]

ggplot(allres, aes(x = temp)) + theme_classic() + 
  geom_point(aes(y = cold_pred - cold_blup), col = adjustcolor(4, .5)) +
  geom_smooth(aes(y = cold_pred - cold_blup), col = 4) +
  geom_point(aes(y = heat_pred - heat_blup), col = adjustcolor(2, .5)) +
  geom_smooth(aes(y = heat_pred - heat_blup), col = 2) +
  geom_hline(yintercept = 0) + 
  ylab("Residuals") + xlab("Mean temperature") + ylim(c(-2, 2))

#----- Residuals vs age -----

ggplot(allres, aes(x = age)) + theme_classic() + 
  geom_point(aes(y = cold_pred - cold_blup), col = adjustcolor(4, .5)) +
  geom_smooth(aes(y = cold_pred - cold_blup), col = 4) +
  geom_point(aes(y = heat_pred - heat_blup), col = adjustcolor(2, .5)) +
  geom_smooth(aes(y = heat_pred - heat_blup), col = 2) +
  geom_hline(yintercept = 0) + 
  ylab("Residuals") + xlab("Age") + ylim(c(-2, 2))

# ggsave("figures/FigS3_4_ResAge.pdf", device = pdf)

#----- Residuals vs number of missings -----

# Add info
allres$nmiss <- metadata$nmiss[repmcc][tokeep]

#Plot
ggplot(allres, aes(x = nmiss)) + theme_classic() + 
  geom_point(aes(y = cold_pred - cold_blup), col = adjustcolor(4, .5)) +
  geom_smooth(aes(y = cold_pred - cold_blup), col = 4) +
  geom_point(aes(y = heat_pred - heat_blup), col = adjustcolor(2, .5)) +
  geom_smooth(aes(y = heat_pred - heat_blup), col = 2) +
  geom_hline(yintercept = 0) + 
  ylab("Residuals") + xlab("# Imputed") + ylim(c(-2, 2))



#----- Residuals vs population -----

ggplot(allres, aes(x = pop)) + theme_classic() + 
  geom_point(aes(y = cold_pred - cold_blup), col = adjustcolor(4, .5)) +
  geom_smooth(aes(y = cold_pred - cold_blup), col = 4) +
  geom_point(aes(y = heat_pred - heat_blup), col = adjustcolor(2, .5)) +
  geom_smooth(aes(y = heat_pred - heat_blup), col = 2) +
  geom_hline(yintercept = 0) + scale_x_log10() +
  ylab("Residuals") + xlab("Population") + ylim(c(-2, 2))

ggsave("figures/residuals/population.pdf")


# #----- Residuals vs all metavariables -----
# 
# for (i in seq_along(metavar)) {
#   ggplot(st2df_long) + theme_classic() + ylim(quantile(res, c(.01, .99))) + 
#     geom_point(aes_string(x = names(metavar)[i], y = res)) +
#     geom_smooth(aes_string(x = names(metavar)[i], y = res), col = 2) + 
#     geom_hline(yintercept = 0) + 
#     xlab(names(metavar)[i]) + ylab("Residuals") + 
#     facet_wrap(~ coef)
#   ggsave(sprintf("figures/residuals/%s.pdf", names(metavar)[i]), device = pdf)
# }

#----- Residuals vs totdeath -----

# Add info
allres$totdeath <- sapply(unlistresults, "[[", "totdeath")[tokeep]

ggplot(allres, aes(x = totdeath)) + theme_classic() + 
  geom_point(aes(y = cold_pred - cold_blup), col = adjustcolor(4, .5)) +
  geom_smooth(aes(y = cold_pred - cold_blup), col = 4) +
  geom_point(aes(y = heat_pred - heat_blup), col = adjustcolor(2, .5)) +
  geom_smooth(aes(y = heat_pred - heat_blup), col = 2) +
  geom_hline(yintercept = 0) + scale_x_log10() +
  ylab("Residuals") + xlab("# recorded deaths") + ylim(c(-2, 2))

# ggsave("figures/FigS3_7_ResDeaths.pdf", device = pdf)


#---------------------------
# Curves plots
#---------------------------

#----- Plot
pdf("figures/ERFcomparison.pdf", width = 9, height = 13, pointsize = 8)
layout(matrix(seq(6 * 4), nrow = 6, byrow = T))
par(mar = c(4,3.8,3,2.4), mgp = c(2.5,1,0), las = 1)

# Loop on all cities
for(i in seq_along(tokeep)){
  # Plot cold and heat separately
  plot(fserfs[[i]], xlab = "Temperature percentile", ylab = "RR", 
    main = metadata$LABEL[repmcc][tokeep][i], 
    ylim = c(.5, 3.5), cex.main = .9, xaxt = "n",
    ci.arg = list(col = adjustcolor(grey(.5), .2)))
  abline(v = fserfs[[i]]$cen, lty = 3)
  lines(bluperfs[[i]], col = 2, ci = "area", 
    ci.arg = list(col = adjustcolor(2, .2)))
  abline(v = bluperfs[[i]]$cen, lty = 3, col = 2)
  lines(prederfs[[i]], col = 4, ci = "area", 
    ci.arg = list(col = adjustcolor(4, .2)))
  abline(v = prederfs[[i]]$cen, lty = 3, col = 4)
  axis(1, at = ovper[predper %in% c(1, 25, 50, 75, 99)], 
    labels = c(1, 25, 50, 75, 99))
  legend("topleft", legend = c("First-stage", "BLUP", "Prediction"),
    lty = 1, col = c(1, 2, 4), horiz = T, bty = "n", cex = .8)
}

dev.off()