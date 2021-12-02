
#----------------------
# Antonio's 
#----------------------

blup <- unique(blup(stage2res, type="residual"))
mccgeo <- st_as_sf(cities, coords=c("long","lat"), crs=st_crs(4326))
mccgeo <- cbind(mccgeo, blup)
mccgeo <- st_transform(mccgeo, crs=st_crs(metageo))
plot(mccgeo)

ggplot(data=mccgeo) + 
  geom_sf(data = euromap, fill = grey(.95), inherit.aes = F, col = grey(.5)) + 
  geom_sf(aes(col=b5)) +
  scale_colour_gradient2(low="blue", mid="white", high="red", midpoint=0) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  theme_void() 

library(gstat)
idwpred <- idw(b5~1, mccgeo, newdata=metageo, idp=2)

ggplot(data=idwpred) + 
  geom_sf(aes(col=var1.pred)) +
  geom_sf(data = euromap, fill=NA, inherit.aes = F, col = grey(.5)) + 
  scale_colour_gradient2(low="blue", mid="white", high="red", midpoint=0) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  theme_void() 

xyrst <- raster(extent(euromap), nrow=400, ncol=400) |>
  coordinates() |>
  as.data.frame() |>
  st_as_sf(coords=c("x","y"), crs=st_crs(euromap)) |>
  st_intersection(st_union(euromap)) |>
  st_transform(crs=st_crs(mccgeo))

idwpred <- idw(b5~1, mccgeo, newdata=xyrst, idp=2)

ggplot(data=idwpred) + 
  geom_sf(aes(col=var1.pred), alpha=0.7) +
  geom_sf(data = euromap, fill=NA, inherit.aes = F, col = grey(.5)) + 
  scale_colour_gradient2(low="blue", mid="white", high="red", midpoint=0) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  theme_void() 

#----------------------
# An attempt at multivariate
#----------------------

# IDW
multidw <- NULL
unidw <- vector("list", nc)
for (i in 1:nc) {
  form <- sprintf("b%i ~ 1", i)
  multidw <- gstat(multidw, formula = as.formula(form), 
    data = mccgeo, set = list(idp = 2))
  unidw[[i]] <- idw(as.formula(form), mccgeo, newdata = metageo, idp = 2)
}
multpred <- st_as_sf(predict(multidw, as_Spatial(metageo)))

head(sapply(unidw, function(x) st_drop_geometry(x)[,1]))
head(st_drop_geometry(multpred[,1:nc * 2 - 1]))

mapply(all.equal, lapply(unidw, function(x) st_drop_geometry(x)[,1]),
  st_drop_geometry(multpred[,1:nc * 2 - 1]))


rstpred <- st_as_sf(predict(multidw, xyrst))
ggplot(data=rstpred) + 
  geom_sf(aes(col=var5.pred), alpha=0.7) +
  geom_sf(data = euromap, fill=NA, inherit.aes = F, col = grey(.5)) + 
  scale_colour_gradient2(low="blue", mid="white", high="red", midpoint=0) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  theme_void() 

# Kriging
cokrig <- NULL
for (i in 1:nc) {
  form <- sprintf("b%i ~ 1", i)
  cokrig <- gstat(cokrig, formula = as.formula(form), data = mccgeo,
    set = list(nocheck = 1))
}

vario <- variogram(cokrig, cutoff = 2000)
varmod <- vgm("Gau")
vgfit <- fit.lmc(vario, cokrig, varmod, fit.ranges = T)

plot(vario, vgfit)

krigpred <- st_as_sf(predict(vgfit, as_Spatial(metageo)))
head(st_drop_geometry(krigpred[,1:nc * 2 - 1]))

krigrstpred <- st_as_sf(predict(vgfit, xyrst))
ggplot(data=krigrstpred) + 
  geom_sf(aes(col=var5.pred), alpha=0.7) +
  geom_sf(data = euromap, fill=NA, inherit.aes = F, col = grey(.5)) + 
  scale_colour_gradient2(low="blue", mid="white", high="red", midpoint=0) + 
  coord_sf(xlim = urauext[c(1,3)], ylim = urauext[c(2,4)],
    crs = sf::st_crs(3035), default_crs = sf::st_crs(4326),
    lims_method = "box") +
  theme_void() 

# Directional Kriging
cokrig <- NULL
for (i in 1:nc) {
  form <- sprintf("b%i ~ 1", i)
  cokrig <- gstat(cokrig, formula = as.formula(form), data = mccgeo,
    set = list(nocheck = 1))
}

vario <- variogram(cokrig, alpha = c(0, 45, 90, 135))
plot(vario)



#----------------------
# Cross-validation
#----------------------

# Params
nfold <- 10
idpgrid <- seq(.1, 2, by = .1)

folds <- sample(rep_len(1:nfold, nrow(mccgeo)))

idwres <- matrix(NA, length(idpgrid), nc)
for (i in 1:nc) {
  form <- sprintf("b%i ~ 1", i)
  for (j in seq_along(idpgrid)){
    resfold <- vector("numeric", nfold)
    for (f in seq_len(nfold)){
      preds <- idw(as.formula(form), mccgeo[folds != f,], 
        newdata = mccgeo[folds == f,], idp = idpgrid[j])
      resfold[f] <- mean((
        st_drop_geometry(mccgeo)[folds == f, sprintf("b%i", i)] - 
          preds$var1.pred)^2)
    }
    idwres[j, i] <- mean(resfold)
  }
}


# matplot(idpgrid, scale(idwres), pch = 15:19)

krigres <- matrix(NA, nrow(mccgeo), nc)
for (f in seq_len(nfold)){
  fkrig <- NULL
  for (i in 1:nc) {
    form <- sprintf("b%i ~ 1", i)
    fkrig <- gstat(fkrig, formula = as.formula(form), 
      data = mccgeo[folds != f,], set = list(nocheck = 1))
  }
  fvario <- variogram(fkrig)
  fmod <- vgm("Gau")
  ffit <- fit.lmc(fvario, fkrig, fmod)
  fpred <- st_as_sf(predict(ffit, as_Spatial(mccgeo[folds == f,])))
  krigres[folds == f,] <- data.matrix(st_drop_geometry(fpred)[,1:5*2 - 1])
}

cvkrig <- colMeans((krigres - blup)^2)

par(mfrow = n2mfrow(nc))
for (i in 1:nc){
  plot(idpgrid, idwres[,i], main = sprintf("b%i", i), pch = 16,
    ylim = range(c(idwres[,i], cvkrig[i])))
  abline(v = idpgrid[which.min(idwres[,i])])
  abline(h = cvkrig[i])
}


#-------------------------------
# Curves
#-------------------------------

ykrig <- predict(vgfit, as_Spatial(mccgeo))@data[,1:5 * 2 - 1]
ytot <- t(sapply(yhat, "[[", "fit")) + krigres
firstpred <- ov_basis %*% t(ytot)
mmts <- ovper[inrange][apply(firstpred[inrange,], 2, which.min)]
krigerfs <- Map(function(x, mmt) exp(x - x[ovper == mmt]), 
  as.data.frame(firstpred), mmts)


pdf("figures/ERFcomparison_krig.pdf", width = 9, height = 13, pointsize = 8)
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
  # BLUP
  lines(bluperfs[[i]], col = 2, ci = "area", 
    ci.arg = list(col = adjustcolor(2, .2)))
  abline(v = bluperfs[[i]]$cen, lty = 3, col = 2)
  # Predicted
  lines(prederfs[[i]], col = 3, ci = "area", 
    ci.arg = list(col = adjustcolor(3, .2)))
  abline(v = prederfs[[i]]$cen, lty = 3, col = 3)
  # Predicted + krig
  lines(ovper, krigerfs[[i]], col = 4, lty = 2)
  abline(v = mmts[i], lty = 3, col = 4)
  # Other elements
  axis(1, at = ovper[predper %in% c(1, 25, 50, 75, 99)], 
    labels = c(1, 25, 50, 75, 99))
  legend("topleft", 
    legend = c("First-stage", "BLUP", "Prediction", "Predict + krig"),
    lty = 1, col = 1:4, ncol = 2, bty = "n", cex = .6)
}

dev.off()