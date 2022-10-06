################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix B3: Compare different handlings of
#                             meta predictors
#
################################################################################

if (length(ls()) == 0) source("13_Plots.R") # To get the basic_map object

#---------------------------
# Parameters
#---------------------------

# Maximum number of components
maxk <- 15

#---------------------------
# Fit models with different number of components
#---------------------------

# Prepare object to store results
aicscores <- vector("numeric", maxk + 1)

#----- Baseline model (No component)

# baseline formula
basicform <- as.formula(sprintf("coefs ~ region + 
    ns(age, knots = %s, Boundary.knots = c(0, 100))",
  deparse(ageknots)))

# Fit mixmeta model
mixbasic <- mixmeta(basicform, data = stage2df, S = vcovs, random = ~ 1|city) 

# Exctract AIC
aicscores[1] <- summary(mixbasic)$AIC

#----- PLS

# Extract the maxk first scores to compare
pcvar <- predict(plsres, newdata = scale(metavar), ncomp = 1:maxk, 
  type = "scores")
colnames(pcvar) <- sprintf("pls%i", seq_len(maxk))

# Create data.frame
plsdf <- cbind(stage2df[,-grep("pls", names(stage2df))], pcvar[repmcc,])

# Initialize formula and result object
plsform <- basicform
mixpls <- vector("list", maxk)

# Loop on number of components
for (i in seq_len(maxk)){
  
  print(sprintf("PLS : %i / %i", i, maxk))
  
  # Update formula
  plsform <- update(plsform, as.formula(sprintf("~ . + pls%i", i)))
  
  # Fit mixmeta model
  mixpls[[i]] <- mixmeta(plsform, data = plsdf, S = vcovs, random = ~ 1|city)
  
  # Extract scores
  aicscores[i + 1] <- summary(mixpls[[i]])$AIC
}


#---------------------------
# Plots
#---------------------------

#----- AIC versus number of components

# Plot 
plot(0:maxk, aicscores, type = "b", pch = 16, cex = 1.5,
  xlim = c(0, maxk), xlab = "", ylab = "AIC", 
  col = ifelse(0:maxk == npc, 2, 1))
abline(v = npc, lty = 2)

# Save
dev.print(png, file = "figures/FigS_nPCchoice.png", width = 9, height = 6,
  units = "in", res = 300)

#----- Correlation matrix between metapredictors variables

# Compute correlation matrix
metacor <- cor(metadata[,metaprednames])
colnames(metacor) <- rownames(metacor) <- metadesc$label[match(metaprednames, 
  metadesc$metavar)]

# Color scale
pal <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
  "#4393C3", "#2166AC", "#053061")))

# Plot correlation matrix
corrplot.mixed(metacor, upper = "square", tl.pos = "lt",
  tl.srt = 45, tl.col = "black", cl.cex = 1.1, cl.align.text = "l",
  upper.col = pal(200), lower.col = pal(200))

# Save
dev.print(png, file = "figures/FigS_metacor.png", width = 15, height = 15,
  units = "in", res = 300)


#----- Image of PLS components

# Change col and row names for plot labelling
plotload <- plsres$loadings[,1:npc]
colnames(plotload) <- sprintf("Comp. %i", 1:npc)
rownames(plotload) <- metadesc$label[match(metaprednames, metadesc$metavar)]

# Color scale
pal <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
  "#4393C3", "#2166AC", "#053061")))

# Plot loadings (correlation between components and meta-variables)
corrplot(t(plotload), method = "square", is.corr = F, col.lim = c(-1, 1), 
  tl.srt = 45, tl.col = "black", cl.cex = .7, cl.align.text = "l",
  col = pal(200))

# Save
dev.print(png, file = "figures/FigS_PLScor.png", units = "in", res = 300,
  width = 10, height = 7)


#----- Create map for each PLS component

# Loop
plsmaps <- lapply(seq_len(npc), function(i){
  cutpts <- unname(round(quantile(cityres[, sprintf("pls%i", i)], 
    seq(0, 1, length.out = 5))))
  basic_map + aes_string(fill = sprintf("pls%i", i)) + 
    # scale_fill_viridis(name = sprintf("Comp. %i", i), direction = -1) + 
    guides(fill = guide_coloursteps(title.position = "top", title.hjust = .5,
      barwidth = 10, barheight = .8, even.steps = T)) +
    scale_fill_stepsn(colours = viridis(length(cutpts) - 1, direction = -1),
      values = rescale(cutpts), breaks = cutpts,
      name = sprintf("Comp. %i", i))
})
names(plsmaps) <- letters[1:npc]

# Add legend for size
plsmaps[[letters[npc + 1]]] <- basic_map + coord_sf(xlim = c(0, 0), ylim = c(0, 0)) + 
  scale_size(breaks = c(1, 5, 10, 50) * 10^5, 
    labels = c(1, 5, 10, 50) / 10, name = "Population (millions)",
    guide = guide_legend(override.aes = list(colour = "black"), 
      title.position = "top")) + 
  theme(legend.position = "right")

# Design
design <- matrix(letters[1:(2 * round(npc / 2))], ncol = 2, byrow = T)
if (npc %% 2 == 1) design <- rbind(design, letters[npc])
design <- cbind(design, letters[npc + 1])
design_char <- paste(apply(design, 1, paste, collapse = ""), collapse = "\n")

# Plot
wrap_plots(plsmaps, widths = c(1, 1, .1), design = design_char)

# Save
ggsave("figures/FigS_PLSmaps.png", width = 9, height = 13)


#----- Curve changes at extreme PLS

# Colors
pal <- viridis(2, direction = -1)

# Plot layout
design <- matrix(1:(2 * round(npc / 2)), ncol = 2, byrow = T)
if (npc %% 2 == 1) {
  design <- design[,rep(1:2, each = 2)]
  design <- rbind(design, c(0, npc, npc, 0))
}
design <- cbind(design, npc + 1)

# Initialize layout
layout(design, widths = c(rep(1, 2 * (1 + (npc %% 2))), .2 * (1 + (npc %% 2))))

# Loop on PLS components
for (i in seq_len(npc)){
  inds <- 1:2 + (2 * (i - 1))
  
  # Lowest
  plot(plscp[inds][[1]], xlab = "Temperature percentile", ylab = "RR", 
    main = sprintf("Comp. %i", i), col = pal[1], lwd = 2, ylim = c(.8, 2.5), 
    cex.main = .9, ci.arg = list(col = adjustcolor(pal[1], .2)), xaxt = "n")
  
  # Highest
  lines(plscp[inds][[2]], lwd = 2, col = pal[2], ci = "area", 
    ci.arg = list(col = adjustcolor(pal[2], .2)))
  
  # Axis
  abline(v = ovaxis, h = axTicks(2), lty = 3, col = "lightgrey")
  axis(1, at = ovaxis, labels = axisper)
  abline(h = 1)
  
  # MMTs
  abline(v = plscp[inds][[1]]$cen, col = pal[1], lty = 2)
  abline(v = plscp[inds][[2]]$cen, col = pal[2], lty = 2)
}

# Add legend
par(mar = rep(0, 4))
plot.new()
legend("center", legend = c("1st", "99th"), lwd = 2,
  col = pal, bty = "n", title = "Component\npercentile")

# Save
dev.print(png, file = "figures/FigS_PLS_ERF.png", height = 13, width = 10,
  units = "in", res = 300)

