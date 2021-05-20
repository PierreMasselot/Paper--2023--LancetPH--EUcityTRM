################################################################################
#
#                         MCC-EUcityTRM
#
#                   Appendix: residuals analysis
#
################################################################################

library(reshape2)

#---------------------------
# Extract residual
#---------------------------

# Extract from stage 2 result
res <- residuals(stage2res)

# Create a long format DF with residuals
st2df_long <- melt(cbind(stage2df, res), id.vars = names(stage2df),
  variable.name = "coef", value.name = "res")

#---------------------------
# Residuals vs country
#---------------------------

ggplot(st2df_long) + theme_classic() + 
  geom_boxplot(aes(x = res, y = country, col = country), show.legend = F) +
  geom_vline(xintercept = 0) +
  xlab("Residuals") + ylab("Country") + scale_y_discrete(limits = rev)