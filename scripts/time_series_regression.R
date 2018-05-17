# Time Series Regression
# 17.835 Final Project
# Milo & Jen

# install.packages('DataCombine')
install.packages('xtable')
library(DataCombine)
library(xtable)

# Use the WDI/WGI matched dataset, since we don't care about global terrorism.
data = read.csv("../data/final/data_matched_wdi_wgi.csv")
data$year = as.factor(data$year)

# Variables to try:
  # SH.H2O.SAFE.ZS = Access to improved water source (% of pop)
  # SH.H2O.BASW.ZS = People using basic drinking water services (% of pop)
  # EG.ELC.ACCS.ZS = Access to electricity (% of pop)
  # SN.ITK.DEFC.ZS = Percent undernourishment
  # AG.PRD.FOOD.XD = Food production index
  # SN.ITK.DFCT = Food kilocalorie deficit per person

# See which columns have an acceptable number of NaNs.
summary(data$SH.H2O.SAFE.ZS) # 376
summary(data$SH.H2O.BASW.ZS) # 633
summary(data$EG.ELC.ACCS.ZS) # 444
summary(data$SN.ITK.DEFC.ZS) # 1071
summary(data$AG.PRD.FOOD.XD) # 491
summary(data$SN.ITK.DFCT) # 1517

# Understanding columns.
wdi.codes = read.csv("../data/original/wdi_info.csv")
wdi.codes = wdi.codes[,-c(3)] # Remove long definitions.

################### Create Lagged Dataset ##################
# Note: data must be sorted in ascending data order, at regular intervals.
data.chrono = data[order(data$year),]

# For each country-year pair, store the stability_index_estimate at the next observation.
# Also store several more lookaheads (t+1, t+2, t+3,...).
# Note: Positive slideBy means that we are LEADING the data (getting future values).
data.slid1 = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                   GroupVar='country', NewVar='stability_index_estimate_tplus1',
                   slideBy=1, keepInvalid=FALSE, reminder=TRUE)

data.slid2 = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                   GroupVar='country', NewVar='stability_index_estimate_tplus2',
                   slideBy=2, keepInvalid=FALSE, reminder=TRUE)

data.slid3 = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                   GroupVar='country', NewVar='stability_index_estimate_tplus3',
                   slideBy=3, keepInvalid=FALSE, reminder=TRUE)

data.tm1 = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                 GroupVar='country', NewVar='stability_index_estimate_tm1',
                 slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.tm2 = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                 GroupVar='country', NewVar='stability_index_estimate_tm2',
                 slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.tm3 = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                 GroupVar='country', NewVar='stability_index_estimate_tm3',
                 slideBy=-3, keepInvalid=FALSE, reminder=TRUE)

#################### SINGLE VARIABLE MODELS ######################
# Variables to try:
# SH.H2O.SAFE.ZS = Access to improved water source (% of pop)
# SH.H2O.BASW.ZS = People using basic drinking water services (% of pop)
# EG.ELC.ACCS.ZS = Access to electricity (% of pop)
# SN.ITK.DEFC.ZS = Percent undernourishment
# AG.PRD.FOOD.XD = Food production index
# SN.ITK.DFCT = Food kilocalorie deficit per person

## SH.H2O.SAFE.ZS = Access to improved water source (% of pop)
model.water_access.tplus1 = lm(stability_index_estimate_tplus1 ~ SH.H2O.SAFE.ZS, data=data.slid1)
model.water_access.tplus2 = lm(stability_index_estimate_tplus2 ~ SH.H2O.SAFE.ZS, data=data.slid2)
model.water_access.tplus3 = lm(stability_index_estimate_tplus3 ~ SH.H2O.SAFE.ZS, data=data.slid3)

summary(model.water_access.tplus1) # R^2 = 0.2607
summary(model.water_access.tplus2) # R^2 = 0.2629
summary(model.water_access.tplus3) # R^2 = 0.2637

plot(x = data.slid1$SH.H2O.SAFE.ZS, y = data.slid1$stability_index_estimate_tplus1,
     pch = 1, cex = 0.2, col='blue', main='Stability Index (t+1)',
     xlab = 'Access to improved water source (% of pop)', ylab='Stability Index')
abline(model.water_access.tplus1)

## SH.H2O.BASW.ZS = People using basic drinking water services (% of pop)
model.basic_water.tplus1 = lm(stability_index_estimate_tplus1 ~ SH.H2O.BASW.ZS, data=data.slid1)
model.basic_water.tplus2 = lm(stability_index_estimate_tplus2 ~ SH.H2O.BASW.ZS, data=data.slid2)
model.basic_water.tplus3 = lm(stability_index_estimate_tplus3 ~ SH.H2O.BASW.ZS, data=data.slid3)

summary(model.basic_water.tplus1) # R^2 = 0.2566
summary(model.basic_water.tplus2) # R^2 = 0.2572
summary(model.basic_water.tplus3) # R^2 = 0.2591

## SN.ITK.DEFC.ZS = Percent undernourishment
model.undernourishment.tplus1 = lm(stability_index_estimate_tplus1 ~ SN.ITK.DEFC.ZS, data=data.slid1)
model.undernourishment.tplus2 = lm(stability_index_estimate_tplus2 ~ SN.ITK.DEFC.ZS, data=data.slid2)
model.undernourishment.tplus3 = lm(stability_index_estimate_tplus3 ~ SN.ITK.DEFC.ZS, data=data.slid3)

summary(model.undernourishment.tplus1) # R^2 = 0.194
summary(model.undernourishment.tplus2) # R^2 = 0.1894
summary(model.undernourishment.tplus3) # R^2 = 0.1862

## AG.PRD.FOOD.XD = Food production index
# Note: this has a really tiny R^2 = 0.001, so it isn't a good predictor.
model.fpi.tplus1 = lm(stability_index_estimate_tplus1 ~ AG.PRD.FOOD.XD, data=data.slid1)
model.fpi.tplus2 = lm(stability_index_estimate_tplus2 ~ AG.PRD.FOOD.XD, data=data.slid2)
model.fpi.tplus3 = lm(stability_index_estimate_tplus3 ~ AG.PRD.FOOD.XD, data=data.slid3)

summary(model.fpi.tplus1)
summary(model.fpi.tplus2)
summary(model.fpi.tplus3)

## SN.ITK.DFCT = Food kilocalorie deficit per person
# Note: also a small R^2 = 0.0833, not a good predictor.
model.kcaldef.tplus1 = lm(stability_index_estimate_tplus1 ~ SN.ITK.DFCT, data=data.slid1)
model.kcaldef.tplus2 = lm(stability_index_estimate_tplus2 ~ SN.ITK.DFCT, data=data.slid2)
model.kcaldef.tplus3 = lm(stability_index_estimate_tplus3 ~ SN.ITK.DFCT, data=data.slid3)

summary(model.kcaldef.tplus1)
summary(model.kcaldef.tplus2)
summary(model.kcaldef.tplus3)

## EG.ELC.ACCS.ZS = Access to electricity (% of pop)
# R^2 = 0.1706
model.elec.tplus1 = lm(stability_index_estimate_tplus1 ~ EG.ELC.ACCS.ZS, data=data.slid1)
model.elec.tplus2 = lm(stability_index_estimate_tplus2 ~ EG.ELC.ACCS.ZS, data=data.slid2)
model.elec.tplus3 = lm(stability_index_estimate_tplus3 ~ EG.ELC.ACCS.ZS, data=data.slid3)

summary(model.elec.tplus1)
summary(model.elec.tplus2)
summary(model.elec.tplus3)

# Some correlation tests...
length(data.slid1$stability_index_estimate)
length(data.slid1$stability_index_estimate_tplus1)

summary(data.slid1$stability_index_estimate)
summary(data.slid1$stability_index_estimate_tplus1)

# Really strong autocorrelation between stability now and stability next year (expected)
cor(data.slid1$stability_index_estimate, data.slid1$stability_index_estimate_tplus1, use='complete.obs')

# Slightly higher correlation between current stability and access to water than
# next year's stability and access to water.
cor(data.slid1$stability_index_estimate_tplus1, data.slid1$SH.H2O.BASW.ZS, use='complete.obs')
cor(data.slid1$stability_index_estimate, data.slid1$SH.H2O.BASW.ZS, use='complete.obs')

###################### SINGLE VARIABLE MODELS, CONTROLLING FOR CONFOUNDING VARIABLES ##############
model.foodwater.formula = stability_index_estimate_tplus1 ~ SH.H2O.SAFE.ZS + SH.H2O.BASW.ZS +
  SN.ITK.DEFC.ZS + AG.PRD.FOOD.XD + stability_index_estimate

model.foodwater.tplus1 = lm(model.foodwater.formula, data=data.slid1)
model.foodwater.tplus2 = lm(model.foodwater.formula, data=data.slid2)
model.foodwater.tplus3 = lm(model.foodwater.formula, data=data.slid3)

summary(model.foodwater.tplus1) # R^2 = 0.2607
summary(model.foodwater.tplus2) # R^2 = 0.2629
summary(model.foodwater.tplus3) # R^2 = 0.2637

###################### DISTRIBUTED LAG MODEL ############################

## SN.ITK.DEFC.ZS
data.all.lagged = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                        GroupVar='country', NewVar='stability_index_estimate_tm1',
                        slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.all.lagged = slide(data.all.lagged, Var='SN.ITK.DEFC.ZS', TimeVar='year',
                        GroupVar='country', NewVar='SN.ITK.DEFC.ZS_tm1',
                        slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.all.lagged = slide(data.all.lagged, Var='SN.ITK.DEFC.ZS', TimeVar='year',
                        GroupVar='country', NewVar='SN.ITK.DEFC.ZS_tm2',
                        slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.all.lagged = slide(data.all.lagged, Var='SN.ITK.DEFC.ZS', TimeVar='year',
                        GroupVar='country', NewVar='SN.ITK.DEFC.ZS_tm3',
                        slideBy=-3, keepInvalid=FALSE, reminder=TRUE)

data.all.lagged$SN.ITK.DEFC.ZS_delta1 = data.all.lagged$SN.ITK.DEFC.ZS - data.all.lagged$SN.ITK.DEFC.ZS_tm1
data.all.lagged$SN.ITK.DEFC.ZS_delta2 = data.all.lagged$SN.ITK.DEFC.ZS_tm1 - data.all.lagged$SN.ITK.DEFC.ZS_tm2
data.all.lagged$SN.ITK.DEFC.ZS_delta3 = data.all.lagged$SN.ITK.DEFC.ZS_tm2 - data.all.lagged$SN.ITK.DEFC.ZS_tm3

model.all.lagged.formula = stability_index_estimate ~ stability_index_estimate_tm1 +
  SN.ITK.DEFC.ZS_delta1 + SN.ITK.DEFC.ZS_delta2 + SN.ITK.DEFC.ZS_delta3 # SN.ITK.DEFC.ZS_tm1 + SN.ITK.DEFC.ZS

model.all.lagged = lm(model.all.lagged.formula, data=data.all.lagged)
summary(model.all.lagged)

## AG.PRD.FOOD.XD
# Lag the stability index estimate.
data.foodprod.lagged = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                        GroupVar='country', NewVar='stability_index_estimate_tm1',
                        slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
# Lag the food production index.
data.foodprod.lagged = slide(data.foodprod.lagged, Var='AG.PRD.FOOD.XD', TimeVar='year',
                        GroupVar='country', NewVar='AG.PRD.FOOD.XD_tm1',
                        slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.foodprod.lagged = slide(data.foodprod.lagged, Var='AG.PRD.FOOD.XD', TimeVar='year',
                        GroupVar='country', NewVar='AG.PRD.FOOD.XD_tm2',
                        slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.foodprod.lagged = slide(data.foodprod.lagged, Var='AG.PRD.FOOD.XD', TimeVar='year',
                        GroupVar='country', NewVar='AG.PRD.FOOD.XD_tm3',
                        slideBy=-3, keepInvalid=FALSE, reminder=TRUE)

data.foodprod.lagged$SIE_delta1 = data.foodprod.lagged$stability_index_estimate - data.foodprod.lagged$stability_index_estimate_tm1
data.foodprod.lagged$SIE_delta2 = data.foodprod.lagged$stability_index_estimate_tm1 - data.foodprod.lagged$stability_index_estimate_tm2
data.foodprod.lagged$SIE_delta3 = data.foodprod.lagged$stability_index_estimate_tm2 - data.foodprod.lagged$stability_index_estimate_tm3

data.foodprod.lagged$AG.PRD.FOOD.XD_delta1 = data.foodprod.lagged$AG.PRD.FOOD.XD - data.foodprod.lagged$AG.PRD.FOOD.XD_tm1
data.foodprod.lagged$AG.PRD.FOOD.XD_delta2 = data.foodprod.lagged$AG.PRD.FOOD.XD_tm1 - data.foodprod.lagged$AG.PRD.FOOD.XD_tm2
data.foodprod.lagged$AG.PRD.FOOD.XD_delta3 = data.foodprod.lagged$AG.PRD.FOOD.XD_tm2 - data.foodprod.lagged$AG.PRD.FOOD.XD_tm3

model.foodprod.lagged.formula = stability_index_estimate ~ stability_index_estimate_tm1 +
  AG.PRD.FOOD.XD_delta1 + AG.PRD.FOOD.XD_delta2 + AG.PRD.FOOD.XD_delta3

model.foodprod.lagged = lm(model.foodprod.lagged.formula, data=data.foodprod.lagged)
summary(model.foodprod.lagged)

############################# Access to basic drinking water.
data.basicwater.lagged = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                             GroupVar='country', NewVar='stability_index_estimate_tm1',
                             slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.basicwater.lagged = slide(data.basicwater.lagged, Var='SH.H2O.BASW.ZS', TimeVar='year',
                               GroupVar='country', NewVar='SH.H2O.BASW.ZS_tm1',
                               slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.basicwater.lagged = slide(data.basicwater.lagged, Var='SH.H2O.BASW.ZS', TimeVar='year',
                               GroupVar='country', NewVar='SH.H2O.BASW.ZS_tm2',
                               slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.basicwater.lagged = slide(data.basicwater.lagged, Var='SH.H2O.BASW.ZS', TimeVar='year',
                               GroupVar='country', NewVar='SH.H2O.BASW.ZS_tm3',
                               slideBy=-3, keepInvalid=FALSE, reminder=TRUE)

data.basicwater.lagged$SH.H2O.BASW.ZS_delta1 = data.basicwater.lagged$SH.H2O.BASW.ZS - data.basicwater.lagged$SH.H2O.BASW.ZS_tm1
data.basicwater.lagged$SH.H2O.BASW.ZS_delta2 = data.basicwater.lagged$SH.H2O.BASW.ZS_tm1 - data.basicwater.lagged$SH.H2O.BASW.ZS_tm2
data.basicwater.lagged$SH.H2O.BASW.ZS_delta3 = data.basicwater.lagged$SH.H2O.BASW.ZS_tm2 - data.basicwater.lagged$SH.H2O.BASW.ZS_tm3

model.basicwater.lagged.formula = stability_index_estimate ~ stability_index_estimate_tm1 +
  SH.H2O.BASW.ZS_delta1 + SH.H2O.BASW.ZS_delta2 + SH.H2O.BASW.ZS_delta3

model.basicwater.lagged = lm(model.basicwater.lagged.formula, data=data.basicwater.lagged)
summary(model.basicwater.lagged)

############################# Percent undernourishment
data.undernourished.lagged = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                                   GroupVar='country', NewVar='stability_index_estimate_tm1',
                                   slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.undernourished.lagged = slide(data.undernourished.lagged, Var='SN.ITK.DEFC.ZS', TimeVar='year',
                                   GroupVar='country', NewVar='SN.ITK.DEFC.ZS_tm1',
                                   slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.undernourished.lagged = slide(data.undernourished.lagged, Var='SN.ITK.DEFC.ZS', TimeVar='year',
                                   GroupVar='country', NewVar='SN.ITK.DEFC.ZS_tm2',
                                   slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.undernourished.lagged = slide(data.undernourished.lagged, Var='SN.ITK.DEFC.ZS', TimeVar='year',
                                   GroupVar='country', NewVar='SN.ITK.DEFC.ZS_tm3',
                                   slideBy=-3, keepInvalid=FALSE, reminder=TRUE)

data.undernourished.lagged$SN.ITK.DEFC.ZS_delta1 = data.undernourished.lagged$SN.ITK.DEFC.ZS - data.undernourished.lagged$SN.ITK.DEFC.ZS_tm1
data.undernourished.lagged$SN.ITK.DEFC.ZS_delta2 = data.undernourished.lagged$SN.ITK.DEFC.ZS_tm1 - data.undernourished.lagged$SN.ITK.DEFC.ZS_tm2
data.undernourished.lagged$SN.ITK.DEFC.ZS_delta3 = data.undernourished.lagged$SN.ITK.DEFC.ZS_tm2 - data.undernourished.lagged$SN.ITK.DEFC.ZS_tm3

model.undernourished.lagged.formula = stability_index_estimate ~ stability_index_estimate_tm1 +
  SN.ITK.DEFC.ZS_delta1 + SN.ITK.DEFC.ZS_delta2 + SN.ITK.DEFC.ZS_delta3

model.undernourished.lagged = lm(model.undernourished.lagged.formula, data=data.undernourished.lagged)
summary(model.undernourished.lagged)

############################### Kcal deficit
data.kcaldef.lagged = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                            GroupVar='country', NewVar='stability_index_estimate_tm1',
                            slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.kcaldef.lagged = slide(data.kcaldef.lagged, Var='SN.ITK.DFCT', TimeVar='year',
                            GroupVar='country', NewVar='SN.ITK.DFCT_tm1',
                            slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.kcaldef.lagged = slide(data.kcaldef.lagged, Var='SN.ITK.DFCT', TimeVar='year',
                            GroupVar='country', NewVar='SN.ITK.DFCT_tm2',
                            slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.kcaldef.lagged = slide(data.kcaldef.lagged, Var='SN.ITK.DFCT', TimeVar='year',
                            GroupVar='country', NewVar='SN.ITK.DFCT_tm3',
                            slideBy=-3, keepInvalid=FALSE, reminder=TRUE)

data.kcaldef.lagged$SN.ITK.DFCT_delta1 = data.kcaldef.lagged$SN.ITK.DFCT - data.kcaldef.lagged$SN.ITK.DFCT_tm1
data.kcaldef.lagged$SN.ITK.DFCT_delta2 = data.kcaldef.lagged$SN.ITK.DFCT_tm1 - data.kcaldef.lagged$SN.ITK.DFCT_tm2
data.kcaldef.lagged$SN.ITK.DFCT_delta3 = data.kcaldef.lagged$SN.ITK.DFCT_tm2 - data.kcaldef.lagged$SN.ITK.DFCT_tm3

model.kcaldef.lagged.formula = stability_index_estimate ~ stability_index_estimate_tm1 +
  SN.ITK.DFCT_delta1 + SN.ITK.DFCT_delta2 + SN.ITK.DFCT_delta3

model.kcaldef.lagged = lm(model.kcaldef.lagged.formula, data=data.kcaldef.lagged)
summary(model.kcaldef.lagged)

################# Electricity
data.elec.lagged = slide(data.chrono, Var='stability_index_estimate', TimeVar='year',
                         GroupVar='country', NewVar='stability_index_estimate_tm1',
                         slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.elec.lagged = slide(data.elec.lagged, Var='EG.ELC.ACCS.ZS', TimeVar='year',
                         GroupVar='country', NewVar='EG.ELC.ACCS.ZS_tm1',
                         slideBy=-1, keepInvalid=FALSE, reminder=TRUE)
data.elec.lagged = slide(data.elec.lagged, Var='EG.ELC.ACCS.ZS', TimeVar='year',
                         GroupVar='country', NewVar='EG.ELC.ACCS.ZS_tm2',
                         slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.elec.lagged = slide(data.elec.lagged, Var='EG.ELC.ACCS.ZS', TimeVar='year',
                         GroupVar='country', NewVar='EG.ELC.ACCS.ZS_tm3',
                         slideBy=-3, keepInvalid=FALSE, reminder=TRUE)
data.elec.lagged = slide(data.elec.lagged, Var='EG.ELC.ACCS.ZS', TimeVar='year',
                         GroupVar='country', NewVar='EG.ELC.ACCS.ZS_tm4',
                         slideBy=-4, keepInvalid=FALSE, reminder=TRUE)
data.elec.lagged = slide(data.elec.lagged, Var='EG.ELC.ACCS.ZS', TimeVar='year',
                         GroupVar='country', NewVar='EG.ELC.ACCS.ZS_tm5',
                         slideBy=-5, keepInvalid=FALSE, reminder=TRUE)

data.elec.lagged$EG.ELC.ACCS.ZS_delta1 = data.elec.lagged$EG.ELC.ACCS.ZS - data.elec.lagged$EG.ELC.ACCS.ZS_tm1
data.elec.lagged$EG.ELC.ACCS.ZS_delta2 = data.elec.lagged$EG.ELC.ACCS.ZS_tm1 - data.elec.lagged$EG.ELC.ACCS.ZS_tm2
data.elec.lagged$EG.ELC.ACCS.ZS_delta3 = data.elec.lagged$EG.ELC.ACCS.ZS_tm2 - data.elec.lagged$EG.ELC.ACCS.ZS_tm3
data.elec.lagged$EG.ELC.ACCS.ZS_delta4 = data.elec.lagged$EG.ELC.ACCS.ZS_tm3 - data.elec.lagged$EG.ELC.ACCS.ZS_tm4
data.elec.lagged$EG.ELC.ACCS.ZS_delta5 = data.elec.lagged$EG.ELC.ACCS.ZS_tm4 - data.elec.lagged$EG.ELC.ACCS.ZS_tm5

model.elec.lagged.formula = stability_index_estimate ~ stability_index_estimate_tm1 +
  EG.ELC.ACCS.ZS_delta1 + EG.ELC.ACCS.ZS_delta2 + EG.ELC.ACCS.ZS_delta3 +
  EG.ELC.ACCS.ZS_delta4 + EG.ELC.ACCS.ZS_delta5

model.elec.lagged = lm(model.elec.lagged.formula, data=data.elec.lagged)
summary(model.elec.lagged)

########################## Distributed lag for all vars
data.all.lagged = merge(data.basicwater.lagged, data.foodprod.lagged, by=intersect(names(data.basicwater.lagged),
                        names(data.foodprod.lagged)), all.x = TRUE, all.y = TRUE)
data.all.lagged = merge(data.all.lagged, data.undernourished.lagged, by=intersect(names(data.basicwater.lagged),
                        names(data.foodprod.lagged)), all.x = TRUE, all.y = TRUE)
data.all.lagged = subset(data.all.lagged, select=-c(X))

data.all.lagged = slide(data.all.lagged, Var='stability_index_estimate', TimeVar='year',
                        GroupVar='country', NewVar='stability_index_estimate_tm2',
                        slideBy=-2, keepInvalid=FALSE, reminder=TRUE)
data.all.lagged = slide(data.all.lagged, Var='stability_index_estimate', TimeVar='year',
                        GroupVar='country', NewVar='stability_index_estimate_tm3',
                        slideBy=-3, keepInvalid=FALSE, reminder=TRUE)

data.all.lagged$sie_delta1 = data.all.lagged$stability_index_estimate - data.all.lagged$stability_index_estimate_tm1
data.all.lagged$sie_delta2 = data.all.lagged$stability_index_estimate_tm1 - data.all.lagged$stability_index_estimate_tm2
data.all.lagged$sie_delta3 = data.all.lagged$stability_index_estimate_tm2 - data.all.lagged$stability_index_estimate_tm3

model.all.lagged.formula = sie_delta1 ~ sie_delta2 + # sie_delta3 +
  SN.ITK.DEFC.ZS_delta1 + SN.ITK.DEFC.ZS_delta2 + # SN.ITK.DEFC.ZS_delta3 +
  SH.H2O.BASW.ZS_delta1 + SH.H2O.BASW.ZS_delta2 + # SH.H2O.BASW.ZS_delta3 +
  AG.PRD.FOOD.XD_delta1 + AG.PRD.FOOD.XD_delta2 # + # AG.PRD.FOOD.XD_delta3

model.all.lagged = lm(model.all.lagged.formula, data=data.all.lagged)
summary(model.all.lagged)
  
table = xtable(model.all.lagged)
print(table)

# Plot water access
plot(x = data.all.lagged$SH.H2O.BASW.ZS_delta1, y = data.all.lagged$sie_delta1,
     cex=0.8, col='blue', xlab='Change in Basic Water Access (D1)',
     ylab='Change in Stability Index (D1)',
     main='Change in Stability Index vs. Basic Water Access (1 Year Lag)')
abline(model.all.lagged$coefficients['(Intercept)'],
       model.all.lagged$coefficients['SH.H2O.BASW.ZS_delta1'],
       col='black', lwd=2)

# Plot water access
plot(x = data.all.lagged$SH.H2O.BASW.ZS_delta2, y = data.all.lagged$sie_delta1,
     cex=0.8, col='blue', xlab='Change in Basic Water Access (D2)',
     ylab='Change in Stability Index (D1)',
     main='Change in Stability Index vs. Basic Water Access (2 Year Lag)')
abline(model.all.lagged$coefficients['(Intercept)'],
       model.all.lagged$coefficients['SH.H2O.BASW.ZS_delta2'],
       col='black', lwd=2)

