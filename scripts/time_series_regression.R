# Time Series Regression
# 17.835 Final Project
# Milo & Jen

install.packages('DataCombine')
library(DataCombine)

# Models to try:
  # TSR w/ Single Variables over multiple lags
  # TSR w/ Multi Variables over multiple lags

# Use the WDI/WGI matched dataset, since we don't care about global terrorism.
data = read.csv("../data/final/data_matched_wdi_wgi.csv")

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

summary(model.undernourishment.tplus1)
summary(model.undernourishment.tplus2)
summary(model.undernourishment.tplus3)

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
