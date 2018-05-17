#Jen 
#Final Project
#Analysis 

#data = read.csv("data_full.csv")
data = read.csv("data_matched_all.csv")
str(data)

#understanding columns
wdi.codes = read.csv("wdi_info.csv")
wdi.codes = wdi.codes[,-c(3)]

linreg1 = lm(stability_index_estimate ~., data)

linreg2 = lm(stability_index_estimate ~ country + SH.H2O.SAFE.ZS, data = data)

# % undernourished: SN.ITK.DEFC.ZS
linreg3 = lm(stability_index_estimate ~ country + SN.ITK.DEFC.ZS, data = data)
summary(linreg3)
#p value = 6.32e-05 ***
#R^2 = 0.9034

# % using safely managed drinking water services: SH.H2O.SMDW.ZS
linreg4 = lm(stability_index_estimate ~ country + SH.H2O.SMDW.ZS, data = data)
# p value = 0.060051 . 
# R^2 = 0.9279

# % using basic drinking water services: SH.H2O.BASW.ZS
linreg5 = lm(stability_index_estimate ~ country + SH.H2O.BASW.ZS, data = data)
summary(linreg5)
#p value = 0.040100 * 
#R^2 0.9081

# depth of food deficit: SN.ITK.DFCT
linreg6 = lm(stability_index_estimate ~ country + SN.ITK.DFCT, data = data)
summary(linreg6)
#p value = 0.000167 *** 
#R^2 0.8203

# food exports: TX.VAL.FOOD.ZS.UN
linreg7 = lm(stability_index_estimate ~ country + TX.VAL.FOOD.ZS.UN, data = data)
summary(linreg7)
#p value = 0.963155 
#R^2 = 0.894

# access to electricity: EG.ELC.ACCS.ZS
linreg8 = lm(stability_index_estimate ~ country + EG.ELC.ACCS.ZS, data = data)
summary(linreg8)
#p value = 2.23e-08 *** 
#R^2 = 0.8919

#all the significant ones
linreg.all = lm(stability_index_estimate ~ country + SN.ITK.DEFC.ZS + SN.ITK.DFCT + EG.ELC.ACCS.ZS, data = data)
summary(linreg.all)

linreg.all = lm(stability_index_estimate ~ country + SN.ITK.DEFC.ZS + SN.ITK.DFCT, data = data)
summary(linreg.all)

#multivariate regression 
cor(data$SN.ITK.DEFC.ZS, data$SN.ITK.DFCT)

#making lag variable
head(data[data$country == "Israel",])
#data$last_score = c(NA, head(data$stability_index_estimate, -1))
#data$PreviousSales = c(NA, head(data$MoisturizerSales, -1))

for(i in 1:length(unique(data$country))){
#  print(data$country == )
}

library(DataCombine)
slided = slide(data, Var = "stability_index_estimate", GroupVar = "country",
                                  slideBy = -1)

write.csv(slided, "slide_data_test.csv")

slided$year = as.factor(slided$year)

colnames(slided)[38] = "lag_var"
linlag = lm(stability_index_estimate ~ SN.ITK.DEFC.ZS + lag_var, data=slided)
summary(linlag)

# SH.H2O.BASW.ZS the one that Talia is using for water 
