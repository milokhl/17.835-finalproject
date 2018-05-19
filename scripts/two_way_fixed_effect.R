# Jen McDermott
# 17.835
# Final Project
# Analysis 

#data = read.csv("data_full.csv")
data = read.csv("data_matched_all.csv")
str(data)

#understanding columns
wdi = read.csv("wdi_info.csv")
wdi = wdi[,-c(3)]


##### initial linear regressions #####
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

# food production index: AG.PRD.FOOD.XD
linreg9 = lm(stability_index_estimate ~ country + AG.PRD.FOOD.XD, data = data)
summary(linreg9) 

#all the significant ones
linreg.all = lm(stability_index_estimate ~ country + SN.ITK.DEFC.ZS + SN.ITK.DFCT + EG.ELC.ACCS.ZS, data = data)
summary(linreg.all)

linreg.all = lm(stability_index_estimate ~ country + SN.ITK.DEFC.ZS + SN.ITK.DFCT, data = data)
summary(linreg.all)


############### Final, clean code ###############
data = read.csv("data_matched_all.csv")
#understanding columns
wdi = read.csv("wdi_info.csv")
wdi = wdi[,-c(3)]
data$year = as.factor(data$year)

# two-way fixed effect 

all = lm(stability_index_estimate ~
           AG.PRD.FOOD.XD + AG.PRD.LVSK.XD + EG.CFT.ACCS.ZS + EG.ELC.ACCS.RU.ZS
         + EG.ELC.ACCS.UR.ZS + EG.ELC.ACCS.ZS + EN.POP.DNST + ER.GDP.FWTL.M3.KD
         + NV.MNF.FBTO.ZS.UN + SH.H2O.BASW.RU.ZS + SH.H2O.BASW.UR.ZS +  SH.H2O.BASW.ZS
         + SH.H2O.SAFE.RU.ZS + SH.H2O.SAFE.UR.ZS + SH.H2O.SAFE.ZS + SH.H2O.SMDW.RU.ZS
         + SH.H2O.SMDW.UR.ZS + SH.H2O.SMDW.ZS + SM.POP.REFG + SM.POP.REFG.OR
         + SN.ITK.DEFC.ZS + SN.ITK.DFCT + SP.POP.GROW + SP.POP.TOTL
         + TM.VAL.FOOD.ZS.UN + TX.VAL.FOOD.ZS.UN
           , data=data)
summary(all)

#just undernourished and using basic drinking water 
fixed1 = lm(lm(stability_index_estimate 
                 ~ country + year 
                 + SN.ITK.DEFC.ZS  # % undernourished
                 + SH.H2O.BASW.ZS  # % basic drinking water services
                 , data = data))
summary(fixed1)


# all relevant 
fixed2 = lm(lm(stability_index_estimate 
               ~ country + year 
               + SN.ITK.DEFC.ZS  # % undernourished
               #+ SN.ITK.DFCT     # kilocalorie deficit
               + AG.PRD.FOOD.XD  # Food production index 
               + SH.H2O.BASW.ZS  # % basic drinking water services
               #+ SH.H2O.SMDW.ZS  # % using safely managed drinking water services 
               + EG.CFT.ACCS.ZS  # % access to clean fuels and technologies for cooking
               #+ EG.ELC.ACCS.ZS  # % access to electricity 
               + SP.POP.TOTL     # total population 
               , data = data))
summary(fixed2)

country_names = unique(data$country)

for(i in 55:length(country_names)){ 
  dcc = data[data$country == country_names[i],]
  plot(dcc$SN.ITK.DEFC.ZS, dcc$stability_index_estimate,
       main = paste("Stability Index vs. % Undernourished \n in", country_names[i]), 
       xlab = "% Undernourished", ylab = "Stability Index", 
       xlim=c(0,60), ylim=c(-3,2), col='red', cex = 1.1)
}

write.csv(country_names, "plot_labels.csv")

labeled = read.csv("plot_labels.csv")

vshape = labeled[labeled$Undernourish == "v",]



for(i in 1:dim(vshape)[1]){ 
  dcc = data[data$country == vshape$country[i],]
  print(paste("Country:", vshape$country[i]))
  
  #plot water 
  plot(dcc$SH.H2O.BASW.ZS, dcc$stability_index_estimate,
       main = paste("Stability Index vs. basic water \n in", vshape$country[i]),
       xlab = "% access to water", ylab = "Stability Index",
       xlim=c(20,100), ylim=c(-3,2), col='blue', cex = 1.1)
  text(dcc$SH.H2O.BASW.ZS, dcc$stability_index_estimate, 
       labels = dcc$year)
  
  print(paste("Population: ", mean(dcc$SP.POP.TOTL)))
  
  #food production 
  plot(dcc$AG.PRD.FOOD.XD, dcc$stability_index_estimate,
       main = paste("Stability Index vs. Food Prod \n in", vshape$country[i]),
       xlab = "Food Production Index", ylab = "Stability Index",
       xlim=c(40,200), ylim=c(-3,2), col='green', cex = 1.1)
  
  #access to cooking technology
  plot(dcc$EG.CFT.ACCS.ZS, dcc$stability_index_estimate,
       main = paste("Stability Index vs. Access to cooking tech \n in", vshape$country[i]),
       xlab = "% Access to cooking tech", ylab = "Stability Index",
       xlim=c(0,100), ylim=c(-3,2), col='navy', cex = 1.1)
  
  # over time 
  plot(as.numeric(as.character(dcc$year)),
       dcc$stability_index_estimate, 
       main = paste("Stability Index over time \n in", vshape$country[i]),
       xlab = "Year", ylab = "Stability Index", type = 'p', pch = 16)
  
  #undernourished over time 
  plot(as.numeric(as.character(dcc$year)), 
       dcc$SN.ITK.DEFC.ZS,
       main = paste("% Undernourished over time \n in", vshape$country[i]), 
       xlab = "Year", ylab = "% Undernourished", col='red')
  
  # water over time
  plot(as.numeric(as.character(dcc$year)), 
       dcc$SH.H2O.BASW.ZS,
       main = paste("% Access to water over time \n in", vshape$country[i]), 
       xlab = "Year", ylab = "% Access to water", col='blue')
  
  #food production 
  plot(as.numeric(as.character(dcc$year)), 
       dcc$AG.PRD.FOOD.XD,
       main = paste("food production over time \n in", vshape$country[i]), 
       xlab = "Year", ylab = "food production", col='green')  
  
  #access cooking tech 
  plot(as.numeric(as.character(dcc$year)), 
       dcc$EG.CFT.ACCS.ZS,
       main = paste("Acces to cooking technology over time \n in", vshape$country[i]), 
       xlab = "Year", ylab = "% access to cooking technology", col='orange')  
    
}


# Uzbekistan
# data 

uz = data[data$country == "Uzbekistan",]

# % undernourishment 
plot(uz$SN.ITK.DEFC.ZS, uz$stability_index_estimate,
     main = paste("Stability Index vs. % Undernourished in", "Uzbekistan"), 
     xlab = "Percent Populatoin Undernourished", ylab = "Political Stability Index", 
     xlim=c(4,25), ylim=c(-2.5,0.5), col='red', cex = 1.6, pch = 16)
text(uz$SN.ITK.DEFC.ZS, uz$stability_index_estimate,
     labels=uz$year, cex = 1, pos = 4)

# water 
plot(uz$SH.H2O.BASW.ZS, uz$stability_index_estimate,
     main = paste("Stability Index vs. People using basic drinking water service \n in", "Uzbekistan"), 
     xlab = "People using at least basic drinking water services (% pop.)", ylab = "Political Stability Index", 
     xlim=c(84,95), ylim=c(-2.5,0.5), col='blue', cex = 1.6, pch = 16)
text(uz$SH.H2O.BASW.ZS, uz$stability_index_estimate,
     labels=uz$year, cex = 1, pos = 4)

# over time 
plot(as.numeric(as.character(uz$year)),
     uz$stability_index_estimate, 
     main = paste("Stability Index over time in", "Uzbekistan"),
     xlab = "Year", ylab = "Political Stability Index", 
     pch = 16, cex = 1.6)

text(2005, uz$stability_index_estimate[uz$year == 2005], 
     labels = c("2005 Andijan Massacre"), 
     pos = 2, col = 'forestgreen', cex = 1.3)
points(2005, uz$stability_index_estimate[uz$year == 2005], 
       col = 'forestgreen', pch=16, cex = 1.6)






peru = data[data$country == "Peru",]
sl = data[data$country == "Sierra Leone",]
cor(peru$SN.ITK.DEFC.ZS, peru$stability_index_estimate, use="complete.obs")
cor(sl$SN.ITK.DEFC.ZS, sl$stability_index_estimate, use = "complete.obs")
swi = data[data$country == "Switzerland",]
plot(swi$SN.ITK.DEFC.ZS, swi$stability_index_estimate,
     main = paste("Stability Index vs. % Undernourished \n in", "Switzerland"), 
     xlab = "% Undernourished", ylab = "Stability Index", 
     xlim=c(0,60), ylim=c(-3.5,2))
aus = data[data$country == "Australia",]
plot(aus$SN.ITK.DEFC.ZS, aus$stability_index_estimate,
     main = paste("Stability Index vs. % Undernourished \n in", "Australia"), 
     xlab = "% Undernourished", ylab = "Stability Index", 
     xlim=c(0,60), ylim=c(-3.5,2))
can = data[data$country == "Canada",]
plot(can$SN.ITK.DEFC.ZS, can$stability_index_estimate,
     main = paste("Stability Index vs. % Undernourished \n in", "Canada"), 
     xlab = "% Undernourished", ylab = "Stability Index", 
     xlim=c(0,60), ylim=c(-3.5,2))

