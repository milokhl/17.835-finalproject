library(ggplot2)
library(car)
# taliab, 17.835 final project - data processing/ visualization

setwd("C:\\Users\\Talia Blum\\Dropbox (MIT)\\2Spring 2018\\17.835\\17.835 Final Project")
data <- read.csv("data_matched_all.csv")

#descriptive analysis:
#================================
#percent undernourished vs poltical stability linear regression for 2000 and 2015
data_kcal1 <- data[,c('code','year','stability_index_estimate','SN.ITK.DEFC.ZS','SP.POP.TOTL')]
data_kcal1 <- na.omit(data_kcal1)
pdf(file = "pct_undernourished_2000vs20152skinnyTEST.pdf", 16)
par(cex.main = 1.5, cex.lab=1.5,
    mar=c(5,5,4,4))
par(mfrow=c(1,2))
for(yeer in c(2000,2015)){
  #limx <- yeer*(-80)+163500
  limx <- 3000
  plot(data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer],
       data_kcal1$stability_index_estimate[data_kcal1$year == yeer],
       #type = "n",
       
       xlab = "Percent Undernourished",
       ylab = "Political Stability Index",
       main = as.character(yeer))
  linee <- lm(data_kcal1$stability_index_estimate[data_kcal1$year == yeer] ~
                data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer])
  abline(a = linee$coefficients[1],
         b = linee$coefficients[2])
  points(data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer], #x axis
         data_kcal1$stability_index_estimate[data_kcal1$year == yeer], #y axis
         pch = 16, #point style
         cex = data_kcal1$SP.POP.TOTL/3000000, #point size
         col = rgb(1,0,0,alpha=.25))
  text(data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer], #x axis
       y = data_kcal1$stability_index_estimate[data_kcal1$year == yeer], #y axis
       labels = data_kcal1$code[data_kcal1$year == yeer],
       cex = .7)
}
dev.off()



#=====================================
#percent access to water vs poltical stability linear regression for 2000 and 2015
data_water1 <- data[,c('code','year','stability_index_estimate','SH.H2O.BASW.ZS','SP.POP.TOTL')]
data_water1 <- na.omit(data_water1)
pdf(file = "basic_drinking_water_2000vs20152skinnyTEST.pdf", 16)
par(cex.main = 1.5, cex.lab=1.5,
    mar=c(5,5,4,4))
par(mfrow=c(1,2))
for(yeer in c(2000,2015)){
  limx <- 3500 # yeer*(-80)+163500
  plot(data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer],
       data_water1$stability_index_estimate[data_water1$year == yeer],
       type = "n",
       xlab = "People Using At Least Basic Drinking Water (% of pop.)",
       ylab = "Political Stability Index",
       main = as.character(yeer))
  linee <- lm(data_water1$stability_index_estimate[data_water1$year == yeer] ~
                data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer])
  abline(a = linee$coefficients[1],
         b = linee$coefficients[2])
  points(data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer], #x axis
         data_water1$stability_index_estimate[data_water1$year == yeer], #y axis
         pch = 16, #point style
         cex = data_water1$SP.POP.TOTL/2500000, #point size
         col = rgb(0,0,1,alpha=.25))
  text(data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer], #x axis
       y = data_water1$stability_index_estimate[data_water1$year == yeer], #y axis
       labels = data_water1$code[data_water1$year == yeer],
       cex = .7)
}
dev.off()

#===========================================================
#percent undernourished vs PSI for all years (except 2001) for animation in slides
setwd("C:\\Users\\Talia Blum\\Dropbox (MIT)\\2Spring 2018\\17.835\\17.835 Final Project\\basic_drinking_water figs")
data_water1 <- data[,c('code','year','stability_index_estimate','SH.H2O.BASW.ZS','SP.POP.TOTL')]
data_water1 <- na.omit(data_water1)
for(yeer in c(2000,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)){
  png(file = paste("basic_drinking_water",as.character(yeer),".png", sep = ""))
  par(cex.main = 1.5, cex.lab=1.5,
      mar=c(5,5,4,4))
  plot(data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer],
       data_water1$stability_index_estimate[data_water1$year == yeer],
       type = "n",
       xlim = c(0,100),
       ylim = c(-3.5,2),
       xlab = "People Using At Least Basic Drinking Water (% of pop.)",
       ylab = "Political Stability Index",
       main = as.character(yeer))
  linee <- lm(data_water1$stability_index_estimate[data_water1$year == yeer] ~
                data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer])
  abline(a = linee$coefficients[1],
         b = linee$coefficients[2])
  abline(h = 0, untf = TRUE, lty = 2)
  
  points(data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer], #x axis
         data_water1$stability_index_estimate[data_water1$year == yeer], #y axis
         pch = 16, #point style
         cex = data_water1$SP.POP.TOTL/3000000, #point size
         col = rgb(0,0,1,alpha=.25))
  text(data_water1$SH.H2O.BASW.ZS[data_water1$year == yeer], #x axis
       y = data_water1$stability_index_estimate[data_water1$year == yeer], #y axis
       labels = data_water1$code[data_water1$year == yeer],
       cex = .7)
  dev.off()
  
}


#percent undernourished vs PSI for all years except 2001 --> for the animation in presentation
setwd("C:\\Users\\Talia Blum\\Dropbox (MIT)\\2Spring 2018\\17.835\\17.835 Final Project\\pct_undernourished_ figs")
data_kcal1 <- data[,c('code','year','stability_index_estimate','SN.ITK.DEFC.ZS','SP.POP.TOTL')]
data_kcal1 <- na.omit(data_kcal1)
for(yeer in c(2000,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)){
  png(file = paste("pct_undernourished_",as.character(yeer),".png", sep = ""))
  par(cex.main = 1.5, cex.lab=1.5,
      mar=c(5,5,4,4))
  #limx <- yeer*(-80)+163500
  limx <- 3000
  plot(data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer],
       data_kcal1$stability_index_estimate[data_kcal1$year == yeer],
       #type = "n",
       xlab = "Percent Undernourished",
       ylab = "Political Stability Index",
       main = as.character(yeer),
       xlim = c(0,60),
       ylim = c(-3.5,2))
  linee <- lm(data_kcal1$stability_index_estimate[data_kcal1$year == yeer] ~
                data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer])
  abline(a = linee$coefficients[1],
         b = linee$coefficients[2])
  abline(h = 0, untf = TRUE, lty = 2)
  
  points(data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer], #x axis
         data_kcal1$stability_index_estimate[data_kcal1$year == yeer], #y axis
         pch = 16, #point style
         cex = data_kcal1$SP.POP.TOTL/4000000, #point size
         col = rgb(1,0,0,alpha=.25))
  text(data_kcal1$SN.ITK.DEFC.ZS[data_kcal1$year == yeer], #x axis
       y = data_kcal1$stability_index_estimate[data_kcal1$year == yeer], #y axis
       labels = data_kcal1$code[data_kcal1$year == yeer],
       cex = .7)
  dev.off()
}


#==============================================================================
#water vs PSI, subsetted by population for any year = yeer
setwd("C:\\Users\\Talia Blum\\Dropbox (MIT)\\2Spring 2018\\17.835\\17.835 Final Project\\top50_water figs")
yeer = 2015 #change to desired year for plot
data_water1 <- data[,c('code','year','stability_index_estimate','SH.H2O.BASW.ZS','SP.POP.TOTL')]
data_water1 <- na.omit(data_water1)
data_water1 <- data_water1[data_water1$year == yeer,]
data_water1 <- data_water1[order(data_water1$SP.POP.TOTL),]
data_water1bottom <- data_water1[1:50,]
nr <- nrow(data_water1)
data_water1top <- data_water1[(nr - 50):nr,]

pdf(file = "basic_drinking_water_2000skinnyTOP50POP.pdf", 16)
par(cex.main = 1.5, cex.lab=1.5,
    mar=c(5,5,4,4))
par(mfrow=c(1,2))
#for(yeer in c(2000,2015)){
limx <- 3500 # yeer*(-80)+163500
plot(data_water1bottom$SH.H2O.BASW.ZS,
     data_water1bottom$stability_index_estimate,
     type = "n",
     ylim = c(-2.5,1.7),
     xlab = "People Using At Least Basic Drinking Water (% of pop.)",
     ylab = "Political Stability Index",
     main = paste("Bottom 50,", as.character(yeer)))
linee <- lm(data_water1bottom$stability_index_estimate ~
              data_water1bottom$SH.H2O.BASW.ZS)
abline(a = linee$coefficients[1],
       b = linee$coefficients[2])
abline(h = 0, untf = TRUE, lty = 2)
text(data_water1bottom$SH.H2O.BASW.ZS, #x axis
     y = data_water1bottom$stability_index_estimate, #y axis
     labels = data_water1bottom$code,
     cex = .7,
     col = "blue")

limx <- 3500 # yeer*(-80)+163500
plot(data_water1top$SH.H2O.BASW.ZS,
     data_water1top$stability_index_estimate,
     type = "n",
     ylim = c(-2.5,1.7),
     xlab = "People Using At Least Basic Drinking Water (% of pop.)",
     ylab = "Political Stability Index",
     main = paste("Top 50,", as.character(yeer)))
linee <- lm(data_water1top$stability_index_estimate ~
              data_water1top$SH.H2O.BASW.ZS)
abline(a = linee$coefficients[1],
       b = linee$coefficients[2])
abline(h = 0, untf = TRUE, lty = 2)
text(data_water1top$SH.H2O.BASW.ZS, #x axis
     y = data_water1top$stability_index_estimate, #y axis
     labels = data_water1top$code,
     cex = .7,
     col = "blue")
#}
dev.off()


#percent undernourished vs PSI, subsetted by population, for any year = yeer
setwd("C:\\Users\\Talia Blum\\Dropbox (MIT)\\2Spring 2018\\17.835\\17.835 Final Project\\top50_pct_under figs")
yeer = 2000 #change to whatever year you want the plot of
data_kcal1 <- data[,c('code','year','stability_index_estimate','SN.ITK.DEFC.ZS','SP.POP.TOTL')]
data_kcal1 <- na.omit(data_kcal1)
data_kcal1 <- data_kcal1[data_kcal1$year == yeer,]
data_kcal1 <- data_kcal1[order(data_kcal1$SP.POP.TOTL),]
data_kcal1bottom <- data_kcal1[1:50,]
nr <- nrow(data_kcal1)
data_kcal1top <- data_kcal1[(nr - 50):nr,]

pdf(file = "pct_undernourished_2000skinny50POP.pdf", 16)
par(cex.main = 1.5, cex.lab=1.5,
    mar=c(5,5,4,4))
par(mfrow=c(1,2))
#for(yeer in c(2000,2015)){
limx <- 3500 # yeer*(-80)+163500
plot(data_kcal1bottom$SN.ITK.DEFC.ZS,
     data_kcal1bottom$stability_index_estimate,
     type = "n",
     ylim = c(-2.5,1.7),
     xlab = "Percent Undernourished",
     ylab = "Political Stability Index",
     main = paste("Bottom 50,", as.character(yeer)))
linee <- lm(data_kcal1bottom$stability_index_estimate ~
              data_kcal1bottom$SN.ITK.DEFC.ZS)
abline(a = linee$coefficients[1],
       b = linee$coefficients[2])
abline(h = 0, untf = TRUE, lty = 2)
text(data_kcal1bottom$SN.ITK.DEFC.ZS, #x axis
     y = data_kcal1bottom$stability_index_estimate, #y axis
     labels = data_kcal1bottom$code,
     cex = .7,
     col = "red")

limx <- 3500 # yeer*(-80)+163500
plot(data_kcal1top$SN.ITK.DEFC.ZS,
     data_kcal1top$stability_index_estimate,
     type = "n",
     ylim = c(-2.5,1.7),
     xlab = "Percent Undernourished",
     ylab = "Political Stability Index",
     main = paste("Top 50,", as.character(yeer)))
linee <- lm(data_kcal1top$stability_index_estimate ~
              data_kcal1top$SN.ITK.DEFC.ZS)
abline(a = linee$coefficients[1],
       b = linee$coefficients[2])
abline(h = 0, untf = TRUE, lty = 2)

text(data_kcal1top$SN.ITK.DEFC.ZS, #x axis
     y = data_kcal1top$stability_index_estimate, #y axis
     labels = data_kcal1top$code,
     cex = .7,
     col = "red")
#}
dev.off()





