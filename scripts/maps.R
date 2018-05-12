# AJ's work on maps

# Get XLSX package.
# install.packages('openxlsx') # Uncomment this to install the package.
library('openxlsx')
library('rworldmap')
library(magick)
setwd('C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/images/maps/')


# MAKE SURE CURRENT WORKING DIRECTORY IS SET TO THIS FILE!
PROCESSED_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/processed/'
ORIGINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/original/'
FINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/final'

processed.codes = read.csv(file.path(PROCESSED_DATA_DIR, 'country_codes.csv'))
processed.terror = read.csv(file.path(PROCESSED_DATA_DIR, 'terrorism_incidents.csv'))
processed.wdi = read.csv(file.path(PROCESSED_DATA_DIR, 'wdi.csv'))
processed.wgi = read.csv(file.path(PROCESSED_DATA_DIR, 'wgi.csv'))

data.full = read.csv(file.path(FINAL_DATA_DIR, 'data_matched_wdi_wgi.csv'))


yeared = c(2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
          2009, 2010, 2011, 2012, 2013, 2014, 2015)


important_columns = c("code", "SH.H2O.BASW.ZS", "SN.ITK.DFCT")


data.vis <- data.frame(matrix(ncol = 0, nrow = nrow(data.full)))

data.vis$code <- data.full$code
data.vis$year <- data.full$year
data.vis$water <- data.full$SH.H2O.BASW.ZS
data.vis$deficit <- -data.full$SN.ITK.DEFC.ZS
data.vis$deficitneg <- -data.full$SN.ITK.DFCT
data.vis$stable <- data.full$stability_index_estimate

n=10
cmcolors = cm.colors(n, alpha = 1)

heatcolors = heat.colors(n, alpha = 1)



for(yd in yeared)
{
data.vis.year <- subset(data.vis, data.vis$year==yd)

print(yd)
print("HELLOOOOOOOOOOOO")
  
sPDF <- joinCountryData2Map(data.vis.year, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)

name =paste("water", toString(yd), ".jpg", sep="")
jpeg(name)


par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

#mapDevice(device="x11")
map.params <- mapCountryData(sPDF, nameColumnToPlot='water', catMethod=seq(0, 100, by=5), addLegend='TRUE', missingCountryCol = "grey", 
                             mapTitle=paste("Percent Access to Drinking Water", toString(yd), sep=" "), oceanCol="lightblue", colourPalette =heatcolors)



dev.off()

rough <- image_read(name)
test <- image_annotate(rough, toString(yd), size = 30, color = "black", degrees = 0, location = "+15+320")
test <- image_flip(test)
test <- image_crop(test, "600x400")
test <- image_flip(test)

print(test)

image_write(test, paste("water", toString(yd), "annot", ".jpg", sep=""))


name =paste("undernourished", toString(yd), ".jpg", sep="")
jpeg(name)

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")


map.params <- mapCountryData(sPDF, nameColumnToPlot='deficit', catMethod=seq(-60, 0, by=5), addLegend='TRUE', missingCountryCol = "grey", 
                             mapTitle = paste("Percent Undernourished", toString(yd), sep=" ") , oceanCol="lightblue", colourPalette =heatcolors)

dev.off()

rough <- image_read(name)
test <- image_annotate(rough, toString(yd), size = 30, color = "black", degrees = 0, location = "+15+320")
test <- image_flip(test)
test <- image_crop(test, "600x400")
test <- image_flip(test)

print(test)

image_write(test, paste("undernourished", toString(yd), "annot", ".jpg", sep=""))


name =paste("stability", toString(yd), ".jpg", sep="")
jpeg(name)


par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")


map.params <- mapCountryData(sPDF, nameColumnToPlot='stable', catMethod=seq(-3, 2, by=.5), addLegend='TRUE', missingCountryCol = "grey", 
                             mapTitle=paste("Stability Index Estimate", toString(yd), sep=" "), oceanCol="lightblue", colourPalette =heatcolors)



dev.off()

rough <- image_read(name)
test <- image_annotate(rough, toString(yd), size = 30, color = "black", degrees = 0, location = "+15+320")
test <- image_flip(test)
test <- image_crop(test, "600x400")
test <- image_flip(test)

print(test)

image_write(test, paste("stable", toString(yd), "annot", ".jpg", sep=""))

}