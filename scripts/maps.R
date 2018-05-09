# AJ's work on maps

# Get XLSX package.
# install.packages('openxlsx') # Uncomment this to install the package.
library('openxlsx')
library('rworldmap')
setwd('C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/images/maps/')


# MAKE SURE CURRENT WORKING DIRECTORY IS SET TO THIS FILE!
PROCESSED_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/processed/'
ORIGINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/original/'
FINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/final'

processed.codes = read.csv(file.path(PROCESSED_DATA_DIR, 'country_codes.csv'))
processed.terror = read.csv(file.path(PROCESSED_DATA_DIR, 'terrorism_incidents.csv'))
processed.wdi = read.csv(file.path(PROCESSED_DATA_DIR, 'wdi.csv'))
processed.wgi = read.csv(file.path(PROCESSED_DATA_DIR, 'wgi.csv'))

data.full = read.csv(file.path(FINAL_DATA_DIR, 'data_try.csv'))


yeared = c(2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
          2009, 2010, 2011, 2012, 2013, 2014, 2015)


important_columns = c("code", "SH.H2O.BASW.ZS", "SN.ITK.DFCT")


data.vis <- data.frame(matrix(ncol = 0, nrow = nrow(data.full)))

data.vis$code <- data.full$code
data.vis$year <- data.full$year
data.vis$water <- data.full$SH.H2O.BASW.ZS
data.vis$deficit <- data.full$SN.ITK.DFCT
data.vis$deficitneg <- -data.full$SN.ITK.DFCT

n=10
cmcolors = cm.colors(n, alpha = 1)
revheatcolors = rev(heatcolors)

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


map.params <- mapCountryData(sPDF, nameColumnToPlot='water', catMethod=seq(0, 100, by=5), addLegend='TRUE', missingCountryCol = "grey", 
                             mapTitle="Percent Access to Drinking Water", oceanCol="lightblue", colourPalette =cmcolors)



dev.off()


name =paste("deficit", toString(yd), ".jpg", sep="")
jpeg(name)

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")


map.params <- mapCountryData(sPDF, nameColumnToPlot='deficitneg', catMethod=seq(-600, 0, by=50), addLegend='TRUE', missingCountryCol = "grey", 
                             mapTitle="Kcal Deficit (Negative)", oceanCol="lightblue", colourPalette =heatcolors)

dev.off()


}