# AJ's work on global mapping and possibly kmeans

# Get XLSX package.
# install.packages('openxlsx') # Uncomment this to install the package.
library('openxlsx')
library('rworldmap')
setwd('C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/images/')


# MAKE SURE CURRENT WORKING DIRECTORY IS SET TO THIS FILE!
PROCESSED_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/processed/'
ORIGINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/original/'
FINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/final'

processed.codes = read.csv(file.path(PROCESSED_DATA_DIR, 'country_codes.csv'))
processed.terror = read.csv(file.path(PROCESSED_DATA_DIR, 'terrorism_incidents.csv'))
processed.wdi = read.csv(file.path(PROCESSED_DATA_DIR, 'wdi.csv'))
processed.wgi = read.csv(file.path(PROCESSED_DATA_DIR, 'wgi.csv'))



# Only these 18 years are in the WGI dataset.
years = c(1996, 1998, 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
          2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)


# for(year in years){
#   year.data = subset(processed.wgi, processed.wgi$year == year)
#   print(sum((year.data$stability_index_estimate == '#N/A')))
# }
#All have 162 N/As, will remove those countries/years

# year.data = subset(processed.wgi, processed.wgi$stability_index_estimate != '#N/A')
# 
# 
# for(year in years){
#   my.sub = subset(year.data, year.data$year == year)
#   print(dim(my.sub))
# }
#same deal as above, same number of countries in each remain

datast = na.omit(year.data)
all.years <- aggregate(.~country, data=datast, FUN=mean)

#year.sub = na.omit(subset(year.data, year.data$year == 2004))
#sPDF <- joinCountryData2Map(year.sub, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
sPDF <- joinCountryData2Map(all.years, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)

#mapDevice() #create world map shaped window
pdf('test.pdf')
mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', addLegend='FALSE')
dev.off()

allCo <- unique(year.data$country)

avg.data <- as.matrix(year.data[0:length(allCo),-c(1,4)])

counter<- 1


for(countr in unique(year.data$country))
{
  tempSub <- subset(year.data, year.data$country == countr)
  
  avg.data[counter,] = c(toString(tempSub$country[1]), toString(tempSub$code[1]), mean(as.numeric(as.character(tempSub$stability_index_estimate))), 
                         mean(as.numeric(as.character(tempSub$stability_index_stderr))), mean(as.numeric(as.character(tempSub$stability_index_numsrc))), 
                         mean(as.numeric(as.character(tempSub$stability_index_rank))), mean(as.numeric(as.character(tempSub$stability_index_lower))), 
                         mean(as.numeric(as.character(tempSub$stability_index_upper))))
  
  
  counter <- counter + 1
}

avg.frame <- data.frame(avg.data)

sPDF <- joinCountryData2Map(avg.frame, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
pdf("average.pdf")
mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', addLegend='FALSE')
dev.off()


for(year in years){
  year.sub = na.omit(subset(year.data, year.data$year == year))
  sPDF <- joinCountryData2Map(year.sub, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
  name =paste("testyear", toString(year), ".pdf", sep="")
  pdf(name)
  mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', addLegend='FALSE')
  dev.off()
}

