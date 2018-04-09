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

year.data = subset(processed.wgi, processed.wgi$stability_index_estimate != '#N/A')

# datast = na.omit(year.data)
# all.years <- aggregate(.~country, data=datast, FUN=mean)

# #year.sub = na.omit(subset(year.data, year.data$year == 2004))
# #sPDF <- joinCountryData2Map(year.sub, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
# sPDF <- joinCountryData2Map(all.years, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
# 
# #mapDevice() #create world map shaped window
# pdf('test.pdf')
# mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', addLegend='FALSE')
# dev.off()

allCo <- unique(year.data$country)

avg.data <- as.matrix(year.data[0:length(allCo),-c(1,2,4)])

counter<- 1


for(countr in unique(year.data$country))
{
  tempSub <- subset(year.data, year.data$country == countr)
  
  avg.data[counter,] = c(toString(tempSub$code[1]), mean(as.numeric(as.character(tempSub$stability_index_estimate)), na.rm = "TRUE"), 
                         mean(as.numeric(as.character(tempSub$stability_index_stderr))), mean(as.numeric(as.character(tempSub$stability_index_numsrc))), 
                         mean(as.numeric(as.character(tempSub$stability_index_rank))), mean(as.numeric(as.character(tempSub$stability_index_lower))), 
                         mean(as.numeric(as.character(tempSub$stability_index_upper))))
  
  
  counter <- counter + 1
}

avg.frame <- data.frame(avg.data)

avg.frame$stability_index_estimate <- as.numeric(as.character(avg.frame$stability_index_estimate))

n=10
heatcolors = heat.colors(n, alpha = 1)
revheatcolors = rev(heatcolors)
# 
# plot(1:10, 1:10, pch=16, cex=2, col = hcolrev)


sPDF <- joinCountryData2Map(avg.frame, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
pdf("average.pdf")
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# colourPalette <- brewer.pal(5,'RdYlGn')

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map.params <- mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', catMethod=seq(-3, 2, by=.5), addLegend='TRUE', missingCountryCol = "grey", 
                             mapTitle="Average Political Stability Score", oceanCol="lightblue", colourPalette =heatcolors)
#do.call( addMapLegend, c(map.params, legendWidth=0.5, legendMar = 2))

# do.call( addMapLegend, c( map.params
#                           , legendLabels="all"
#                           , legendWidth=0.5 ))



dev.off()


# for(year in years){
#   year.sub = na.omit(subset(year.data, year.data$year == year))
#   sPDF <- joinCountryData2Map(year.sub, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
#   name =paste("testyear", toString(year), ".pdf", sep="")
#   pdf(name)
#   mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', addLegend='FALSE')
#   dev.off()
# }




######################################SPECIFIC YEAR FOR JEN

year.data = subset(processed.wgi, processed.wgi$stability_index_estimate != '#N/A')

jen.data = subset(year.data, year.data$year == 2016)

allCo <- unique(jen.data$country)

avg.data1 <- as.matrix(jen.data[0:length(allCo),-c(1,2,4)])

counter <- 1

for(countr in unique(jen.data$country))
{
  tempSub <- subset(jen.data, jen.data$country == countr)
  
  avg.data1[counter,] = c(toString(tempSub$code[1]), mean(as.numeric(as.character(tempSub$stability_index_estimate)), na.rm = "TRUE"), 
                         mean(as.numeric(as.character(tempSub$stability_index_stderr))), mean(as.numeric(as.character(tempSub$stability_index_numsrc))), 
                         mean(as.numeric(as.character(tempSub$stability_index_rank))), mean(as.numeric(as.character(tempSub$stability_index_lower))), 
                         mean(as.numeric(as.character(tempSub$stability_index_upper))))
  
  
  counter <- counter + 1
}

avg.frame1 <- data.frame(avg.data1)

avg.frame1$stability_index_estimate <- as.numeric(as.character(avg.frame1$stability_index_estimate))

sPDF <- joinCountryData2Map(avg.frame1, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
pdf("2016.pdf")
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# colourPalette <- brewer.pal(5,'RdYlGn')

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map.params <- mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', catMethod=seq(-3, 2, by=.5), addLegend='TRUE',  
                             missingCountryCol = "grey", mapTitle="Political Stability Score 2016", oceanCol="lightblue", colourPalette =heatcolors)
#do.call( addMapLegend, c(map.params, legendWidth=0.5, legendMar = 2))

# do.call( addMapLegend, c( map.params
#                           , legendLabels="all"
#                           , legendWidth=0.5 ))

dev.off()





######################################SPECIFIC YEAR FOR JEN

year.data = subset(processed.wgi, processed.wgi$stability_index_estimate != '#N/A')

jen.data = subset(year.data, year.data$year == 1996)

allCo <- unique(jen.data$country)

avg.data1 <- as.matrix(jen.data[0:length(allCo),-c(1,2,4)])

counter <- 1

for(countr in unique(jen.data$country))
{
  tempSub <- subset(jen.data, jen.data$country == countr)
  
  avg.data1[counter,] = c(toString(tempSub$code[1]), mean(as.numeric(as.character(tempSub$stability_index_estimate)), na.rm = "TRUE"), 
                          mean(as.numeric(as.character(tempSub$stability_index_stderr))), mean(as.numeric(as.character(tempSub$stability_index_numsrc))), 
                          mean(as.numeric(as.character(tempSub$stability_index_rank))), mean(as.numeric(as.character(tempSub$stability_index_lower))), 
                          mean(as.numeric(as.character(tempSub$stability_index_upper))))
  
  
  counter <- counter + 1
}

avg.frame1 <- data.frame(avg.data1)

avg.frame1$stability_index_estimate <- as.numeric(as.character(avg.frame1$stability_index_estimate))

sPDF <- joinCountryData2Map(avg.frame1, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
pdf("1996.pdf")
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# colourPalette <- brewer.pal(5,'RdYlGn')

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map.params <- mapCountryData(sPDF, nameColumnToPlot='stability_index_estimate', catMethod=seq(-3, 2, by=.5), addLegend='TRUE',  
                             missingCountryCol = "grey", mapTitle="Political Stability Score 1996", oceanCol="lightblue", colourPalette =heatcolors)
#do.call( addMapLegend, c(map.params, legendWidth=0.5, legendMar = 2))

# do.call( addMapLegend, c( map.params
#                           , legendLabels="all"
#                           , legendWidth=0.5 ))

dev.off()