# AJ's work on kmeans

# Get XLSX package.
# install.packages('openxlsx') # Uncomment this to install the package.
library('openxlsx')
library('rworldmap')
setwd('C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/images/kmeans/')


# MAKE SURE CURRENT WORKING DIRECTORY IS SET TO THIS FILE!
PROCESSED_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/processed/'
ORIGINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/original/'
FINAL_DATA_DIR = 'C:/Users/aroot/Desktop/Classes/17.835/Project/gitstuff/17.835-finalproject/data/final'

processed.codes = read.csv(file.path(PROCESSED_DATA_DIR, 'country_codes.csv'))
processed.terror = read.csv(file.path(PROCESSED_DATA_DIR, 'terrorism_incidents.csv'))
processed.wdi = read.csv(file.path(PROCESSED_DATA_DIR, 'wdi.csv'))
processed.wgi = read.csv(file.path(PROCESSED_DATA_DIR, 'wgi.csv'))


# data.full = read.csv(file.path(FINAL_DATA_DIR, 'data_matched_all.csv'))

data.copy <- read.csv(file.path(FINAL_DATA_DIR, 'data_matched_all.csv'))

#remove columns with more than 100 NAs

# removed_columns <- function(data.stuff){
#   
#   fourty_percent <- .40 * nrow(data.stuff)
#   
#   counter <- 0
#   for(i in names(data.stuff)){
#   
#     is.val <- sum(is.na(data.stuff[[i]]))
#     if (is.val > fourty_percent){
#       #print("removed")
#       #print(i)
#       data.stuff[[i]] <- NULL
#       counter <- (counter + 1)
#     }
#     else
#     {
#       #print("saved")
#       #print(i)
#     }
#   }
#   return(counter)
# }


# library(cluster)
# library(HSAUR)
# 
# data(pottery)
# km    <- kmeans(pottery,4)
# dissE <- daisy(pottery) 
# dE2   <- dissE^2
# sk2   <- silhouette(km$cl, dE2)
# plot(sk2)


# data(iris)
# dat <- iris[, -5] # without known classification 
# # Kmeans clustre analysis
# clus <- kmeans(dat, centers=4)
# plotcluster(dat, clus$cluster)


library(cluster)
library(fpc)


data.full = read.csv(file.path(FINAL_DATA_DIR, 'data_matched_all.csv'))

data.copy <- as.data.frame(data.full)

years = c(1996, 1998, 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
          2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
# 
# for (year_val in years) {
#   dir.create(toString(year_val))
# }


for (year_val in years) {
  

  temp.data <- subset(data.copy, data.copy$year == year_val)
  #print(removed_columns(temp.data))
  
  twenty_percent <- .20 * nrow(temp.data)
  
  counter <- 0
  for(i in names(temp.data)){
    
    is.val <- sum(is.na(temp.data[[i]]))
    if (is.val > twenty_percent){
      #print("removed")
      #print(i)
      temp.data[[i]] <- NULL
      counter <- (counter + 1)
    }
  }
  print(counter)
  
  full.year <- na.omit(temp.data)
  
  altered.year <- as.data.frame(full.year)
  
  row.names(altered.year) <- altered.year$code
  

  altered.index <- altered.year[, c("stability_index_numsrc", "stability_index_rank", "stability_index_lower", "stability_index_upper", "stability_index_estimate", 
                                    "stability_index_stderr")]
  
  maintain.stab <- altered.year$stability_index_estimate
  
  remove.names <- c("stability_index_numsrc", "stability_index_rank", "stability_index_lower", "stability_index_upper", "stability_index_estimate", 
                    "stability_index_stderr", "X", "country", "year", "code")
  
  for(i in remove.names){
    
    if ( i %in% names(altered.year))
    {
      altered.year[[i]] <- NULL
      
    }
  }
  
  # Kmeans clustre analysis
  
  scaled.year <- scale(altered.year)
  scaled.index <- scale(altered.index)
  
  
  # for(i in 1:20) {
  #   jpeg(paste0(year_val,"/", "kmeans_", year_val, "_",i,".jpg")) 
  #   clus <- kmeans(scaled.year, centers=i)
  #   clusplot(scaled.year, clus$cluster, color=TRUE, shade=TRUE, 
  #            labels=2, lines=0)
  #   dev.off() 
  #   
  #   jpeg(paste0(year_val, "/",  "kmeans_", year_val, "_",i,"_index.jpg")) 
  #   clus_ind <- kmeans(scaled.index, centers=i)
  #   clusplot(scaled.index, clus_ind$cluster, color=TRUE, shade=TRUE, 
  #            labels=2, lines=0)
  #   dev.off() 
  # }
  
  
  for(i in 2:10) {
    # jpeg(paste0(year_val,"/", "kmaps_", year_val, "_",i,".jpg")) 
    clus <- kmeans(scaled.year, centers=i)

    world_data = as.data.frame(clus$cluster)
    world_data$code = row.names(world_data)
    
    heatcolors = heat.colors(i, alpha = 1)
    revheatcolors = rev(heatcolors)
    
    # sPDF <- joinCountryData2Map(world_data, joinCode = "NAME", nameJoinColumn = "code", verbose=TRUE)
    # 
    # par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
    # 
    # map.params <- mapCountryData(sPDF, nameColumnToPlot='clus$cluster', addLegend='TRUE',  catMethod = "pretty",  missingCountryCol = "grey", 
    #                              mapTitle=paste0("K Means Clustering - Year: ", year_val, " Clusters: ", i), oceanCol="lightblue", colourPalette =heatcolors)
    # 
    # 
    # 
    # dev.off() 
    
    jpeg(paste0(year_val,"/", "corre_", year_val, "_",i,".jpg")) 
    
    plot(world_data$`clus$cluster`, maintain.stab)
    
    dev.off()
    
  }

  
  

}


