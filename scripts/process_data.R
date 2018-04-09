# This script primarily puts all of the datasets into (country, year) row ordering.

# Get XLSX package.
# install.packages('openxlsx') # Uncomment this to install the package.
library('openxlsx')

# MAKE SURE CURRENT WORKING DIRECTORY IS SET TO THIS FILE!
PROCESSED_DATA_DIR = '../data/processed/'
ORIGINAL_DATA_DIR = '../data/original/'
FINAL_DATA_DIR = '../data/final'

original.wdi = read.csv(file.path(ORIGINAL_DATA_DIR, 'wdi.csv'))
original.wgi = read.xlsx(file.path(ORIGINAL_DATA_DIR, 'wgi_1996-2016.xlsx'), 3, startRow = 14, rowNames=T, colNames=T)
original.tincident = read.csv(file.path(ORIGINAL_DATA_DIR, 'terrorist_incidents_1970-2016.csv'))

### PROCESS THE WGI DATA. ###
# Create new dataframe.
new.wgi = data.frame(matrix(ncol = 9, nrow = dim(original.wgi)[1]-1))
column_names = c('country', 'code', 'year', 'stability_index_estimate', 'stability_index_stderr', 'stability_index_numsrc',
                 'stability_index_rank', 'stability_index_lower', 'stability_index_upper')
colnames(new.wgi) = column_names

# Only these 18 years are in the WGI dataset.
years = c(1996, 1998, 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
          2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)

rowIdx = 1
# For every country (exclude header).
for (country in 2:dim(original.wgi)[1]) {
  print(row.names(original.wgi)[country])
  
  # For every year, we have 6 columns.
  colIdx = 2
  for (year in years) {
    new.wgi[rowIdx,]$country = row.names(original.wgi)[country]
    new.wgi[rowIdx,]$code = original.wgi[country, 1]
    new.wgi[rowIdx,]$year = year
    new.wgi[rowIdx,]$stability_index_estimate = original.wgi[country, colIdx]
    new.wgi[rowIdx,]$stability_index_stderr = original.wgi[country, colIdx+1]
    new.wgi[rowIdx,]$stability_index_numsrc = original.wgi[country, colIdx+2]
    new.wgi[rowIdx,]$stability_index_rank = original.wgi[country, colIdx+3]
    new.wgi[rowIdx,]$stability_index_lower = original.wgi[country, colIdx+4]
    new.wgi[rowIdx,]$stability_index_upper = original.wgi[country, colIdx+5]
    rowIdx = rowIdx + 1 # Start a new row for the next year.
    colIdx = colIdx + 6 # Offset to the columns for the next year.
  }
}

# Save the new.wgi to csv.
write.csv(new.wgi, file.path(PROCESSED_DATA_DIR, 'wgi.csv'))

### PROCESS THE TERRORIST INCIDENT DATA. ###
head(original.tincident)
new.terrorism_incidents = original.tincident
colnames(new.terrorism_incidents) = c('country', 'code', 'year', 'terrorism_incidents')
write.csv(new.terrorism_incidents, file.path(PROCESSED_DATA_DIR, 'terrorism_incidents.csv'))

### PROCESS THE WDI DATA. ###
# In the original data, rows are (country, series) pairs, unfortunately.
# We need (country, code) rows, with each series as a column.
# There are 26 different factors in Series.Code.
# There are 217 countries * 58 years in the dataset.
new.wdi = data.frame(matrix(ncol = 3 + 26, nrow = 217 * 58))
column_names = c('country', 'code', 'year', levels(factor(original.wdi$Series.Code)))
colnames(new.wdi) = column_names

# Fill in country year pairs.
all_countries = unique(original.wdi$Country.Name)
ctr = 1
for (country in all_countries) {
  for (year in 1960:2017) {
    new.wdi[ctr, 'country'] = as.character(country)
    new.wdi[ctr, 'year'] = year
    ctr = ctr + 1
  }
}

# Fill in the rest of the data. This is really slow and unoptimized.
for (rowIdx in 1:dim(new.wdi)[1]) {
  print(rowIdx)
  for (colIdx in 4:29) {
    seriesCode = colnames(new.wdi)[colIdx]
    country = new.wdi[rowIdx, 'country']
    year = new.wdi[rowIdx, 'year']
    yearColIdx = year - 1960 + 5
    new.wdi[rowIdx, colIdx] = as.numeric(as.character(original.wdi[original.wdi$Country.Name == country & original.wdi$Series.Code == seriesCode, yearColIdx]))
    # print(as.numeric(as.character(original.wdi[original.wdi$Country.Name == country & original.wdi$Series.Code == seriesCode, yearColIdx])))
  }
}

as.numeric(original.wdi[original.wdi$Country.Name == country & original.wdi$Series.Code == seriesCode, yearColIdx])

# Finally, map country names to abbreviations.
country_codes = data.frame(matrix(ncol = 2, nrow = length(all_countries)))
country_codes[, 1] = all_countries
country_codes[, 2] = as.character(unique(original.wdi$Country.Code))
colnames(country_codes) = c('country', 'code')
write.csv(country_codes, file.path(PROCESSED_DATA_DIR, 'country_codes.csv'))
for (rowIdx in 1:dim(new.wdi)[1]) {
  new.wdi[rowIdx, 'code'] = country_codes[country_codes$country == new.wdi[rowIdx, 'country'], 'code']
}

# Save to disk.
write.csv(new.wdi, file.path(PROCESSED_DATA_DIR, 'wdi.csv'))

### DATA MERGING ###
data.wdi = read.csv(file.path(PROCESSED_DATA_DIR, 'wdi.csv'))
data.wgi = read.csv(file.path(PROCESSED_DATA_DIR, 'wgi.csv'))
data.terr = read.csv(file.path(PROCESSED_DATA_DIR, 'terrorism_incidents.csv'))

# Create the full dataset where ALL rows are kept during merge.
data.all = merge(data.wdi, data.wgi, by=c('country', 'year', 'code'), all.x = TRUE, all.y = TRUE)
data.all = merge(data.all, data.terr, by=c('country', 'year', 'code'), all.x = TRUE, all.y = TRUE)
data.all = subset(data.all, select=-c(X.x, X, X.y))

convert_numeric_columns = colnames(data.all)[30:35]
write.csv(data.all, file.path(FINAL_DATA_DIR, 'data_full.csv'))

# Create a matched dataset where WDI and WGI must have correspond.
# Terrorism data is filled in where available.
data.matched_wdi_wgi = merge(data.wdi, data.wgi, by=c('country', 'year', 'code'))
data.matched_wdi_wgi = merge(data.matched_wdi_wgi, data.terr, by=c('country', 'year', 'code'), all.x = TRUE)
data.matched_wdi_wgi = subset(data.matched_wdi_wgi, select=-c(X.x, X, X.y))
write.csv(data.matched_wdi_wgi, file.path(FINAL_DATA_DIR, 'data_matched_wdi_wgi.csv'))

# Create fully matched dataset.
data.matched_all = merge(data.wdi, data.wgi, by=c('country', 'year', 'code'))
data.matched_all = merge(data.matched_all, data.terr, by=c('country', 'year', 'code'))
data.matched_all = subset(data.matched_all, select=-c(X.x, X, X.y))
for (cname in convert_numeric_columns) {
  print(cname)
  data.matched_all[,cname] = as.numeric(as.character(data.matched_all[,cname]))
}

# Remove rows with #NA for WGI.
data.matched_all = data.matched_all[complete.cases(data.matched_all),]
write.csv(data.matched_all, file.path(FINAL_DATA_DIR, 'data_matched_all.csv'))
