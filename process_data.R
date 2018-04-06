# This script primarily puts all of the datasets into (country, year) row ordering.

# Get XLSX package.
# install.packages('openxlsx') # Uncomment this to install the package.
library('openxlsx')

PROCESSED_DATA_DIR = './data/processed/'
ORIGINAL_DATA_DIR = './data/original/'
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
    new.wdi[rowIdx, colIdx] = original.wdi[original.wdi$Country.Name == country & original.wdi$Series.Code == seriesCode, yearColIdx]
  }
}
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



