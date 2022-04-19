# Script for downloading stream temperatures for GAGES sites with data from 2016-2020

library(lubridate)
library(dataRetrieval)
library(zoo)

## CHANGE FILE DIRECTORY
# Read in site list (sites with any data during 2016-2020)
sites <- read.csv("/stream-temperature-rf/site_id/AllSites_with_2016_2020_data.csv")

# Remove apostrophe from sites
sites$site <- sub('.','',sites$site)

# Set date range from WY 2016 to WY 2020
startDate <- "2016-10-01"
endDate <- "2020-09-30"

# Set other USGS data retrieval parameters
parameterCd <- "00010" # Temperature
statCd <- "00003" # Daily Mean Values

# Create dataframe to store stream temperatures
temp.df <- data.frame(Date = seq(from=ymd(startDate),to=ymd(endDate),by='days'))
temp.df$MonYr <- paste(year(temp.df$Date),month(temp.df$Date),sep="")

# Add columns for each site
temp.df <- cbind(temp.df,matrix(0,nrow=1461,ncol=dim(sites)[1]))

# Rename site columns
names(temp.df)[c(-1,-2)] <- sites$site

# Create dummy zoo time series of dates for alignment
date.df <- temp.df[,c(1,3)]
date.zoo <- zoo(date.df[,-1],order.by = date.df[,1])


# Download data from each site
for (i in seq(1,length(sites$site))){
  
  # Index site number
  siteNumber <- sites$site[i]

  # Download data from site
  tempdata <- readNWISdv(siteNumber, parameterCd, startDate, endDate, statCd = statCd)

  # Check if tempdata is empty
  if (dim(tempdata)[1] != 0){
  
    # Conver to zoo format
    tempdata.zoo <- zoo(tempdata[,4], order.by = tempdata[,3])
    
    # Merge with dummy zoo time series to align dates
    tempmerge <- merge(date.zoo, tempdata.zoo, all= c(TRUE,TRUE), fill = NA)
    
    # Store data in temp.df
    temp.df[,(i+2)] <- tempmerge$tempdata.zoo
  
  }
}

# Filter out sites that have temp = 0 for all observations
tempsum <- colSums(temp.df[,c(-1,-2)],na.rm=TRUE)
siteswithouttemp <- which(tempsum == 0)
sitenameswithouttemp <- names(siteswithouttemp)
 
# Subset disch.month by sites with complete data
temp.df <- temp.df[,-which(names(temp.df) %in% sitenameswithouttemp)]

# Find number of missing values at each site
na_count <- sapply(temp.df[,c(-1,-2)], function(y) sum(length(which(is.na(y)))))
 
# Calculate percentage of missing values (x/1461)
na_percent <- (na_count / 1461) * 100

# Sites with less than 10% of data missing
siteswithdata <- na_percent[which(na_percent < 10)]

# Get names of sites with complete (>90%) data
site_names <- names(siteswithdata)

# Subset temp.df using site_names
stmean <- temp.df[,c(1,2,match(site_names, names(temp.df)))]

# # Subset site information for sites with data
siteinfo <- data.frame(site=site_names)
siteinfo$site <- paste("'",siteinfo$site,sep="")

# Replace stream temperatures below 0 with 0 C.
# Loop through all sites
for (i in seq(3,412)){
  
  # Replace values less than 0 with 0
  stmean[which(stmean[,i] < 0),i] <- 0
  
}

## As the site query was performed in 2020, it returned a slightly different set of sites than it does presently.
## This code will utilize sites the original 2020 query. Rerun the code write new sites to file (not performed in the paper).

## CHANGE FILE DIRECTORY
# Write site names to csv
# write.csv(siteinfo, '/stream-temperature-rf/data_download/StreamTemperature/sitelist_2016_2020.csv',row.names=FALSE)

## CHANGE FILE DIRECTORY
# Write stream temperatures to csv
# write.csv(temp.df,'/stream-temperature-rf/data_download/StreamTemperature/StreamTemperature/ST_2016_2020.csv',row.names=FALSE)






