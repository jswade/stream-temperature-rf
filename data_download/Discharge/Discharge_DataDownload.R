# Script for downloading stream temperatures for GAGES sites with data from 2016-2020

library(lubridate)
library(dataRetrieval)
library(zoo)

## CHANGE FILE DIRECTORY
# Read in site list (sites with any data during 2016-2020)
sites <- read.csv("/stream-temperature-rf/data_download/StreamTemperature/sitelist_2016_2020.csv")

# Remove apostrophe from sites
sites$site <- sub('.','',sites$site)

# Set date range
startDate <- "2016-10-01"
endDate <- "2020-09-30"

# Create dataframe to store dishcarge
disch.df <- data.frame(Date = seq(from=ymd(startDate),to=ymd(endDate),by='days'))
disch.df$MonYr <- paste(year(disch.df$Date),month(disch.df$Date),sep="")

# Add columns for each site
disch.df <- cbind(disch.df,matrix(0,nrow=1461,ncol=410))

parameterCd <- "00060" # Discharge
statCd <- "00003" # Daily Mean

# Rename site columns
names(disch.df)[c(-1,-2)] <- sites$ID

# Create dummy zoo time series of dates for alignment
date.df <- disch.df[,c(1,3)]
date.zoo <- zoo(date.df[,-1],order.by = date.df[,1])


# Download data from each site from USGS NWIS
for (i in seq(1,length(sites$site))){
  
  # Index site number
  siteNumber <- sites$site[i]
  
  # Download data from site
  dischdata <- readNWISdv(siteNumber, parameterCd, startDate, endDate, statCd = statCd)
  
  # Check if tempdata is empty
  if (dim(dischdata)[1] != 0){
    
    # Conver to zoo format
    dischdata.zoo <- zoo(dischdata[,4], order.by = dischdata[,3])
    
    # Merge with dummy zoo time series to align dates
    dischmerge <- merge(date.zoo, dischdata.zoo, all= c(TRUE,TRUE), fill = NA)
    
    # Store data in disc.df
    disch.df[,(i+2)] <- dischmerge$dischdata.zoo
    
  }
}


## CHANGE FILE DIRECTORY
# Write daily discharge values to file
write.csv(disch.df, '/stream-temperature-rf/data_download/Discharge/Discharge_2016_2020.csv',row.names=FALSE)


## Calculate monthly medians of flow
# Reformat dates
disch.df$Date <- as.Date(disch.df$Date, format="%m/%d/%Y")

# Add month and year columns
disch.df$month <- month(disch.df$Date)
disch.df$yr <- year(disch.df$Date)

# Aggregate discharge by month
disch.month <- aggregate(disch.df[,c(-1,-2,-413,-414)],by=list(disch.df$month),FUN=median,na.rm=TRUE)
disch.month$Group.1 <- c("JanQ","FebQ","MarQ", "AprQ", "MayQ", "JunQ", "JulQ","AugQ","SeptQ", "OctQ", "NovQ", "DecQ")
names(disch.month)[1] <- "Month"

# Transpose dataframe
disch.month <- as.data.frame(t(disch.month))
names(disch.month) <- disch.month[1,]
disch.month <- disch.month[-1,]
disch.month <- cbind(rownames(disch.month), disch.month)
names(disch.month)[1] <- "Site"
rownames(disch.month) <- NULL

## CHANGE FILE DIRECTORY
# Write to csv
write.csv(disch.month,'/stream-temperature-rf/data_download/Discharge/MedianQMonth_2016_2020.csv',row.names=FALSE)
