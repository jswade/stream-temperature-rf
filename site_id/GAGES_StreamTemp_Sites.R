# Identify sites in GAGES-II dataset meeting data availability requirements

library(dataRetrieval)
library(readr)
library(lubridate)


## CHANGE FILE DIRECTORY
# Read in gages site dat
gages <- read_csv("/stream-temperature-rf/site_id/gagesII_2011_conterm_all.csv", 
                 col_types = cols(STAID = col_character()))

# Extract Gages sites
gages_sites <- as.character(gages$STAID)

# Create dataframe to store site numbers and start and end date for stream temperatures
site.df <-  data.frame(site = gages_sites, begin_date = rep(as.Date(0,origin="1970-01-01"),length(gages_sites)), 
                     end_date = rep(as.Date(0,origin="1970-01-01"),length(gages_sites)), n_days = rep(0,length(gages_sites)))


# Loop through GAGESII sites (this loop takes a significant amount of time to run)
for (i in seq(1,length(gages_sites))){

  # Retrieve what NWIS data is available from site i
  # Catch errors caused by site missing from USGS NWIS database
  dailyData <- tryCatch(whatNWISdata(siteNumber = gages_sites[i],service="dv", statCd="00003"),error = function(e) e)
                       
  # If no error occurs, run through normal code
  if (class(dailyData) == "data.frame") {
  
    # Check if stream temperature data (parm_cd = '00010') exists 
    # If x = 1, data exists
    # If x = 0, data does not exist
    x <- length(which(dailyData$parm_cd == "00010"))
    
    # If Stream temperature data exists
    if (x == 1) {
      
      # Extract dates where stream temperature data exists
      site.df$begin_date[i] <- as.Date(dailyData[which(dailyData$parm_cd == "00010"),]$begin_date,format="Y-%m-%d")
      site.df$end_date[i] <- as.Date(dailyData[which(dailyData$parm_cd == "00010"),]$end_date,format="Y-%m-%d")
      site.df$n_days[i] <- dailyData[which(dailyData$parm_cd == "00010"),]$count_nu
      
    } else { # If Stream temperature data doesn't exist
      
      site.df$begin_date[i] <- NA
      site.df$end_date[i] <- NA
      site.df$n_days[i] <- NA
      
    } 

  } else { # If error occurs, set row to NA
    
    site.df$begin_date[i] <- NA
    site.df$end_date[i] <- NA
    site.df$n_days[i] <- NA
    
  }
}


# Filter sites with temperature data
temp_sites <- site.df[which(is.na(site.df$begin_date) == FALSE),]


#### 4 Year Intervals: Find 4 year interval with most temperature data between 1980-2020 ####

# Create dataframe for identifying years with the most complete data
tempmat <- matrix(nrow=dim(temp_sites)[1],ncol=37)
date4yr.df <- cbind(temp_sites,tempmat)
names(date4yr.df)[5:41] <- paste("Y",seq(1980,2016),"-",seq(1984,2020),sep="")

# Create list of intervals for start and end dates of years 1980-2020
int.list4yr <- list()

for (i in seq(1980,2016)){
  
  # Define start and end dates
  d1 <- as.Date(paste(i,"-01-01",sep=""),format="%Y-%m-%d")
  d2 <- as.Date(paste((i+4),"-01-01",sep=""),format="%Y-%m-%d")
  
  # Add interval to lsit
  int.list4yr[[(i-1979)]] <- interval(d1,d2)
  
}


# Loop through sites to identify years with most temperature data
for (i in seq(1, length(temp_sites$site))){
  
  # Define start and end date interval
  int <- interval(date4yr.df$begin_date[i], date4yr.df$end_date[i])
  
  # Loop through interval list
  for (j in seq(1,length(int.list4yr))){
    
    # Check if interval is within each year's interval
    if (int.list4yr[[j]] %within% int == TRUE) {
      date4yr.df[i,(j+4)] <- 1 # Set year column to 1 if date is contained in range
    } else {
      date4yr.df[i,(j+4)] <- 0 # Set year column to 0 if date is contained in range
    }
  }
}

# Sum 4 year intervals to find years with most sites having data
data_4yr <- colSums(date4yr.df[,c(5:41)])

# Y2016-2020 is the maximum: Pull out sites with data from 2016-2020
sites_16.20 <- date4yr.df[which(date4yr.df$`Y2016-2020` == 1),c(1:4)]

# Add apostrophe to site numbers to preserve leading 0 in Excel
sites_16.20$site <- paste("'",sites_16.20$site,sep="")

# As the site query was performed in 2020, it returned a slightly different set of sites than it does presently.
# The following code will utilize the original 2020 query. Rerun the code write new sites to file.

## CHANGE FILE DIRECTORY
# Write sites to csv
#write.csv(sites_16.20, "/stream-temperature-rf/site_id/AllSites_with_2016_2020_data.csv",row.names=FALSE)

#### end ####