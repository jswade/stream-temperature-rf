# Format PRISM AT files into one CSV file

library(lubridate)


## CHANGE FILE DIRECTORY
# Read in Raw PRISM csv files
AT_17 <- read.csv("/stream-temperature-rf/data_download/AirTemperature/PRISM_raw/PRISM_tmean_stable_4km_20161001_20170930.csv")
AT_18 <- read.csv("/stream-temperature-rf/data_download/AirTemperature/PRISM_raw/PRISM_tmean_stable_4km_20171001_20180930.csv")
AT_19 <- read.csv("/stream-temperature-rf/data_download/AirTemperature/PRISM_raw/PRISM_tmean_stable_4km_20181001_20190930.csv")
AT_20 <- read.csv("/stream-temperature-rf/data_download/AirTemperature/PRISM_raw/PRISM_tmean_stable_4km_20191001_20200930.csv")

# Convert temperatures to celcius
AT_17$tmean_C <- (5/9)*(AT_17$tmean_F-32)
AT_18$tmean_C <- (5/9)*(AT_18$tmean_F-32)
AT_19$tmean_C <- (5/9)*(AT_19$tmean_F-32)
AT_20$tmean_C <- (5/9)*(AT_20$tmean_F-32)

# Get list of site names
sites <- unique(AT_17$Name)

# Remove blank value
sites <- sites[-2]

# Create sequence of dates
dates <- seq(from=ymd('2016-10-01'),to=ymd('2020-09-30'),by='days')

# Create dataframe to store air temperatures
at <- data.frame(Date = dates, MonYr = paste(year(dates),month(dates),sep=""))

# Add columns for each site
at <- cbind(at, matrix(nrow=1461,ncol=length(sites)))

# Rename columns by site name
names(at)[3:412] <- sites


# Loop through sites, storing values in at
for (i in seq(1,length(sites))){

  # Index each year's CSV file, compiling data from each site
  tempdata <- c(AT_17$tmean_C[which(AT_17$Name == sites[i])], 
               AT_18$tmean_C[which(AT_18$Name == sites[i])],
               AT_19$tmean_C[which(AT_19$Name == sites[i])],
               AT_20$tmean_C[which(AT_20$Name == sites[i])])
  
  # Store array in at
  at[(i+2)] <- tempdata
  
}


## CHANGE FILE DIRECTORY
# Write to csv
write.csv(at,'/stream-temperature-rf/data_download/AirTemperature/AirTemperature/PRISM_tmean_2016_2020.csv',row.names=FALSE)
