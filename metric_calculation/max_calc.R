# Calculate maximum monthly stream temperatures from each site.

library(stringi)
library(lubridate)
library(dplyr)

## CHANGE FILE DIRECTORY
# Read in stream temperature daily mean values
stmean <- read.csv('/stream-temperature-rf/data_download/StreamTemperature/ST_2016_2020.csv')

# Convert date column to date
stmean$Date <- as.Date(stmean$Date,format = "%m/%d/%Y")

# Fix stmean MonYr values
stmean$MonYr <- as.character(stmean$MonYr)

# Insert missing 0 values for months
for (i in seq(1,length(stmean$MonYr))){
  if (nchar(stmean$MonYr[i]) == 5) {
    stri_sub(stmean$MonYr[i],5,4) <-  0
  }
}

# Convert MonYr values to integer
stmean$MonYr <- as.integer(stmean$MonYr)


#### Calculate monthly maximum water temperatures ####

# Calculate monthly means of stream temperature
stmax <- stmean %>% group_by(MonYr) %>% summarize_all(max, na.rm = TRUE)

# Replace Inf or -Inf with NA
stmax <- do.call(data.frame, 
                lapply(stmax,function(x) replace(x, is.infinite(x), NA)))

# Remove Date
stmax <- stmax[,c(-2)]

# Convert to Dataframe
stmax <- as.data.frame(t(stmax))
colnames(stmax) <- paste("M",stmax[1,],sep="")
stmax <- stmax[-1,]

# Move site names from rownames to column
stmax$Site <- colnames(stmean)[3:412]
stmax <- stmax[,c(49,1:48)]
rownames(stmax) <- NULL

# Aggregate 4 years of monthly means into mean monthly values
stmax_allmon <- data.frame(Site = stmax$Site, 
                           Oct_max = rep(0,410), Nov_max = rep(0,410), Dec_max = rep(0,410),
                           Jan_max = rep(0,410), Feb_max = rep(0,410), Mar_max = rep(0,410),
                           Apr_max = rep(0,410), May_max = rep(0,410), Jun_max = rep(0,410),
                           Jul_max = rep(0,410), Aug_max = rep(0,410), Sep_max = rep(0,410))


# loop through all months, calculating mean maximum st for each month across 4 years
for (i in seq(2,13)){
  
  # Index columns for each month (i=3 is first october, by 12)
  stmon <- stmax[,(seq(i,49,by=12))]
  
  # Calculate mean for each month and assign to dataframe
  stmax_allmon[,i] <- rowMeans(stmon, na.rm = TRUE)
  
}

# Rearrange months to be ordered 1-12
stmax_allmon <- stmax_allmon[,c(1,5:13,2:4)]


## CHANGE FILE DIRECTORY
# Write to csv
write.csv(stmax_allmon,'/stream-temperature-rf/metric_calculation/STmax.csv',row.names=FALSE)

