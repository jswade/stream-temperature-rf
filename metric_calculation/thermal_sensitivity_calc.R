# Calculate monthly linear slope between daily air and water temperatures (thermal sensitivity)

library(stringi)
library(lme4)
library(lubridate)

## CHANGE FILE DIRECTORY
# Read in air temperature data
atmean <- read.csv('/stream-temperature-rf/data_download/AirTemperature/PRISM_tmean_2016_2020.csv')

## CHANGE FILE DIRECTORY
# Read in water temperature data
stmean <- read.csv('/stream-temperature-rf/data_download/StreamTemperature/ST_2016_2020.csv')

# Fix MonYr values so they are in order
# If monyr is 5 characters instead of 6
for (i in seq(1,length(atmean$MonYr))){
  if (nchar(atmean$MonYr[i])<6) {
    # Add zero to month
    stri_sub(atmean$MonYr[i],5,4) <- "0"
    stri_sub(stmean$MonYr[i],5,4) <- "0"
  }
}

# Convert MonYr back to numeric
atmean$MonYr <- as.numeric(atmean$MonYr)
stmean$MonYr <- as.numeric(stmean$MonYr)


#### Calculate monthly slopes: Mean ####

# Get Site numbers from colnames
sitenm <- colnames(atmean)[3:412]

# Create dataframe to store monthly slopes
mean_monslope <- as.data.frame(matrix(0,ncol=49,nrow=410))
mean_monslope[,1] <- colnames(atmean)[3:412]
colnames(mean_monslope) <- c("Site",unique(atmean$MonYr))

# Create dataframe to store monthly intercepts
mean_monint <- as.data.frame(matrix(0,ncol=49,nrow=410))
mean_monint[,1] <- colnames(atmean)[3:412]
colnames(mean_monint) <- c("Site",unique(atmean$MonYr))

# Create dataframe to store monthly R2
mean_monR2 <- as.data.frame(matrix(0,ncol=49,nrow=410))
mean_monR2[,1] <- colnames(atmean)[3:412]
colnames(mean_monR2) <- c("Site",unique(atmean$MonYr))

# Calculate mean temperature air-water monthly slopes at each site
for (i in seq(3,dim(atmean)[2])){
  
  # Create temporary dataframe to store AT and WT columns
  tempdf <- data.frame(MonYr = atmean$MonYr,atmean = atmean[,i], stmean = stmean[,i])
  names(tempdf) <- c('MonYr','atmean','stmean')
  
  # Calculate monthly slopes
  monslope <- lmList(stmean~atmean | MonYr, data=tempdf, pool=FALSE)
  
  # Convert monthly slope coefficients to dataframe
  monslopedf <- as.data.frame(coef(monslope))
  
  
  # Assign slope/intercept/R2 to columns
  mean_monslope[(i-2),(2:49)] <- monslopedf$atmean
  mean_monint[(i-2),(2:49)] <- monslopedf$`(Intercept)`
  
  
  
  # Store R2 values individually to avoid Null values
  for (j in seq(1, 48)){
    
    # This will equal NULL if R2 is NA
    if (class(monslope[[j]]) == "lm"){
      
      mean_monR2[(i-2),(j+1)] <- summary(monslope[[j]])$r.squared
      
    } else{
      
      # Replace Null with NA
      mean_monR2[(i-2),(j+1)] <- NA
      
    }
  }
  
  # Replace NaN R2 (caused by freezing temperatures) with NA
  mean_monR2[which(is.na(mean_monR2[(i-2),])),(i-2)] <- NA
  
}

# Rename columns as months and remove month row
colnames(mean_monslope)[-1] <- paste("M",colnames(mean_monslope)[-1],sep="")
colnames(mean_monint)[-1] <- paste("M",colnames(mean_monint)[-1],sep="")
colnames(mean_monR2)[-1] <- paste("M",colnames(mean_monR2)[-1],sep="")


# Calculate mean and sd of values at each site over all months
mean_monslope_agg <- data.frame(Site = mean_monslope$Site, 
                               OctTS = rep(0,410), NovTS = rep(0,410), DecTS = rep(0,410),
                               JanTS = rep(0,410), FebTS = rep(0,410), MarTS = rep(0,410),
                               AprTS = rep(0,410), MayTS = rep(0,410), JunTS = rep(0,410),
                               JulTS = rep(0,410), AugTS = rep(0,410), SepTS = rep(0,410))

sd_monslope_agg <- data.frame(Site = mean_monslope$Site,  
                             OctSD = rep(0,410), NovSD = rep(0,410), DecSD = rep(0,410),
                             JanSD = rep(0,410), FebSD = rep(0,410), MarSD = rep(0,410),
                             AprSD = rep(0,410), MaySD = rep(0,410), JunSD = rep(0,410),
                             JulSD = rep(0,410), AugSD = rep(0,410), SepSD = rep(0,410))

# Calculate mean and sd values for months in loop
for (i in seq(2,13)){

  # Index columns for each month (i=2 is first october, by 12)
  tempmon <- mean_monslope[,(seq(i,49,by=12))]
  
  # Calculate mean and SD and assign to dataframes
  mean_monslope_agg[,i] <- rowMeans(tempmon, na.rm = TRUE)
  sd_monslope_agg[,i] <- apply(tempmon, 1, sd, na.rm = TRUE)
  
}

# Calculate mean R2 at each site over all months
monR2_mean <- data.frame(Site = mean_monslope$Site,
                        OctR2 = rep(0,410), NovR2 = rep(0,410), DecR2 = rep(0,410),
                        JanR2 = rep(0,410), FebR2 = rep(0,410), MarR2 = rep(0,410),
                        AprR2 = rep(0,410), MayR2 = rep(0,410), JunR2 = rep(0,410),
                        JulR2 = rep(0,410), AugR2 = rep(0,410), SepR2 = rep(0,410))


# Calculate R2 summary values for months in loop
# Some sites (109md) with WT = 0 for winter months will have R2 = Inf
for (i in seq(2,13)){
  
  # Index R2 columns for each month (i=2 is first october, by 12)
  monR2 <- mean_monR2[,(seq(i,49,by=12))]
  
  # Calculate mean and SD and assign to dataframes
  monR2_mean[,i] <- rowMeans(monR2, na.rm = TRUE)
  
}

# Rearrange months to be ordered 1-12
mean_monslope_agg <- mean_monslope_agg[,c(1,5:13,2:4)]


## CHANGE FILE DIRECTORY
# Write to csv
write.csv(mean_monslope_agg, "/stream-temperature-rf/metric_calculation/thermalsensitivity.csv",row.names=FALSE)

#### end ####

