# Calculate Spearman Rank between metrics and gages data

#### Data Import ####

# NO DAM SUBSET

## CHANGE FILE DIRECTORIES
# Read in metrics
slope_metric <- read.csv("/stream-temperature-rf/data_raw/thermalsensitivity.csv")
max_metric <- read.csv("/stream-temperature-rf/data_raw/STmax.csv")

# Read in Full Site data
site_data = read.csv("/stream-temperature-rf/data_raw/gagesII_RF_2016_2020_subset.csv")

# Read in discharge data
disch.month = read.csv("/stream-temperature-rf/data_raw/MedianQMonth_2016_2020.csv")

# Read in dam info
damref <- read.csv("/stream-temperature-rf/data_raw/RF_2016_2020_Dams.csv")

# Establish site indices with major dams (distance to maj dam != 0)
dam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM != -999)

# Establish site indices with no major dams (distance to maj dam = -999)
nodam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM == -999)


## Subset data by dam_ind
site_data <- site_data[nodam_ind,]
disch.month <- disch.month[nodam_ind,]
slope_metric <- slope_metric[nodam_ind,]
max_metric <- max_metric[nodam_ind,]


#### end ####


#### Assemble Data frames for each month ####

jan_data = data.frame(site_data[,c(2:6,7,19,31:45)],JAN_Q = disch.month[,2])
feb_data = data.frame(site_data[,c(2:6,8,20,31:45)],FEB_Q = disch.month[,3])
mar_data = data.frame(site_data[,c(2:6,9,21,31:45)],MAR_Q = disch.month[,4])
apr_data = data.frame(site_data[,c(2:6,10,22,31:45)],APR_Q = disch.month[,5])
may_data = data.frame(site_data[,c(2:6,11,23,31:45)],MAY_Q = disch.month[,6])
jun_data = data.frame(site_data[,c(2:6,12,24,31:45)],JUN_Q = disch.month[,7])
jul_data = data.frame(site_data[,c(2:6,13,25,31:45)],JUL_Q = disch.month[,8])
aug_data = data.frame(site_data[,c(2:6,14,26,31:45)],AUG_Q = disch.month[,9])
sep_data = data.frame(site_data[,c(2:6,15,27,31:45)],SEP_Q = disch.month[,10])
oct_data = data.frame(site_data[,c(2:6,16,28,31:45)],OCT_Q = disch.month[,11])
nov_data = data.frame(site_data[,c(2:6,17,29,31:45)],NOV_Q = disch.month[,12])
dec_data = data.frame(site_data[,c(2:6,18,30,31:45)],DEC_Q = disch.month[,13])


#### MaxST Spearman Rank ####

# Create dataframe to store spearman ranks
maxst_df = data.frame(matrix(ncol=13,nrow=23))

# Rename columns
names(maxst_df) = c("Variable","Jan_Max","Feb_Max","Mar_Max","Apr_Max","May_Max","Jun_Max","Jul_Max","Aug_Max","Sep_Max","Oct_Max","Nov_Max","Dec_Max")

# Store variable names
maxst_df$Variable = names(jan_data)
maxst_df$Variable[6] = "MON_PPT7100_CM"
maxst_df$Variable[7] = "MON_TMP7100_DEGC"
maxst_df$Variable[23] = "MonthlyQ"

# Calculate and store Spearman Ranks
maxst_df$Jan_Max = cor(jan_data,max_metric$Jan_max, method="spearman")
maxst_df$Feb_Max = cor(feb_data,max_metric$Feb_max, method="spearman")
maxst_df$Mar_Max = cor(mar_data,max_metric$Mar_max, method="spearman")
maxst_df$Apr_Max = cor(apr_data,max_metric$Apr_max, method="spearman")
maxst_df$May_Max = cor(may_data,max_metric$May_max, method="spearman")
maxst_df$Jun_Max = cor(jun_data,max_metric$Jun_max, method="spearman")
maxst_df$Jul_Max = cor(jul_data,max_metric$Jul_max, method="spearman")
maxst_df$Aug_Max = cor(aug_data,max_metric$Aug_max, method="spearman")
maxst_df$Sep_Max = cor(sep_data,max_metric$Sep_max, method="spearman")
maxst_df$Oct_Max = cor(oct_data,max_metric$Oct_max, method="spearman")
maxst_df$Nov_Max = cor(nov_data,max_metric$Nov_max, method="spearman")
maxst_df$Dec_Max = cor(dec_data,max_metric$Dec_max, method="spearman")

#### end ####


#### Slope Spearman Rank ####

# Create dataframe to store spearman ranks
slope_df = data.frame(matrix(ncol=13,nrow=23))

# Rename columns
names(slope_df) = c("Variable","JanSlope","FebSlope","MarSlope","AprSlope",
                    "MaySlope","JunSlope","JulSlope","AugSlope","SepSlope","OctSlope","NovSlope","DecSlope")

# Store variable names
slope_df$Variable = names(jan_data)
slope_df$Variable[6] = "MON_PPT7100_CM"
slope_df$Variable[7] = "MON_TMP7100_DEGC"
slope_df$Variable[23] = "MonthlyQ"


# Calculate and store Spearman Ranks
slope_df$JanSlope = cor(jan_data,slope_metric$JanTS, method="spearman")
slope_df$FebSlope = cor(feb_data,slope_metric$FebTS, method="spearman")
slope_df$MarSlope = cor(mar_data,slope_metric$MarTS, method="spearman")
slope_df$AprSlope = cor(apr_data,slope_metric$AprTS, method="spearman")
slope_df$MaySlope = cor(may_data,slope_metric$MayTS, method="spearman")
slope_df$JunSlope = cor(jun_data,slope_metric$JunTS, method="spearman")
slope_df$JulSlope = cor(jul_data,slope_metric$JulTS, method="spearman")
slope_df$AugSlope = cor(aug_data,slope_metric$AugTS, method="spearman")
slope_df$SepSlope = cor(sep_data,slope_metric$SepTS, method="spearman")
slope_df$OctSlope = cor(oct_data,slope_metric$OctTS, method="spearman")
slope_df$NovSlope = cor(nov_data,slope_metric$NovTS, method="spearman")
slope_df$DecSlope = cor(dec_data,slope_metric$DecTS, method="spearman")

#### end ####



#### end ####

## CHANGE FILE DIRECTORIES

# # Write files to csv
write.csv(maxst_df,"/stream-temperature-rf/data_results/SpearmanRank/Dam_Subset/max_nodam_spearman.csv",row.names=FALSE)
write.csv(slope_df,"/stream-temperature-rf/data_results/SpearmanRank/Dam_Subset/slope_nodam_spearman.csv",row.names=FALSE)
