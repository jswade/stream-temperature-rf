# Create function to visualize random forest results and generate figures

library(ggplot2)
library(reshape)
library(dplyr)
library(RColorBrewer)
library(tidytext)


# Create function for data visualization of RF runs
# metric = string of either ('Max' or 'Slope'
# subset = string of ('All', 'Dam', 'NoDam','HUC0102','HUC03','HUC17')

rf_visfunc <- function(metric,subset){

  # Create array of month strings for saving to file
  mon_list <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  
  ## Generate filepath given metric input
  if (metric == "Max"){
  
    # Metric string
    metric_str <- 'MaxST'
    
    # File string
    file_str <- 'max'
    
    
  } else if (metric == "Slope"){
    
    # Metric string
    metric_str <- 'Slope'
    
    # File string
    file_str <- 'slope'
    
  } else {
    
    # Return error message if incorrect input is supplied
    stop("Metric string not valid. Please use Max or Slope.")
    
  }
  
  ## Generate filepath based on subset input
  if (subset == "All"){
    
    # Top level folder
    top_fold <- 'AllSites'
    
    # Sub folder
    sub_fold <- '_all'
    
    
  } else if (subset == "Dam"){
    
    # Top level folder
    top_fold <- 'Dam_Subset'
    
    # Sub folder
    sub_fold <- '_dam'
    
    
  }else if (subset == "NoDam"){
    
    # Top level folder
    top_fold <- 'Dam_Subset'
    
    # Sub folder
    sub_fold <- '_nodam'
    
    
  }else if (subset == "HUC0102"){
    
    # Top level folder
    top_fold <- 'HUC2_Subset'
    
    # Sub folder
    sub_fold <- '_huc0102'
    
    
  }else if (subset == "HUC03"){
    
    # Top level folder
    top_fold <- 'HUC2_Subset'
    
    # Sub folder
    sub_fold <- '_huc03'
    
  }else if (subset == "HUC17"){
    
    # Top level folder
    top_fold <- 'HUC2_Subset'
    
    # Sub folder
    sub_fold <- '_huc17'
    
  }else{
    
    stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
    
  } 
  
  ## CHANGE FILE DIRECTORIES

  ## Read in importance dataframes using function input strings
  janimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[1],file_str,sub_fold,'.csv',sep=""))
  febimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[2],file_str,sub_fold,'.csv',sep=""))
  marimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[3],file_str,sub_fold,'.csv',sep=""))
  aprimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[4],file_str,sub_fold,'.csv',sep=""))
  mayimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[5],file_str,sub_fold,'.csv',sep=""))
  junimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[6],file_str,sub_fold,'.csv',sep=""))
  julimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[7],file_str,sub_fold,'.csv',sep=""))
  augimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[8],file_str,sub_fold,'.csv',sep=""))
  sepimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[9],file_str,sub_fold,'.csv',sep=""))
  octimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[10],file_str,sub_fold,'.csv',sep=""))
  novimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[11],file_str,sub_fold,'.csv',sep=""))
  decimp <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",mon_list[12],file_str,sub_fold,'.csv',sep=""))


  # Add dfs to a list
  ST_list = list(janimp,febimp,marimp,aprimp,mayimp,junimp,julimp,augimp,sepimp,octimp,novimp,decimp)

  # Calculate total MDA for each month
  total_mda = rep(0,12)

  for (i in seq(1,12)){

    # Calculate total MDA
    total_mda[i] = sum(ST_list[[i]]$MDA)

    # Calculate percent of total MDA for each dataframe
    ST_list[[i]]$Per_MDA = 100*ST_list[[i]]$MDA/total_mda[i]

  }

  ## Aggregate variables into 1 of 4 categories

  # 1. DRAIN_SQKM =	Hydrology
  # 2. PPTAVG_SITE	=	Climate
  # 3. RH_SITE	=	Climate
  # 4. SNOW_PCT_PRECIP	=	Hydrology
  # 5. PRECIP_SEAS_IND = Climate
  # 6. AUG_PPT7100_CM	=	Climate
  # 7. AUG_TMP7100_DEGC	=	Climate
  # 8. BFI_AVE	=	Hydrology
  # 9. STOR_NID_2009	= Human Impacts
  # 10. FORESTNLCD06 =	Watershed
  # 11. PLANTNLCD06	=	Human Impacts
  # 12. WETLANDSNLCD06 = Hydrology
  # 13. MAINS100_FOREST	=	Watershed
  # 14. IMPNLCD06	=	Human Impacts
  # 15. AWCAVE	=	Hydrology
  # 16. PERMAVE	=	Watershed
  # 17. WTDEPAVE	= Hydrology
  # 18. ROCKDEPAVE	=	Watershed
  # 19. ELEV_MEAN_M_BASIN	=	Watershed
  # 20. SLOPE_PCT	=	Watershed
  # 21. ASPECT_NORTHNESS	=	Watershed
  # 22. ASPECT_EASTNESS	=	Watershed
  # 23. MON_Q = Hydrology

  # Assign variable groups and months to each monthly dataframe
  for (i in seq(1,12)){

    # Assign group ID
    ST_list[[i]]$Group = c("Hydrology","Climate","Climate","Hydrology","Climate","Climate","Climate",
                           "Hydrology","Human","Watershed","Human","Hydrology","Watershed","Human",
                           "Hydrology","Watershed","Hydrology","Watershed","Watershed","Watershed","Watershed","Watershed", "Hydrology")

    # Assign month ID
    ST_list[[i]]$Month = month.abb[i]

    # Rename PPT and TEMP variable names to fix faceting problem
    ST_list[[i]]$X[6:7] = c("MON_PPT7100_CM", "MON_TMP7100_DEGC")

    # Rename Discharge variable names to fix faceting problem
    ST_list[[i]]$X[23] = "MON_MEDIAN_Q"

  }


  # Create dataframe to store ground importance totals
  group_import = data.frame(matrix(ncol=12,nrow=4))
  names(group_import) = month.abb
  rownames(group_import) = c('Watershed','Climate','Human','Hydrology')


  # Calculate grouped importance values as a proportion of total MDA
  for (i in seq(1,12)){

    # Watershed grouped import %
    group_import[1,i] = 100*sum(ST_list[[i]]$MDA[c(10,13,16,18,19,20,21,22)])/total_mda[i]

    # Climate grouped import %
    group_import[2,i] = 100*sum(ST_list[[i]]$MDA[c(2,3,5,6,7)])/total_mda[i]

    # Human grouped import %
    group_import[3,i] = 100*sum(ST_list[[i]]$MDA[c(9,11,14)])/total_mda[i]

    # Hydrology grouped import %
    group_import[4,i] = 100*sum(ST_list[[i]]$MDA[c(1,4,8,12,15,17,23)])/total_mda[i]

  }


  # Create dataframe in correct format for heatmap using melt
  ST_heatdf = melt(group_import)
  names(ST_heatdf) = c("Month","Importance")

  # Add column with variable names
  ST_heatdf$variable = rep(c('Watershed','Climate','Human','Hydrology'),12)


  # Combine all dataframes with full variables for heat map
  ST_allvar_heat = bind_rows(ST_list)

  # Convert month to factor
  ST_allvar_heat$Month = factor(ST_allvar_heat$Month,levels = month.abb)


  ## CHANGE FILE DIRECTORY
  # Read in spearman rank correlations based on input strings
  spearman <- read.csv(paste('/stream-temperature-rf/data_results/SpearmanRank/',top_fold,"/",file_str,sub_fold,'_spearman.csv',sep=""))

  # Melt max_spearman to mach format of importance DF
  melt_spearman = melt(spearman)

  # Rename dataframe
  names(melt_spearman) = c("Variable","Month","Spearman")

  # Add spearman correlation values to dataframe
  ST_allvar_heat$Spearman = melt_spearman$Spearman


  # Create labels for new axis labels
  rf_labs = c("Drainage Area", "Annual Precipitation", "Relative Humidity", "Snow Percentage",
              "Precipitation Seasonality", "Montly Precipitation", "Monthly Air Temp.", "Baseflow Index",
              "Dam Storage","Forest LC", "Agricultural LC", "Wetland LC", "Riparian Forest LC",
              "Impervious LC", "Soil Water Capacity", "Subsurface Permeability", "Water Table Depth",
              "Bedrock Depth", "Watershed Elevation", "Watershed Slope", "Aspect (Northness)", "Aspect (Eastness)", "Median Monthly Discharge")

  rf_labs = rep(rf_labs, 12)

  # Assign names to variables
  ST_allvar_heat$X = rf_labs



  ## Generate order plots based on 'All' or 'HUC0102' to support aligning panels in Illustrator
  if (subset == "All"){

    # Combine percent importance into one dataframe for ordering heatmap
    ST_df = data.frame(Variable = janimp$X, Group = ST_list[[1]]$Group,Jan = ST_list[[1]]$Per_MDA, Feb = ST_list[[2]]$Per_MDA, Mar = ST_list[[3]]$Per_MDA,
                       Apr = ST_list[[4]]$Per_MDA, May = ST_list[[5]]$Per_MDA, Jun = ST_list[[6]]$Per_MDA, Jul = ST_list[[7]]$Per_MDA,
                       Aug = ST_list[[8]]$Per_MDA, Sept = ST_list[[9]]$Per_MDA, Oct = ST_list[[10]]$Per_MDA, Nov = ST_list[[11]]$Per_MDA,
                       Dec = ST_list[[12]]$Per_MDA)

    # Calculate total percent importance across variables
    ST_df$TotalImp = rowSums(ST_df[3:14])

    ## CHANGE FILE DIRECTORY
    # Save order of importance for other plots
    write.csv(ST_df,paste('/stream-temperature-rf/figures/SpearmanHeatmaps/PlotOrder/',file_str,'_all_order.csv',sep=""),row.names = FALSE)


  } else if (subset == "Dam"){

    ## CHANGE FILE DIRECTORY
    # Read order of importance for all plots
    ST_df = read.csv(paste('/stream-temperature-rf/figures/SpearmanHeatmaps/PlotOrder/',file_str,'_all_order.csv',sep=""))

  }else if (subset == "NoDam"){

    ## CHANGE FILE DIRECTORY
    # Read order of importance for all plots
    ST_df = read.csv(paste('/stream-temperature-rf/figures/SpearmanHeatmaps/PlotOrder/',file_str,'_all_order.csv',sep=""))

  }else if (subset == "HUC0102"){

    # Combine percent importance into one dataframe for ordering heatmap
    ST_df = data.frame(Variable = janimp$X, Group = ST_list[[1]]$Group,Jan = ST_list[[1]]$Per_MDA, Feb = ST_list[[2]]$Per_MDA, Mar = ST_list[[3]]$Per_MDA,
                       Apr = ST_list[[4]]$Per_MDA, May = ST_list[[5]]$Per_MDA, Jun = ST_list[[6]]$Per_MDA, Jul = ST_list[[7]]$Per_MDA,
                       Aug = ST_list[[8]]$Per_MDA, Sept = ST_list[[9]]$Per_MDA, Oct = ST_list[[10]]$Per_MDA, Nov = ST_list[[11]]$Per_MDA,
                       Dec = ST_list[[12]]$Per_MDA)

    # Calculate total percent importance across variables
    ST_df$TotalImp = rowSums(ST_df[3:14])

    ## CHANGE FILE DIRECTORY
    # Save order of importance for other plots
    write.csv(ST_df,paste('/stream-temperature-rf/figures/SpearmanHeatmaps/PlotOrder/',file_str,'_huc0102_order.csv',sep=""),row.names = FALSE)


  }else if (subset == "HUC03"){

    ## CHANGE FILE DIRECTORY
    # Read order of importance for all plots
    ST_df = read.csv(paste('/stream-temperature-rf/figures/SpearmanHeatmaps/PlotOrder/',file_str,'_huc0102_order.csv',sep=""))

  }else if (subset == "HUC17"){

    ## CHANGE FILE DIRECTORY
    # Read order of importance for all plots
    ST_df = read.csv(paste('/stream-temperature-rf/figures/SpearmanHeatmaps/PlotOrder/',file_str,'_huc0102_order.csv',sep=""))

  }else{

    stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")

  }


  ## CHANGE FILE DIRECTORY
  # Save plot to pdf
  pdf(file = paste('/stream-temperature-rf/figures/SpearmanHeatmaps/',top_fold,"/",file_str,sub_fold,'_heatmap.pdf',sep=""),
      width = 5.6,
      height = 5.41)
  
  # Order plots by All sites subset, or by HUC0102 subset (to gather plots in illustrator later)
  print(ST_allvar_heat %>%
    mutate(X = reorder_within(X,rep(ST_df$TotalImp,12), Group)) %>%
    ggplot(aes(Month, X)) +
    geom_point(aes(size=Per_MDA,color=Spearman)) +
    scale_size_continuous(range = c(0,5),limits = c(0,90),breaks=c(0,20,40,60,80)) +
    facet_grid(Group~.,scales = "free",space = 'free_y') +
    scale_color_gradient2(low="#4575b4",mid="gray",high="#d73027",midpoint=0,limits=c(-1,1))  +
    labs(x="",
         y="",
         color = "Spearman \nCorrelation",
         size = "% Importance") +
    scale_x_discrete(position = "top") +
    theme_bw() +
    theme(panel.spacing = unit(.25,"lines"),
          axis.text.x = element_text(color='black',size=8,angle = 45, vjust = 0, hjust=0),
          axis.text.y = element_text(color="black",size=8),
          legend.position="right",legend.direction="vertical",
          legend.title=element_text(colour="black"),
          legend.margin=margin(grid::unit(0,"cm")),
          legend.text=element_text(colour="black",size=10,face="bold"),
          legend.key.height=grid::unit(.8,"cm"),
          legend.key.width=grid::unit(.4,"cm"),
          panel.grid.major.x = element_line(size = .25),
          panel.grid.major.y = element_line(size = .25),
          axis.ticks = element_line(color = "black")) +
    ggtitle(paste(metric,subset,sep=" ")))

  # Create PDF of plot
  dev.off()


  ## CHANGE FILE DIRECTORY
  # Save plot to pdf
  pdf(file = paste('/stream-temperature-rf/figures/GroupedImportance/',top_fold,"/",file_str,sub_fold,'_heatmap.pdf',sep=""),
      width = 5.42,
      height = 5.41)


  # Plot grouped importance as proportional area plot
  # Format ST_heatdf
  ST_heatdf2 = ST_heatdf
  ST_heatdf2$Month = rep(seq(0,11), each=4)

  ST_heatdf2$variable = factor(ST_heatdf2$variable, levels = c("Watershed", "Hydrology", "Human", "Climate"))

  print(ggplot(ST_heatdf2, aes(x=Month, y=Importance, fill=variable)) +
    geom_area( color="black",size=.5) +
    scale_fill_manual(values=alpha(c("#FDF39A", "#4E95A4","#EDA364","#DF5C42"),alpha=0.75)) +
    scale_x_continuous(breaks = 0:11,
                       labels = c("Jan","Feb",'Mar','Apr','May',
                                  'Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                       expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_fixed(ratio = .06) +
    ylab("Percent Importance") +
    theme_classic() +
    theme(axis.title.x=element_blank())+
    ggtitle(paste(metric,subset, "Grouped Importance",sep=" ")))


  # Create PDF of plot
  dev.off()




  #### Create text only plot ####

  ST_allvar_v2 = ST_allvar_heat

  ST_allvar_v2$Per_MDA = round(ST_allvar_v2$Per_MDA, 2)


  ## CHANGE FILE DIRECTORY
  # Save plot to pdf
  pdf(file = paste('/stream-temperature-rf/figures/TextHeatmaps/',top_fold,"/",file_str,sub_fold,'_textheatmap.pdf',sep=""),
      width = 10,
      height = 8)


  print(ST_allvar_v2 %>%
    mutate(X = reorder_within(X,rep(ST_df$TotalImp,12), Group)) %>%
    ggplot(aes(Month, X)) +
    geom_text(aes(label=Per_MDA,color=Spearman,size=40)) +
    scale_size_continuous(range = c(0,5),limits = c(0,80),breaks=c(0,20,40,60,80)) +
    facet_grid(Group~.,scales = "free",space = 'free_y') +
    scale_color_gradient2(low="#4575b4",mid="gray",high="#d73027",midpoint=0,limits=c(-1,1))  +
    labs(x="",
         y="",
         title = paste(metric,subset),
         color = "Spearman \nCorrelation",
         size = "% Importance") +
    scale_x_discrete(position = "top") +
    theme_bw() +
    theme(panel.spacing = unit(.25,"lines"),
          axis.text.x = element_text(color='black',size=8,angle = 45, vjust = 0, hjust=0),
          axis.text.y = element_text(color="black",size=8),
          legend.position="right",legend.direction="vertical",
          legend.title=element_text(colour="black"),
          legend.margin=margin(grid::unit(0,"cm")),
          legend.text=element_text(colour="black",size=10,face="bold"),
          legend.key.height=grid::unit(.8,"cm"),
          legend.key.width=grid::unit(.4,"cm"),
          panel.grid.major.x = element_line(size = .25),
          panel.grid.major.y = element_line(size = .25),
          axis.ticks = element_line(color = "black")))


  # Create PDF of plot
  dev.off()



  # Organize ST_allvar into dataframe
  ST_allvar_df = data.frame(X = ST_allvar_v2$X[1:23], Group = ST_allvar_v2$Group[1:23],
                            Jan = ST_allvar_v2$Per_MDA[1:23], Feb = ST_allvar_v2$Per_MDA[24:46],
                            Mar = ST_allvar_v2$Per_MDA[47:69], Apr = ST_allvar_v2$Per_MDA[70:92],
                            May = ST_allvar_v2$Per_MDA[93:115], Jun = ST_allvar_v2$Per_MDA[116:138],
                            Jul = ST_allvar_v2$Per_MDA[139:161], Aug = ST_allvar_v2$Per_MDA[162:184],
                            Sep = ST_allvar_v2$Per_MDA[185:207], Oct = ST_allvar_v2$Per_MDA[208:230],
                            Nov = ST_allvar_v2$Per_MDA[231:253], Dec = ST_allvar_v2$Per_MDA[254:276])

  # Calculate average importance values across all months
  ST_allvar_df$MeanImp = rowMeans(ST_allvar_df[,3:14])

  ## CHANGE FILE DIRECTORY
  # Write to csv
  write.csv(ST_allvar_df, paste('/stream-temperature-rf/figures/ImportanceValues/',top_fold,"/",file_str,sub_fold,'_importance.csv',sep=""), row.names= FALSE)


  
  #### Create Training and Test Set error plots - CV1
  
  ## CHANGE FILE DIRECTORY
  # Read in error dataframe
  errordf <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/cv1_",file_str,sub_fold,'_error.csv',sep=""))
  
  # Assemble test and training nrmse values for plotting
  nrmse_plot_cv1 <- data.frame(mon = rep(mon_list,2),set = c(rep('test',12),rep('train',12)), nrmse = rep(0,24))
  
  # Assign values of mean nrmse
  nrmse_plot_cv1$nrmse[1:12] <- errordf$test_nrmse
  nrmse_plot_cv1$nrmse[13:24] <- errordf$train_nrmse

  
  # Convert groups to factor
  nrmse_plot_cv1$set = factor(nrmse_plot$set, levels = c("train","test"))
  nrmse_plot_cv1$mon = factor(nrmse_plot$mon, levels = mon_list)
  
  ## CHANGE FILE DIRECTORY
  # Save plot to pdf
  pdf(file = paste('/stream-temperature-rf/figures/ModelError_CV1/',top_fold,"/cv1_",file_str,sub_fold,'_error.pdf',sep=""),
      width = 8,
      height = 4)
  
  
  # Create error bar plot
  print(ggplot(nrmse_plot_cv1, aes(x=mon, y=nrmse, fill=set)) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    scale_fill_manual(labels = c("Training Set", "Test Set"),values = c('#a6eaff','#1f78b4')) +
    scale_y_continuous(expand = c(0,0),limits = c(0,0.8)) +
    labs(fill = "") +
    xlab("") + ylab("Normalized RMSE")+
    ggtitle(paste(metric,subset, "Model Error",sep=" ")) + 
    theme_bw() +
    theme(panel.grid.major = element_blank()))
  
  # Create PDF of plot
  dev.off()
  
  

  #### Create Training and Test Set error plots - 5 fold

  ## CHANGE FILE DIRECTORY
  # Read in error dataframe
  errordf <- read.csv(paste('/stream-temperature-rf/data_results/',top_fold,"/",metric_str,sub_fold,"/",file_str,sub_fold,'_error.csv',sep=""))

  # Assemble test and training nrmse values for plotting
  nrmse_plot <- data.frame(mon = rep(mon_list,2),set = c(rep('test',12),rep('train',12)), mean_nrmse = rep(0,24), sd_nrmse = rep(0,24))

  # Assign values of mean nrmse
  nrmse_plot$mean_nrmse[1:12] <- errordf$test_nrmse
  nrmse_plot$mean_nrmse[13:24] <- errordf$train_nrmse

  # Assign values of sd nrmse
  nrmse_plot$sd_nrmse[1:12] <- errordf$test_nrmse_sd
  nrmse_plot$sd_nrmse[13:24] <- errordf$train_nrmse_sd

  # Convert groups to factor
  nrmse_plot$set = factor(nrmse_plot$set, levels = c("train","test"))
  nrmse_plot$mon = factor(nrmse_plot$mon, levels = mon_list)

  ## CHANGE FILE DIRECTORY
  # Save plot to pdf
  pdf(file = paste('/stream-temperature-rf/figures/ModelError_5fold/',top_fold,"/",file_str,sub_fold,'_error.pdf',sep=""),
      width = 8,
      height = 4)


  # Create error bar plot
  print(ggplot(nrmse_plot, aes(x=mon, y=mean_nrmse, fill=set)) +
          geom_bar(stat="identity", color="black",
                   position=position_dodge()) +
          geom_errorbar(aes(ymin=mean_nrmse - sd_nrmse, ymax=mean_nrmse + sd_nrmse), width=.2,
                        position=position_dodge(.9)) +
          scale_fill_manual(labels = c("Training Set", "Test Set"),values = c('#a6eaff','#1f78b4')) +
          scale_y_continuous(expand = c(0,0),limits = c(0,0.8)) +
          labs(fill = "") +
          xlab("") + ylab("Normalized RMSE")+
          ggtitle(paste(metric,subset, "Model Error (5-fold)",sep=" ")) +
          theme_bw() +
          theme(panel.grid.major = element_blank()))

  # Create PDF of plot
  dev.off()

  
}

