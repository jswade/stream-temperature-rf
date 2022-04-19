# Create function to load CRS objects and export predicted values

# Predicted values stored for training sets only
# CRF objects were generated using CVtrain1 and CVtest1 (fold == 1)

library(party)
library(caret)


# Create function for RF runs
# metric = string of either 'Max' or 'Slope'
# subset = string of ('All', 'Dam', 'NoDam', 'HUC0102','HUC03','HUC17')

rf_pred_func <- function(metric, subset){

  ## Read in data from all sites
  
  ## CHANGE ALL FILE DIRECTORIES
  
  # Read in metrics
  slope_metric <- read.csv("/stream-temperature-rf/data_raw/thermalsensitivity.csv")
  max_metric <- read.csv("/stream-temperature-rf/data_raw/STmax.csv")
  
  # Read in Full Site data
  site_data <- read.csv("/stream-temperature-rf/data_raw/gagesII_RF_2016_2020_subset.csv")
  
  # Read in discharge data
  disch.month <- read.csv("/stream-temperature-rf/data_raw/MedianQMonth_2016_2020.csv")
  
  # Read in stratified groupings by hucs
  strat_huc <- read.csv("/stream-temperature-rf/site_classification/StratifiedSampling/HUC_stratifiedsampling.csv")
  
  # Read in dam info
  damref <- read.csv("/stream-temperature-rf/data_raw/RF_2016_2020_Dams.csv")
  
  # Create list of months
  mon_list <- c("Jan","Feb","Mar","Apr",'May',"Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  # Select metric to store for output
  if (metric == "Max"){
    
    # Select maximum ST observed values
    out_df <- max_metric
    names(out_df) <- c("Site",paste(mon_list,'_Obs',sep=""))
    
    
  } else if (metric == "Slope"){
    
    # Select Slope observed values
    out_df <- slope_metric
    names(out_df) <- c("Site",paste(mon_list,'_Obs',sep=""))
    
    
  } else {
    
    # Return error message if incorrect input is supplied
    stop("Metric string not valid. Please use Max or Slope.")
    
  }
  
  
  
  
  # Subset Data based on input 'Subset' ('All', 'Dam', 'NoDam', 'HUC0102','HUC03','HUC17')
  
  if (subset == "All"){
    
    # Move on, keep all sites
    
  } else if (subset == "Dam"){
    
    # Establish site indices with major dams (distance to maj dam != 0)
    dam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM != -999)
    
    # Subset rf_data
    out_df <- out_df[dam_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[dam_ind,]
    
    
  }else if (subset == "NoDam"){
    
    # Establish site indices with no major dams (distance to maj dam = -999)
    nodam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM == -999)
    
    # Subset rf_data
    out_df <- out_df[nodam_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[nodam_ind,]
    
    
  }else if (subset == "HUC0102"){
    
    # Establish site indices located in HUC0102 (Northeast/MidAtlantic)
    huc0102_ind <- which(strat_huc$HUC02 == 1 | strat_huc$HUC02 == 2)
    
    # Subset rf_data
    out_df <- out_df[huc0102_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[huc0102_ind,]
    
    
  }else if (subset == "HUC03"){
    
    # Establish site indices located in HUC03 (Southeast/Gulf)
    huc03_ind <- which(strat_huc$HUC02 == 3)
    
    # Subset rf_data
    out_df <- out_df[huc03_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[huc03_ind,]
    
    
  }else if (subset == "HUC17"){
    
    # Establish site indices located in HUC17 (Pacific NW)
    huc17_ind <- which(strat_huc$HUC02 == 17)
    
    # Subset rf_data
    out_df <- out_df[huc17_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[huc17_ind,]
    
    
  }else{
    
    stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
    
  } 
  
  
  ## Recreate site list used in training set ##
  
  # Perform stratified 5-fold sampling on AllSites, Dam, NoDam subsets
  # Perform random 5-fold sampling on HUC subsets
  
  if (subset == 'All'| subset == 'Dam'| subset == "NoDam" ){
    
    ## Generate training and test set (80/20) stratified sampling,
    # Stratified sampling based on HUC 2 Groupings, each training set and test set will have the same percentage of geographic regions (see huc_count_2016_2020.xls)
    set.seed(1)
    folds <- createFolds(factor(strat_huc$Strat_Group), k = 5, list = FALSE, returnTrain = FALSE)
    
    # Primary Training set will be folds != 1, Test set will be folds = 1
    # Aggregate folds into 5 cv training sets
    cvtrain1 <- out_df[which(folds != 1),] # Primary Training set (Variable Importance)
    cvtrain2 <- out_df[which(folds != 2),]
    cvtrain3 <- out_df[which(folds != 3),]
    cvtrain4 <- out_df[which(folds != 4),]
    cvtrain5 <- out_df[which(folds != 5),]
    
    # Aggregate folds into 5 cv test sets
    cvtest1 <- out_df[which(folds == 1),] # Primary Test set (Variable Importance)
    cvtest2 <- out_df[which(folds == 2),]
    cvtest3 <- out_df[which(folds == 3),]
    cvtest4 <- out_df[which(folds == 4),]
    cvtest5 <- out_df[which(folds == 5),]
    
    # Aggregate train and test cv sets into lists
    cvtrain_list <- list(cvtrain1,cvtrain2, cvtrain3, cvtrain4, cvtrain5)
    cvtest_list <- list(cvtest1, cvtest2, cvtest3, cvtest4, cvtest5)
    
    
  }else if (subset == 'HUC0102' | subset == 'HUC03' | subset == 'HUC17'){
    
    # Find length of data
    n_data <- dim(out_df)[1]
    
    ## Generate RANDOM training and test set (80/20), NOT STRATIFIED
    set.seed(1)
    folds <- createFolds(seq(1:n_data), k = 5, list = FALSE, returnTrain = FALSE)
    
    # Primary Training set will be folds != 1, Test set will be folds = 1
    # Aggregate folds into 5 cv training sets
    cvtrain1 <- out_df[which(folds != 1),] # Primary Training set (Variable Importance)
    cvtrain2 <- out_df[which(folds != 2),]
    cvtrain3 <- out_df[which(folds != 3),]
    cvtrain4 <- out_df[which(folds != 4),]
    cvtrain5 <- out_df[which(folds != 5),]
    
    # Aggregate folds into 5 cv test sets
    cvtest1 <- out_df[which(folds == 1),] # Primary Test set (Variable Importance)
    cvtest2 <- out_df[which(folds == 2),]
    cvtest3 <- out_df[which(folds == 3),]
    cvtest4 <- out_df[which(folds == 4),]
    cvtest5 <- out_df[which(folds == 5),]
    
    # Aggregate train and test cv sets into lists
    cvtrain_list <- list(cvtrain1,cvtrain2, cvtrain3, cvtrain4, cvtrain5)
    cvtest_list <- list(cvtest1, cvtest2, cvtest3, cvtest4, cvtest5) 
    
    
  }else{
    
    stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
    
  }
  
  
  ## Subset out_df by cvtrain1 indices to match with training set
  out_df <- out_df[match(cvtrain1$Site, out_df$Site),]
  
  # Add columns to out_df to store predicted values
  out_df = cbind(out_df,matrix(0,nrow = dim(out_df[1]),ncol=12))
  names(out_df)[14:25] = paste(mon_list,"_Pred",sep="")

  
  
  ## Load RF rds objects ##

  # Select metric MaxST
  if (metric == "Max"){
    
    # Subset Data based on input 'Subset' ('All', 'Dam', 'NoDam', HUCs)
    if (subset == "All"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/AllSites/MaxST_all/crf_",mon_list[i],"max_all.rds",sep="")
      
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred
        
      }
      
    } else if (subset == "Dam"){
        
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/MaxST_dam/crf_",mon_list[i],"max_dam.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      

      }
      
    }else if (subset == "NoDam"){

      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/MaxST_nodam/crf_",mon_list[i],"max_nodam.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else if (subset == "HUC0102"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc0102/crf_",mon_list[i],"max_huc0102.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else if (subset == "HUC03"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc03/crf_",mon_list[i],"max_huc03.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else if (subset == "HUC17"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc17/crf_",mon_list[i],"max_huc17.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else{
      
      stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
      
    } 

    
  } else if (metric == "Slope"){
    
    # Subset Data based on input 'Subset' ('All', 'Dam', 'NoDam', HUCs)
    if (subset == "All"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/AllSites/Slope_all/crf_",mon_list[i],"slope_all.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred
        
      }
      
    } else if (subset == "Dam"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/Slope_dam/crf_",mon_list[i],"slope_dam.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else if (subset == "NoDam"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/Slope_nodam/crf_",mon_list[i],"slope_nodam.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else if (subset == "HUC0102"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc0102/crf_",mon_list[i],"slope_huc0102.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else if (subset == "HUC03"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc03/crf_",mon_list[i],"slope_huc03.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else if (subset == "HUC17"){
      
      # Loop through months
      for (i in seq(1,12)){
        
        ## CHANGE FILE DIRECTORY
        # Create file path for crf obj
        crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc17/crf_",mon_list[i],"slope_huc17.rds",sep="")
        
        # Load rds file
        crf_obj <- readRDS(crf_filepath)
        
        # Predict values from RF object
        rf_pred <- predict(crf_obj)
        
        # Store predicted values in out_df
        out_df[,(i+13)] <- rf_pred      
        
      }
      
    }else{
      
      stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
      
    } 
    
    
  } else {
    
    # Return error message if incorrect input is supplied
    stop("Metric string not valid. Please use Max or Slope.")
    
  }
  
  
return(out_df)  
  
   
}