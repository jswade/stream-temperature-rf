# Create function to run random forest on given metric, month, and subset
# 80/20 Training Set using Geographic Stratified sampling (except for HUCS models, which use a random training set)

# Run this function using RF_runs.R

library(party)
library(caret)
library(hydroGOF) #NSE package

# Create function for RF runs
# metric = string of either 'Max' or 'Slope'
# mon = number between 1 and 12 indicating month of model
# subset = string of ('All', 'Dam', 'NoDam','HUC0102','HUC03','HUC17')

rf_func <- function(metric, mon, subset){

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
  
  
  # If metric is Max
  if (metric == "Max"){
    
    # Assemble data frame of rf max input data, based on input month value
    rf_data <- data.frame(val = max_metric[,(mon+1)], site_data[,c(2:6,(mon+6),(mon+18),31:45)],Q_val = disch.month[,(mon+1)])
    
    
  # If metric is slope (thermal sensitivity)
  } else if (metric == "Slope"){
    
    # Assemble data frame of rf slope input data, based on input month value
    rf_data <- data.frame(val = slope_metric[,(mon+1)], site_data[,c(2:6,(mon+6),(mon+18),31:45)],Q_val = disch.month[,(mon+1)])
    
    
  # If another string is entered  
  } else {
    
    # Return error message if incorrect input is supplied
    stop("Metric string not valid. Please use Max or Slope.")
    
  }
  
  
  # Subset Data based on input 'Subset' ('All', 'Dam', 'NoDam',HUCs)
  
  if (subset == "All"){

    # Move on
    
  # If selected subset is Dam  
  } else if (subset == "Dam"){
    
    # Establish site indices with major dams (distance to maj dam != 0)
    dam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM != -999)
    
    # Subset rf_data
    rf_data <- rf_data[dam_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[dam_ind,]
    
    
  # If selected subset is NoDam
  }else if (subset == "NoDam"){
    
    # Establish site indices with no major dams (distance to maj dam = -999)
    nodam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM == -999)
 
    # Subset rf_data
    rf_data <- rf_data[nodam_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[nodam_ind,]

  
  # If selected subset is HUC0102
  }else if (subset == "HUC0102"){
    
    # Establish site indices located in HUC0102 (Northeast/MidAtlantic)
    huc0102_ind <- which(strat_huc$HUC02 == 1 | strat_huc$HUC02 == 2)

    # Subset rf_data
    rf_data <- rf_data[huc0102_ind,]  

  # If selected subset is HUC03
  }else if (subset == "HUC03"){
    
    # Establish site indices located in HUC03 (Southeast/Gulf)
    huc03_ind <- which(strat_huc$HUC02 == 3)
    
    # Subset rf_data
    rf_data <- rf_data[huc03_ind,]  

  # If selected subset is HUC17   
  }else if (subset == "HUC17"){
    
    # Establish site indices located in HUC17 (Pacific NW)
    huc17_ind <- which(strat_huc$HUC02 == 17)
    
    # Subset rf_data
    rf_data <- rf_data[huc17_ind,]  
  
  }else{
    
    stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
    
  } 

  
  # Perform stratified 5-fold sampling on AllSites, Dam, NoDam subsets
  # Perform random 5-fold sampling on HUC subsets
  
  if (subset == 'All'| subset == 'Dam'| subset == "NoDam" ){
    
    ## Generate training and test set (80/20) stratified sampling,
    # Stratified sampling based on HUC 2 Groupings, each training set and test set will have the same percentage of geographic regions (see huc_count_2016_2020.xls)
    set.seed(1)
    folds <- createFolds(factor(strat_huc$Strat_Group), k = 5, list = FALSE, returnTrain = FALSE)
    
    # Primary Training set will be folds != 1, Test set will be folds = 1
    # Aggregate folds into 5 cv training sets
    cvtrain1 <- rf_data[which(folds != 1),] # Primary Training set (Variable Importance)
    cvtrain2 <- rf_data[which(folds != 2),]
    cvtrain3 <- rf_data[which(folds != 3),]
    cvtrain4 <- rf_data[which(folds != 4),]
    cvtrain5 <- rf_data[which(folds != 5),]
    
    # Aggregate folds into 5 cv test sets
    cvtest1 <- rf_data[which(folds == 1),] # Primary Test set (Variable Importance)
    cvtest2 <- rf_data[which(folds == 2),]
    cvtest3 <- rf_data[which(folds == 3),]
    cvtest4 <- rf_data[which(folds == 4),]
    cvtest5 <- rf_data[which(folds == 5),]
    
    # Aggregate train and test cv sets into lists
    cvtrain_list <- list(cvtrain1,cvtrain2, cvtrain3, cvtrain4, cvtrain5)
    cvtest_list <- list(cvtest1, cvtest2, cvtest3, cvtest4, cvtest5)
    
    
  # HUC-specific models will use a random training/test set, as they can't be geographically sampled easily  
  }else if (subset == 'HUC0102' | subset == 'HUC03' | subset == 'HUC17'){
    
    # Find length of data
    n_data <- dim(rf_data)[1]
    
    ## Generate RANDOM training and test set (80/20) stratified sampling,
    # Stratified sampling based on HUC 2 Groupings, each training set and test set will have the same percentage of geographic regions (see huc_count_2016_2020.xls)
    set.seed(1)
    folds <- createFolds(seq(1:n_data), k = 5, list = FALSE, returnTrain = FALSE)
    
    # Primary Training set will be folds != 1, Test set will be folds = 1
    # Aggregate folds into 5 cv training sets
    cvtrain1 <- rf_data[which(folds != 1),] # Primary Training set (Variable Importance)
    cvtrain2 <- rf_data[which(folds != 2),]
    cvtrain3 <- rf_data[which(folds != 3),]
    cvtrain4 <- rf_data[which(folds != 4),]
    cvtrain5 <- rf_data[which(folds != 5),]
    
    # Aggregate folds into 5 cv test sets
    cvtest1 <- rf_data[which(folds == 1),] # Primary Test set (Variable Importance)
    cvtest2 <- rf_data[which(folds == 2),]
    cvtest3 <- rf_data[which(folds == 3),]
    cvtest4 <- rf_data[which(folds == 4),]
    cvtest5 <- rf_data[which(folds == 5),]
    
    # Aggregate train and test cv sets into lists
    cvtrain_list <- list(cvtrain1,cvtrain2, cvtrain3, cvtrain4, cvtrain5)
    cvtest_list <- list(cvtest1, cvtest2, cvtest3, cvtest4, cvtest5) 

    
  }else{
    
    stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
    
  }
  
  
  # Create dataframes to store error metrics
  test_err <- data.frame(test_R2 = rep(0,5), test_rmse = rep(0,5), test_nrmse = rep(0,5))
  train_err <- data.frame(train_R2 = rep(0,5), train_rmse = rep(0,5), train_nrmse = rep(0,5))
  oob_err <- data.frame(oob_R2 = rep(0,5), oob_rmse = rep(0,5), oob_nrmse = rep(0,5))
  
  # Create list to store crf_objs
  crf_list <- list()
  
  
  # Loop through each cv set (only calculate and store importance for cv1)
  for (i in seq(1,5)){
  
    # Assure same results are generated each time for each model
    set.seed(1)
  
    # Create random forest model 
    crf_obj <- cforest(val ~ ., 
                          data=cvtrain_list[[i]],
                          controls = cforest_unbiased(ntree=1000,mtry=10))
    
    # Add crf_obj to list (only export crf_obj corresponding to cv1)
    crf_list[[i]] <- crf_obj
    
    if(i == 1){ # Calculate variable importance for first training/test set (cv1)
      
      # Calculate variable conditional permutation importance (Mean Decrease in Accuracy)
      rf_import <- varimp(crf_obj, conditional=TRUE)
      rf_import <- as.data.frame(rf_import)
      names(rf_import) = c("MDA")
      
      # Set negative importance to 0
      rf_import$MDA[which(rf_import$MDA < 0)] <- 0
      
    }
    
    # Predict rf test data
    crf_pred_test <-  predict(crf_obj,newdata=cvtest_list[[i]])
    crf_pred_test <- as.numeric(crf_pred_test)
    
    # Predict rf OOB
    crf_pred_oob <- predict(crf_obj,OOB = TRUE)
    crf_pred_oob <- as.numeric(crf_pred_oob)
    
    # Predict rf training set directly
    crf_pred_train <- predict(crf_obj, newdata=cvtrain_list[[i]])
    crf_pred_train <- as.numeric(crf_pred_train)
    
    # Access observed values of metric
    crf_obs_test <- cvtest_list[[i]]$val
    crf_obs_train <- cvtrain_list[[i]]$val
    
    
    # Calculate R2
    test_err$test_R2[i] <- as.numeric((cor(crf_pred_test,crf_obs_test, method="pearson"))^2)
    train_err$train_R2[i] <- as.numeric((cor(crf_pred_train,crf_obs_train, method="pearson"))^2)
    oob_err$oob_R2[i] <- as.numeric((cor(crf_pred_oob,crf_obs_train, method="pearson"))^2)
    
    # Calculate RMSE
    test_err$test_rmse[i] <- as.numeric(sqrt(mean((crf_pred_test-crf_obs_test)^2)))
    train_err$train_rmse[i] <- as.numeric(sqrt(mean((crf_pred_train-crf_obs_train)^2)))
    oob_err$oob_rmse[i] <- as.numeric(sqrt(mean((crf_pred_oob-crf_obs_train)^2)))
    
    # Calculate nRMSE
    test_err$test_nrmse[i] <- as.numeric((sqrt(mean((crf_pred_test-crf_obs_test)^2)))/mean(crf_obs_test))
    train_err$train_nrmse[i] <- as.numeric((sqrt(mean((crf_pred_train-crf_obs_train)^2)))/mean(crf_obs_train))
    oob_err$oob_nrmse[i] <- as.numeric((sqrt(mean((crf_pred_oob-crf_obs_train)^2)))/mean(crf_obs_train))

    
  }
  
  # Only export first crf_object 
  # Create list to return rf_obj and importance
  list_out <- list('crf' = crf_list[[1]], 'imp' = rf_import,'test_err' = test_err, 'train_err' = train_err, 'oob_err' = oob_err)
  
  # Return function outputs
  return(list_out)
  
}

