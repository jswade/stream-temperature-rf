# Create function to run random forest on given metric, month, and subset, optimizing RF hyperparameters on 80% training set using stratified sampling

library(party)
library(caret)
library(hydroGOF) #NSE package

# Create function for RF runs
# metric = string of either 'Max' or 'Slope'
# mon = number between 1 and 12 indicating month of model
# subset = string of ('All', 'Dam', 'NoDam')

rf_func_hyperparameter_tuning <- function(metric, mon, subset){

  ## Read in data from all sites
  
  # Read in metrics
  slope_metric <- read.csv("E:/Users/Jeff/Github/stream-temperature-rf/data_raw/thermalsensitivity.csv")
  max_metric <- read.csv("E:/Users/Jeff/Github/stream-temperature-rf/data_raw/STmax.csv")
  
  # Read in Full Site data
  site_data <- read.csv("E:/Users/Jeff/Github/stream-temperature-rf/data_raw/gagesII_RF_2016_2020_subset.csv")
  
  # Read in discharge data
  disch.month <- read.csv("E:/Users/Jeff/Github/stream-temperature-rf/data_raw/MedianQMonth_2016_2020.csv")
  
  # Read in dam info
  damref <- read.csv("E:/Users/Jeff/Github/stream-temperature-rf/data_raw/RF_2016_2020_DamsRef.csv")
  
  # Read in stratified groupings by hucs
  strat_huc <- read.csv("E:/Users/Jeff/Github/stream-temperature-rf/site_classification/StratifiedSampling/HUC_stratifiedsampling.csv")
  

  if (metric == "Max"){
    
    # Assemble data frame of rf max input data, based on input month value
    rf_data <- data.frame(val = max_metric[,(mon+1)], site_data[,c(2:6,(mon+6),(mon+18),31:45)],Q_val = disch.month[,(mon+1)])
    
    
  } else if (metric == "Slope"){
    
    # Assemble data frame of rf slope input data, based on input month value
    rf_data <- data.frame(val = slope_metric[,(mon+1)], site_data[,c(2:6,(mon+6),(mon+18),31:45)],Q_val = disch.month[,(mon+1)])
    
    
  } else {
    
    # Return error message if incorrect input is supplied
    stop("Metric string not valid. Please use Max or Slope.")
    
  }
  
  
  # Subset Data based on input 'Subset' ('All', 'Dam', 'NoDam',HUCs)
  
  if (subset == "All"){
    
    # Move on
    
  } else if (subset == "Dam"){
    
    # Establish site indices with major dams (distance to maj dam != 0)
    dam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM != -999)
    
    # Subset rf_data
    rf_data <- rf_data[dam_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[dam_ind,]
    
    
  }else if (subset == "NoDam"){
    
    # Establish site indices with no major dams (distance to maj dam = -999)
    nodam_ind <- which(damref$RAW_DIS_NEAREST_MAJ_DAM == -999)
    
    # Subset rf_data
    rf_data <- rf_data[nodam_ind,]
    
    # Subset strat_huc
    strat_huc <- strat_huc[nodam_ind,]
    
    
  }else if (subset == "HUC0102"){
    
    # Establish site indices located in HUC0102 (Northeast/MidAtlantic)
    huc0102_ind <- which(strat_huc$HUC02 == 1 | strat_huc$HUC02 == 2)
    
    # Subset rf_data
    rf_data <- rf_data[huc0102_ind,]  
    
    
  }else if (subset == "HUC03"){
    
    # Establish site indices located in HUC03 (Southeast/Gulf)
    huc03_ind <- which(strat_huc$HUC02 == 3)
    
    # Subset rf_data
    rf_data <- rf_data[huc03_ind,]  
    
    
  }else if (subset == "HUC17"){
    
    # Establish site indices located in HUC17 (Pacific NW)
    huc17_ind <- which(strat_huc$HUC02 == 17)
    
    # Subset rf_data
    rf_data <- rf_data[huc17_ind,]  
    
  }else{
    
    stop("Subset string not valid, Please use All, Dam, NoDam, HUC0102, HUC03, or HUC17")
    
  } 
  
  
  # Get length of data
  n_data <- dim(rf_data)[1]
  
  ## Generate random training and test set (80/20) stratified sampling,
  # Stratified sampling based on HUC 2 Groupings, each training set and test set will have the same percentage of geographic regions (see huc_count_2016_2020.xls)
  set.seed(1)
  folds <- createFolds(factor(strat_huc$Strat_Group), k = 5, list = FALSE, returnTrain = FALSE)
  
  # Training set will be folds != 1, Test set will be folds = 1
  traindf <- rf_data[which(folds != 1),]
  testdf <- rf_data[which(folds == 1),]
  
  # Subset hucs by training set
  strat_huc_train = strat_huc[which(folds != 1),]
  
  # Generate 5-fold stratified cross validation 
  set.seed(2)
  folds_train <- createFolds(factor(strat_huc_train$Strat_Group), k = 5, list = FALSE, returnTrain = FALSE)
  
  # Aggregate folds into 5 cv training sets
  cvtrain1 <- rf_data[which(folds != 1),]
  cvtrain2 <- rf_data[which(folds != 2),]
  cvtrain3 <- rf_data[which(folds != 3),]
  cvtrain4 <- rf_data[which(folds != 4),]
  cvtrain5 <- rf_data[which(folds != 5),]
  
  # Aggregate folds into 5 cv test sets
  cvtest1 <- rf_data[which(folds == 1),]
  cvtest2 <- rf_data[which(folds == 2),]
  cvtest3 <- rf_data[which(folds == 3),]
  cvtest4 <- rf_data[which(folds == 4),]
  cvtest5 <- rf_data[which(folds == 5),]
  
  # Aggregate train and test cv sets into lists
  cvtrain_list <- list(cvtrain1,cvtrain2, cvtrain3, cvtrain4, cvtrain5)
  cvtest_list <- list(cvtest1, cvtest2, cvtest3, cvtest4, cvtest5)
  
  
  # Create dataframe to store mse values for different mtry values (test and oob)
  mtry_val <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

  mtry_test <- data.frame(mtry_val = mtry_val, cv1 = rep(0,20), cv2 = rep(0,20), cv3 = rep(0,20), cv4 = rep(0,20), cv5 = rep(0,20))
  mtry_train  <- data.frame(mtry_val = mtry_val, cv1 = rep(0,20), cv2 = rep(0,20), cv3 = rep(0,20), cv4 = rep(0,20), cv5 = rep(0,20))
  mtry_oob <- data.frame(mtry_val = mtry_val, cv1 = rep(0,20), cv2 = rep(0,20), cv3 = rep(0,20), cv4 = rep(0,20), cv5 = rep(0,20))
  
  
  # Loop through mtry values, calculating rmse for 20 mtry scenarios on test set
  for (i in seq(1,20)){
    
    # Loop through cross-validation sets 
    for (j in seq(1,5)){
    
      # Assure same results are generated each time for each model
      set.seed(1)
      
      # Create random forest model, varying i in loop 
      crf_obj <- cforest(val ~ ., 
                            data=cvtrain_list[[j]],
                            controls = cforest_unbiased(ntree=1000,mtry=mtry_val[i]))
      
      # Predict rf test data
      crf_pred <-  predict(crf_obj,newdata=cvtest_list[[j]])
      crf_pred <- as.numeric(crf_pred)
      
      # Predict rf OOB
      crf_oob <- predict(crf_obj,OOB = TRUE)
      crf_oob <- as.numeric(crf_oob)
      
      # Predict rf training set directly
      crf_train <- predict(crf_obj, newdata=cvtrain_list[[j]])
      crf_train <- as.numeric(crf_train)
      
      # Access observed values of metric
      crf_obs <- cvtest_list[[j]]$val
      crf_obs_train <- cvtrain_list[[j]]$val
      
      # Calculate RMSE between sim and obs
      mtry_test[i,(j+1)] <- as.numeric(sqrt(mean((crf_pred-crf_obs)^2)))
      
      # Calculate RMSE OOB
      # Calculate RMSE between sim and obs
      mtry_oob[i,(j+1)] <- as.numeric(sqrt(mean((crf_oob-crf_obs_train)^2)))
      

      # Calculate RMSE of training set directly
      mtry_train[i,(j+1)] <- as.numeric(sqrt(mean((crf_train-crf_obs_train)^2)))
      
    }
  }
  
  # Calculate mean RMSE at each mtry val
  mtry_RMSE <- data.frame(mtry_val = mtry_val, RMSE_test = rowMeans(mtry_test[,-1]), RMSE_oob = rowMeans(mtry_oob[,-1]),RMSE_train = rowMeans(mtry_train[,-1]) )

  # Return function outputs
  return(mtry_RMSE)
  # 
}

