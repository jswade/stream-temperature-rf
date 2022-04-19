# Using rf_func_hyperparameter_tuning.R, find the optimal mtry value of RF models


library(tidyverse)
library(ggplot2)

## CHANGE FILE DIRECTORY
# Import RF function
source("/stream-temperature-rf/rf_model_tuning/rf_func_hyperparameter_tuning.R")


# Create array of month strings for saving to file
mon_list <- c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')


##### Max ST - All ####

# Initialize dataframes to store error values (test set, training set, out-of-bag)

mtry_test <- data.frame(mtry = seq(1,20), Jan_rrmse = rep(0,20), Feb_rmse = rep(0,20), Mar_rmse = rep(0,20),
                       Apr_rmse = rep(0,20), May_rmse = rep(0,20), Jun_rmse = rep(0,20), Jul_rmse = rep(0,20),
                       Aug_rmse = rep(0,20), Sep_rmse = rep(0,20), Oct_rmse = rep(0,20), Nov_rmse = rep(0,20), Dec_rmse = rep(0,20))

mtry_oob <- data.frame(mtry = seq(1,20), Jan_rmse = rep(0,20), Feb_rmse = rep(0,20), Mar_rmse = rep(0,20),
                       Apr_rmse = rep(0,20), May_rmse = rep(0,20), Jun_rmse = rep(0,20), Jul_rmse = rep(0,20),
                       Aug_rmse = rep(0,20), Sep_rmse = rep(0,20), Oct_rmse = rep(0,20), Nov_rmse = rep(0,20), Dec_rmse = rep(0,20))

mtry_train <- data.frame(mtry = seq(1,20), Jan_rmse = rep(0,20), Feb_rmse = rep(0,20), Mar_rmse = rep(0,20),
                       Apr_rmse = rep(0,20), May_rmse = rep(0,20), Jun_rmse = rep(0,20), Jul_rmse = rep(0,20),
                       Aug_rmse = rep(0,20), Sep_rmse = rep(0,20), Oct_rmse = rep(0,20), Nov_rmse = rep(0,20), Dec_rmse = rep(0,20))


# Loop through months
for (i in seq(1,12)){
  
  # Run RF function to calculate errors for different mtry values for each month i
  rf_temp <- rf_func_hyperparameter_tuning('Max', i, 'All')
  
  # Assign values of MSE_test and MSE_oob for each month
  mtry_test[,(i+1)] <- rf_temp$RMSE_test
  mtry_oob[,(i+1)] <- rf_temp$RMSE_oob
  mtry_train[,(i+1)] <- rf_temp$RMSE_train
  
}

# Calculate column means of mtry dfs
mtry_test$test_rmse <- rowMeans(mtry_test[,-1])
mtry_oob$oob_rmse <- rowMeans(mtry_oob[,-1])
mtry_train$train_rmse <- rowMeans(mtry_train[,-1])

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(mtry_test,"/stream-temperature-rf/rf_model_tuning/hyperparameter_tuning/mtry_test_max_all.csv")
write.csv(mtry_oob,"/stream-temperature-rf/rf_model_tuning/hyperparameter_tuning/mtry_oob_max_all.csv")
write.csv(mtry_train,"/stream-temperature-rf/rf_model_tuning/hyperparameter_tuning/mtry_train_max_all.csv")

# RMSE_df for plotting
RMSE_df <- data.frame(mtry = mtry_test$mtry,test_RMSE = mtry_test$test_rmse, train_RMSE = mtry_train$train_rmse)

# ggplot(RMSE_df, aes(x = mtry)) + 
#   geom_line(aes(y=test_RMSE)) + 
#   geom_line(aes(y=train_RMSE), col='red') + 
#   ylab('RMES') +
#   ggtitle('Max ST All Sites: Full Year')


####




##### Slope - All ####

# Initialize dataframes to store error values (test set, training set, out-of-bag)

mtry_test <- data.frame(mtry = seq(1,20), Jan_rrmse = rep(0,20), Feb_rmse = rep(0,20), Mar_rmse = rep(0,20),
                        Apr_rmse = rep(0,20), May_rmse = rep(0,20), Jun_rmse = rep(0,20), Jul_rmse = rep(0,20),
                        Aug_rmse = rep(0,20), Sep_rmse = rep(0,20), Oct_rmse = rep(0,20), Nov_rmse = rep(0,20), Dec_rmse = rep(0,20))

mtry_oob <- data.frame(mtry = seq(1,20), Jan_rmse = rep(0,20), Feb_rmse = rep(0,20), Mar_rmse = rep(0,20),
                       Apr_rmse = rep(0,20), May_rmse = rep(0,20), Jun_rmse = rep(0,20), Jul_rmse = rep(0,20),
                       Aug_rmse = rep(0,20), Sep_rmse = rep(0,20), Oct_rmse = rep(0,20), Nov_rmse = rep(0,20), Dec_rmse = rep(0,20))

mtry_train <- data.frame(mtry = seq(1,20), Jan_rmse = rep(0,20), Feb_rmse = rep(0,20), Mar_rmse = rep(0,20),
                         Apr_rmse = rep(0,20), May_rmse = rep(0,20), Jun_rmse = rep(0,20), Jul_rmse = rep(0,20),
                         Aug_rmse = rep(0,20), Sep_rmse = rep(0,20), Oct_rmse = rep(0,20), Nov_rmse = rep(0,20), Dec_rmse = rep(0,20))



# Loop through months
for (i in seq(1,12)){
  
  # Run RF function to calculate errors for different mtry values for each month i
  rf_temp <- rf_func_hyperparameter_tuning('Slope', i, 'All')
  
  # Assign values of MSE_test and MSE_oob for each month
  mtry_test[,(i+1)] <- rf_temp$RMSE_test
  mtry_oob[,(i+1)] <- rf_temp$RMSE_oob
  mtry_train[,(i+1)] <- rf_temp$RMSE_train
  
}

# Calculate column means of mtry dfs
mtry_test$test_rmse <- rowMeans(mtry_test[,-1])
mtry_oob$oob_rmse <- rowMeans(mtry_oob[,-1])
mtry_train$train_rmse <- rowMeans(mtry_train[,-1])

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(mtry_test,"stream-temperature-rf/rf_model_tuning/hyperparameter_tuning/mtry_test_thermalsens_all.csv")
write.csv(mtry_oob,"/stream-temperature-rf/rf_model_tuning/hyperparameter_tuning/mtry_oob_thermalsens_all.csv")
write.csv(mtry_train,"/stream-temperature-rf/rf_model_tuning/hyperparameter_tuning/mtry_train_thermalsens_all.csv")

# RMSE_df for plotting
RMSE_df <- data.frame(mtry = mtry_test$mtry,test_RMSE = mtry_test$test_rmse, train_RMSE = mtry_train$train_rmse)

# # Plot RMSE vs mtry
# ggplot(RMSE_df, aes(x = mtry)) + 
#   geom_line(aes(y=test_RMSE)) + 
#   geom_line(aes(y=train_RMSE), col='red') + 
#   ylab('RMSE') + ylim(c(0.05,.2)) +
#   ggtitle('AT-ST Slope All Sites: Full Year')


####







