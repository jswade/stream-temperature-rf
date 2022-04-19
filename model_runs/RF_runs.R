# Using RF_func, run RF on all subsets for Max ST and Thermal Sensitivity (Slope)

## CHANGE FILE DIRECTORY
# Import RF function
source("/stream-temperature-rf/model_runs/RF_func.R")


# Create array of month strings for saving to file
mon_list <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


##### Max ST - All ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                     test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                     test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                     test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                     test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                     test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))

# Loop through months
for (i in seq(1,12)){
  
  # Run RF function
  rf_temp <- rf_func('Max', i, 'All')
  
  # Store crf object
  rf_crf <- rf_temp$crf
  
  # Store importance csv
  rf_imp <- rf_temp$imp
  
  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/AllSites/MaxST_all/crf_",mon_list[i],"max_all.rds",sep="")
  
  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/AllSites/MaxST_all/",mon_list[i],"max_all.csv",sep="")
  
  # Save crf obj
  saveRDS(rf_crf,crf_filepath)
  
  # Write imp to csv
  write.csv(rf_imp, imp_filepath)
  
  
  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)
  
  
  
  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)

  
  
  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]
  
  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]
  
  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]
  
}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/AllSites/MaxST_all/max_all_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/AllSites/MaxST_all/cv1_max_all_error.csv")

####



##### Max ST - Dam ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Max', i, 'Dam')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp
  
  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/MaxST_dam/crf_",mon_list[i],"max_dam.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/MaxST_dam/",mon_list[i],"max_dam.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)


  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/Dam_Subset/MaxST_dam/max_dam_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/Dam_Subset/MaxST_dam/cv1_max_dam_error.csv")

####


##### Max ST - No Dam ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){
  
  # Run RF function
  rf_temp <- rf_func('Max', i, 'NoDam')
  
  # Store crf object
  rf_crf <- rf_temp$crf
  
  # Store importance csv
  rf_imp <- rf_temp$imp
  
  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/MaxST_nodam/crf_",mon_list[i],"max_nodam.rds",sep="")
  
  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/MaxST_nodam/",mon_list[i],"max_nodam.csv",sep="")
  
  # Save crf obj
  saveRDS(rf_crf,crf_filepath)
  
  # Write imp to csv
  write.csv(rf_imp, imp_filepath)
  
  
  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)
  
  
  
  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)
  
  
  
  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]
  
  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]
  
  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]
  
}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/Dam_Subset/MaxST_nodam/max_nodam_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/Dam_Subset/MaxST_nodam/cv1_max_nodam_error.csv")

####





##### Slope - All ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){
  
  # Run RF function
  rf_temp <- rf_func('Slope', i, 'All')
  
  # Store crf object
  rf_crf <- rf_temp$crf
  
  # Store importance csv
  rf_imp <- rf_temp$imp
  
  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/AllSites/Slope_all/crf_",mon_list[i],"slope_all.rds",sep="")
  
  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/AllSites/Slope_all/",mon_list[i],"slope_all.csv",sep="")
  
  # Save crf obj
  saveRDS(rf_crf,crf_filepath)
  
  # Write imp to csv
  write.csv(rf_imp, imp_filepath)
  
  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)
  
  
  
  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)
  
  
  
  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]
  
  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]
  
  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]
  
}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/AllSites/Slope_all/slope_all_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/AllSites/Slope_all/cv1_slope_all_error.csv")

####


##### Slope - Dam ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Slope', i, 'Dam')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp

  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/Slope_dam/crf_",mon_list[i],"slope_dam.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/Slope_dam/",mon_list[i],"slope_dam.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)

  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/Dam_Subset/Slope_dam/slope_dam_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/Dam_Subset/Slope_dam/cv1_slope_dam_error.csv")

####


##### Slope - No Dam ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){
  
  # Run RF function
  rf_temp <- rf_func('Slope', i, 'NoDam')
  
  # Store crf object
  rf_crf <- rf_temp$crf
  
  # Store importance csv
  rf_imp <- rf_temp$imp
  
  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/Slope_nodam/crf_",mon_list[i],"slope_nodam.rds",sep="")
  
  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/Dam_Subset/Slope_nodam/",mon_list[i],"slope_nodam.csv",sep="")
  
  # Save crf obj
  saveRDS(rf_crf,crf_filepath)
  
  # Write imp to csv
  write.csv(rf_imp, imp_filepath)
  
  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)
  
  
  
  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)
  
  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)
  
  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)
  
  
  
  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]
  
  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]
  
  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]
  
}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/Dam_Subset/Slope_nodam/slope_nodam_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/Dam_Subset/Slope_nodam/cv1_slope_nodam_error.csv")

####


##### Max ST - HUC0102 ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Max', i, 'HUC0102')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp

  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc0102/crf_",mon_list[i],"max_huc0102.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc0102/",mon_list[i],"max_huc0102.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)

  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc0102/max_huc0102_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc0102/cv1_max_huc0102_error.csv")

####



##### Max ST - HUC03 ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Max', i, 'HUC03')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp

  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc03/crf_",mon_list[i],"max_huc03.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc03/",mon_list[i],"max_huc03.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)


  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc03/max_huc03_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc03/cv1_max_huc03_error.csv")

####


##### Max ST - HUC17 ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Max', i, 'HUC17')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp

  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc17/crf_",mon_list[i],"max_huc17.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc17/",mon_list[i],"max_huc17.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)


  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc17/max_huc17_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/HUC2_Subset/MaxST_huc17/cv1_max_huc17_error.csv")

####






##### Slope - HUC0102 ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Slope', i, 'HUC0102')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp

  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc0102/crf_",mon_list[i],"slope_huc0102.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc0102/",mon_list[i],"slope_huc0102.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)

  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc0102/slope_huc0102_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc0102/cv1_slope_huc0102_error.csv")

####

##### Slope - HUC03 ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Slope', i, 'HUC03')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp

  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc03/crf_",mon_list[i],"slope_huc03.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc03/",mon_list[i],"slope_huc03.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)

  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc03/slope_huc03_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc03/cv1_slope_huc03_error.csv")

####
##### Slope - HUC17 ####

# Create dataframe to store mean and sd of model errors
errordf <-  data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                       test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                       test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12),
                       test_R2_sd = rep(0,12), train_R2_sd = rep(0,12), oob_R2_sd = rep(0,12),
                       test_rmse_sd = rep(0,12), train_rmse_sd = rep(0,12), oob_rmse_sd = rep(0,12),
                       test_nrmse_sd = rep(0,12), train_nrmse_sd = rep(0,12), oob_nrmse_sd = rep(0,12))

# Create dataframe to store model errors from cv1 (variable importance model)
cv1_error <- data.frame(test_R2 = rep(0,12), train_R2 = rep(0,12), oob_R2 = rep(0,12),
                        test_rmse = rep(0,12), train_rmse = rep(0,12), oob_rmse = rep(0,12),
                        test_nrmse = rep(0,12), train_nrmse = rep(0,12), oob_nrmse = rep(0,12))


# Loop through months
for (i in seq(1,12)){

  # Run RF function
  rf_temp <- rf_func('Slope', i, 'HUC17')

  # Store crf object
  rf_crf <- rf_temp$crf

  # Store importance csv
  rf_imp <- rf_temp$imp

  ## CHANGE FILE DIRECTORY
  # Create file path for crf obj
  crf_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc17/crf_",mon_list[i],"slope_huc17.rds",sep="")

  ## CHANGE FILE DIRECTORY
  # Generate file path for rf_imp
  imp_filepath <- paste("/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc17/",mon_list[i],"slope_huc17.csv",sep="")

  # Save crf obj
  saveRDS(rf_crf,crf_filepath)

  # Write imp to csv
  write.csv(rf_imp, imp_filepath)

  # Aggregate R2 into dataframe
  errordf$test_R2[i] <- mean(rf_temp$test_err$test_R2)
  errordf$train_R2[i] <- mean(rf_temp$train_err$train_R2)
  errordf$oob_R2[i] <- mean(rf_temp$oob_err$oob_R2)

  # Aggregate rmse into dataframe
  errordf$test_rmse[i] <- mean(rf_temp$test_err$test_rmse)
  errordf$train_rmse[i] <- mean(rf_temp$train_err$train_rmse)
  errordf$oob_rmse[i] <- mean(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse into dataframe
  errordf$test_nrmse[i] <- mean(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse[i] <- mean(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse[i] <- mean(rf_temp$oob_err$oob_nrmse)



  # Aggregate R2 sd into dataframe
  errordf$test_R2_sd[i] <- sd(rf_temp$test_err$test_R2)
  errordf$train_R2_sd[i] <- sd(rf_temp$train_err$train_R2)
  errordf$oob_R2_sd[i] <- sd(rf_temp$oob_err$oob_R2)

  # Aggregate rmse sd into dataframe
  errordf$test_rmse_sd[i] <- sd(rf_temp$test_err$test_rmse)
  errordf$train_rmse_sd[i] <- sd(rf_temp$train_err$train_rmse)
  errordf$oob_rmse_sd[i] <- sd(rf_temp$oob_err$oob_rmse)

  # Aggregate nrmse sd into dataframe
  errordf$test_nrmse_sd[i] <- sd(rf_temp$test_err$test_nrmse)
  errordf$train_nrmse_sd[i] <- sd(rf_temp$train_err$train_nrmse)
  errordf$oob_nrmse_sd[i] <- sd(rf_temp$oob_err$oob_nrmse)



  # Aggregate cv1 R2 into dataframe
  cv1_error$test_R2[i] <- rf_temp$test_err$test_R2[1]
  cv1_error$train_R2[i] <- rf_temp$train_err$train_R2[1]
  cv1_error$oob_R2[i] <- rf_temp$oob_err$oob_R2[1]

  # Aggregate cv1 rmse into dataframe
  cv1_error$test_rmse[i] <- rf_temp$test_err$test_rmse[1]
  cv1_error$train_rmse[i] <- rf_temp$train_err$train_rmse[1]
  cv1_error$oob_rmse[i] <- rf_temp$oob_err$oob_rmse[1]

  # Aggregate cv1 nrmse into dataframe
  cv1_error$test_nrmse[i] <- rf_temp$test_err$test_nrmse[1]
  cv1_error$train_nrmse[i] <- rf_temp$train_err$train_nrmse[1]
  cv1_error$oob_nrmse[i] <- rf_temp$oob_err$oob_nrmse[1]

}

## CHANGE FILE DIRECTORY
# Write error metrics to csv
write.csv(errordf,"/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc17/slope_huc17_error.csv")
write.csv(cv1_error,"/stream-temperature-rf/data_results/HUC2_Subset/Slope_huc17/cv1_slope_huc17_error.csv")

####