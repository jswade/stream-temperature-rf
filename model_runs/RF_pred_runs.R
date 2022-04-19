# Retrieves training set predictions and observed values from RF models for Max/Slope and all subsets

## CHANGE ALL FILE DIRECTORIES IN SCRIPT

# Source RF_pred_func.R file
source("/stream-temperature-rf/model_runs/RF_pred_func.R")


## Max All

out_df <- rf_pred_func('Max','All')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/AllSites/max_all_pred.csv')


## Max Dam

out_df <- rf_pred_func('Max','Dam')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/Dam_Subset/max_dam_pred.csv')

## Max No Dam

out_df <- rf_pred_func('Max','NoDam')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/Dam_Subset/max_nodam_pred.csv')


## Max HUC0102

out_df <- rf_pred_func('Max','HUC0102')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/HUC02_Subset/max_huc0102_pred.csv')


## Max HUC03

out_df <- rf_pred_func('Max','HUC03')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/HUC02_Subset/max_huc03_pred.csv')

## Max HUC17

out_df <- rf_pred_func('Max','HUC17')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/HUC02_Subset/max_huc17_pred.csv')

## Slope All

out_df <- rf_pred_func('Slope','All')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/AllSites/slope_all_pred.csv')

## Slope Dam

out_df <- rf_pred_func('Slope','Dam')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/Dam_Subset/slope_dam_pred.csv')

## Slope No Dam

out_df <- rf_pred_func('Slope','NoDam')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/Dam_Subset/slope_nodam_pred.csv')

## Slope HUC0102

out_df <- rf_pred_func('Slope','HUC0102')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/HUC02_Subset/slope_huc0102_pred.csv')

## Slope HUC03

out_df <- rf_pred_func('Slope','HUC03')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/HUC02_Subset/slope_huc03_pred.csv')

## Slope HUC17

out_df <- rf_pred_func('Slope','HUC17')

write.csv(out_df,'/stream-temperature-rf/data_results/Predictions/HUC02_Subset/slope_huc17_pred.csv')