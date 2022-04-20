# Data Results

This folder contains the results of RF model runs, including predictions, residuals, errors, variable importance metrics, and Spearman's rank correlations.

-   **Complete Model Results**: Model results are organized into folders based on the subset of sites used and ST metric modeled (**./AllSites**, .**/Dam_Subset**, **.HUC2_Subset**). For example, results for models using all sites to model Maximum STs are located in **./AllSites/MaxST_all**. Each of these subfolders contains 12 monthly .csv files that store conditional permutation importance results (ex: **Janmax_all.csv)** along with 12 .rds objects that store each model's random forest R object (ex: **crf_Janmax_all.rds)**. The .rds files can be loaded into R to circumvent rerunning RF models (which can take substantial time).

-   **Model Predictions**: **./Predictions** contains .csv files of model predictions at each site for each subset of sites and ST metric.

-   **Model Residuals**: **./Residuals** contains model residuals (calculated as Observed - Predicted) for each site.

-   **Model Error**: **./RF_Error** contains training set, test set, and out-of-bag errors for model predictions. Each subfolder contains two files:

    1.  **cv1_max_all_error.csv**: error of first cross-validation set (used to calculate variable importance)

    2.  **max_all_error.csv**: mean and standard deviation of error of all five cross-validation sets (used to assess model stability)

-   **Spearman's Rank Correlations**: **./SpearmanRank** contains correlations between predictors and stream temperature metrics for each month across each subset of sites.
