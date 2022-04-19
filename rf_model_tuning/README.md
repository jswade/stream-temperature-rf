# Hyperparameter Tuning

This folder contains scripts to tune model hyperparameters to acceptable values before final RF runs. The hyperparameter *mtry* was tuned for both MaxST and Thermal Sensitivity metrics for all months. Sites were first divided into a training set (80%) and test set (20%) using stratified sampling based on geographic regions. The training data was then used in a 5-fold stratified cross-validation scheme to iteratively tune *mtry*. We found that RMSE values decreased with increasing *mtry* values, though decreases tapered off after *mtry* exceeded 10. We chose to use an *mtry* = 10 for final model runs across all metrics, months, and subsets for simplicity.

-   **RF_func_hyperparameter_tuning.R**: Base function to tune *mtry* hyperparameter. Does not need to be run.

-   **RF_runs_hyperparameter_tuning.R**: Applies previous function to tune *mtry* hyperparameter and exports error metrics to **./hyperparameter_tuning**.

-   **./hyperparameter_tuning**: Folder containing out-of-bag, training set, and test set prediction errors for MaxST and Thermal Sensitivity across months.
