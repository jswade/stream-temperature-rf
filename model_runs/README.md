# Model Runs

This folder contains two sets of scripts to fit a suite of random forest models using GAGES-II variables to predict two stream temperature metrics and evaluate predictor importance. Model runs are performed at a monthly time scale for maximum stream temperature and thermal sensitivity. Subsets of sites include: all sites, sites affected by major dams, sites unaffected by major dams, sites within the Northeast/Mid-Atlantic region (HUC01/HUC02), sites within the South Atlantic-Gulf region (HUC03), and sites within the Pacific Northwest region (HUC17).

-   **Final Model Runs**: **RF_func.R** is a base function used as a root for all RF model runs. The function takes in a user-defined model configuration (ST metric, month, and subset of sites), performs geographically stratified sampling to define a training and test set, fits a random forest model, and exports conditional permutation importance results to **./data_results**. **RF_runs.R** supplies inputs to the **RF_func.R** script to run and export results for each combination of metric, month, and subset of sites.

-   **Model Predictions**: **RF_pred_func.R** and **RF_pred_runs.R** follow a similar format to the model run scripts to export model predictions and corresponding observed values to **./data_results/Predictions**

![](visualization/figures/ConceptualMethods/Conceptual%20Methods%20Figure.png "Conceptual RF Methods")
**Graphical representation of the analytical steps used to develop RF models and assess the dominant drivers of stream temperature regimes through time.**
