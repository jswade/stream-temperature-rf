# Identifying controls on stream temperature behavior using random forest (RF) models

Water temperature is vitally important to the health of rivers and streams, influencing the integrity of ecosystems, the processing of solutes, and the habitability of waterways for sensitive aquatic species. To better understand the underlying drivers of stream temperature variability, we apply a suite of random forest models to identify the most influential climate and landscape predictions of thermal regimes. We quantify the controls of monthly maximum water temperatures and thermal sensitivity (defined as the linear slope between air temperature and water temperature) across 410 watersheds in the conterminous US. All data used in this analysis is publicly available and all code used is made available within this repository.

![Conceptual depiction of the complex nature of the controls of stream temperature regimes. Random forest predictor variables selected from the GAGES-II dataset are displayed in their respective categories. The two quantitative stream temperature signatures, monthly maximum temperature and thermal sensitivity, arise from the nested interactions of climate, watershed structure, hydrology, and human impacts.](visualization/figures/ConceptualMethods/Conceptual%20Methods%20Figure.pdf "Conceptual RF Methods")

## Contents

-   **./data_download**: Download records of air temperature (PRISM), discharge (USGS NWIS), and water temperature (USGS NWIS) from 2016 to 2020 from selected sites.

-   **./data_raw**: Compile raw data used as inputs for random forest models.

-   **./data_results**: Output of random forest models, including model *.rds* objects, predictions, residuals, errors, variable importance values, and correlations.

-   **./metric_calculation**: Calculation of maximum temperatures and thermal sensitivities at each site.

-   **./model_runs**: Primary R scripts to run RF models.

-   **./rf_model_tuning**: Optimization of RF model hyperparameters (*mtry*) before final model runs.

-   **./site_classification**: Unique site identifiers and stratified sampling procedure.

-   **./site_id**: Identify sites in the GAGES-II dataset with available stream temperature records.

-   **./spearman**: Run Spearman's rank correlation between predictors and temperature metrics.

-   **./visualization**: Generate figures from random forest model results.

## Procedure

All code is written in R. File references are relative to the current repository and should be changed to match your local directory. Additional information on each step of our analysis is available within individual repository folders.

To recreate our analysis, heed the following instructions:

1.  Run *GAGES_StreamTemp_Sites.R* in **./site_id** to identify sites used in the analysis.

2.  Within **./data_download**, run the following scripts:

    -   **./AirTemperature**: *PRISM_data_formatting.R* to extract daily air temperature at each site.

    -   **./Discharge**: *Discharge_DataDownload.R* to download daily USGS discharge data at each site.

    -   **./StreamTemperature**: *ST_DataDownload.R* to download daily USGS ST data at each site.

3.  Run *max_calc.R* and *thermal_sensitivity_calc.R* in **./metric_calculation** to calculate ST metrics. *(Note: Thermal Sensitivity and Slope are used interchangeably in this repository)*

4.  View **./StratifiedSampling** in **./site_classification** to confirm manual-determined stratified sampling groups.

5.  Run *RF_runs_hyperparameter_tuning.R* in **./rf_model_tuning** to optimize *mtry* in RF models.

6.  Run *RF_runs.R* and *RF_pred_runs.R in* **./model_runs** to fit monthly RF models to different combinations of metrics and sites.

7.  Perform Spearman's rank correlation between predictors and metrics using *RF_spearman.R* scripts in **./spearman**.

8.  Generate final figures using *RF_vis_run.R* in **./visualization**. *(Note: Many figures were altered for publication using Adobe Illustrator. Where applicable, Illustrator files are included.)*
