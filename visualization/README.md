# Visualization

This folder contains scripts used to generate a series of figures used to interpret RF model results.

-   **RF_visfunc.R**: A function that acts as the base for figure creation. The script takes in a user-defined model configuration (subset of sites and ST metric), plot several figures using ggplot2, and saves figures to PDF in **./figures**.

-   **RF_vis_run.R**: A script that supplies various combinations of site subsets and ST metrics to generate full set of figures.

Please note that although we export raw figures from ggplot2 as PDFs, we modified these files in Adobe Illustrator into the final published figures. Where possible, we provide Illustrator files to supplement the presented code.

-   **./figures**: Folder containing the following figures:

    -    **./CompiledFigures**: Compilation of all figures used in the analysis.

    -   **./ConceptualMethods**: Graphical depiction of steps used in model fitting and interpretation.

    -   **./ConceptualWatershed**: Graphical depiction of model predictors used and their role in forcing stream temperature regimes.

    -   **./GroupedImportance**: Variable importance metrics aggregated by predictor category over time.

    -   **./ImportanceValues**: CSV backups of importance metrics, aggregated across all months.

    -   **./ModelError_5fold**: Model errors for each subset and metric combination, aggregated across 5-fold cross-validation runs.

    -   **./ModelError_CV1**: Model error for CV1 (used in final models to generate variable importance metrics).

    -   **./SiteFigure**: Figure displaying the locations of selected watersheds.

    -   **./SpatialMetrics**: Figure displaying geographic distribution of stream temperature metrics in selected months.

    -   **./SpearmanMaps**: Primary interpretive figure, displaying how variable importance varies within and across months.

    -   **./TextHeatmaps**: Figure similar to **./SpearmanMaps**, except in text form to enable manuscript writing.
