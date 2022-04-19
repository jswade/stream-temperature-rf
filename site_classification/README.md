# Site Classification

This folder contains several spreadsheets used to classify sites into subsets for dam- and region-specific RF models, and to generate geographically stratified samples.

1.  **site_class.xlsx**: Contains USGS site numbers, site IDs, latitude, longitude, USGS HUC02 region, and major dam identifiers.

2.  **./StratifiedSampling:** Contains files used to manually assign stratified sample groupings based on geography. Stratified sampling used in final RF models ensures that training and test sets contain equivalent proportions of sites located within different geographic regions.

    -   **huc_count_2016_2020.xlsx**: Creates 7 groupings of HUC02 regions such that each combined region has a roughly equivalent number of sites.

    -   **HUC_stratifiedsampling.csv:** Assigns groupings to sites to enable geographic stratified sampling.
