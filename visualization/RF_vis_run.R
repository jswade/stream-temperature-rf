# Run Random Forest data visualization script

# This function will generate:
  # Spearman Circle Importance Heatmaps
  # Grouped Importance Charts
  # Text heatmaps
  # CSVs of aggregated importance

## CHANGE FILE DIRECTORY
# Import RF visualization function
source("/stream-temperature-rf/visualization/RF_visfunc.R")


# Run data vis:

# Max All
rf_visfunc('Max','All')

# Max Dam
rf_visfunc('Max','Dam')

# Max No Dam
rf_visfunc('Max','NoDam')


# Slope All
rf_visfunc('Slope','All')

# Slope Dam
rf_visfunc('Slope','Dam')

# Slope No Dam
rf_visfunc('Slope','NoDam')


# Max HUC
rf_visfunc('Max','HUC0102')

# Max Dam
rf_visfunc('Max','HUC03')

# Max No Dam
rf_visfunc('Max','HUC17')


# Slope HUC
rf_visfunc('Slope','HUC0102')

# Slope Dam
rf_visfunc('Slope','HUC03')

# Slope No Dam
rf_visfunc('Slope','HUC17')