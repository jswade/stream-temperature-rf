# Metric Calculation

This folder contains scripts to calculate two monthly metrics of stream temperature behavior, maximum water temperature and thermal sensitivity (linear slope between air and water temperatures). Note that 'thermal sensitivity' and 'slope' may be used interchangeably in code.

1.  **max_calc.R**: Summarizes records at each site into average maximum monthly temperatures across 4 years of data, exporting results to **STmax.csv**.

2.  **thermalsensitivity.R**: Calculates the average slope of the monthly linear relationship between daily air and water temperatures across 4 years of data, exproting results to **thermalsensitivity.csv**.
