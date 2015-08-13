Regional Sensitivity Analysis (RSA)

The code provides functions to run regional sensitivity analysis in R. With a supplied input file of model parameter sets, 
and associated performanc metrics (or signature values) the code produces:

- Posterior parameter CDF plots, plotted over the prior range, for different (user defined) behavioural thresholds 
- Plot of the maximum difference between prior and posterior CDFs for all parameters, as a function of behavioural threshold
- Parallel coordinate plot of model parameters, grouped by behavioural threshold
- Parameter interaction plot. spearman's rank correlation coefficient between parameters is plotted as a function of the
  behavioural threshold applied
  
Plotting and calculation of plot metrics are executed in separate functions, such that the raw results can be saved, or plotted 
differently.

Files:

exampleData.txt - example model output from the Hymod rainfall runoff model used as input to the workflow example
SAFunctions.R - source functions used to run regional sensitivity analysis
workflowSensitivityAnalysis.R - workflow that calls in exampleData.txt and runs SAFunctions.R to produce example output

R libraries required:

library(ggplot2)
library(reshape2)
library(GGally)
library(RColorBrewer)
library(grid)

