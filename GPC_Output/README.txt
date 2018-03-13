All of the output is stored in this folder. There is a separate folder for each run. A run is done for multiple packages, multiple sample sizes, and multiple replicates.

Inside each folder is:

1. A folder for each sample size. These contain:

  a. OPPs (folder): Original Point Predictions. Each package is asked to predict on the input points. These should generally have zero error and zero variance. These were never really used.

  b. Params (folder)

  c. Plots (folder)

  d. RunFiles (folder)

  e. RunTimes (folder)

  f. The actual output for each package for each rep (*.csv)

2. Summary plots that have results for all sample sizes.

3. OutputTable.csv: This summary table contains all of the main data from the run. It is created in the comparison function, can easily be read into R or Excel to analyze or plot results across different sample sizes or even runs.