In this folder is all the code for running the GP comparisons.

Folders:

1. GPC_Runfiles: The files that are used to run the individual packages. Very important.

2. Scratch: scratchwork, some useful.

3. Deletethis: can be deleted

Files: 

1. GPC_Main.R: This is the most important file of everything. It holds the main code for creating the comparisons. When its functions are run it creates the data, sends the data to Python, Matlab, etc, and gathers back the results for comparison.

2. GPC_Main_OldRunFunctions.R: old functions that were used to run comparisons

3. GPC_Submit.R: This is where the function calls are actually placed. This file is run when submitted to the server, so only the function you want to run should be uncommented.

4. GPC_Submit.pbs: This is the pbs file to submit to run jobs on the server. You should change the time needed to run the file depending on how long you think it will take. Under ten minutes to just compare. GPfit is by far the slowest, mlegp next slowest. Also need to account for number of reps, number of sample sizes, etc. Never took more than 3 days, usually less than 1, often just hours.

5. GPC_TestFunctions.R: This contains test functions which can be run by GPC_Main.R since it is sourced. Inputs should be a single vector, not separated arguments.

6. GPC_Visualize.R: There are some functions here to visualize the results from a bunch of different test. Can be used to look at hundreds of reps, plots are most useful for scattering RMSE v PRMSE  and RMSE v best RMSE to get an idea of which package is best.