This folder contains the files that are run by software other than R, run through command line calls.

Folders:

1. DACE: contains the DACE files for Matlab, this is needed to run DACE.

2. GPY, GPy-master: I actually think these aren't needed. I think GPy is installed on the server, I should really figure this out and delete this is it isn't used.

Files:

1. filesToRun(package).csv: GPC_Main.R writes out the files that each package needs to run and where they should save. So these files pass the instructions to the GPC_(package).* files

2. GPC_(package).* : The actual file that runs each of the other packages to make their fits and predictions. These are called in GPC_Main.R through the command line.