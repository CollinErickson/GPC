# This file is submitted by GPC_Submit.pbs
# Only the function calls you want to run should be uncommented.
# This file contains all of the function calls I've used. 
#   It's an easy way to go back and rerun things or saw what I ran before.

timestamp()
setwd('~/Research/GPC/GPC_Codes')
source('./GPC_Main.R')

# qsub <- function() {system('qsub /sscc/home/c/cbe117/Research/GPC/GPC_Codes/GPC_Submit.pbs')}
# pbstat <- function() {system('pbstat')}
# system('qsub /sscc/home/c/cbe117/Research/GPC/GPC_Codes/GPC_Submit.pbs -a 0254') # submit at a certain time


# Redoing everything, including JMP separately
#Borehole1357_03(stepstorun=3,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#Borehole03(stepstorun=3,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#OTLCircuit2(stepstorun=3, laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#Detpep108d2(stepstorun=3, laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP', GPfit.include=T)
# RGPP2_D2_B.7( stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D4_B.7( stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D6_B.7( stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D2_B1.3(stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D4_B1.3(stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D6_B1.3(stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')

# Jan 2017 adding Matern
# RGPP2_D2_B.7( stepstorun=1:3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# OTLCircuit2( stepstorun=1:3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=F, mlegp.include=F, Dice.include=T, laGP.include=T, DACE.include=T)
# Morris1( stepstorun=3:3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=F, mlegp.include=F, Dice.include=T, laGP.include=T, DACE.include=T)
# Borehole2740(stepstorun=1:3,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# Borehole03(stepstorun=2:3,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=T, GPy.include=F, GPfit.include=F, mlegp.include=F, Dice.include=F, laGP.include=T, DACE.include=F)

# Feb 2017 Runs for first revision, first time adding DK, redoing sklearn
# RGPP2_D2_B.7( stepstorun=1:3, GPfit.powers=c(1.95, 2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# RGPP2_D2_B1.3( stepstorun=1:3, GPfit.powers=c(1.95, 2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# RGPP2_D4_B.7( stepstorun=1:3, GPfit.powers=c(1.95, 2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# RGPP2_D4_B1.3( stepstorun=1:3, GPfit.powers=c(1.95, 2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# RGPP2_D6_B.7( stepstorun=1:3, GPfit.powers=c(1.95, 2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# RGPP2_D6_B1.3( stepstorun=1:3, GPfit.powers=c(1.95, 2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# Borehole1357_03(stepstorun=2:2,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=T, GPy.include=F, GPfit.include=F, mlegp.include=F, Dice.include=F, laGP.include=F, DACE.include=F)
# Borehole1357_03(stepstorun=1:3,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
Borehole03(stepstorun=2:2,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=F, GPy.include=T, GPfit.include=F, mlegp.include=F, Dice.include=F, laGP.include=F, DACE.include=F)
Borehole03(stepstorun=3:3,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T) # , reps.run=5)
# OTLCircuit2( stepstorun=2:2, laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=T, GPy.include=F, GPfit.include=F, mlegp.include=F, Dice.include=F, laGP.include=F, DACE.include=F)
# OTLCircuit2( stepstorun=1:3, laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# Detpep108d2(stepstorun=2:2,laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=F, GPy.include=F, GPfit.include=F, mlegp.include=T, Dice.include=F, laGP.include=T, DACE.include=F)
# Detpep108d2(stepstorun=1:3,GPfit.powers=c(1.95,2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F,external.fits='JMP', Python.include=T, GPy.include=T, GPfit.include=T, mlegp.include=T, Dice.include=T, laGP.include=T, DACE.include=T)
# Morris1( stepstorun=2:2, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=F, GPfit.include=F, mlegp.include=F, Dice.include=F, laGP.include=F, DACE.include=F)
# Morris1( stepstorun=3:3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=T, GPfit.include=F, mlegp.include=F, Dice.include=T, laGP.include=T, DACE.include=T)

timestamp()
print(getwd())
timestamp()