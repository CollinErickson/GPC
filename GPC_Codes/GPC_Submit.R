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
#RGPP2_D2_B.7( stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D4_B.7( stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D6_B.7( stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D2_B1.3(stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D4_B1.3(stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')
#RGPP2_D6_B1.3(stepstorun=3, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=T,external.fits='JMP')

RGPP2_D2_B.7( stepstorun=2, GPfit.powers=c(2), laGP.nuggets=c('E',1e-6),laGP.nuggets.names=c('E',6),JMP.include=F, Python.include=T, GPy.include=F, GPfit.include=F, mlegp.include=F, Dice.include=F, laGP.include=F, DACE.include=F)


timestamp()
print(getwd())
timestamp()