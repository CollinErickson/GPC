# This is the main file of GPC. It does all parts of comparing the various package.
# There are three main functions in this file:
#  1. comparison.create.data: Creates the data for comparison. Create files telling the non-R packages 
#     (GPy, DACE, scikit-learn) what files they need to run.
#  2. comparison.run: This actually runs all of the comparisons. 
#     R packages are run in the function, the others are called through command line functions.
#  3. comparison.compare: This reads in the outputs of each package, makes plots, and 
#     creates the OutputTable.csv. This step is often called by itself to just compare the results of things already run.
# The other important functions combine the above functions:
#  4. comparison.all: Runs all three above functions (or a subset). This makes it much easier.
#     This is always better to use than running the above 3 separately. This only runs for a single configuration
#     (eg only 1 sample size), so use the next one to run a bunch.
#  5. comparison.all.batch: This is really the one that should be used by the user, not the above 4. 
#     It takes in the input configuration (can be multiple sample sizes, etc), and runs the steps told to.
# The last 700 lines are just functions that give specific input configurations to be run. These functions will be called
# by GPC_Submit.R, and are helpful to keep track of past run configurations. These should probably all be moved to their
# own files (the above functions should probably also be separated) but it hasn't been done yet.
#
# QUICKSTART (you just want to run something):
#  1: Check the bottom of this file to make sure the function you will run looks correct.
#  2: If you want to run just on the host, just run it.
#  3: If submitting as a job, make sure only the function you want to run is called in GPC_Submit.R (others commented out).
#  4: Make sure the run time is right in GPC_Submit.pbs
#  5: In the command line navigate to the proper folder then enter "qsub GPC_Submit.R". 
#     If in R, "system('qsub /sscc/home/c/cbe117/Research/GPC/GPC_Codes/GPC_Submit.pbs')".


# To do list
# Add GPML
# Add qqplot again
# Give seeds
# Save/compare params
# Add noise
# LHS vs minimax/maximin

# Change file names and locations
# Add loading file
# Have run files list be queue so they can be split up
# Switch to GGPlot or Lattice
# Save images without showing


# Need a file that says where everything is located
FileDetails <- read.csv('~//Research//GPC//GPC_FileDetails.csv',stringsAsFactors=F)
MainFolderPath <- FileDetails$GPC_folder
CodeFolderPath <- paste0(MainFolderPath,'GPC_Codes//')
RunFilesFolderPath <- paste0(CodeFolderPath,'GPC_RunFiles//')
OutputFolderPath <- paste0(MainFolderPath,'GPC_Output//')
setwd(MainFolderPath)

# Load libraries
require(GPfit)
require(mlegp)
require(DiceKriging)
#require(lhs) # 1/11/17 removing this for MaxPro
require(MaxPro) # 1/11/17 adding to replace lhs
require(png)
require(grid)
require(knitr)
require(plyr)
require(MASS)
require(laGP)
#require(lattice)
#require(htmlTable)
#source('C://Users//cbe117//School//DOE//JMPvsGPfit//GPC_TestFunctions.R')
source(paste0(CodeFolderPath,'GPC_TestFunctions.R')) # This sources the test functions
#rmse.func <- function(r1,r2){sqrt(mean((r1-r2)^2))}


# Function that returns a bunch of files names for a batch
get.file.names <- function(path.batch,batch.name,reps,fit.name,pre=NULL,post=NULL,subfolder=NULL) {
  file.names.preds <- c()
  prestring = ''
  if (!is.null(pre)) {if(!(pre=="")) {prestring = paste0("_",pre)}}
  poststring = ''  # "Preds"
  if (!is.null(post)) {if(!(post=="")) {poststring = paste0("_",post)}}
  subfolderstring = ''
  #if (!is.null(subfolder)) {if(!(subfolder=="")) {subfolderstring = paste0(subfolder,"\\") }}
  if (!is.null(subfolder)) {if(!(subfolder=="")) {subfolderstring = paste0(subfolder,"//") }}
  #print('Changed gsub here 52723752')  

  for (i in 1:reps) {
    file.name.pred <- paste0(path.batch,subfolderstring,batch.name,prestring,"_",i,poststring,'_',fit.name,'.csv')
    #file.names.preds <- c(file.names.preds,gsub("//","\\\\",file.name.pred))
    file.names.preds <- c(file.names.preds,file.name.pred)
    #print('Changed gsub here 37235982732')
  }
  return(file.names.preds)
}

# Function that gives sqrt if non-neg, else NA, prevents me from getting errors
sqrtNA <- Vectorize(function(xx){ifelse(xx>=0,sqrt(xx),NA)})




####### Begin comparison.create.data
# First major function. Creates the data needed for the run, #
# writes out telling other packages what they need to run.
# Runs are done in next function
comparison.create.data <- function(path.base=OutputFolderPath,
                                   run.files.folder=RunFilesFolderPath,
                                   batch.name,reps,input.dim,input.ss,pred.ss,
                                   seed.start=0,seed.preds=100,seed.fit=200,
                                   func,func.string=NULL,
                                   DACE.meanfuncs,DACE.corrfuncs,
                                   external.fits=c()
                                   ) {
  # Writes the input data and instructions for packages
  # Parameters are all explained in comparison.all.batch
  # Output: Creates files with data and instructions. Returns nothing
  
  # Create path for the batch. 
  # I think this gives me problems when I pass in a folder as batch.name, should fix
  path.batch = paste0(path.base,batch.name,"//")
  
  # Create the directories. Suppress warnings.
  # Couldn't find way to check for directory first.
  dir.create(path.batch,showWarnings=F)  # dangerous ?!?
  dir.create(paste0(path.batch,"//","Params","//"),showWarnings=F)
  dir.create(paste0(path.batch,"//","OPPs","//"),showWarnings=F)
  dir.create(paste0(path.batch,"//","Plots","//"),showWarnings=F)
  dir.create(paste0(path.batch,"//","RunTimes","//"),showWarnings=F)
  dir.create(paste0(path.batch,"//","RunFiles","//"),showWarnings=F)
  
  seed.set <- seed.start
  
  # Added 2/21, scales outputs to be in [-.5,.5]
  standardize <- TRUE
  
  # Set the function to be used as funcToApply
  funcToApply <- NULL 
  if (is.null(func.string)) {funcString='No funcString given'} else {funcString=func.string}
  #funcString='No funcString given'
  if (is.function(func)){
    funcToApply <- func
  } else if(is.character(func)) {
    funcString=func
    if (func == 'ackley') funcToApply=ackley
    else if (func == 'ackley2') funcToApply=ackley2
    else if (func == 'phughes') funcToApply=phughes
    else if (func == 'rastrigin.unit' | func == 'rast.u' | func == 'r.u') funcToApply=rastrigin.unit
    else if (func == 'otlcircuit' | func == 'OTLCircuit' | func == 'OTL.Circuit') funcToApply=otlcircuit
    else if (func == 'braninsc' | func == 'Braninsc' | func == 'BraninSc') funcToApply=braninsc
    else if (func == 'detpep108d' | func == 'detpep' | func == 'DetPep') funcToApply=detpep108d
    else if (func == 'franke2d' | func == 'Franke2d' | func == 'Franke2D') funcToApply=franke2d
    else if (func == 'limetal02pol' | func == 'limetal' | func == 'lim') funcToApply=limetal02pol
    else if (func == 'fried' | func == 'Fried' | func == 'Friedman') funcToApply=fried
    else {stop("String given for function, but function not known")}
  } else if (is.list(func)) {
    if(func[[1]]=='RGP.points') { # okay
      funcString <- paste('RGP.points',input.dim,input.ss,pred.ss,func$beta,func$corr.power,seed.set)
    }
    else {stop('List given for function, but not of allowed type')}
  } else {print(func);print(str(func));stop("Function given is neither function nor known string")}
  #print(funcToApply)

  
  # begin report, writes a R markdown file, I don't even use it
  report.path <- paste0(path.batch,'Report.Rmd')
  add.to.report <- function(...,append=T,sep.paste=' '){cat(paste(...,sep=sep.paste),file=report.path,append=append,sep='\n\n')}
  atr <- function(...){add.to.report(...)}
  add.to.report('',append=F)
  add.to.report('---\ntitle: "Report"\noutput: html_document\n---\n\n')
  add.to.report('# Input\n')
  atr('Input | Value')
  atr('------------------- | ----------------')
  add.to.report('Function | ',funcString)
  atr('Input dimension | ',input.dim)
  atr('Design sample size | ',input.ss)
  atr('Prediction sample size | ',pred.ss)
  
  
  file.names.write <- c()
  file.names.preds.write <- c()
  file.names.write.external <- c()
  file.names.preds.write.external <- c()
  
  xs <- list()
  ys <- list()
  xps <- list()
  ypas <- list()
  for (i in 1:reps) {
    if(is.function(funcToApply)) {
      set.seed(seed.set)
      seed.set <- seed.set + 1
      # Create and write out data
      #x <- lhs::maximinLHS(input.ss,input.dim) # 1/11/17 Removing for maxpro
      if (input.dim > 1 ) { # MaxPro is bad for 1D
        x <- MaxPro::MaxProLHD(n=input.ss,p=input.dim, total_iter=1e4)$Design # 1/11/17 Adding to replace lhs, 1e6 total_iter (default) is too slow, this is 13s for 250 pts, 54s for 500 pts
      } else {
        x <- lhs::maximinLHS(input.ss, input.dim)
      }
      y <- apply(x,1,funcToApply)
      
      set.seed(seed.preds)
      seed.preds <- seed.preds + 1
      #xp <- lhs::maximinLHS(pred.ss,input.dim) # 1/11/17 removing for maxpro
      if (input.dim > 1) {
        xp <- MaxPro::MaxProLHD(n=pred.ss,p=input.dim, total_iter=1e2)$Design # 1/11/17 replacing lhs. 2000pts takes 10 seconds with total_iter=1e2 but 90s for 1e3.
      } else {
        xp <- lhs::maximinLHS(pred.ss,input.dim) # 1/11/17 removing for maxpro
      }
      ypa <- apply(xp,1,funcToApply)
    } else if (is.list(func)) {
      if(func[[1]]=='RGP.points') {#browser()
         temp.list<- RGP.points(d=input.dim,n=input.ss,np=pred.ss,betas=func$betas,corr.power=func$corr.power,seed=seed.set)
         x <- temp.list$x
         xp <- temp.list$xp
         y <- temp.list$y
         ypa <- temp.list$ypa
      }
      seed.set <- seed.set + 1
      seed.preds <- seed.preds + 1 # Not even used
    }
    

    if (standardize) { # scale y
      meany <- mean(y)
      miny <- min(y)
      maxy <- max(y)
      #y <- (y - miny) / (maxy - miny) - 0.5 # These put it [-.5,.5]
      #ypa <- (ypa - miny) / (maxy - miny) - 0.5
      y <- (y - meany) / (maxy - miny) # These give it mean 0 and range 1, as suggested by Gramacy in tgp vignette
      ypa <- (ypa - meany) / (maxy - miny)
    }
    
    xs[[i]] <- x
    ys[[i]] <- y
    xps[[i]] <- xp
    ypas[[i]] <- ypa
    
    file.name.write <- paste0(path.batch,batch.name,"_",i,'.csv')
    #file.names.write <- c(file.names.write,gsub("//","\\\\",file.name.write))
    file.names.write <- c(file.names.write,file.name.write)
    #print('Changed gsub here 0923589235')
    file.name.preds.write <- paste0(path.batch,batch.name,"_",i,'_PredPts.csv')
    file.names.preds.write <- c(file.names.preds.write,file.name.preds.write)
    
    if (length(external.fits)>0) {
      # External runs use different file path
      # Just remove path.batch from file path
      file.name.write.external <- paste0(batch.name,"_",i,'.csv')
      file.names.write.external <- c(file.names.write.external,file.name.write.external)
      file.name.preds.write.external <- paste0(batch.name,"_",i,'_PredPts.csv')
      file.names.preds.write.external <- c(file.names.preds.write.external,file.name.preds.write.external)
    }
    
    write.csv(data.frame(x=x,y=y),file.name.write)
    write.csv(data.frame(x=xp,y=ypa),file.name.preds.write)
  }
  #
  if (F) { # Old way was to use same pred pts for all runs
    set.seed(seed.preds)
    xp = lhs::maximinLHS(pred.ss,input.dim)  #runif(200)
    ypa <- apply(xp,1,funcToApply)
    
    # Getting some error when writing out PredPts.csv on cluster, testing change here
    #print('Test change for cluster gsub PredPts.csv line 120 error 833237 CHANGING ALL GSUBS< NEED TO FIX ALL IF DOESNT WORK')
    #file.name.predin <- gsub("//","\\\\",    paste0(path.batch,batch.name,'_PredPts.csv')   )
    file.name.predin <-  paste0(path.batch,batch.name,'_PredPts.csv')  #gsub("//","\\\\",    paste0(path.batch,batch.name,'_PredPts.csv')   )
    write.csv(data.frame(x=xp,y=ypa),
              file.name.predin)
    print(paste('Wrote out PredPts to',file.name.predin))
  } else { # New has different pred pts for each run, specifically to use RGP.points. Need to change run files for R, Python, Matlab.

     # Moved it all to above right next to x and y
  }
  
  
  # For each package, write out their files to run.
  
  # write file for JMP
  JMP.df.out <- data.frame(names=file.names.write,
                        #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                        preds.in=file.names.preds.write,
                        preds2NN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2NN",post="Preds"),#file.names.JMP2NN.preds,
                        preds2WN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2WN",post="Preds"),#file.names.JMP2WN.preds,
                        preds3NN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3NN",post="Preds"),#file.names.JMP3NN.preds,
                        preds3WN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3WN",post="Preds"),#file.names.JMP3WN.preds,
                        preds2NNOPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2NN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP2NN.OPP.preds,
                        preds2WNOPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2WN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP2WN.OPP.preds,
                        preds3NNOPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3NN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP3NN.OPP.preds,
                        preds3WNOPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3WN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP3WN.OPP.preds,
                        params2NN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2NN",subfolder='Params'),
                        params2WN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2WN",subfolder='Params'),
                        params3NN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3NN",subfolder='Params'),
                        params3WN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3WN",subfolder='Params'),
                        runtimes2NN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2NN",subfolder='RunTimes'),
                        runtimes2WN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP2WN",subfolder='RunTimes'),
                        runtimes3NN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3NN",subfolder='RunTimes'),
                        runtimes3WN=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="JMP3WN",subfolder='RunTimes'),
                        seed.fit=seed.fit:(seed.fit+reps-1),
                        length=length(file.names.write),
                        inputdim=input.dim
  )
  write.csv(JMP.df.out, paste0(run.files.folder,'filesToRunJMP.csv'))
  write.csv(JMP.df.out, paste0(path.batch,'RunFiles//filesToRunJMP.csv'))
  
  
  # Add option to run it externally (copied from GPy below, make sure everything is changed correctly)
  if ('JMP' %in% external.fits) {
    # This writes the instructions for GPy to be run on another computer outside SSCC
    external.JMP.file.name <- paste0(run.files.folder,'filesToRunJMP_External.csv') # File name
    external.JMP.df.new <- data.frame(names=file.names.write.external,
                             #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                             preds.in=file.names.preds.write.external,
                             preds2NN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2NN",post="Preds"),#file.names.JMP2NN.preds,
                             preds2WN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2WN",post="Preds"),#file.names.JMP2WN.preds,
                             preds3NN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3NN",post="Preds"),#file.names.JMP3NN.preds,
                             preds3WN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3WN",post="Preds"),#file.names.JMP3WN.preds,
                             preds2NNOPP=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2NN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP2NN.OPP.preds,
                             preds2WNOPP=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2WN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP2WN.OPP.preds,
                             preds3NNOPP=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3NN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP3NN.OPP.preds,
                             preds3WNOPP=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3WN",pre="OPP",post="Preds",subfolder='OPPs'),#file.names.JMP3WN.OPP.preds,
                             params2NN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2NN",subfolder='Params'),
                             params2WN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2WN",subfolder='Params'),
                             params3NN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3NN",subfolder='Params'),
                             params3WN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3WN",subfolder='Params'),
                             runtimes2NN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2NN",subfolder='RunTimes'),
                             runtimes2WN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP2WN",subfolder='RunTimes'),
                             runtimes3NN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3NN",subfolder='RunTimes'),
                             runtimes3WN=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="JMP3WN",subfolder='RunTimes'),
                             seed.fit=seed.fit:(seed.fit+reps-1),
                             length=length(file.names.write),
                             inputdim=input.dim
    )
    
    if(file.exists(external.JMP.file.name)) { # If it already exists it appends, need this for multiple replicates
      external.JMP.df.read <- read.csv(file=external.JMP.file.name,header = T) # Read in
      external.JMP.df.write <- rbind(external.JMP.df.read[,-1],external.JMP.df.new) # Append, exclude X column
      # It will rewrite X column, leaving this in so GPy can read it in the same as before
    } else { # Write out just the new stuff
      external.JMP.df.write <- external.JMP.df.new
    }
    write.csv(external.JMP.df.write,
              paste0(run.files.folder,'filesToRunJMP_External.csv')) # Write it
    write.csv(external.JMP.df.write,
              paste0(path.batch,'RunFiles//filesToRunJMP_External.csv')) # Write it
  }
  
  
  # write file for Python scikit-learn
  write.csv(data.frame(names=file.names.write,
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(run.files.folder,'filesToRunPython.csv'))
  write.csv(data.frame(names=file.names.write, # Also write to batch folder
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="Python",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(path.batch,'RunFiles//filesToRunPython.csv'))
  
  
  # write file for Python scikit-learn RBF
  write.csv(data.frame(names=file.names.write,
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(run.files.folder,'filesToRunsklearnRBF.csv'))
  write.csv(data.frame(names=file.names.write, # Also write to batch folder
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnRBF",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(path.batch,'RunFiles//filesToRunsklearnRBF.csv'))
  
  
  # write file for Python scikit-learn Matern 5/2
  write.csv(data.frame(names=file.names.write,
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(run.files.folder,'filesToRunsklearnMatern32.csv'))
  write.csv(data.frame(names=file.names.write, # Also write to batch folder
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern32",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(path.batch,'RunFiles//filesToRunsklearnMatern32.csv'))
  
  
  # write file for Python scikit-learn Matern 3/2
  write.csv(data.frame(names=file.names.write,
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(run.files.folder,'filesToRunsklearnMatern52.csv'))
  write.csv(data.frame(names=file.names.write, # Also write to batch folder
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="sklearnMatern52",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(path.batch,'RunFiles//filesToRunsklearnMatern52.csv'))
  
  
  
  # write file for Python GPy
  write.csv(data.frame(names=file.names.write,
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(run.files.folder,'filesToRunGPy.csv'))
  write.csv(data.frame(names=file.names.write, # Also write to batch folder
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPy",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(path.batch,'RunFiles//filesToRunGPy.csv'))
  # 1/11/17 adding Matern32
  write.csv(data.frame(names=file.names.write,
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(run.files.folder,'filesToRunGPyM32.csv'))
  write.csv(data.frame(names=file.names.write, # Also write to batch folder
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM32",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(path.batch,'RunFiles//filesToRunGPyM32.csv'))
  # 1/11/17 Adding Matern 52
  write.csv(data.frame(names=file.names.write,
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(run.files.folder,'filesToRunGPyM52.csv'))
  write.csv(data.frame(names=file.names.write, # Also write to batch folder
                       #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                       preds.in=file.names.preds.write,
                       preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",post="Preds"),
                       preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",pre="OPP",post="Preds",subfolder='OPPs'),
                       runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",subfolder='RunTimes'),
                       paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name="GPyM52",pre="",subfolder="Params"),
                       seed.fit=seed.fit:(seed.fit+reps-1),
                       length=length(file.names.write)),
            paste0(path.batch,'RunFiles//filesToRunGPyM52.csv'))
  
  if ('GPy' %in% external.fits) {
    # This writes the instructions for GPy to be run on another computer outside SSCC
    external.GPy.file.name <- paste0(run.files.folder,'filesToRunGPy_External.csv') # File name
    external.GPy.df.new <- data.frame(names=file.names.write.external,
                         preds.in=file.names.preds.write.external,
                         preds=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="GPy",post="Preds"),
                         preds.OPP=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="GPy",pre="OPP",post="Preds",subfolder='OPPs'),
                         runtimes=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="GPy",subfolder='RunTimes'),
                         paramsout=get.file.names(path.batch='',batch.name=batch.name,reps=reps,fit.name="GPy",pre="",subfolder="Params"),
                         seed.fit=seed.fit:(seed.fit+reps-1),
                         length=length(file.names.write)) # New stuff to write out
    if(file.exists(external.GPy.file.name)) { # If it already exists it appends, need this for multiple replicates
      external.GPy.df.read <- read.csv(file=external.GPy.file.name,header = T) # Read in
      external.GPy.df.write <- rbind(external.GPy.df.read[,-1],external.GPy.df.new) # Append, exclude X column
        # It will rewrite X column, leaving this in so GPy can read it in the same as before
    } else { # Write out just the new stuff
      external.GPy.df.write <- external.GPy.df.new
    }
    write.csv(external.GPy.df.write,
              paste0(run.files.folder,'filesToRunGPy_External.csv')) # Write it
  }
  
  # write file for DACE
  if (  length(DACE.meanfuncs) != length(DACE.corrfuncs)  )  {stop("DACE mean and corr funcs must have same length")}
  if (  length(DACE.meanfuncs)<1 || length(DACE.corrfuncs)<1  )  {stop("DACE mean and corr funcs must have at least one item")}
  DACEdf <- data.frame()
  for (i in 1:length(DACE.meanfuncs)) {
    DACE.fit.name <- paste0("DACE",DACE.meanfuncs[i],DACE.corrfuncs[i])
    DACEdf1 <- data.frame(names=file.names.write,
                          #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                          preds.in=file.names.preds.write,
                          preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=DACE.fit.name,post="Preds"),
                          preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=DACE.fit.name,pre="OPP",post="Preds",subfolder='OPPs'),
                          paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=DACE.fit.name,pre="",subfolder="Params"),
                          mean=DACE.meanfuncs[i],
                          corr=DACE.corrfuncs[i],
                          runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=DACE.fit.name,subfolder='RunTimes'),
                          seed.fit=seed.fit:(seed.fit+reps-1)
    )
    DACEdf <- rbind(DACEdf,DACEdf1)
  }
  write.csv(DACEdf,
            paste0(run.files.folder,'filesToRunDACE.csv'))
  write.csv(DACEdf, # Also write to local folder
            paste0(path.batch,'RunFiles//filesToRunDACE.csv'))
  
  # write file for ooDACE no nugget
  if (  length(DACE.meanfuncs) != length(DACE.corrfuncs)  )  {stop("DACE mean and corr funcs must have same length")}
  if (  length(DACE.meanfuncs)<1 || length(DACE.corrfuncs)<1  )  {stop("DACE mean and corr funcs must have at least one item")}
  ooDACEdf <- data.frame()
  for (i in 1:length(DACE.meanfuncs)) {
    ooDACE.fit.name <- paste0("ooDACE",DACE.meanfuncs[i],DACE.corrfuncs[i])
    ooDACEdf1 <- data.frame(names=file.names.write,
                          #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                          preds.in=file.names.preds.write,
                          preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACE.fit.name,post="Preds"),
                          preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACE.fit.name,pre="OPP",post="Preds",subfolder='OPPs'),
                          paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACE.fit.name,pre="",subfolder="Params"),
                          mean=DACE.meanfuncs[i],
                          corr=DACE.corrfuncs[i],
                          runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACE.fit.name,subfolder='RunTimes'),
                          seed.fit=seed.fit:(seed.fit+reps-1)
    )
    ooDACEdf <- rbind(ooDACEdf,ooDACEdf1)
  }
  write.csv(ooDACEdf,
            paste0(run.files.folder,'filesToRunooDACE.csv'))
  write.csv(ooDACEdf, # Also write to local folder
            paste0(path.batch,'RunFiles//filesToRunooDACE.csv'))
  
  # write file for ooDACE with estimating nugget
  if (  length(DACE.meanfuncs) != length(DACE.corrfuncs)  )  {stop("DACE mean and corr funcs must have same length")}
  if (  length(DACE.meanfuncs)<1 || length(DACE.corrfuncs)<1  )  {stop("DACE mean and corr funcs must have at least one item")}
  ooDACEEdf <- data.frame()
  for (i in 1:length(DACE.meanfuncs)) {
    ooDACEE.fit.name <- paste0("ooDACEE",DACE.meanfuncs[i],DACE.corrfuncs[i])
    ooDACEEdf1 <- data.frame(names=file.names.write,
                            #preds.in=file.name.predin, # no longer single preds pt for all, each will be diff
                            preds.in=file.names.preds.write,
                            preds=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACEE.fit.name,post="Preds"),
                            preds.OPP=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACEE.fit.name,pre="OPP",post="Preds",subfolder='OPPs'),
                            paramsout=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACEE.fit.name,pre="",subfolder="Params"),
                            mean=DACE.meanfuncs[i],
                            corr=DACE.corrfuncs[i],
                            runtimes=get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=ooDACEE.fit.name,subfolder='RunTimes'),
                            seed.fit=seed.fit:(seed.fit+reps-1)
    )
    ooDACEEdf <- rbind(ooDACEEdf,ooDACEEdf1)
  }
  write.csv(ooDACEEdf,
            paste0(run.files.folder,'filesToRunooDACEE.csv'))
  write.csv(ooDACEEdf, # Also write to local folder
            paste0(path.batch,'RunFiles//filesToRunooDACEE.csv'))
  
  
  
  # write out details ??? still testing
  func.write.name = (if (is.character(func)) {func} else {"NotAvailable"})
  write.csv(
    data.frame(path.base,batch.name,reps,input.dim,input.ss,pred.ss,
               seed.start,seed.preds,seed.fit,
               func=func.write.name
    ),
    paste0(path.batch,batch.name,'_details.csv')
  )
  
  # write out plot if low-D
  # if 1D, create plot
  if (input.dim==1 & is.function(funcToApply)) {
    #dev.copy(png,filename=paste0(path.batch,"Plots//function_curve.png"),width = 640,height = 640,units = "px")
    png(filename=paste0(path.batch,"Plots//function_curve.png"),width = 640,height = 640,units = "px")
    curve(sapply(x,funcToApply),from=0,to=1,n = 1000)
    for(i in 1:reps) {
      points(xs[[i]],ys[[i]],col=i,pch=i)
    }
    dev.off()
  }
  if (input.dim==1 & is.function(funcToApply)) {
    #dev.copy(png,filename=paste0(path.batch,"Plots//function_curve.png"),width = 640,height = 640,units = "px")
    for(i in 1:reps) {
      png(filename=paste0(path.batch,"Plots//function_points_",i,".png"),width = 640,height = 640,units = "px")
      curve(sapply(x,funcToApply),from=0,to=1,n = 1000)
      plot(xps[[i]],ypas[[i]],pch=19)
      points(xs[[i]],ys[[i]],col='red',pch=19,cex=2)
      dev.off()
    }
  }
  if (input.dim==2 & is.function(funcToApply)) {
    #dev.copy(png,filename=paste0(path.batch,"Plots//function_contour.png"),width = 640,height = 640,units = "px")
    png(filename=paste0(path.batch,"Plots//function_contour.png"),width = 640,height = 640,units = "px")
    x <- y <- seq(0,1,.01)
    z <- matrix(-1,length(x),length(y))
    for(i in 1:length(x)){for(j in 1:length(y)) {z[i,j] <- funcToApply(c(x[i],y[j]))}}
    filled.contour(x,y,z,
                   plot.axes=points(as.numeric(sapply(xs,function(xxx){xxx[,1]})),as.numeric(sapply(xs,function(xxx){xxx[,2]})),
                                    pch = as.numeric(sapply(1:reps,function(xx){rep(xx,input.ss)})),
                                    col = as.numeric(sapply(1:reps,function(xx){rep(xx,input.ss)}))
                                    )
                   )
    dev.off()
  }
}


# Runs the specified Gaussian process fits, either here (R packages) or through command line calls (other)
comparison.run <- function (path.base=OutputFolderPath,
                            run.files.folder=RunFilesFolderPath,
                            batch.name,reps,input.dim,
                            GPfit.powers=c(1.95,2),GPfit.include=T,GPfit.controls=c(1),
                            mlegp.include=T,Dice.include=T,
                            JMP.include=T,DACE.include=T,ooDACE.include=F,Python.include=T,GPy.include=T,
                            laGP.include=T,laGP.nuggets,laGP.nuggets.names,
                            seed.fit=200,
                            external.fits=c(),
                            #Python.file.name='comparison2_python.py',
                            Python.file.name='GPC_sklearn.py',
                            GPy.file.name='GPC_GPy.py',  #'comparison2_GPy.py',
                            JMP.file.name =   'comparison2_JMP.jsl',
                            reps.run=NULL
                            ) {
  # Runs the specified Gaussian process fits, either here (R packages) or through command line calls (other)
  # Parameters are all explained in comparison.all.batch
  # Output: Fit data are written to file, nothing returned
  
  path.batch = paste0(path.base,batch.name,"//")
  
  # Give option to only include certain reps, replace 1:reps with reps.run below
  if (is.null(reps.run)) {reps.run = 1:reps} 
  reps.run.length = length(reps.run)
  
  # load prediction data
  #file.name.predin <- gsub("//","\\\\",    paste0(path.batch,batch.name,'_PredPts.csv')   )
  #write.csv(data.frame(x=xp,y=ypa),
  #          file.name.predin)
  
  # Moved this into loops after making pred pts diff for each run
  #preds.data <- read.csv(paste0(path.batch,batch.name,'_PredPts.csv'))
  #xp <- preds.data[,c(-1,-(1+input.dim+1))]
  #ypa <- preds.data$y
  
  run.times <- data.frame(index=1:reps)
  
  # Run DACE first since system doesn't wait
  if (DACE.include & !('DACE' %in% external.fits) ) {
    print('About to start DACE, in R now')
    # run DACE through system OS command  
    # wasn't working, trying to change apostrophe cmd.DACE <- "matlab -nodisplay -nosplash -nodesktop -r \"run('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//GPC_DACE.m');exit;"   
    cmd.DACE <- paste0("matlab -nodisplay -nosplash -nodesktop -r \"filesToRunName='",path.batch,"RunFiles//filesToRunDACE.csv';run('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//GPC_DACE.m');exit;\"" )
    system(cmd.DACE)
    #print('DACE is running, it must finish before its output can be read in. No longer a readline here.')
    #lineread <- readline(prompt="Press [enter] to continue once MatLab has closed (or type 'exit' to halt): ")
    #if (lineread=="exit") {stop("You said exit")}
    print('Finished DACE, back in R')
  }
  # Run ooDACE next, CUTTING (2/7/17) since ooDACE is terrible
  if (ooDACE.include & !('ooDACE' %in% external.fits) ) {
    print('About to start ooDACE, in R now')
    # run DACE through system OS command  
    # wasn't working, trying to change apostrophe cmd.DACE <- "matlab -nodisplay -nosplash -nodesktop -r \"run('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//GPC_DACE.m');exit;"   
    cmd.ooDACE <- paste0("matlab -nodisplay -nosplash -nodesktop -r \"filesToRunName='",path.batch,"RunFiles//filesToRunooDACE.csv';run('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//GPC_ooDACE.m');exit;\"" )
    system(cmd.ooDACE)
    cmd.ooDACEE <- paste0("matlab -nodisplay -nosplash -nodesktop -r \"filesToRunName='",path.batch,"RunFiles//filesToRunooDACEE.csv';run('//sscc//home//c//cbe117//Research//GPC//GPC_Codes//GPC_RunFiles//GPC_ooDACEE.m');exit;\"" )
    system(cmd.ooDACEE)
    #print('DACE is running, it must finish before its output can be read in. No longer a readline here.')
    #lineread <- readline(prompt="Press [enter] to continue once MatLab has closed (or type 'exit' to halt): ")
    #if (lineread=="exit") {stop("You said exit")}
    print('Finished ooDACE, back in R')
  }
  if (GPfit.include & !('GPfit' %in% external.fits) ) {
    print("Starting GPfit")
    # run GPfit on data
    for (i in reps.run) {
      print(paste('  rep',i,'of',reps))
      #GPfit.powers <- c(1.95,2)
      for (GPfit.power in GPfit.powers) {
        
        # Use different control params
        for (icontrol in GPfit.controls) {
          controldefault <- c(200*input.dim,80*input.dim,2*input.dim)
          if (icontrol==1) {  # default
            GPcontrol <- controldefault
            controlstring <- ''
          } else if (icontrol==2) {
            GPcontrol <- c(100*input.dim,40*input.dim,2*input.dim)#controldefault/2+c(0,0,1)
            controlstring <- '-2'
          } else if (icontrol==3) {
            GPcontrol <- c(100,40,2)#controldefault/2+c(0,0,1)
            controlstring <- '-A'
          } else {
            stop('GPfit control error 0915252929')
          }
          
          
          # start time
          start.time <- proc.time()
          
          # model fitting
          dat <- read.csv(paste0(path.batch,batch.name,"_",i,'.csv'))
          x <- dat[,c(-1,-(input.dim+2))]
          y <- dat$y
          datp <- read.csv(paste0(path.batch,batch.name,"_",i,'_PredPts.csv')) # Added this after adding diff pred pts for each run
          xp <- datp[,c(-1,-(input.dim+2))]
          ypa <- datp$y
          set.seed(seed.fit+i-1)
          mod <- GPfit::GP_fit(x,y,corr = list(type="exponential",power=GPfit.power),control = GPcontrol)
          # save model parameters
          if (length(mod$beta)==1) {
            write.csv(data.frame(beta.1=t(10^mod$beta),sigma2=mod$sig2,delta=mod$delta),
                      paste0(path.batch,"Params","//",batch.name,"_",i,"_GPfit",GPfit.power,controlstring,".csv"))          
          } else {
            write.csv(data.frame(beta=t(10^mod$beta),sigma2=mod$sig2,delta=mod$delta),
                      paste0(path.batch,"Params","//",batch.name,"_",i,"_GPfit",GPfit.power,controlstring,".csv"))
          }
          # make and save predictions
          modp <- GPfit::predict.GP(mod,xp)
          write.csv(data.frame(xp,y=ypa,yp=modp$Y,yv=modp$MSE,ysd=sqrt(modp$MSE)),
                    paste0(path.batch,batch.name,"_",i,'_Preds_GPfit',GPfit.power,controlstring,'.csv'))
          modp0 <- GPfit::predict.GP(mod,dat[,c(-1,-(input.dim+2))])
          # make and save predictions on OPP
          write.csv(data.frame(x,y,yp=modp0$Y,yv=modp0$MSE,ysd=sqrt(modp0$MSE)),
                    #paste0(path.batch,batch.name,"_OPP_",i,'_Preds_GPfit',GPfit.power,controlstring,'.csv')) Moving OPPs
                    paste0(path.batch,'OPPs//',batch.name,"_OPP_",i,'_Preds_GPfit',GPfit.power,controlstring,'.csv'))
          
          # save run time
          write.csv(data.frame(elapsed=(proc.time()-start.time)['elapsed']),
                    paste0(path.batch,"RunTimes","//",batch.name,"_",i,"_GPfit",GPfit.power,controlstring,".csv"))
        } # end control for
        
      }
    }
  } # end of if GPfit.include
  
  if (mlegp.include & !('mlegp' %in% external.fits) ) {
    print("Starting mlegp")
    
    mlegp.nuggets <- c(0,1e-6) # for nugget=0 set to NULL, not 0 or it will estimate
    mlegp.names <- c('0','E')
    for(jj in 1:length(mlegp.nuggets)) {
      mlegp.nugget <- mlegp.nuggets[jj]
      mlegp.name <- mlegp.names[jj]
      
      
      # run mlegp on data
      for (i in reps.run) {
        
        # start time
        start.time <- proc.time()
        
        # Use default options
        dat <- read.csv(paste0(path.batch,batch.name,"_",i,'.csv'))
        x <- dat[,c(-1,-(input.dim+2))]
        y <- dat$y
        datp <- read.csv(paste0(path.batch,batch.name,"_",i,'_PredPts.csv')) # Added this after adding diff pred pts for each run
        xp <- datp[,c(-1,-(input.dim+2))]
        ypa <- datp$y
        if (mlegp.nugget > 0) {mod <- mlegp::mlegp(x,y,nugget = mlegp.nugget,verbose=0,seed = seed.fit+i-1)}
        else if (mlegp.nugget <= 0) {mod <- mlegp::mlegp(x,y,nugget = NULL,verbose=0,seed = seed.fit+i-1)}
        else{stop('Bad nugget in mlegp, error #9122002')}
        # save model parameters
        if (length(mod$beta)==1) {
          write.csv(data.frame(beta.1=t(mod$beta),sigma2=mod$sig2,delta=mod$nugget),
                    paste0(path.batch,"Params","//",batch.name,"_",i,"_mlegp",mlegp.name,".csv"))         
        } else {
          write.csv(data.frame(beta=t(mod$beta),sigma2=mod$sig2,delta=mod$nugget),
                    paste0(path.batch,"Params","//",batch.name,"_",i,"_mlegp",mlegp.name,".csv")) 
        }
        # make and save predictions
        modp <- mlegp::predict.gp(mod,as.matrix(xp),se.fit=T)
        write.csv(data.frame(xp,y=ypa,yp=modp$fit,yv=(modp$se)^2,ysd=(modp$se)),
                  paste0(path.batch,batch.name,"_",i,"_Preds_mlegp",mlegp.name,".csv"))
        # make and save predictions on OPP
        modp0 <- mlegp::predict.gp(mod,as.matrix(dat[,c(-1,-(input.dim+2))]),se.fit=T)
        write.csv(data.frame(x,y,yp=modp0$fit,yv=(modp0$se)^2,ysd=(modp0$se)),
                  #paste0(path.batch,batch.name,"_OPP_",i,'_Preds_mlegp.csv')) Moving OPPs
                  paste0(path.batch,"OPPs//",batch.name,"_OPP_",i,"_Preds_mlegp",mlegp.name,".csv"))
        
        # save run time
        write.csv(data.frame(elapsed=(proc.time()-start.time)['elapsed']),
                  paste0(path.batch,"RunTimes","//",batch.name,"_",i,"_mlegp",mlegp.name,".csv"))
      } # end for reps
    } # end for nuggets
  } # end if mlegp.include
  
  if (Dice.include & !('Dice' %in% external.fits) ) {
    print("Starting DiceKriging")
    
    Dice.covtypes <- c("gauss", "matern5_2", "matern3_2")
    Dice.nugget.estims <- c(TRUE, FALSE) # FALSE didn't work, leading minor problem
    #Dice.names <- c('0','E')
    Dice.paramtable <- expand.grid(Dice.covtypes=Dice.covtypes, Dice.nugget.estims=Dice.nugget.estims, stringsAsFactors=FALSE)
    Dice.names <- c('2','M52','M32', '20', 'M520', 'M320')
    
    for(jj in 1:nrow(Dice.paramtable)) {
      Dice.covtype <- Dice.paramtable$Dice.covtypes[jj]
      Dice.nugget.estim <- Dice.paramtable$Dice.nugget.estims[jj]
      Dice.name <- Dice.names[jj]
      
      
      # run Dice on data
      for (i in reps.run) {
        
        # start time
        start.time <- proc.time()
        
        # Use default options
        dat <- read.csv(paste0(path.batch,batch.name,"_",i,'.csv'))
        x <- dat[,c(-1,-(input.dim+2)), drop=FALSE]
        y <- dat$y
        datp <- read.csv(paste0(path.batch,batch.name,"_",i,'_PredPts.csv')) # Added this after adding diff pred pts for each run
        xp <- datp[,c(-1,-(input.dim+2)), drop=FALSE]
        ypa <- datp$y
        set.seed(seed.fit+i-1)
        #if (Dice.nugget > 0) {
        DK.try <- try(
          capture.output(mod <- DiceKriging::km(design=x,response=y,nugget.estim = Dice.nugget.estim, covtype=Dice.covtype))
        )
        if (class(DK.try) == "try-error") { browser()
          
        }
        #}
        #else if (Dice.nugget <= 0) {mod <- DiceKriging::Dice(x,y,nugget = NULL,verbose=0,seed = seed.fit+i-1)}
        #else{stop('Bad nugget in Dice, error #9122452')}
        
        # save model parameters
        DK.nugget.out <- if (Dice.nugget.estim) {mod@covariance@nugget} else{0}
        if (length(mod@covariance@range.val)==1) {
          write.csv(data.frame(beta.1=t(mod@covariance@range.val),sigma2=mod@covariance@sd2,delta=DK.nugget.out),
                    paste0(path.batch,"Params","//",batch.name,"_",i,"_Dice",Dice.name,".csv"))         
        } else {
          write.csv(data.frame(beta=t(mod@covariance@range.val),sigma2=mod@covariance@sd2,delta=DK.nugget.out),
                    paste0(path.batch,"Params","//",batch.name,"_",i,"_Dice",Dice.name,".csv")) 
        }
        # make and save predictions
        modp <- DiceKriging::predict(mod,as.matrix(xp),se.compute=T, type="SK")
        write.csv(data.frame(xp,y=ypa,yp=modp$mean,yv=(modp$sd)^2,ysd=(modp$sd)),
                  paste0(path.batch,batch.name,"_",i,"_Preds_Dice",Dice.name,".csv"))
        # make and save predictions on OPP
        modp0 <- DiceKriging::predict(mod,as.matrix(dat[,c(-1,-(input.dim+2))]),se.compute=T, type="SK")
        write.csv(data.frame(x,y,yp=modp0$mean,yv=(modp0$sd)^2,ysd=(modp0$sd)),
                  #paste0(path.batch,batch.name,"_OPP_",i,'_Preds_Dice.csv')) Moving OPPs
                  paste0(path.batch,"OPPs//",batch.name,"_OPP_",i,"_Preds_Dice",Dice.name,".csv"))
        
        # save run time
        write.csv(data.frame(elapsed=(proc.time()-start.time)['elapsed']),
                  paste0(path.batch,"RunTimes","//",batch.name,"_",i,"_Dice",Dice.name,".csv"))
      } # end for reps
    } # end for nuggets
  } # end if Dice.include
  
  if (laGP.include & !('laGP' %in% external.fits) ) {
    print("Starting laGP")
    
    # can pick nuggets: 'E' is estimate it, otherwise you have to give the value, 1e-8 or 1e-6 or 1e-3 are reasonable
    #laGP.nuggets <- c('E',1e-8), now pass in through args
    
    #for (laGP.nugget in laGP.nuggets) {
    for (inug in 1:length(laGP.nuggets)) {
      laGP.nugget <- laGP.nuggets[inug]
      laGP.name <- laGP.nuggets.names[inug]  # files will be written out as 'laGP' + laGP.name, eg laGPE
      if (laGP.nugget == 'E') {
        laGP.estimate.nugget <- TRUE
      } else {
        laGP.estimate.nugget <- FALSE
      }
      
      # run laGP on data
      for (i in reps.run) {
        print(paste('   ',i,'of',reps))
        
        # start time
        start.time <- proc.time()
        
        # Use default options
        dat <- read.csv(paste0(path.batch,batch.name,"_",i,'.csv'))
        x <- dat[,c(-1,-(input.dim+2))]
        y <- dat$y
        datp <- read.csv(paste0(path.batch,batch.name,"_",i,'_PredPts.csv')) # Added this after adding diff pred pts for each run
        xp <- datp[,c(-1,-(input.dim+2))]
        ypa <- datp$y
        #  copied from mlegp mod <- mlegp::mlegp(x,y,verbose=0,seed = seed.fit+i-1)
        set.seed(seed.fit+i-1)
        
        ## prior and inits for theta (d) and nugget (g) 
        
        da <- laGP::darg(list(mle=TRUE), X=as.matrix(x))
        if (laGP.estimate.nugget) ga <- laGP::garg(list(mle=TRUE), y=y) 
        # Change nugget below to 1e-8, don't use 0 or get bad Cholesky decomp
        #mod <- laGP::newGPsep(X=as.matrix(x), Z=y, d=da$start, g=ga$start, dK = TRUE) # with nugget
        if(input.dim==1) # don't use sep in 1D, gives neg variance
          mod <- laGP::newGP(X=as.matrix(x), Z=y, d=da$start, g=ifelse(laGP.estimate.nugget,ga$start,laGP.nugget), dK = TRUE) # no nugget
        else 
          mod <- laGP::newGPsep(X=as.matrix(x), Z=y, d=da$start, g=ifelse(laGP.estimate.nugget,ga$start,laGP.nugget), dK = TRUE) # no nugget
        # Change above to remove nugget. Change below, not join, no nugget
        #mleGPsep.out <- laGP::jmleGPsep(mod, drange=c(da$min, da$max), grange=c(ga$min, ga$max), dab=da$ab, gab=ga$ab, verb=1)
        if (input.dim==1) {
          if (laGP.estimate.nugget)
            mleGPsep.out <- laGP::jmleGP(mod, drange=c(da$min, da$max), grange=c(ga$min, ga$max), dab=da$ab, gab=ga$ab, verb=1)
          else
            mleGPsep.out <- laGP::mleGP(mod,  verb=1)
        } else {
          if (laGP.estimate.nugget)
            mleGPsep.out <- laGP::jmleGPsep(mod, drange=c(da$min, da$max), grange=c(ga$min, ga$max), dab=da$ab, gab=ga$ab, verb=1)
          else
            mleGPsep.out <- laGP::mleGPsep(mod,  verb=1)
        }
        # save model parameters
        if (input.dim==1){#length(mod$beta)==1) {
          write.csv(data.frame(beta.1=t(mleGPsep.out$d),sigma2=0,delta=laGP.nugget),
                    paste0(path.batch,"Params","//",batch.name,"_",i,"_laGP",laGP.name,".csv"))         
        } else {
          #write.csv(data.frame(beta=t(mleGPsep.out$d),sigma2=0,delta=0),
          #          paste0(path.batch,"Params","//",batch.name,"_",i,"_laGP.csv")) 
          laGP.betas = ''
          if(laGP.estimate.nugget) {laGP.betas <- unname(mleGPsep.out[1:input.dim])}
          else {laGP.betas <- t(unname(mleGPsep.out$d[1:input.dim]))}
          write.csv(data.frame(beta=laGP.betas,sigma2=0,delta=ifelse(laGP.estimate.nugget,mleGPsep.out$g,laGP.nugget)),
                    paste0(path.batch,"Params","//",batch.name,"_",i,"_laGP",laGP.name,".csv")) 
        }
        # make and save predictions
        # from mlegp modp <- mlegp::predict.gp(mod,as.matrix(xp),se.fit=T)
        if (input.dim==1)
          modp <- laGP::predGP(mod, as.matrix(xp),lite=T)
        else
          modp <- laGP::predGPsep(mod, as.matrix(xp),lite=T)
        #if(min(modp$s2)<0) stop()
        write.csv(data.frame(xp,y=ypa,yp=modp$mean,yv=(modp$s2),ysd=sqrtNA(modp$s2)),
                  paste0(path.batch,batch.name,"_",i,'_Preds_laGP',laGP.name,'.csv'))
        # make and save predictions on OPP
        # from mlegpmodp0 <- mlegp::predict.gp(mod,as.matrix(dat[,c(-1,-(input.dim+2))]),se.fit=T)
        if (input.dim==1)
          modp0 <- laGP::predGP(mod, as.matrix(dat[,c(-1,-(input.dim+2))]),lite=T)
        else
          modp0 <- laGP::predGPsep(mod, as.matrix(dat[,c(-1,-(input.dim+2))]),lite=T)
        write.csv(data.frame(x,y,yp=modp0$mean,yv=(modp0$s2),ysd=sqrtNA(modp0$s2)),
                  #paste0(path.batch,batch.name,"_OPP_",i,'_Preds_laGP.csv')) Moving OPPs
                  paste0(path.batch,'OPPs//',batch.name,"_OPP_",i,'_Preds_laGP',laGP.name,'.csv'))
        
        # save run time
        write.csv(data.frame(elapsed=(proc.time()-start.time)['elapsed']),
                  paste0(path.batch,"RunTimes","//",batch.name,"_",i,"_laGP",laGP.name,".csv"))
        
        if (input.dim==1) 
          laGP::deleteGP(mod)
        else
          laGP::deleteGPsep(mod)
      }
    }
  }
  
  # Just predict mean of prediction points
  if (T){#PredictMean.include) {
    print("Starting predict mean")
    # run predict mean on data
    for (i in reps.run) {
      
      # start time
      start.time <- proc.time()
      
      # Use default options
      dat <- read.csv(paste0(path.batch,batch.name,"_",i,'.csv'))
      x <- dat[,c(-1,-(input.dim+2))]
      y <- dat$y
      datp <- read.csv(paste0(path.batch,batch.name,"_",i,'_PredPts.csv')) # Added this after adding diff pred pts for each run
      xp <- datp[,c(-1,-(input.dim+2))]
      ypa <- datp$y
      #mod <- mlegp::mlegp(x,y,verbose=0,seed = seed.fit+i-1)
      # save model parameters
      if (input.dim==1) {  
        write.csv(data.frame(beta.1=t(0),sigma2=var(y),delta=0),
                  paste0(path.batch,"Params","//",batch.name,"_",i,"_PredictMean.csv"))         
      } else {
        write.csv(data.frame(beta=t(rep(0,dim(x)[2])),sigma2=var(y),delta=0),
                  paste0(path.batch,"Params","//",batch.name,"_",i,"_PredictMean.csv")) 
      }
      # make and save predictions
      #modp <- mlegp::predict.gp(mod,as.matrix(xp),se.fit=T)
      write.csv(data.frame(xp,y=ypa,yp=mean(y),yv=var(y),ysd=sd(y)),
                paste0(path.batch,batch.name,"_",i,'_Preds_PredictMean.csv'))
      # make and save predictions on OPP
      #modp0 <- mlegp::predict.gp(mod,as.matrix(dat[,c(-1,-(input.dim+2))]),se.fit=T)
      write.csv(data.frame(x,y,yp=mean(y),yv=var(y),ysd=sd(y)),
                #paste0(path.batch,batch.name,"_OPP_",i,'_Preds_mlegp.csv')) Moving OPPs
                paste0(path.batch,'OPPs//',batch.name,"_OPP_",i,'_Preds_PredictMean.csv'))
      
      # save run time
      write.csv(data.frame(elapsed=(proc.time()-start.time)['elapsed']),
                paste0(path.batch,"RunTimes","//",batch.name,"_",i,"_PredictMean.csv"))
    }
    print('Ending predict mean')
  }
  # End predict mean of prediction points
  
  # LM predict
  if (T){#LM.include) {
    print("Starting LM")
    # run LM on data
    for (i in reps.run) {
      
      # start time
      start.time <- proc.time()
      
      # Use default options
      dat <- read.csv(paste0(path.batch,batch.name,"_",i,'.csv'))
      x <- dat[,c(-1,-(input.dim+2))]
      y <- dat$y
      datp <- read.csv(paste0(path.batch,batch.name,"_",i,'_PredPts.csv')) # Added this after adding diff pred pts for each run
      xp <- datp[,c(-1,-(input.dim+2))]
      ypa <- datp$y
      
      mod <- lm(y~as.matrix(x))
      modvar <- sum(mod$res^2)/(length(y)-2)
      # save model parameters 
      
      if (length(mod$coeff)==2) {
        write.csv(data.frame(beta.1=t(as.numeric(mod$coeff[2])),sigma2=modvar,delta=0),
                  paste0(path.batch,"Params","//",batch.name,"_",i,"_LM.csv"))         
      } else {
        write.csv(data.frame(beta=t(as.numeric(mod$coeff[2:length(mod$coeff)])),sigma2=sum(mod$res^2)/(length(y)-2),delta=0),
                  paste0(path.batch,"Params","//",batch.name,"_",i,"_LM.csv")) 
      }
      # make and save predictions
      
      yplm <- as.matrix(cbind(rep(1,ifelse(length(mod$coeff)==2,length(xp),dim(xp)[1])),xp)) %*% as.matrix(mod$coeff)
      write.csv(data.frame(xp,y=ypa,yp=yplm,yv=modvar,ysd=sqrt(modvar)),
                paste0(path.batch,batch.name,"_",i,'_Preds_LM.csv'))
      # make and save predictions on OPP
      yp0lm <- as.matrix(cbind(rep(1,ifelse(length(mod$coeff)==2,length(x),dim(x)[1])),x)) %*% as.matrix(mod$coeff)
      write.csv(data.frame(x,y,yp=yp0lm,yv=modvar,ysd=sqrt(modvar)),
                #paste0(path.batch,batch.name,"_OPP_",i,'_Preds_mlegp.csv')) Moving OPPs
                paste0(path.batch,'OPPs//',batch.name,"_OPP_",i,'_Preds_LM.csv'))
      
      # save run time
      write.csv(data.frame(elapsed=(proc.time()-start.time)['elapsed']),
                paste0(path.batch,"RunTimes","//",batch.name,"_",i,"_LM.csv"))
    }
    print("Finished LM")
  }
  # End LM
  
  # QM predict
  if (input.dim <= 10){#QM.include) {  # 1/13/17 Crashes when D=20 so I'm going to exclude QM if input.dim > 10
    print("Starting QM")
    # run QM on data
    for (i in reps.run) {
      
      # start time
      start.time <- proc.time()
      
      # Use default options
      dat <- read.csv(paste0(path.batch,batch.name,"_",i,'.csv'))
      x <- dat[,c(-1,-(input.dim+2))]
      y <- dat$y
      datp <- read.csv(paste0(path.batch,batch.name,"_",i,'_PredPts.csv')) # Added this after adding diff pred pts for each run
      xp <- datp[,c(-1,-(input.dim+2))]
      ypa <- datp$y
      
      mod <- lm(y~poly(as.matrix(x),degree=2,raw=T))
      modvar <- sum(mod$res^2)/(length(y)-2)
      # save model parameters 
      
      if (length(mod$coeff)==3) { # 1-dimension
        write.csv(data.frame(beta.1=t(as.numeric(mod$coeff[2])),sigma2=modvar,delta=0),
                  paste0(path.batch,"Params","//",batch.name,"_",i,"_QM.csv"))         
      } else { # More than 1 dim
        write.csv(data.frame(beta=t(as.numeric(mod$coeff[2:length(mod$coeff)])),sigma2=sum(mod$res^2)/(length(y)-2),delta=0),
                  paste0(path.batch,"Params","//",batch.name,"_",i,"_QM.csv")) 
      }
      # make and save predictions
      
      #yplm <- as.matrix(cbind(rep(1,ifelse(length(mod$coeff)==2,length(xp),dim(xp)[1])),xp)) %*% as.matrix(mod$coeff)
      #ypqm <- predict(mod,poly(as.matrix(xp),degree=2,raw=T)) # This isn't working
      ypqm <- mod$coef[1] + poly(as.matrix(xp),degree=2,raw=T) %*% mod$coef[2:length(mod$coef)]
      write.csv(data.frame(xp,y=ypa,yp=ypqm,yv=modvar,ysd=sqrt(modvar)),
                paste0(path.batch,batch.name,"_",i,'_Preds_QM.csv'))
      # make and save predictions on OPP
      #yp0lm <- as.matrix(cbind(rep(1,ifelse(length(mod$coeff)==2,length(x),dim(x)[1])),x)) %*% as.matrix(mod$coeff)
      #yp0qm <- predict(mod,poly(as.matrix(x),degree=2,raw=T)) # This isn't working
      yp0qm <- mod$coef[1] + poly(as.matrix(x),degree=2,raw=T) %*% mod$coef[2:length(mod$coef)]
      write.csv(data.frame(x,y,yp=yp0qm,yv=modvar,ysd=sqrt(modvar)),
                paste0(path.batch,'OPPs//',batch.name,"_OPP_",i,'_Preds_QM.csv'))
      
      # save run time
      write.csv(data.frame(elapsed=(proc.time()-start.time)['elapsed']),
                paste0(path.batch,"RunTimes","//",batch.name,"_",i,"_QM.csv"))
    }
    print("Finished QM")
  }
  # End QM
  
  
  
  if (Python.include & !('Python' %in% external.fits) ) {
    # run python through system OS command
    print("Starting scikitlearn")
    # 1/11/17 Removing old run command below, adding new ones for RBF and two Materns, no longer using Python.file.name
    #  system(paste0('python ',run.files.folder,Python.file.name,' ',path.batch,'RunFiles//filesToRunPython.csv'))
    system(paste0('python ',run.files.folder,'GPC_sklearnRBF.py',' ',path.batch,'RunFiles//filesToRunsklearnRBF.csv'))
    system(paste0('python ',run.files.folder,'GPC_sklearnMatern52.py',' ',path.batch,'RunFiles//filesToRunsklearnMatern52.csv'))
    system(paste0('python ',run.files.folder,'GPC_sklearnMatern32.py',' ',path.batch,'RunFiles//filesToRunsklearnMatern32.csv'))
    print("Finished scikitlearn")
  }
  if (GPy.include & !('GPy' %in% external.fits) ) {
    # run python through system OS command
    print("Starting GPy")
    system(paste0('python ',run.files.folder,GPy.file.name,' ',path.batch,'RunFiles//filesToRunGPy.csv'))
    # 1/11/17 Changing this to include M32 and M52
    system(paste0('python ',run.files.folder,'GPC_GPyM32.py',' ',path.batch,'RunFiles//filesToRunGPyM32.csv'))
    system(paste0('python ',run.files.folder,'GPC_GPyM52.py',' ',path.batch,'RunFiles//filesToRunGPyM52.csv'))
    print("Finished GPy")
  }
  # Put JMP at end because you have to manually close it
  if (JMP.include & !('JMP' %in% external.fits) ) {
    # run JMP through system OS command
    #cat("Running JMP, must manually exit after done\n")
    system(paste0('"C://Program Files//SAS//JMPPRO//11//jmp.exe" ',run.files.folder,JMP.file.name))
    #system(paste0('"C://Program Files//SAS//JMPPRO//11//jmp.exe" -NoTip -NoSession -NoSplash ',path.base,JMP.file.name))
  }
}


###### Begin of comparison.compare
# Reads in the results, compares them, makes plots
comparison.compare <- function (path.base=OutputFolderPath,
                                batch.name,reps,input.dim,input.ss,pred.ss,
                                GPfit.powers=c(1.95,2),GPfit.include=T,GPfit.controls=c(1),
                                mlegp.include=T, Dice.include=T,
                                JMP.include=T,DACE.include=T,ooDACE.include=F,Python.include=T,GPy.include=T,
                                laGP.include=T,laGP.nuggets,laGP.nuggets.names,
                                DACE.meanfuncs,DACE.corrfuncs,
                                knit.report=F,
                                xmax_on=list(),
                                external.fits=c(),external.runs.folder='ExternalRuns',
                                reps.run=NULL
                                ) {
  # Reads in the results, compares them, makes plots
  # Parameters are explained in comparison.all.batch
  # Output: writes out plots, writes out OutputTable.csv. Returns the OutputTable too.
  
  path.batch = paste0(path.base,batch.name,"//")
  path.external.runs = paste0(path.base,external.runs.folder,'//') # This if in GPC_Output/<DetPep8d>/ExternalRuns
  path.external.runs = paste0(path.base,'..//',external.runs.folder,'//') # This if in GPC_Output/ExternalRuns
  
  report.path <- paste0(path.batch,'Report.Rmd')
  report.path.html.out <- paste0(path.batch,'Report.html')
  add.to.report <- function(...,append=T,sep.paste=' '){cat(paste(...,sep=sep.paste),file=report.path,append=append,sep='\n\n')}
  atr <- function(...){add.to.report(...)}
  atri <- function(image.path) {atr('\n```{r fig.width=10, fig.height=10,echo=FALSE}\nlibrary(png)\nlibrary(grid)\nimg <- readPNG("',image.path,'")\ngrid.raster(img)\n  ```',sep.paste='')}
  #print(report.path)
  #print(atri)
  #print(atr)
  
  
  # load prediction data REMOVED this after moving pred pts to diff for each run
  #preds.data <- read.csv(paste0(path.batch,batch.name,'_PredPts.csv'))
  #xp <- preds.data$x
  #xp <- preds.data[,c(-1,-(1+input.dim+1))]
  #ypa <- preds.data$y
  
  
  # Compare
  # The order they are in fits is the REVERSE order they are plotted
  #fits <- c("JMP2NN","JMP2WN","JMP3NN","JMP3WN",'Python',paste0("GPfit",GPfit.powers),'mlegp','DACE')
  fits <- c()
  fits <- c(fits,'PredictMean','LM')
  if (input.dim <= 10) {fits <- c(fits, 'QM')} # 1/13/17 QM not calculated for large models
  
  #if (Python.include) fits <- c(fits,'Python') # removing 1/11/17
  if (Python.include) fits <- c(fits,'sklearnRBF') # added 1/11/17
  #if (Python.include) fits <- c(fits,'sklearnMatern52') # added 1/11/17
  #if (Python.include) fits <- c(fits,'sklearnMatern32') # added 1/11/17
  
  #if (GPy.include) fits <- c(fits,'GPy') # Removed 1/11/17
  if (GPy.include) fits <- c(fits,'GPy') # Added 1/11/17
  #if (GPy.include) fits <- c(fits,'GPyM32', 'GPyM52') # Added 1/11/17
  
  if (DACE.include) fits <- c(fits,paste0('DACE',DACE.meanfuncs,DACE.corrfuncs))
  if (ooDACE.include) fits <- c(fits,paste0('ooDACE',DACE.meanfuncs,DACE.corrfuncs))
  if (ooDACE.include) fits <- c(fits,paste0('ooDACEE',DACE.meanfuncs,DACE.corrfuncs))
  
  if (JMP.include) {
    fits <- c(fits,"JMP2WN","JMP2NN")#,"JMP3NN","JMP3WN")
    external.fits <- c(external.fits,"JMP2WN","JMP2NN")#,"JMP3NN","JMP3WN")
  }
  
  if (mlegp.include) fits <- c(fits,paste0('mlegp',c('E','0')))
  
  #if(laGP.include) fits <- c(fits,'laGP')
  if(laGP.include) fits <- c(fits,paste0('laGP',laGP.nuggets.names))
  
  if (GPfit.include) {
    if (1 %in% GPfit.controls) fits <- c(fits,paste0("GPfit",rev(GPfit.powers)))
    if (2 %in% GPfit.controls)fits <- c(fits,paste0("GPfit",GPfit.powers,'-2'))
    if (3 %in% GPfit.controls)fits <- c(fits,paste0("GPfit",GPfit.powers,'-A'))
  }
  
  #if (Dice.include) fits <- c(fits,paste0('Dice',c('2', 'M52', 'M32')))
  if (Dice.include) fits <- c(fits,paste0('Dice',c('2')))
  #if (Dice.include) fits <- c(fits,paste0('Dice',c('2', 'M52', 'M32'), '0')) # If include no nugget
  if (Dice.include) fits <- c(fits,paste0('Dice',c('M52')))
  #if (Dice.include) fits <- c(fits,paste0('Dice',c('M32')))
  
  fits.cut <- fits[!(fits %in% c('QM','LM','PredictMean'))]
  #print(fits)
  
  # Allow external fits, create single list which gives path.batch
  path.batch.fits <- list()
  for(fit in fits) {
    path.batch.fits[[fit]] <- ifelse(fit %in% external.fits,path.external.runs,path.batch)
  }
  
  # These names will be used for plotting, fits was for finding files
  fits.plot.names <- fits
  fits.plot.names[fits.plot.names=='DACEregpoly0corrgauss'] <- 'DACE'
  fits.plot.names[fits.plot.names=='ooDACEregpoly0corrgauss'] <- 'ooDACE'
  fits.plot.names[fits.plot.names=='ooDACEEregpoly0corrgauss'] <- 'ooDACEE'
  fits.plot.names[fits.plot.names=='Python'] <- 'sklearn'
  fits.plot.names[fits.plot.names=='sklearnRBF'] <- 'sklearn'
  fits.plot.names[fits.plot.names=='JMP2WN'] <- 'JMPE'
  fits.plot.names[fits.plot.names=='JMP2NN'] <- 'JMP0'
  names(fits.plot.names) <- fits # So you can index into plot names with file names
  # Cut excludes LM and PredictMean
  fits.plot.names.cut <- fits.plot.names[!(fits.plot.names %in% c('QM','LM','PredictMean'))]
  
  fit.colors.indices = c(24,33,26,258,31,8,393,450,503,501,640,653,128,656,210,259)
  fit.colors.indices = c('black','red','green','blue','cyan','magenta','magenta4',
                         'chartreuse','tomato4','navy','gray40','orange','olivedrab',
                         'firebrick','goldenrod','mediumspringgreen')
  if (length(fit.colors.indices) < length(fits)) {fit.colors.indices <- c(fit.colors.indices,as.integer(runif(length(fits))*650+1))}
  #fit.colors <- colors()[fit.colors.indices]
  fit.colors <- c('black','red','green','blue','cyan','magenta','magenta4',
                  'chartreuse','gray40','orange','tomato4','navy','olivedrab',
                  'firebrick','goldenrod','mediumspringgreen')
  
  # Going to give each a unique color
  fit.colors.plot.names <- list(GPfit1.95=fit.colors[10],GPfit2=fit.colors[1]
                                ,DACE=fit.colors[2],DACEregpoly0corrgauss=fit.colors[2]
                                ,sklearn=fit.colors[3],Python=fit.colors[3], sklearnRBF=fit.colors[3]
                                ,GPy=fit.colors[4]
                                ,laGPE=fit.colors[5],laGP6=fit.colors[6],laGP3=fit.colors[8],laGP8=fit.colors[9]
                                ,mlegp=fit.colors[7],mlegp0=fit.colors[7],mlegpE=fit.colors[8]
                                ,QM='black',LM='black',PredictMean='black'
                                ,JMP2WN=fit.colors[11],JMP2NN=fit.colors[12]
                                ,Dice2=fit.colors[13], DiceM52=fit.colors[14]
                                )
  for(fit in fits[!(fits %in% names(fit.colors.plot.names))]) {
    print(paste('No color for',fit))
    fit.colors.plot.names[[fit]] <- 'brown'
  }
  
  # Give option to only include certain reps, replace 1:reps with reps.run below
  if (is.null(reps.run)) {reps.run = 1:reps} 
  reps.run.length = length(reps.run)
  
  

  #if () fits <- c(fits,)
  #fits <- c('Python',paste0("GPfit",GPfit.powers),'mlegp')
  dats <- list()
  dats$rmses <- list()
  dats$prmses <- list()
  dats$poarmses <- list()
  dats$pwbrmses <- list()
  dats$xi <- list()
  dats$pi <- list()
  dats$df <- data.frame()
  nn <- pred.ss  #length(ypa)
  normquants <- qnorm((1:nn - .5)/(nn))
  
  for (i in 1:reps.run.length) {
    # open file
    dat <- list()
    minrmse <- Inf # min RMSE for this rep
    for (fit in fits) {
      if (i==1) {
        dats[[fit]] <- list()
        dats$rmses[[fit]] <- list()
        dats$prmses[[fit]] <- list()
        dats$poarmses[[fit]] <- list()
        dats$pwbrmses[[fit]] <- list()
      }
      #load
      #print(paste0(path.batch,batch.name,'_',i,'_Preds_',fit,'.csv'))
      #dat[[fit]] <- list(dat=read.csv(paste0(path.batch,batch.name,'_',i,'_Preds_',fit,'.csv'))   )
      #
      dat[[fit]] <- list(dat=read.csv(paste0(path.batch.fits[[fit]],batch.name,'_',reps.run[i],'_Preds_',fit,'.csv'))   )
      # if sd==0, set to min that is not zero
      dat[[fit]]$dat$ysd0 <- dat[[fit]]$dat$ysd
      if (length(dat[[fit]]$dat$ysd0[dat[[fit]]$dat$ysd0>0])==0) {
        cat(paste(fit,reps.run[i],"predicted all variances to be <= 0\n... setting all to 1e-8\n"))
        dat[[fit]]$dat$ysd0[dat[[fit]]$dat$ysd0<=0] = 1e-8
      } else { # If not all nonpositive then set the nonpositive ones to smallest positive
        dat[[fit]]$dat$ysd0[dat[[fit]]$dat$ysd0==0] <- min(dat[[fit]]$dat$ysd0[dat[[fit]]$dat$ysd0>0])
      }
      dat[[fit]]$dat$zscore <- ( (dat[[fit]]$dat$yp - dat[[fit]]$dat$y)/dat[[fit]]$dat$ysd0 )
      dat[[fit]]$rmse <-   sqrt( sum((dat[[fit]]$dat$yp-dat[[fit]]$dat$y)^2)/length(dat[[fit]]$dat$yp)) 
      minrmse <- min(minrmse,dat[[fit]]$rmse)
      if (mean(dat[[fit]]$dat$yv) < 0) {
        print(paste('Error 5718 in compare, MSE is negative, cant take sqrt. Fit, rep, and value are',
                    fit,reps.run[i],mean(dat[[fit]]$dat$yv),'\n... setting prmse to 1e-8'))
        dat[[fit]]$prmse <- 1e-8
      } else { # Should pretty much always do this
        dat[[fit]]$prmse <-   sqrt( mean(dat[[fit]]$dat$yv)   ) 
      }
      dat[[fit]]$poarmse <-   dat[[fit]]$prmse / dat[[fit]]$rmse # Predicted over actual (so no div by 0) 
      #print(shapiro.test((dat$yp-ypa)/dat$ysd))
      #print(min(dat$ysd))
      
      # add data to dats
      dats[[fit]]$rmses <- c(dats[[fit]]$rmses,dat[[fit]]$rmse)
      dats$rmses[[fit]] <- c(dats$rmses[[fit]],dat[[fit]]$rmse)
      dats[[fit]]$prmses <- c(dats[[fit]]$prmses,dat[[fit]]$prmse)
      dats$prmses[[fit]] <- c(dats$prmses[[fit]],dat[[fit]]$prmse)
      dats[[fit]]$poarmses <- c(dats[[fit]]$poarmses,dat[[fit]]$poarmse)
      dats$poarmses[[fit]] <- c(dats$poarmses[[fit]],dat[[fit]]$poarmse)
      dats$df <- rbind(dats$df,data.frame(fit=fit,rep=i,rmse=dat[[fit]]$rmse,prmse=dat[[fit]]$prmse))
      
      # write out updated data, now has nonzero sd and zscores
      write.csv(dat[[fit]]$dat,paste0(path.batch,batch.name,'_',reps.run[i],'_Preds_',fit,'_post.csv'))
    }
    #if (i==1) {
    #  nn <- dim(dat)[1]
    #  normquants <- qnorm((1:nn - .5)/(nn))
    #  plot(  normquants ,  dat$zscore[order(dat$zscore)],type='l',ylim=c(-3,3))
    #  curve(1*x,add=T,lty=3)
    #} else {
    #  points(  normquants ,  dat$zscore[order(dat$zscore)],type='l',col=i)
    #}
    for(fit in fits) {
      dat[[fit]]$pwbrmse <-   dat[[fit]]$rmse / minrmse - 1 
      dats[[fit]]$pwbrmses <- c(dats[[fit]]$pwbrmses,dat[[fit]]$pwbrmse)
      dats$pwbrmses[[fit]] <- c(dats$pwbrmses[[fit]],dat[[fit]]$pwbrmse)
      
      # Want to add xi and pi
      dat[[fit]]$xi <-   dat[[fit]]$rmse / dat[['LM']]$rmse 
      dats[[fit]]$xi <- c(dats[[fit]]$xi,dat[[fit]]$xi)
      dats$xi[[fit]] <- c(dats$xi[[fit]],dat[[fit]]$xi)
      dat[[fit]]$pi <-   dat[[fit]]$prmse / dat[['LM']]$rmse # MUST divide by rmse of LM, NOT prmse, for correct scaling 
      dats[[fit]]$pi <- c(dats[[fit]]$pi,dat[[fit]]$pi)
      dats$pi[[fit]] <- c(dats$pi[[fit]],dat[[fit]]$pi)
    }
  }
  
  # datsp or datsplot: don't want to plot predict mean, so new list excluding it
  
  datsp <- list()
  #datsp$rmses <- dats$rmses[names(dats$rmses)!='PredictMean']
  #datsp$prmses <- dats$prmses[names(dats$prmses)!='PredictMean']
  datsp_names <- setdiff(names(dats$rmses), c('QM','LM','PredictMean'))
  datsp$rmses <- dats$rmses[!(names(dats$rmses) %in% c('QM','LM','PredictMean'))]
  datsp$prmses <- dats$prmses[!(names(dats$prmses) %in% c('QM','LM','PredictMean'))]
  
  # Convert NA's to a big number, not Inf so it will actually plot something
  datsp$rmses <- lapply(datsp$rmses,function(a)ifelse(is.na(a),1e8, a))
  datsp$prmses <- lapply(datsp$prmses,function(a)ifelse(is.na(a),1e8, a))
  
  # stripchart to compare all rmses
  #stripchart(dats$rmses,pch=4,cex.axis=.7,las=1,xlab='RMSE',main='Actual RMSE')
  #stripchart(dats$prmses,pch=4,cex.axis=.7,las=1,xlab='RMSE',main='Predicted RMSE')
  #
  # save RMSEstripchart
  #RMSE_stripchart_filename <- paste0(path.batch,"Plots//RMSE_stripchart.png")
  #dev.copy(png,filename=paste0(path.batch,"Plots//RMSE_stripchart.png"),width = 640,height = 640,units = "px")
  #dev.copy(png,filename=RMSE_stripchart_filename,width = 640,height = 640,units = "px")
  #png(filename=RMSE_stripchart_filename,width = 640,height = 640,units = "px")
  #stripchart(dats$rmses,pch=4,cex.axis=.7,las=1,xlab='RMSE',main='Actual RMSE')
  #dev.off()
  #atri(paste0(path.batch,"Plots//RMSE_stripchart.png"))
  #atri(RMSE_stripchart_filename)
  
  # save PRMSE stripchart
  #PRMSE_stripchart_filename <- paste0(path.batch,"Plots//PRMSE_stripchart.png")
  #dev.copy(png,filename=paste0(path.batch,"Plots//PRMSE_stripchart.png"),width = 640,height = 640,units = "px")
  #dev.copy(png,filename=PRMSE_stripchart_filename,width = 640,height = 640,units = "px")
  #png(filename=PRMSE_stripchart_filename,width = 640,height = 640,units = "px")
  #stripchart(dats$prmses,pch=4,cex.axis=.7,las=1,xlab='RMSE',main='Predicted RMSE')
  #dev.off()
  #atri(paste0(path.batch,"Plots//PRMSE_stripchart.png"))
  #atri(PRMSE_stripchart_filename)
  #
  # NEW RMSE stripchart
  RMSE_stripchart_filename <- paste0(path.batch,"Plots//RMSE_stripchart.png")
  png(filename=RMSE_stripchart_filename,width = 640,height = 640,units = "px")
  stripchart(datsp$rmses,pch=4,cex.axis=.7,las=1,
             xlab=paste0('RMSE (',signif(mean(unlist(dats$rmses$PredictMean)),2),', ',signif(mean(unlist(dats$rmses$LM)),2),')'),
             main=paste0('Actual RMSE for\n',batch.name),col='white',
             group.names=fits.plot.names.cut
             )
  abline(v=dats$rmses$PredictMean,col=1:reps.run.length)
  abline(v=dats$rmses$LM,col=1:reps.run.length,lty=2,lwd=2)
  for(ii in 1:reps.run.length) {
    stripchart(sapply(datsp$rmses,function(xx){xx[ii]}),add=T,pch=ii,col=ii,cex=2) # could do as.character(ii)
  }
  dev.off()
  atri(RMSE_stripchart_filename)
  
  # NEW PRMSE stripchart
  PRMSE_stripchart_filename <- paste0(path.batch,"Plots//PRMSE_stripchart.png")
  png(filename=PRMSE_stripchart_filename,width = 640,height = 640,units = "px")
  stripchart(datsp$prmses,pch=4,cex.axis=.7,las=1,
             xlab=paste0('RMSE (',signif(mean(unlist(dats$prmses$PredictMean)),2),', ',signif(mean(unlist(dats$prmses$LM)),2),')'),
             main=paste0('Predicted RMSE for\n',batch.name),col='white',
             group.names=fits.plot.names.cut
             )
  abline(v=dats$prmses$PredictMean,col=1:reps.run.length)
  abline(v=dats$prmses$LM,col=1:reps.run.length,lty=2,lwd=2)
  for(ii in 1:reps.run.length) {
    stripchart(sapply(datsp$prmses,function(xx){xx[ii]}),add=T,pch=ii,col=ii) # could do as.character(ii)
  }
  dev.off()
  atri(PRMSE_stripchart_filename)
  
  
  # Finds min and max of RMSE and PRMSE combined, allows to plot on same scale
  minr <- Inf
  maxr <- -Inf
  for (fit in fits.cut){  #j in 1:length(fits)) {
    # Changing this to exclude LM and PredictMean, don't want plot skewed by those
    #fit <- fits[j]
    #print(dats$rmses[[fit]])
    if (fit=='PredictMean') {next}
    minr <- min(minr,unlist(dats$rmses[[fit]]),unlist(dats$prmses[[fit]]), na.rm=T)
    maxr <- max(maxr,unlist(dats$rmses[[fit]]),unlist(dats$prmses[[fit]]), na.rm=T)
    
  }
  
  
  
  #### Start of new RMSE and PRMSE side by side
  default.par.mar <- par('mar')
  RMSE_by_PRMSE_stripchart_filename <- paste0(path.batch,"Plots//RMSE_by_PRMSE_stripchart.png")
  png(filename=RMSE_by_PRMSE_stripchart_filename,width = 1280,height = 640,units = "px")
  par(mfrow=c(1,2))
  par(mar=c(5.1,6,4.1,1))
  stripchart(datsp$rmses,pch=4,cex.axis=1.2,las=1,
             xlab=paste0('RMSE (',signif(mean(unlist(dats$rmses$PredictMean)),2),', ',signif(mean(unlist(dats$rmses$LM)),2),')'),
             main=paste0('Actual RMSE for\n',batch.name),col='white',
             group.names=fits.plot.names.cut,xlim=c(minr,maxr)
  )
  abline(v=dats$rmses$PredictMean,col=1:reps.run.length)
  abline(v=dats$rmses$LM,col=1:reps.run.length,lty=2,lwd=2)
  for(ii in 1:reps.run.length) {
    stripchart(sapply(datsp$rmses,function(xx){xx[ii]}),add=T,pch=14+ii,col=fit.colors,cex=2) # could do as.character(ii)
  }
  
  # Right PRMSE strip
  par(mar=c(5.1,1,4.1,6))
  stripchart(datsp$prmses,pch=4,cex.axis=1.2,las=1,
             xlab=paste0('RMSE (',signif(mean(unlist(dats$prmses$PredictMean)),2),', ',signif(mean(unlist(dats$prmses$LM)),2),')'),
             main=paste0('Predicted RMSE for\n',batch.name),col='white',
             group.names = '',xlim=c(minr,maxr)
  )
  abline(v=dats$prmses$PredictMean,col=1:reps.run.length)
  abline(v=dats$prmses$LM,col=1:reps.run.length,lty=2,lwd=2)
  for(ii in 1:reps.run.length) {
    stripchart(sapply(datsp$prmses,function(xx){xx[ii]}),add=T,pch=14+ii,col=fit.colors,cex=2) # could do as.character(ii)
  }
  dev.off()
  atri(RMSE_by_PRMSE_stripchart_filename)
  par(mfrow=c(1,1))
  par(mar=default.par.mar)
  ###### End of RMSE and PRMSE side by side
  
  
  
  #### Start of new RMSE and PRMSE on same stripchart
  #default.par.mar <- par('mar')
  RMSE_on_PRMSE_stripchart_filename <- paste0(path.batch,"Plots//RMSE_on_PRMSE_stripchart.png")
  png(filename=RMSE_on_PRMSE_stripchart_filename,width = 640,height = 640,units = "px")
  if(is.null(xmax_on[[as.character(input.ss)]])) xmax_on_=maxr else xmax_on_=xmax_on[[as.character(input.ss)]]
  #par(mfrow=c(1,2))
  #par(mar=c(5.1,6,4.1,1))
  stripchart(datsp$rmses,pch=4,cex.axis=1.2,las=1,
             xlab=paste0('RMSE (',signif(mean(unlist(dats$rmses$PredictMean)),2),', ',signif(mean(unlist(dats$rmses$LM)),2),')'),
             main=paste0('RMSE for ',batch.name),col='white',
             group.names=fits.plot.names.cut,xlim=c(minr,xmax_on_)
  )
  abline(v=dats$rmses$PredictMean,col=1:reps.run.length)
  abline(v=dats$rmses$LM,col=1:reps.run.length,lty=2,lwd=2)
  abline(h=1:length(names(dats$rmses)),col='gray51')
  #for(ii in 1:reps) {
  #  stripchart(sapply(datsp$rmses,function(xx){xx[ii]}),add=T,pch=14+((ii-1)%%5+1),col=fit.colors,cex=2) # could do as.character(ii)
  #}
  for(ifit in 1:length(names(datsp$rmses))) {
    fit <- names(datsp$rmses)[ifit]
    for (ii in 1:reps.run.length) {
      if (datsp$rmses[[fit]][[ii]]>xmax_on_) { # Put as outlier at max, not actual value
        points(xmax_on_,ifit,pch='/')
        points(xmax_on_+(.01*(xmax_on_-minr)),ifit,pch='/')
        stripchart(xmax_on_+(.025*(xmax_on_-minr)),add=T,at=ifit,pch=20+((ii-1)%%5+1),
                   col=fit.colors[ifit],bg=fit.colors[ifit],cex=2)
      } else { # plot as normal
        stripchart(datsp$rmses[[fit]][[ii]],add=T,at=ifit,pch=20+((ii-1)%%5+1),
                   col=fit.colors[ifit],bg=fit.colors[ifit],cex=2)
      }
    }
  }
  
  # Add PRMSEs offset up
  abline(v=dats$prmses$PredictMean,col=1:reps.run.length)
  abline(v=dats$prmses$LM,col=1:reps.run.length,lty=2,lwd=2)
  for(ifit in 1:length(names(datsp$prmses))) {
    fit <- names(datsp$prmses)[ifit]
    for (ii in 1:reps.run.length) {
      #stripchart(datsp$prmses[[fit]][[ii]],add=T,at=ifit+.1,pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
      if (datsp$prmses[[fit]][[ii]]>xmax_on_) { # Put as outlier at max, not actual value
        points(xmax_on_,ifit+.1,pch='/')
        points(xmax_on_+(.01*(xmax_on_-minr)),ifit+.1,pch='/')
        stripchart(xmax_on_+(.025*(xmax_on_-minr)),add=T,at=ifit+.1,pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
      } else { # plot as normal
        stripchart(datsp$prmses[[fit]][[ii]],add=T,at=ifit+.1,pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
      }
    }
  }
  dev.off()
  atri(RMSE_on_PRMSE_stripchart_filename)
  #par(mfrow=c(1,1))
  #par(mar=default.par.mar)
  ###### End of RMSE and PRMSE on same stripchart

  
  
  
  
  
  
  
  
  #### Start of new RMSE and PRMSE on same stripchart OVER LM  LINEAR
  excludefromLMplotmax <- c()
  excludefromLMplotmax <- #c("JMP2WN","JMP2NN")#c('mlegp0','mlegpE'), 'JMP2WN','JMP2NN')
  #default.par.mar <- par('mar')
  RMSE_on_PRMSE_over_LM_stripchart_filename <- paste0(path.batch,"Plots//RMSE_on_PRMSE_over_LM_stripchart.png")
  png(filename=RMSE_on_PRMSE_over_LM_stripchart_filename,width = 640,height = 640,units = "px")
  #if(is.null(xmax_on[[as.character(input.ss)]])) xmax_on_=maxr else xmax_on_=xmax_on[[as.character(input.ss)]]
  rep.rmse.mins <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
  rep.prmse.mins <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
  rmse.over.lm.min <- min(rep.rmse.mins/unlist(dats$rmses$LM),rep.prmse.mins/unlist(dats$rmses$LM))
  
  rep.rmse.maxs <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
  rep.prmse.maxs <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
  # Below two let you leave some from affecting plot scale
  rep.rmse.maxs <- apply(sapply(datsp$rmses[!names(datsp$rmses)%in%excludefromLMplotmax],function(xx){xx}),1,function(xrow){max(unlist(xrow))})
  rep.prmse.maxs <- apply(sapply(datsp$prmses[!names(datsp$rmses)%in%excludefromLMplotmax],function(xx){xx}),1,function(xrow){max(unlist(xrow))})
  rmse.over.lm.max <- max(rep.rmse.maxs/unlist(dats$rmses$LM),rep.prmse.maxs/unlist(dats$rmses$LM))

  if (F) {
    message("Setting RMSE/LM plot limits!!! #185720")
    #rmse.over.lm.min <- 0.195 # RGPP 4D B.7
    #rmse.over.lm.max <- .385
    rmse.over.lm.min <- 0
    rmse.over.lm.max <- .27
  }
  
  #par(mfrow=c(1,2))
  # bottom left top right
  #par(mar=c(4.5,5.5,2,1))
  #par(mar=c(2.5,5.5,1,1))
  par(mar=c(2.5,8,.4,1))
  stripchart(datsp$rmses,pch=4,cex.axis=1.2,las=1,
             #xlab=paste0('RMSE/LM'),
             #main=paste0('RMSE/LM for ',batch.name)
             xlab='',main='',
             ,col='white',
             group.names=fits.plot.names.cut,xlim=c(rmse.over.lm.min,rmse.over.lm.max)#c(minr,xmax_on_)
             ,cex.axis=2
  )
  #abline(v=dats$rmses$PredictMean,col=1:reps)
  #abline(v=dats$rmses$LM,col=1:reps,lty=2,lwd=2)
  abline(h=1:length(names(dats$rmses)),col='gray51')
  #for(ii in 1:reps) {
  #  stripchart(sapply(datsp$rmses,function(xx){xx[ii]}),add=T,pch=14+((ii-1)%%5+1),col=fit.colors,cex=2) # could do as.character(ii)
  #}
  for(ifit in 1:length(names(datsp$rmses))) {
    fit <- names(datsp$rmses)[ifit]
    for (ii in 1:reps.run.length) {
      if (datsp$rmses[[fit]][[ii]]>xmax_on_) { # Put as outlier at max, not actual value
        points(xmax_on_,ifit,pch='/')
        points(xmax_on_+(.01*(xmax_on_-minr)),ifit,pch='/')
        stripchart(xmax_on_+(.025*(xmax_on_-minr)),add=T,at=ifit,pch=20+((ii-1)%%5+1),
                   col=fit.colors.plot.names[[fit]],bg=fit.colors.plot.names[[fit]],cex=2)
      } else { # plot as normal
        stripchart(datsp$rmses[[fit]][[ii]]/dats$rmses[['LM']][[ii]],add=T,at=ifit,
                   #pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg=fit.colors[ifit],cex=2)
                   pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg=fit.colors.plot.names[[fit]],cex=2)
      }
    }
  }
  
  # Add PRMSEs offset up
  #abline(v=dats$prmses$PredictMean,col=1:reps)
  #abline(v=dats$prmses$LM,col=1:reps,lty=2,lwd=2)
  for(ifit in 1:length(names(datsp$prmses))) {
    fit <- names(datsp$prmses)[ifit]
    for (ii in 1:reps.run.length) {
      #stripchart(datsp$prmses[[fit]][[ii]],add=T,at=ifit+.1,pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
      if (datsp$prmses[[fit]][[ii]]>xmax_on_) { # Put as outlier at max, not actual value
        points(xmax_on_,ifit+.1,pch='/')
        points(xmax_on_+(.01*(xmax_on_-minr)),ifit+.1,pch='/')
        stripchart(xmax_on_+(.025*(xmax_on_-minr)),add=T,at=ifit+.1,pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg='gray76',cex=2)
      } else { # plot as normal
        stripchart(datsp$prmses[[fit]][[ii]]/dats$rmses[['LM']][[ii]],add=T,at=ifit+.1,
                   #pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
                   pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg='gray76',cex=2)
      }
    }
  }
  dev.off()
  atri(RMSE_on_PRMSE_over_LM_stripchart_filename)
  #par(mfrow=c(1,1))
  par(mar=default.par.mar)
  ###### End of RMSE and PRMSE on same stripchart OVER LM
  

  
  
  
  
  
  #### Start of new RMSE and PRMSE on same stripchart OVER LM LINEAR VERSION 2 !!!
  excludefromLMplotmax <- c()
  excludefromLMplotmax <- #c("JMP2WN","JMP2NN")#c('mlegp0','mlegpE'), 'JMP2WN','JMP2NN')
  RMSE_on_PRMSE_over_LM_stripchart_filename <- paste0(path.batch,"Plots//RMSE_on_PRMSE_over_LM_stripchart_outlier.png")
  png(filename=RMSE_on_PRMSE_over_LM_stripchart_filename,width = 640,height = 640,units = "px") # res=300 zooms in really far, bad
  #if(is.null(xmax_on[[as.character(input.ss)]])) xmax_on_=maxr else xmax_on_=xmax_on[[as.character(input.ss)]]
  
  # ONLY recalculate lims if needed
  if (F) {
    rep.rmse.mins <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
    rep.prmse.mins <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
    rmse.over.lm.min <- min(rep.rmse.mins/unlist(dats$rmses$LM),rep.prmse.mins/unlist(dats$rmses$LM))
    
    rep.rmse.maxs <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rep.prmse.maxs <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    # Below two let you leave some from affecting plot scale
    rep.rmse.maxs <- apply(sapply(datsp$rmses[!names(datsp$rmses)%in%excludefromLMplotmax],function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rep.prmse.maxs <- apply(sapply(datsp$prmses[!names(datsp$rmses)%in%excludefromLMplotmax],function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rmse.over.lm.max <- max(rep.rmse.maxs/unlist(dats$rmses$LM),rep.prmse.maxs/unlist(dats$rmses$LM))
  }
  # Only redo if you don't want to use values above
  if (F) {
    warning("Setting RMSE/LM plot limits!!! in version 2 #42498")
    rmse.over.lm.min <- .017
    rmse.over.lm.max <- 0.58282059
    # For first revision, Feb 2017
    # Borehole1357 40 has laGPE xi max 0.7999101, use this for same scale
    # Borehole 80 has laGPE xi max 0.7442428
    # OTL 60 DiceM52 has max pi 0.343757837, JMP0 0.52933703 and laGPE are bigger
    # OTL 120 laGPE has max pi 0.215664666
    # Detpep has JMP2NN pi max 0.58282058
    
    # Borehole1357 (0,.53221) works for all 4 on same scale
    # OTL 200 use full
    # OTL 400 (0, .037) one at .03671
    # Detpep 400 (0, .1) a DK at .0618, laGPE at .0968
    
    
    ## From first submission
    # OTL 200 0, .105; 400 0, .03
    # Borehole1357 100 ?; 250 full
    # Borehole 200 0, .525; 500 0, .4
    # Borehole same scale 0, .6
    # RGP 2D B.7  50 0, .026
    # RGP 2D B1.3  50 .1, .203
    # RGP 4D B1.3 150 .66, 1.08
    # RGP 6D B1.3 300 .78, 1.48
    # DetPep 400 0, .165
  }
  xmax_on_ <- rmse.over.lm.max + .075 * (rmse.over.lm.max - rmse.over.lm.min)
  xmin_on_ <- rmse.over.lm.min - .075 * (rmse.over.lm.max - rmse.over.lm.min)
  
  # set area
  # bottom left top right
  par(mar=c(2.5,8,.4,2))
  stripchart(datsp$rmses,pch=4,cex.axis=1.2,las=1,
             xlab='',main='',
             ,col='white',
             group.names=fits.plot.names.cut,xlim=c(rmse.over.lm.min,rmse.over.lm.max)#c(minr,xmax_on_)
             ,ylim=c(1, length(names(datsp$rmses)) + .3)
             ,cex.axis=2
  )
  abline(h=1:length(names(dats$rmses)),col='gray51')
  
  # both at once
  # First time just do lines
  par(xpd=T) # Lets points be plotted off of edge
  for(ifit in 1:length(names(datsp$rmses))) {
    fit <- names(datsp$rmses)[ifit]
    for (ii in 1:reps.run.length) {
      xi <- datsp$rmses[[fit]][[ii]] /dats$rmses[['LM']][[ii]]
      pi <- datsp$prmses[[fit]][[ii]]/dats$rmses[['LM']][[ii]]
      xi.x <- xi
      xi.y <- ifit #- 0.15
      pi.x <- pi
      pi.y <- ifit + .4#0.15
      if (xi.x > rmse.over.lm.max) { # Put as outlier at max, not actual value
        xi.x <- xmax_on_
      } else if (xi.x < rmse.over.lm.min) { # Put as outlier at min, not actual value
        xi.x <- xmin_on_
      }
      if (pi.x > rmse.over.lm.max) { # plot outlier
        pi.x <- xmax_on_
      } else if (pi.x < rmse.over.lm.min) { # plot outlier
        pi.x <- xmin_on_
      }
      lines(x=c(xi.x, pi.x), 
            y=c(xi.y, pi.y))
      #stripchart(xi.x,add=T,at=xi.y,
      #           pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg=fit.colors.plot.names[[fit]],cex=2)
      #stripchart(pi.x,add=T,at=pi.y,
      #           pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg='gray76',cex=2)
    }
  }
  # Second time do points
  for(ifit in 1:length(names(datsp$rmses))) {
    fit <- names(datsp$rmses)[ifit]
    for (ii in 1:reps.run.length) {
      xi <- datsp$rmses[[fit]][[ii]] /dats$rmses[['LM']][[ii]]
      pi <- datsp$prmses[[fit]][[ii]]/dats$rmses[['LM']][[ii]]
      xi.x <- xi
      xi.y <- ifit #- 0.15
      pi.x <- pi
      pi.y <- ifit + .4#0.15
      if (xi.x > rmse.over.lm.max) { # Put as outlier at max, not actual value
        xi.x <- xmax_on_
      } else if (xi.x < rmse.over.lm.min) { # Put as outlier at min, not actual value
        xi.x <- xmin_on_
      }
      if (pi.x > rmse.over.lm.max) { # plot outlier
        pi.x <- xmax_on_
      } else if (pi.x < rmse.over.lm.min) { # plot outlier
        pi.x <- xmin_on_
      }
      #lines(x=c(xi.x, pi.x), 
      #      y=c(xi.y, pi.y))
      stripchart(xi.x,add=T,at=xi.y,
                 pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg=fit.colors.plot.names[[fit]],cex=2)
      stripchart(pi.x,add=T,at=pi.y,
                 pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg='gray76',cex=2)
    }
  }
  
  dev.off()
  atri(RMSE_on_PRMSE_over_LM_stripchart_filename)
  par(mar=default.par.mar)
  par(xpd=F)
  ###### End of RMSE and PRMSE on same stripchart OVER LM VERSION 2!!!!!
  

  
  
  
  #### Start of new RMSE and PRMSE on same stripchart OVER LM LOG SCALE ADDING 1/11/17
  excludefromLMplotmax <- c()
  #excludefromLMplotmax <- #c("JMP2WN","JMP2NN")#c('mlegp0','mlegpE'), 'JMP2WN','JMP2NN')
  RMSE_on_PRMSE_over_LM_stripchart_log_filename <- paste0(path.batch,"Plots//RMSE_on_PRMSE_over_LM_stripchart_log.png")
  png(filename=RMSE_on_PRMSE_over_LM_stripchart_log_filename,width = 640,height = 640,units = "px") # res=300 zooms in really far, bad
  #if(is.null(xmax_on[[as.character(input.ss)]])) xmax_on_=maxr else xmax_on_=xmax_on[[as.character(input.ss)]]
  
  # ONLY recalculate lims if needed
  if (T) {
    rep.rmse.mins <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
    rep.prmse.mins <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
    rmse.over.lm.min <- min(rep.rmse.mins/unlist(dats$rmses$LM),rep.prmse.mins/unlist(dats$rmses$LM))
    
    rep.rmse.maxs <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rep.prmse.maxs <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    # Below two let you leave some from affecting plot scale
    rep.rmse.maxs <- apply(sapply(datsp$rmses[!names(datsp$rmses)%in%excludefromLMplotmax],function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rep.prmse.maxs <- apply(sapply(datsp$prmses[!names(datsp$rmses)%in%excludefromLMplotmax],function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rmse.over.lm.max <- max(rep.rmse.maxs/unlist(dats$rmses$LM),rep.prmse.maxs/unlist(dats$rmses$LM))
  }
  # Only redo if you don't want to use values above
  if (F) {
    warning("Setting RMSE/LM plot limits!!! in version 2 #42498")
    rmse.over.lm.min <- .0
    rmse.over.lm.max <- 1
    # OTL 200 0, .105; 400 0, .03
    # Borehole1357 100 ?; 250 full
    # Borehole 200 0, .525; 500 0, .4
    # Borehole same scale 0, .6
    # RGP 2D B.7  50 0, .026
    # RGP 2D B1.3  50 .1, .203
    # RGP 4D B1.3 150 .66, 1.08
    # RGP 6D B1.3 300 .78, 1.48
    # DetPep 400 0, .165
  }
  xmax_on_ <- rmse.over.lm.max + .075 * (rmse.over.lm.max - rmse.over.lm.min)
  xmin_on_ <- rmse.over.lm.min - .075 * (rmse.over.lm.max - rmse.over.lm.min)
  #if (xmin_on_ <= 0) {xmin_on_ <- rmse.over.lm.min}
  
  # set area
  # bottom left top right
  par(mar=c(2.5,8,.4,2))
  stripchart(datsp$rmses,pch=4,cex.axis=1.2,las=1,
             xlab='',main='',
             ,col='white',
             group.names=fits.plot.names.cut,xlim=c(rmse.over.lm.min,rmse.over.lm.max)#c(minr,xmax_on_)
             ,ylim=c(1, length(names(datsp$rmses)) + .3)
             ,cex.axis=2
             ,log='x'
  )
  abline(h=1:length(names(dats$rmses)),col='gray51')
  
  # both at once
  # First time just do lines
  par(xpd=T) # Lets points be plotted off of edge
  for(ifit in 1:length(names(datsp$rmses))) {
    fit <- names(datsp$rmses)[ifit]
    for (ii in 1:reps.run.length) {
      xi <- datsp$rmses[[fit]][[ii]] /dats$rmses[['LM']][[ii]]
      pi <- datsp$prmses[[fit]][[ii]]/dats$rmses[['LM']][[ii]]
      xi.x <- xi
      xi.y <- ifit #- 0.15
      pi.x <- pi
      pi.y <- ifit + .4#0.15
      if (xi.x > rmse.over.lm.max) { # Put as outlier at max, not actual value
        xi.x <- xmax_on_
      } else if (xi.x < rmse.over.lm.min) { # Put as outlier at min, not actual value
        xi.x <- xmin_on_
      }
      if (pi.x > rmse.over.lm.max) { # plot outlier
        pi.x <- xmax_on_
      } else if (pi.x < rmse.over.lm.min) { # plot outlier
        pi.x <- xmin_on_
      }
      lines(x=c(xi.x, pi.x), 
            y=c(xi.y, pi.y))
      #stripchart(xi.x,add=T,at=xi.y,
      #           pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg=fit.colors.plot.names[[fit]],cex=2)
      #stripchart(pi.x,add=T,at=pi.y,
      #           pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg='gray76',cex=2)
    }
  }
  # Second time do points
  for(ifit in 1:length(names(datsp$rmses))) {
    fit <- names(datsp$rmses)[ifit]
    for (ii in 1:reps.run.length) {
      xi <- datsp$rmses[[fit]][[ii]] /dats$rmses[['LM']][[ii]]
      pi <- datsp$prmses[[fit]][[ii]]/dats$rmses[['LM']][[ii]]
      xi.x <- xi
      xi.y <- ifit #- 0.15
      pi.x <- pi
      pi.y <- ifit + .4#0.15
      if (xi.x > rmse.over.lm.max) { # Put as outlier at max, not actual value
        xi.x <- xmax_on_
      } else if (xi.x < rmse.over.lm.min) { # Put as outlier at min, not actual value
        xi.x <- xmin_on_
      }
      if (pi.x > rmse.over.lm.max) { # plot outlier
        pi.x <- xmax_on_
      } else if (pi.x < rmse.over.lm.min) { # plot outlier
        pi.x <- xmin_on_
      }
      #lines(x=c(xi.x, pi.x), 
      #      y=c(xi.y, pi.y))
      stripchart(xi.x,add=T,at=xi.y,
                 pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg=fit.colors.plot.names[[fit]],cex=2)
      stripchart(pi.x,add=T,at=pi.y,
                 pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg='gray76',cex=2)
    }
  }
  
  dev.off()
  atri(RMSE_on_PRMSE_over_LM_stripchart_log_filename)
  par(mar=default.par.mar)
  par(xpd=F)
  ###### End of RMSE and PRMSE on same stripchart OVER LM LOG SCALE
  
  
  
  
  
  
  
  try( # trying since QM has NA for preds and 0 for pvar when sample size too small
  #### Start of new RMSE and PRMSE on same stripchart OVER QM  QUADRATIC
  if (input.dim <= 10) { # 1/14/17 QM only calculated for input.dim<=10
    #default.par.mar <- par('mar')
    RMSE_on_PRMSE_over_QM_stripchart_filename <- paste0(path.batch,"Plots//RMSE_on_PRMSE_over_QM_stripchart.png")
    png(filename=RMSE_on_PRMSE_over_QM_stripchart_filename,width = 640,height = 640,units = "px")
    #if(is.null(xmax_on[[as.character(input.ss)]])) xmax_on_=maxr else xmax_on_=xmax_on[[as.character(input.ss)]]
    rep.rmse.mins <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
    rep.prmse.mins <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){min(unlist(xrow))})
    rmse.over.qm.min <- min(rep.rmse.mins/unlist(dats$rmses$QM),rep.prmse.mins/unlist(dats$prmses$QM))
    rep.rmse.maxs <- apply(sapply(datsp$rmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rep.prmse.maxs <- apply(sapply(datsp$prmses,function(xx){xx}),1,function(xrow){max(unlist(xrow))})
    rmse.over.qm.max <- max(rep.rmse.maxs/unlist(dats$rmses$QM),rep.prmse.maxs/unlist(dats$prmses$QM))
    #par(mfrow=c(1,2))
    #par(mar=c(5.1,6,4.1,1))
    stripchart(datsp$rmses,pch=4,cex.axis=1.2,las=1,
               xlab=paste0('RMSE/QM'),
               main=paste0('RMSE/QM for ',batch.name),col='white',
               group.names=fits.plot.names.cut,xlim=c(rmse.over.qm.min,rmse.over.qm.max)#c(minr,xmax_on_)
    )
    #abline(v=dats$rmses$PredictMean,col=1:reps)
    #abline(v=dats$rmses$LM,col=1:reps,lty=2,lwd=2)
    #abline(h=1:length(names(dats$rmses)),col='gray51')
    #for(ii in 1:reps) {
    #  stripchart(sapply(datsp$rmses,function(xx){xx[ii]}),add=T,pch=14+((ii-1)%%5+1),col=fit.colors,cex=2) # could do as.character(ii)
    #}
    for(ifit in 1:length(names(datsp$rmses))) {
      fit <- names(datsp$rmses)[ifit]
      for (ii in 1:reps.run.length) {
        if (datsp$rmses[[fit]][[ii]]>xmax_on_) { # Put as outlier at max, not actual value
          points(xmax_on_,ifit,pch='/')
          points(xmax_on_+(.01*(xmax_on_-minr)),ifit,pch='/')
          stripchart(xmax_on_+(.025*(xmax_on_-minr)),add=T,at=ifit,pch=20+((ii-1)%%5+1),
                     col=fit.colors[ifit],bg=fit.colors[ifit],cex=2)
        } else { # plot as normal
          stripchart(datsp$rmses[[fit]][[ii]]/dats$rmses[['QM']][[ii]],add=T,at=ifit,
                     pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg=fit.colors[ifit],cex=2)
        }
      }
    } # end for
    
    # Add PRMSEs offset up
    #abline(v=dats$prmses$PredictMean,col=1:reps)
    #abline(v=dats$prmses$LM,col=1:reps,lty=2,lwd=2)
    for(ifit in 1:length(names(datsp$prmses))) {
      fit <- names(datsp$prmses)[ifit]
      for (ii in 1:reps.run.length) {
        #stripchart(datsp$prmses[[fit]][[ii]],add=T,at=ifit+.1,pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
        if (datsp$prmses[[fit]][[ii]]>xmax_on_) { # Put as outlier at max, not actual value
          points(xmax_on_,ifit+.1,pch='/')
          points(xmax_on_+(.01*(xmax_on_-minr)),ifit+.1,pch='/')
          stripchart(xmax_on_+(.025*(xmax_on_-minr)),add=T,at=ifit+.1,pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
        } else { # plot as normal
          stripchart(datsp$prmses[[fit]][[ii]]/dats$prmses[['QM']][[ii]],add=T,at=ifit+.1,
                     pch=20+((ii-1)%%5+1),col=fit.colors[ifit],bg='gray76',cex=2)
        }
      }
    }
    dev.off()
    atri(RMSE_on_PRMSE_over_QM_stripchart_filename)
    #par(mfrow=c(1,1))
    #par(mar=default.par.mar)
    ###### End of RMSE and PRMSE on same stripchart OVER QM
    
  } # end if input.dim <=10
  ) # end try, had problem with QM when sample size was small
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Scatter of RMSE vs PRMSE
  if (F){ # Copied this to above, shouldn't need this part
    minr <- Inf
    maxr <- -Inf
    for (fit in fits.cut){  #j in 1:length(fits)) {
      # Changing this to exclude LM and PredictMean, don't want plot skewed by those
      #fit <- fits[j]
      #print(dats$rmses[[fit]])
      if (fit=='PredictMean') {next}
      minr <- min(minr,unlist(dats$rmses[[fit]]),unlist(dats$prmses[[fit]]))
      maxr <- max(maxr,unlist(dats$rmses[[fit]]),unlist(dats$prmses[[fit]]))
    }
  }
  RMSE_scatter_filename <- paste0(path.batch,"Plots//RMSE_scatter.png")
  #
  for  (k in 1:1) {    # Was 1:2, now don't want to display plot, only save it
    if (k==1) { # First one saves to file, second is displayed
      #dev.copy(png,filename=paste0(path.batch,"Plots//RMSE_scatter.png"),width = 640,height = 640,units = "px") 
      #dev.copy(png,filename=RMSE_scatter_filename,width = 640,height = 640,units = "px")
      png(filename=RMSE_scatter_filename,width = 640,height = 640,units = "px")
    }
    for (j in 1:length(fits)) {
      fit <- fits[j]
      if (j==1) {
        plot(dats$rmses[[fit]],dats$prmses[[fit]],xlim=c(minr,maxr),ylim=c(minr,maxr),col=fit.colors[j],pch=1:reps.run.length,
             main=paste0('Predicted vs actual RMSEs for\n',batch.name),
             xlab=paste0('Actual RMSE (',signif(mean(unlist(dats$rmses$PredictMean)),2),', ',signif(mean(unlist(dats$rmses$LM)),2),')'),
             ylab=paste0('Predicted RMSE (',signif(mean(unlist(dats$prmses$PredictMean)),2),', ',signif(mean(unlist(dats$prmses$LM)),2),')'))
        #legend(x='topleft',legend=fits,fill=fit.colors[1:length(fits)],cex=.7) Change to fits.plot.names
        legend(x='topleft',legend=fits.plot.names,fill=fit.colors[1:length(fits)],cex=.7)
        curve(1*x,col='pink',add=T)
      } else {
        points(dats$rmses[[fit]],dats$prmses[[fit]],col=fit.colors[j],pch=1:reps.run.length)
      }
    }
    if (k==1) {
      dev.off()
      #atri(paste0(path.batch,"Plots//RMSE_scatter.png"))
      atri(RMSE_scatter_filename)
    }
  }
  
  #print(dats$rmses)
  #print(dats$prmses)
  #plot(dats$rmses,dats$prmses)
  
  # Try to get R^2# R2 get
  dats$df$R2 <- NA
  dats$df$R <- NA
  dats$df$PBPM
  for(rowind in 1:(dim(dats$df)[1])){
    predictmeanind <- which(dats$df$fit=='PredictMean' & dats$df$rep==dats$df$rep[rowind])
    dats$df$R2[rowind] <- 1 - ( dats$df$rmse[rowind] / dats$df$rmse[predictmeanind] )^2 #square for R2 to get MSE not RMSE
    dats$df$R[rowind] <- sqrt(abs(dats$df$R2[rowind]))*sign(dats$df$R2[rowind])
    dats$df$PBPM[rowind] <- 1 - dats$df$rmse[rowind] / dats$df$rmse[predictmeanind]
  }
  
  #for(i in 1:length(fits)) {
  #  fit <- fits[i]  
  #}
  R2_stripchart_filename <- paste0(path.batch,"Plots//R2_stripchart.png")
  png(filename=R2_stripchart_filename,width = 640,height = 640,units = "px")
  stripchart(dlply(dats$df,'fit',function(xx){xx$R2}),pch=4,cex.axis=.7,las=1,xlab='R2',main=paste0('R2 for ',batch.name),col='white')
  for(ii in 1:reps.run.length) {
    stripchart(dlply(dats$df,'fit',function(xx){xx$R2[ii]}),add=T,pch=ii,col=ii) # could do as.character(ii)
  }
  dev.off()
  atri(R2_stripchart_filename)
  
  
  
  # plot all qqs
  if (T) {
    #dev.copy(png,filename=paste0(path.batch,"Plots//QQplot_all.png"),width = 640,height = 640,units = "px")
    png(filename=paste0(path.batch,"Plots//QQplot_all.png"),width = 640,height = 640,units = "px")
    for (j in 1:length(fits)) {#print(j)
      fit <- fits[j]
      for (i in 1:reps.run.length) {
        #print(c(i,j))
        dat=read.csv(paste0(path.batch,batch.name,'_',i,'_Preds_',fit,'_post.csv')) 
        if (i==1 & j==1) {
          plot(  normquants ,  dat$zscore[order(dat$zscore)],type='l',ylim=c(-5,5),lty=i,
                 ylab="Predicted z-scores",xlab="Expected z-scores",main="Q-Q plot of all data")
          curve(1*x,add=T,col='brown',lty=2,lwd=4)
          legend('topleft',legend=fits,fill=1:length(fits))
        } else {
          points(  normquants ,  dat$zscore[order(dat$zscore)],type='l',col=j,lty=i)
        }
      }
    }
    dev.off()
  }
  
  # plot all qqs
  if (T) {
    #dev.copy(png,filename=paste0(path.batch,"Plots//errorplot_all.png"),width = 640,height = 640,units = "px")
    png(filename=paste0(path.batch,"Plots//errorplot_all.png"),width = 640,height = 640,units = "px")
    for (j in 1:length(fits)) {#print(j)
      fit <- fits[j]
      for (i in 1:reps.run.length) { 
        #print(c(i,j))
        dat=read.csv(paste0(path.batch,batch.name,'_',i,'_Preds_',fit,'_post.csv')) 
        #print(length(dat$ysd));print(length(dat$yp-dat$y))
        if (i==1 & j==1) {
          plot(  dat$ysd ,  abs(dat$y-dat$yp), pch=19,col=j,
                 ylab="prediction error",xlab="ysd",main="error plot of all data")
          curve(1*x,add=T,col='brown',lty=2,lwd=4)
          legend('topleft',legend=fits,fill=1:length(fits))
        } else {
          points(  dat$ysd ,  abs(dat$y-dat$yp) ,col=j,pch=19)
        }
      }
    }
    dev.off()
  }

  #print('Param estimates')
  # compare param estimates
  if(T) { # Was going to remove this
    param.estimates <- list()
    sigma2s <- data.frame()
    betas <- list()
    for (dd in 1:input.dim) {betas[[dd]] = data.frame()}
    for (fit in fits){
      param.estimates[[fit]] <- data.frame()
      param.paths <- get.file.names(path.batch=path.batch.fits[[fit]],batch.name=batch.name,reps=reps,fit.name=fit,pre="",subfolder="Params")
      for (pp in param.paths) {
        pdat <- read.csv(pp)
        param.estimates[[fit]] <- rbind(param.estimates[[fit]],pdat)
      }  
      if (length(sigma2s)==0) {
        sigma2s <- data.frame(param.estimates[[fit]]$sigma2)
        names(sigma2s) <- c(fit)
      }
      else {
        sigma2s[fit] <- param.estimates[[fit]]$sigma2
      }
      for (dd in 1:input.dim) {
        if (length(betas[[dd]])==0) {
          betas[[dd]] <- data.frame(fit=param.estimates[[fit]][paste0('beta.',dd)])
          names(betas[[dd]]) <- c(fit)
        }
        else {
          betas[[dd]][fit] <- param.estimates[[fit]][paste0('beta.',dd)]
        }
      }
    }
    #print (param.estimates)
    Sigma2s_stripchart_filename <- paste0(path.batch,"Plots//Sigma2s.png")
    png(filename=Sigma2s_stripchart_filename,width = 640,height = 640,units = "px")
    stripchart(sigma2s,pch=4,cex.axis=.7,las=1,xlab=expression(sigma^2),main=expression(sigma^2))
    dev.off()
    for (dd in 1:input.dim) {
      #print('here are betas');print(input.dim)
      #print(betas[[dd]])
      
      Betas_stripchart_filename <- paste0(path.batch,"Plots//Betas.png")
      png(filename=Betas_stripchart_filename,width = 640,height = 640,units = "px")
      stripchart(betas[[dd]],pch=4,cex.axis=.7,las=1,xlab=substitute(beta[ddd],list(ddd=dd)),main=substitute(beta[ddd],list(ddd=dd)))
      dev.off()
    }
  }
  
  # Run Time comparison
  run.times <- matrix(0,nrow=reps,ncol=length(fits))
  colnames(run.times) <- fits
  run.times <- as.data.frame(run.times)
  #run.times <- list()
  #run.times.fit <- data.frame()
  for (fit in fits){
    #print(fit)
    #run.times[[fit]] <- data.frame()
    #if (fit == 'mlegp' || substr(fit,1,5)=='GPfit' || substr(fit,1,3)=='JMP' ) {
      #run.time.paths <- get.file.names(path.batch=path.batch,batch.name=batch.name,reps=reps,fit.name=fit,pre="",subfolder="RunTimes")
      run.time.paths <- get.file.names(path.batch=path.batch.fits[[fit]],batch.name=batch.name,reps=reps,fit.name=fit,pre="",subfolder="RunTimes")
      #for (rt in run.time.paths) {
      for (irt in 1:reps.run.length) {
        rt <- run.time.paths[reps.run[irt]]
        rtdat <- read.csv(rt)
        #print(rtdat)
        #param.estimates[[fit]] <- rbind(param.estimates[[fit]],rtdat)
        run.times[irt,fit] <- rtdat$elapsed
        #print(pdat)
      }  
    #}
    #if (length(sigma2s)==0) {
    #  sigma2s <- data.frame(param.estimates[[fit]]$sigma2)
    #  names(sigma2s) <- c(fit)
    #}
    #else {
    #  sigma2s[fit] <- param.estimates[[fit]]$sigma2
    #}
      
  }
  #print (param.estimates)


  ### Make run time stripchart
  # This chunk is used to set the numbers on the x-axis
  # Added since some scientific notation looked bad
  setruntimeaxis = FALSE #TRUE
  #scipen0 = options()$scipen
  #options(scipen=5)
  if (setruntimeaxis) {
    message("WARNING: Run time axis set")
    xaxtruntime = 'n'
    # xaxisatruntime = c(0.5,5,50,500,5000)
    xaxisatruntime = c(0.1,1,10,100) # For Borehole 8D SS=80
    xaxisatruntime = c(1,10,100, 1000) # For Borehole 8D SS=160
  } else {
    xaxtruntime = NULL
  }
  
  #stripchart(run.times,main='Run times',las=1,pch=4,cex.axis=.7) # no longer showing plots, only saving
  RunTimes_stripchart_filename <- paste0(path.batch,"Plots//RunTimes.png")
  #dev.copy(png,filename=paste0(path.batch,"Plots//RunTimes.png"),width = 640,height = 640,units = "px")
  #dev.copy(png,filename=RunTimes_stripchart_filename,width = 640,height = 640,units = "px")
  png(filename=RunTimes_stripchart_filename,width = 640,height = 640,units = "px")
  par(mar=c(4.5,5.5,2,1))
  par(mar=c(2.5,8,.4,1))
  run.timesp <- run.times[,names(run.times)[names(run.times)%in%fits.cut]] # which ones to plot
  stripchart(run.timesp,
             main='',#paste0('Run times for ',batch.name), 
             xlab='',#Run time (seconds)',
             las=1,pch=4,cex.axis=2,#1.2,
             col='white',log='x',
             group.names=fits.plot.names.cut
             ,xaxt = xaxtruntime # This removes axis labels if set to 'n'
             )
  if (setruntimeaxis) {axis(1, at=xaxisatruntime, labels=xaxisatruntime,cex.axis=2)}
  abline(h=1:length(names(dats$rmses)),col='gray51')
  #for(ii in 1:reps.run.length) {
  #  stripchart(run.times[ii,],add=T,pch=ii,col=ii,log='x')
  #}
  for(ifit in 1:length(names(run.timesp))) {
    fit <- names(run.timesp)[ifit]
    for (ii in 1:reps.run.length) {
      stripchart(run.timesp[[fit]][[ii]],add=T,at=ifit,
                 pch=20+((ii-1)%%5+1),col=fit.colors.plot.names[[fit]],bg=fit.colors.plot.names[[fit]],cex=2)      
    }
  }
  
  par(mar=default.par.mar)
  #options(scipen=scipen0)
  dev.off()
  atri(RunTimes_stripchart_filename)
  
  # plot images side by side onto same image
  imgRMSE_stripchart <- readPNG(RMSE_stripchart_filename)
  imgPRMSE_stripchart <- readPNG(PRMSE_stripchart_filename)
  imgRMSE_scatter <- readPNG(RMSE_scatter_filename)
  imgRunTimes_stripchart <- readPNG(RunTimes_stripchart_filename)
  
  # plot RMSE and PRMSE stripcharts next to each other
  png(paste0(path.batch,"Plots//RMSE_stripchart_PRMSE_stripchart.png"),width=1280,height=640,units='px')
  omargins <- par()$mar  
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:2, ncol=2, byrow=TRUE))
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
  rasterImage(imgRMSE_stripchart,0,0,1,1)
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
  rasterImage(imgPRMSE_stripchart,0,0,1,1)
  #dev.print(png, paste0(path.batch,"Plots//RMSE_stripchart_PRMSE_stripchart.png"),width=1280,height=640,units='px') # before was just dev.print, no png or dev.off
  dev.off()
  
  # plot stripcharts and scatter next to each other
  png(paste0(path.batch,"Plots//RMSE_stripchart_PRMSE_stripchart_RMSE_scatter.png"),width=1920,height=640,units='px')
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:3, ncol=3, byrow=TRUE))
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
  rasterImage(imgRMSE_stripchart,0,0,1,1)
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
  rasterImage(imgPRMSE_stripchart,0,0,1,1)
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
  rasterImage(imgRMSE_scatter,0,0,1,1)
  #dev.print(png, paste0(path.batch,"Plots//RMSE_stripchart_PRMSE_stripchart_RMSE_scatter.png"),width=1920,height=640,units='px') # before was just dev.print, no png or dev.off
  dev.off()
  
  # plot stripcharts and scatter and run times next to each other
  png(paste0(path.batch,"Plots//RMSE_stripchart_PRMSE_stripchart_RMSE_scatter_RunTime_stripchart_RunTimes_stripchart.png"),width=2560,height=640,units='px')
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:4, ncol=4, byrow=TRUE))
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
  rasterImage(imgRMSE_stripchart,0,0,1,1)
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
  rasterImage(imgPRMSE_stripchart,0,0,1,1)
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
  rasterImage(imgRMSE_scatter,0,0,1,1)
  plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
  rasterImage(imgRunTimes_stripchart,0,0,1,1)
  #dev.print(png, paste0(path.batch,"Plots//RMSE_stripchart_PRMSE_stripchart_RMSE_scatter_RunTime_stripchart_RunTimes_stripchart.png"),width=2560,height=640,units='px')
  dev.off()
  
  layout(matrix(1))
  par(mar=omargins)

  
  
  
  
  #report.path <- paste0(path.batch,'Report.Rmd')
  #report.path.html.out <- paste0(path.batch,'ReportOut.html')
  if (knit.report) {
    knitr::knit2html(input=report.path,output=report.path.html.out)
  }
  
  
  # Write out a table for the supplementary data provided with the paper
  # # names(dats$rmses)
  supplementary_table <- data.frame()
  for (fit in fits) {
    for (rep in 1:reps.run.length) {
      new.row <- data.frame(Fit=fits.plot.names[fit],
                            EMRMSE=dats[[fit]][['rmses']][rep],
                            PMRMSE=dats[[fit]][['prmses']][rep],
                            POARMSE=dats[[fit]][['poarmses']][rep],
                            PWBRMSE=dats[[fit]][['pwbrmses']][rep],
                            xi=dats[[fit]][['xi']][rep],
                            pi=dats[[fit]][['pi']][rep],
                            Rep=rep,
                            RunTime=run.times[rep,fit]
      )
      supplementary_table <- rbind(supplementary_table,new.row)
    }
  }
  write.csv(x=supplementary_table, file=paste0(path.base,batch.name,"//",batch.name,"_SupplementaryTable.csv"), row.names=FALSE)
  
  
  
  # create return data table
  return.data <- data.frame()
  for (fit in fits) {
    for (rep in 1:reps.run.length) {
      new.row <- data.frame(fit=fit,
                            rmse=dats[[fit]][['rmses']][rep],
                            prmse=dats[[fit]][['prmses']][rep],
                            poarmse=dats[[fit]][['poarmses']][rep],
                            pwbrmse=dats[[fit]][['pwbrmses']][rep],
                            xi=dats[[fit]][['xi']][rep],
                            pi=dats[[fit]][['pi']][rep],
                            rep=rep,
                            elapsed=run.times[rep,fit]
                            #,beta=betas[[rep]][[fit]]
                            , sigma2 = param.estimates[[fit]][rep,'sigma2']
                            , delta = param.estimates[[fit]][rep,'delta']
                            )
      beta.row <- data.frame(param.estimates[[fit]][rep,paste0('beta.',dd)])
      if (input.dim>1) {
        for (dd in 2:input.dim) {
          beta.row <- cbind(beta.row,param.estimates[[fit]][rep,paste0('beta.',dd)])
        }
      }
      if (length(beta.row)==0) {beta.row <- as.data.frame(matrix(NA,1,input.dim))}
      names(beta.row) <- paste0('beta.',1:input.dim)
      new.row <- cbind(new.row,beta.row)
      return.data <- rbind(return.data,new.row)
    }
  }
  #print('return data is')
  #print(return.data)
  return(return.data)
}


random.qqplots <- function() {
  # This function makes random qqplots to get an idea of how they vary.
  # No use at all, can delete.
  # Test randomness
  for (i in 1:100) {
    zscore <- rnorm(200,0,1)
    if (i==1) {
      nn <- 200
      normquants <- qnorm((1:nn - .5)/(nn))
      plot(  normquants ,  zscore[order(zscore)],type='l',ylim=c(-3,3))
      curve(1*x,add=T,lty=3)
    } else {
      points(  normquants ,  zscore[order(zscore)],type='l',col=i)
    }
  }
}


# Runs all three steps of comparison in a single step
comparison.all <- function(path.base=OutputFolderPath,
                           run.files.folder=RunFilesFolderPath,
                           batch.name,reps,input.dim,input.ss,pred.ss,
                           seed.start=0,seed.preds=100,seed.fit=200,
                           func,func.string=NULL,
                           GPfit.powers=c(1.95,2),GPfit.include=T,GPfit.controls=c(1),
                           mlegp.include=T, Dice.include=F,
                           JMP.include=F,
                           DACE.include=T,DACE.meanfuncs=c("regpoly0","regpoly1"),DACE.corrfuncs=c("corrgauss","correxp"),
                           ooDACE.include=F,
                           Python.include=T,GPy.include=T,laGP.include=T,laGP.nuggets,laGP.nuggets.names,
                           only.compare=F,only.createandrun=F,stepstorun=c(1,2,3),
                           knit.report=F,
                           xmax_on=list(),
                           external.fits=c(),external.runs.folder='ExternalRuns',
                           reps.run=NULL
                           ) {
  # Runs all three steps of comparison in a single step, adds nothing else
  # Parameters are explained in comparison.all.batch
  # Maybe returns results of comparison.compare, not sure
  if (only.compare) {stepstorun <- c(3)}
  else if (only.createandrun) {stepstorun <- c(1,2)}
  if (1 %in% stepstorun) {
    comparison.create.data(path.base=path.base, run.files.folder=run.files.folder,
                           batch.name = batch.name,
                           reps=reps,input.dim=input.dim,input.ss=input.ss,pred.ss=pred.ss,
                           seed.start=seed.start,seed.preds=seed.preds,seed.fit=seed.fit,
                           func = func,func.string=func.string,
                           DACE.meanfuncs=DACE.meanfuncs,DACE.corrfuncs=DACE.corrfuncs,
                           external.fits=external.fits
    )
  }
  if (2 %in% stepstorun) { 
    comparison.run(path.base=path.base, run.files.folder=run.files.folder,
                   batch.name = batch.name,
                   reps=reps,input.dim=input.dim,
                   seed.fit=seed.fit,
                   GPfit.powers=GPfit.powers,GPfit.include=GPfit.include,GPfit.controls=GPfit.controls,
                   mlegp.include=mlegp.include, Dice.include=Dice.include,
                   JMP.include=JMP.include,DACE.include=DACE.include,ooDACE.include=ooDACE.include,
                   Python.include=Python.include,GPy.include=GPy.include,
                   laGP.include=laGP.include,laGP.nuggets=laGP.nuggets,laGP.nuggets.names=laGP.nuggets.names,
                   external.fits=external.fits,
                   reps.run=reps.run
    )
  }
  if (3 %in% stepstorun) {
    comparison.compare(path.base=path.base,
                       batch.name = batch.name,
                       reps=reps,input.dim=input.dim,input.ss=input.ss,pred.ss=pred.ss,
                       GPfit.powers=GPfit.powers,GPfit.include=GPfit.include,GPfit.controls=GPfit.controls,
                       mlegp.include=mlegp.include, Dice.include=Dice.include,
                       JMP.include=JMP.include,
                       DACE.include=DACE.include,DACE.meanfuncs=DACE.meanfuncs,DACE.corrfuncs=DACE.corrfuncs,
                       ooDACE.include=ooDACE.include,
                       Python.include=Python.include,GPy.include=GPy.include,
                       laGP.include=laGP.include,laGP.nuggets=laGP.nuggets,laGP.nuggets.names=laGP.nuggets.names,
                       knit.report=knit.report,xmax_on=xmax_on,external.fits=external.fits,external.runs.folder=external.runs.folder,
                       reps.run=reps.run
    )
  }
}

# This function does all comparisons for a bunch of things at once.
comparison.all.batch <- function(path.base=OutputFolderPath,
                           batch.name,reps,input.dim,input.ss,pred.ss,
                           seed.start=1,seed.preds=101,seed.fit=201,
                           func,func.string=NULL,
                           GPfit.powers=c(1.95,2),GPfit.include=T,GPfit.controls=c(1),
                           mlegp.include=T, Dice.include=F,
                           JMP.include=F,
                           DACE.include=T,DACE.meanfuncs=c("regpoly0"),DACE.corrfuncs=c("corrgauss"),#DACE.meanfuncs=c("regpoly0","regpoly1"),DACE.corrfuncs=c("corrgauss","correxp"),
                           ooDACE.include=F,
                           Python.include=T,GPy.include=T,
                           laGP.include=T,laGP.nuggets,laGP.nuggets.names,
                           only.compare=F,stepstorun=c(1,2,3)
                           ,xmax_on=list(),
                           external.fits=c(),external.runs.folder='ExternalRuns',
                           reps.run=NULL
                           ) {
  # This function does all comparisons for a bunch of things at once.
  # This is the function that the user will call most likely.
  # Input:
  #  path.base=OutputFolderPath - The folder the output will be in
  #  batch.name - The name of the batch
  #  reps - The number of replicates to be run
  #  input.dim - The input dimension of the function
  #  input.ss - The input sample sizes (vector) to be run
  #  pred.ss - The prediction sample size (integer) (number of prediction points) that should be tested at
  #  seed.start=1 - The seed set before picking the input sample
  #  seed.preds=101 - The seed set before picking the prediction points
  #  seed.fit=201 - The seed set before fitting each package. Doesn't work for some outside (Python, Matlab) packages, but I tried
  #  func - The function that we are emulating, used to get input and prediction points. Can be a function or string for recognized functions.
  #          Can be a list for RGP.points. Check comparison.create.data for details.
  #  func.string=NULL - Name of the function, written out but not needed
  #  GPfit.powers=c(1.95,2) - Powers used by GPfit
  #  GPfit.include=T - Should GPfit be included?
  #  GPfit.controls=c(1) - Allows you to run GPfit with other params. Can use half to optimization search size, or minimal. Not much difference.
  #  mlegp.include=T - Should mlegp be included?
  #  Dice.include=F - Should Dice Kriging be included? Added Jan 2017, not working as of 1/5/17
  #  JMP.include=F, - Should JMP be included? Doesn't work right now (6/6/16).
  #  DACE.include=T - Should DACE be included?
  #  DACE.meanfuncs=c("regpoly0") - DACE mean functions to be run
  #  DACE.corrfuncs=c("corrgauss") - DACE correlation functions to be run
  #  ooDACE.include=T - Should ooDACE be included? (2/7/17) It shouldn't because it is far worse than predicting the mean.
  #  Python.include=T - Should scikit-learn be included?
  #  GPy.include=T - Should GPy be included?
  #  laGP.include=T - Should laGP be included?
  #  laGP.nuggets - Numeric vector of nuggets to be used in laGP, e.g. 0, 1e-6, 1e-3, 1e-8
  #  laGP.nuggets.names - What the names of the nuggets are, used in the filenames and plots
  #  only.compare=F - if TRUE it only runs the third step of comparison.all, ie only comparison.compare
  #  stepstorun=c(1,2,3) - Which comparison steps to run. 1=comparison.create.data, 2=comparison.run, 3=comparison.compare
  #  xmax_on=list() - a plotting option for the comparison to LM plot, sets a max
  #  external.fits=c() - which fits are run elsewhere, meaning that you will do it manually and put the data back in, not for ones R calls through command line
  #                      ie not GPy, but yes for JMP on the cluster
  #  external.runs.folder='ExternalRuns' - where the external runs data are put
  #  reps.run=NULL - which specific reps to run. Eg if you specify reps=8, reps.run should be a subset of 1:8, NULL will do all.
  #                     This was useful when I had done 8 reps but only wanted to plot the first 5.
  #
  # Output: Writes the OutputTable.csv, returns the OutputTable
  
  # This creates each configuration that will be fed to comparison.compare
  looper <- expand.grid(input.dim=input.dim,reps=reps,
                        input.ss=input.ss,pred.ss=pred.ss,
                        seed.start=seed.start,seed.preds=seed.preds,seed.fit=seed.fit,
                        
                        stringsAsFactors=F)
  #bindwith <- data.frame(path.base=path.base,
  #                       batch.name=batch.name,
  ##                       func=func,
  #                       GPfit.powers=GPfit.powers,GPfit.include=GPfit.include,
  ##                       mlegp.include=mlegp.include,
  #                       JMP.include=JMP.include,
  #                       DACE.include=DACE.include,DACE.meanfuncs=DACE.meanfuncs,DACE.corrfuncs=DACE.corrfuncs,
  #                       Python.include=Python.include,
  #                       only.compare=only.compare)
  #looper <- cbind(looper,bindwith)
  #print(looper)
  #readline()
  #stop()
  
  
  
  
  # Create path for batch
  dir.create(paste0(path.base,batch.name,"//"),showWarnings=F)
  
  # Create df for the output
  output.data <- data.frame()
  
  # Loop over each set of parameters in looper, send a row at a time to comparison.compare
  for (rownum in 1:(dim(looper)[1])) {
    #do.call(comparison.all,looper[rownum,])
    #print(paste0(path.base,batch.name,"//"))
    batch.name.rownum <- paste0(batch.name,'_D',looper$input.dim[rownum],'_SS',looper$input.ss[rownum],'_PS',looper$pred.ss[rownum],'_R',looper$reps[rownum])
    new.rows <- comparison.all(
      path.base=paste0(path.base,batch.name,"//"),  # Put them all in subfolder within base
      
      func=func,func.string=func.string,
      GPfit.powers=GPfit.powers,GPfit.include=GPfit.include,GPfit.controls=GPfit.controls,
      mlegp.include=mlegp.include, Dice.include=Dice.include,
      JMP.include=JMP.include,
      DACE.include=DACE.include,DACE.meanfuncs=DACE.meanfuncs,DACE.corrfuncs=DACE.corrfuncs,
      ooDACE.include=ooDACE.include,
      Python.include=Python.include,GPy.include=GPy.include,
      laGP.include=laGP.include,laGP.nuggets=laGP.nuggets,laGP.nuggets.names=laGP.nuggets.names,
      only.compare=only.compare,stepstorun=stepstorun,
      
      batch.name=batch.name.rownum,#paste0(batch.name,'_D',looper$input.dim[rownum],'_SS',looper$input.ss[rownum],'_PS',looper$pred.ss[rownum],'_R',looper$reps[rownum]),   # name_SS15_PS200_R3_D2_
      
      input.dim=looper$input.dim[rownum],reps=looper$reps[rownum],
      input.ss=looper$input.ss[rownum],pred.ss=looper$pred.ss[rownum],
      seed.start=looper$seed.start[rownum],seed.preds=looper$seed.preds[rownum],seed.fit=looper$seed.fit[rownum],
      xmax_on=xmax_on,external.fits=external.fits,external.runs.folder=external.runs.folder,
      reps.run=reps.run
      
      )
    
    if( 3%in%stepstorun | only.compare ) { # Only do output if comparison step was run
      #reps.df is 1:reps if all reps used, else reps.run
      reps.df <- NULL
      if(is.null(reps.run)) {reps.df <- 1:reps} else {reps.df <- reps.run}
      new.rows2 <- cbind(
        new.rows,
        data.frame(input.dim=looper$input.dim[rownum],input.ss=looper$input.ss[rownum],pred.ss=looper$pred.ss[rownum],
                   #seed.start=looper$seed.start[rownum]:(looper$seed.start[rownum]+reps-1),
                   #seed.preds=looper$seed.preds[rownum]:(looper$seed.preds[rownum]+reps-1),
                   #seed.fit=looper$seed.fit[rownum]:(looper$seed.fit[rownum]+reps-1),
                   seed.start=looper$seed.start[rownum]+reps.df-1,
                   seed.preds=looper$seed.preds[rownum]+reps.df-1,
                   seed.fit=looper$seed.fit[rownum]+reps.df-1,
                   func.string=ifelse(is.null(func.string),'',func.string),
                   batch.name=batch.name
                   )
      )
      output.data <- rbind(output.data,new.rows2)
    }
  }
  #comparison.all(path.base='C://Users//cbe117//School//DOE//Comparison//comparison2//',
  #                           batch.name,reps,input.dim,input.ss,pred.ss,
  #                           seed.start=0,seed.preds=100,func,
  #                           GPfit.powers=c(1.95,2),GPfit.include=T,mlegp.include=T,
  #                           JMP.include=T,
  #                           DACE.include=T,DACE.meanfuncs=c("regpoly0","regpoly1"),DACE.corrfuncs=c("corrgauss","correxp"),
  #                           Python.include=T,
  #                           only.compare=F) 
  #  )
  #}
  
  # If compare step not run, just return an empty data frame
  if(!(3%in%stepstorun) & only.compare==F) {
    print('Comparison not run, return blank DF. Check# 32592357')
    return(output.data)
  }
  
  
  
  
  # plot RMSE and PRMSE stripcharts next to each other
  png(paste0(path.base,batch.name,"//RMSE_stripchart_PRMSE_stripchart_RMSE_scatter_RunTime_stripchart_RunTimes_stripchart.png"),width=2560,height=640*(dim(looper)[1]),units='px')
  omargins <- par()$mar  
  # plot stripcharts and scatter and run times next to each other
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:(dim(looper)[1]), ncol=1, byrow=TRUE))
  for (rownum in 1:(dim(looper)[1])) {
    imgToAdd <- readPNG(paste0(path.base,batch.name,"//",batch.name,'_D',looper$input.dim[rownum],'_SS',looper$input.ss[rownum],'_PS',looper$pred.ss[rownum],'_R',looper$reps[rownum],"//Plots//RMSE_stripchart_PRMSE_stripchart_RMSE_scatter_RunTime_stripchart_RunTimes_stripchart.png"))
    plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
    rasterImage(imgToAdd,0,0,1,1)
  }
  #dev.print(png, paste0(path.base,batch.name,"//RMSE_stripchart_PRMSE_stripchart_RMSE_scatter_RunTime_stripchart_RunTimes_stripchart.png"),width=2560,height=640*(dim(looper)[1]),units='px')
  dev.off() # before was just dev.print, no png or dev.off
  layout(matrix(1))
  par(mar=omargins)
  
  
  
  # plot RMSE on PRMSE stripchart for all reps 
  png(paste0(path.base,batch.name,"//RMSE_on_PRMSE_stripchart.png"),width=640,height=640*(dim(looper)[1]),units='px')
  omargins <- par()$mar  
  # plot stripcharts and scatter and run times next to each other
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:(dim(looper)[1]), ncol=1, byrow=TRUE))
  for (rownum in 1:(dim(looper)[1])) {
    imgToAdd <- readPNG(paste0(path.base,batch.name,"//",batch.name,'_D',looper$input.dim[rownum],'_SS',looper$input.ss[rownum],'_PS',looper$pred.ss[rownum],'_R',looper$reps[rownum],"//Plots//RMSE_on_PRMSE_stripchart.png"))
    plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
    rasterImage(imgToAdd,0,0,1,1)
  }
  dev.off()
  layout(matrix(1))
  par(mar=omargins)
  
  
  # plot RMSE on PRMSE OVER LM stripchart for all reps 
  png(paste0(path.base,batch.name,"//RMSE_on_PRMSE_over_LM_stripchart.png"),width=640,height=640*(dim(looper)[1]),units='px')
  omargins <- par()$mar  
  # plot stripcharts and scatter and run times next to each other
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:(dim(looper)[1]), ncol=1, byrow=TRUE))
  for (rownum in 1:(dim(looper)[1])) {
    imgToAdd <- readPNG(paste0(path.base,batch.name,"//",batch.name,'_D',looper$input.dim[rownum],'_SS',looper$input.ss[rownum],'_PS',looper$pred.ss[rownum],'_R',looper$reps[rownum],"//Plots//RMSE_on_PRMSE_over_LM_stripchart.png"))
    plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
    rasterImage(imgToAdd,0,0,1,1)
  }
  dev.off()
  layout(matrix(1))
  par(mar=omargins)
  
  # plot RMSE on PRMSE OVER LM  LOG scale stripchart for all reps 
  png(paste0(path.base,batch.name,"//RMSE_on_PRMSE_over_LM_stripchart_log.png"),width=640,height=640*(dim(looper)[1]),units='px')
  omargins <- par()$mar  
  # plot stripcharts and scatter and run times next to each other
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:(dim(looper)[1]), ncol=1, byrow=TRUE))
  for (rownum in 1:(dim(looper)[1])) {
    imgToAdd <- readPNG(paste0(path.base,batch.name,"//",batch.name,'_D',looper$input.dim[rownum],'_SS',looper$input.ss[rownum],'_PS',looper$pred.ss[rownum],'_R',looper$reps[rownum],"//Plots//RMSE_on_PRMSE_over_LM_stripchart_log.png"))
    plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
    rasterImage(imgToAdd,0,0,1,1)
  }
  dev.off()
  layout(matrix(1))
  par(mar=omargins)
  
  
  # plot RMSE on PRMSE OVER QM stripchart for all reps 
  png(paste0(path.base,batch.name,"//RMSE_on_PRMSE_over_QM_stripchart.png"),width=640,height=640*(dim(looper)[1]),units='px')
  omargins <- par()$mar  
  # plot stripcharts and scatter and run times next to each other
  par(mar=rep(0,4)) # no margins
  layout(matrix(1:(dim(looper)[1]), ncol=1, byrow=TRUE))
  try( # Adding try since not all use QM, e.g. when sample size too small or dim too large
    for (rownum in 1:(dim(looper)[1])) {
        imgToAdd <- readPNG(paste0(path.base,batch.name,"//",batch.name,'_D',looper$input.dim[rownum],'_SS',looper$input.ss[rownum],'_PS',looper$pred.ss[rownum],'_R',looper$reps[rownum],"//Plots//RMSE_on_PRMSE_over_QM_stripchart.png"))
        plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n",ylab='',xlab='')
        rasterImage(imgToAdd,0,0,1,1)
    }
  )
  dev.off()
  layout(matrix(1))
  par(mar=omargins)
  
  
  
  
  
  write.csv(output.data,paste0(path.base,batch.name,"//OutputTable.csv"))
  return(output.data)
}




# test functions
Borehole03 <- function(...) {
  comparison.all.batch (batch.name = "Borehole03",
                        reps=5, input.dim=8, input.ss=c(80, 160), #c(200,500),
                        pred.ss=2000,
                        seed.start=1001,seed.preds=1101,seed.fit=1201,
                        func = borehole,func.string='borehole',
                        ...
  )
}
Borehole1357_03 <- function(...) {
  comparison.all.batch (batch.name = "Borehole1357_03",
                        reps=5, input.dim=4, input.ss=c(40,80), #c(100,250), # only want 100 and 250 
                        pred.ss=2000,
                        seed.start=1002,seed.preds=1102,seed.fit=1202,
                        func = function(xx){borehole(c(xx[1],.5,xx[2],.5,xx[3],.5,xx[4],.5))},
                        func.string='borehole1357',
                        ...
  )
}
OTLCircuit2 <- function(...) {
  comparison.all.batch (batch.name = "OTLCircuit2",
                        reps=5, input.dim=6, input.ss= c(60, 120), #c(200,400), 
                        pred.ss=2000, 
                        seed.start=1022,seed.preds=1122,seed.fit=1322,
                        func = otlcircuit, ...
  )
}
Detpep108d2 <- function(...) {
  comparison.all.batch (batch.name = "Detpep108d2",
                        reps=5, input.dim=8, input.ss= c(80, 160), #c(200,400), # 400 gives error bc of JMP2WN rep 3, NA sd vals 
                        pred.ss=2000, # mlegp crashed on 800
                        seed.start=1024,seed.preds=1124,seed.fit=1324,
                        func = 'detpep108d',
                        ...
  )
}

# Try Theta=5 which is Beta=.7
RGPP2_D2_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP2_D2_B.7",
                        reps=5, input.dim=2, input.ss=c(50), 
                        pred.ss=2000,
                        seed.start=1028,seed.preds=1128,seed.fit=1328,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}
RGPP2_D4_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP2_D4_B.7",
                        reps=5, input.dim=4, input.ss=c(150), 
                        pred.ss=2000, 
                        seed.start=1030,seed.preds=1130,seed.fit=1330,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}
RGPP2_D6_B.7 <- function(...) {
  comparison.all.batch (batch.name = "RGPP2_D6_B.7",
                        reps=5, input.dim=6, input.ss=c(300), 
                        pred.ss=2000, 
                        seed.start=1032,seed.preds=1132,seed.fit=1332,
                        func = list('RGP.points',betas=.7,corr.power=2),
                        ...
  )
}


# Theta=20 is beta=1.3
RGPP2_D2_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP2_D2_B1.3",
                        reps=5, input.dim=2, input.ss=c(50), 
                        pred.ss=2000,
                        seed.start=1014,seed.preds=1114,seed.fit=1314,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
RGPP2_D4_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP2_D4_B1.3",
                        reps=5, input.dim=4, input.ss=c(150), 
                        pred.ss=2000,
                        seed.start=1016,seed.preds=1116,seed.fit=1316,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
RGPP2_D6_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP2_D6_B1.3",
                        reps=5, input.dim=6, input.ss=c(300), 
                        pred.ss=2000, 
                        seed.start=1018,seed.preds=1118,seed.fit=1318,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}








# Adding functions Jan 2017
Morris1 <- function(...) {
  comparison.all.batch (batch.name = "Morris1",
                        reps=5, input.dim=20, input.ss=c(200, 400),#[2], 
                        pred.ss=2000, 
                        seed.start=1019,seed.preds=1119,seed.fit=1319,
                        func = Morris,
                        ...
  )
}
Borehole2740 <- function(...) {
  comparison.all.batch (batch.name = "Borehole2740",
                        reps=10, input.dim=8, input.ss=c(27,40),
                        pred.ss=2000,
                        seed.start=1001,seed.preds=1101,seed.fit=1201,
                        func = borehole,func.string='borehole',
                        ...
  )
}

Discrepancy1DGP <- function(...) { # Created 3/27/17 to use as example of discrepancy between packages
  comparison.all.batch (batch.name = "Discrepancy1DGP",
                        reps=5, input.dim=1, input.ss=c(6), 
                        pred.ss=2000, 
                        seed.start=0,seed.preds=100,seed.fit=300,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}
# Bringing this back since it was the original discrepancy, can delete it without copying
RGPP_D1_B1.3 <- function(...) {
  comparison.all.batch (batch.name = "RGPP_D1_B1.3",   # CHANGED BELOW TO [1] BC ONLY 6 WORKS FOR JMP
                        reps=8, input.dim=1, input.ss=c(6,9,12,15,18)[c(1)], pred.ss=2000,
                        seed.start=13,seed.preds=113,seed.fit=313,
                        func = list('RGP.points',betas=1.3,corr.power=2),
                        ...
  )
}