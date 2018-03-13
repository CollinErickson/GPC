#setwd("/sscc/home/c/cbe117/Research/GPC/GPC_Codes/")
# qsub <- function() system('qsub /sscc/home/c/cbe117/Research/GPC/GPC_Codes/GPC_Submit.pbs')
# pbstat <- function() system('pbstat')

compare.1D.error.predictions <- function(){
  run=1 # 1 and 4
  file0=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS2000_R8/RGPP_D1_B2_D1_SS18_PS2000_R8_',run,'.csv')
  #  ,
  file1=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS2000_R8/RGPP_D1_B2_D1_SS18_PS2000_R8_',run,'_Preds_DACEregpoly0corrgauss_post.csv')
  #,
  file2=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS2000_R8/RGPP_D1_B2_D1_SS18_PS2000_R8_',run,'_Preds_GPy_post.csv')
  #,  
  file3=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS2000_R8/RGPP_D1_B2_D1_SS18_PS2000_R8_',run,'_Preds_sklearnRBF_post.csv')
  #) {
  d0 <- read.csv(file0)
  d1 <- read.csv(file1)
  d2 <- read.csv(file2)
  d3 <- read.csv(file3)
  ord <- order(d1$x)
  plot(d1$x[ord],d1$y[ord],type='l',pch=19,cex=.2)
  points(d0$x,d0$y,pch=19)
  points(d1$x[ord],d1$yp[ord],type='l',col='red',lwd=2)
  points(d2$x[ord],d2$yp[ord],type='l',col='blue',lwd=2)
  points(d3$x[ord],d3$yp[ord],type='l',col='green',lwd=2)
  
  
  plot(d1$x[ord],d1$ysd[ord],type='l',col='red',lwd=2,ylim=c(0,1))
  points(d2$x[ord],d2$ysd[ord],type='l',col='blue',lwd=2)
  points(d3$x[ord],d3$ysd[ord],type='l',col='green',lwd=2)
  
}

compare.RGPP.D1.B1.3 <- function(run=2) {#browser()
  fits <- c('laGPE','laGP6','DACEregpoly0corrgauss','mlegpE','mlegp0','GPfit2','GPy','sklearnRBF','Dice2')
  fits.names <- c('laGPE','laGP6','DACE','mlegpE','mlegp0','GPfit2','GPy','sklearnRBF','Dice2')
  only.use <- c(1, 4, 9)
  #only.use <- 1:length(fits)
  fits <- fits[only.use]
  fits.names <- fits.names[only.use]
  fits.length <- length(fits)
  ltys <- c(1,2,1,1,2,2, 1)
  ltys <- rep(1,length(fits))
  ltys <- 1:fits.length
  fit.colors.indices.all <- c('black','red','blue','green','blue','cyan','magenta','magenta4',
                         'chartreuse','tomato4','navy','gray40','orange','olivedrab',
                         'firebrick','goldenrod','mediumspringgreen')
  fits.colors.indices <- fit.colors.indices.all[1:length(fits)]
  
  truedat=read.csv(paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B1.3/RGPP_D1_B1.3_D1_SS6_PS2000_R8/RGPP_D1_B1.3_D1_SS6_PS2000_R8_',run,'.csv'))
  dats <- list()
  for (fit in fits) {
    dats[[fit]] <- read.csv(paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B1.3/RGPP_D1_B1.3_D1_SS6_PS2000_R8/RGPP_D1_B1.3_D1_SS6_PS2000_R8_',run,'_Preds_',fit,'_post.csv'))
  }
  do_axes_separately <- F
  plot(truedat$x,truedat$y,xlim=c(0,1),ylim=c(-.47,.63),pch=19,cex=1.5,xlab='x',ylab='Predicted y', ann=!do_axes_separately) # yaxt='n'
  #axis(2, at=c(-.4,0,.4))
  # plot(truedat$x,truedat$y,xlim=c(0,1),pch=19,cex=1.5,xlab='x',ylab='Predicted y')
  legend(x='topright',legend=fits.names,col=fits.colors.indices,lty=ltys,lwd=2)
  if (do_axes_separately) {
    mtext('x', side=1,line=3, font=1)
    mtext('Predicted y', side=2,line=3, font=1) 
  }

  for (i in 1:fits.length){
    fit <- fits[i]
    plotfunc <- points#if(i==1) plot else points
    fit.order <- order(dats[[fit]]$x)
    x <- dats[[fit]]$x[fit.order]
    yp <- dats[[fit]]$yp[fit.order]
    plotfunc(x,yp,type='l',col=fits.colors.indices[i],lty=ltys[i],lwd=2)
  }
  
  
  error.fits <- fits
  error.fits.names <- fits.names
  error.fits.length <- length(error.fits)
  error.fits.colors.indices <- fits.colors.indices[1:error.fits.length]
  for (i in 1:error.fits.length) {#browser()
    # fit <- ifelse(i==1,'laGPE','DACEregpoly0corrgauss')
    fit <- error.fits[i]
    #plot(truedat$x,truedat$y,xlim=c(0,1),ylim=c(-.05,1.05),pch=19,cex=1.5,xlab='x',ylab='Predicted y')
    
    fit.order <- order(dats[[fit]]$x)
    x <- dats[[fit]]$x[fit.order]
    yp <- dats[[fit]]$yp[fit.order]
    yv <- dats[[fit]]$yv[fit.order]
    plotfunc <- if(i==1) plot else points
    plotfunc(x,yv,col=i,type='l',lty=i,lwd=2)
    legend(x='topright',legend=error.fits.names,col=error.fits.colors.indices,lty=1:error.fits.length,lwd=2)
  }
  for (i in 1:error.fits.length) {
    fit <- error.fits[i]
    # fit <- ifelse(i==1,'laGPE','DACEregpoly0corrgauss')
    #plot(truedat$x,truedat$y,xlim=c(0,1),ylim=c(-.05,1.05),pch=19,cex=1.5,xlab='x',ylab='Predicted y')
    
    fit.order <- order(dats[[fit]]$x)
    x <- dats[[fit]]$x[fit.order]
    yp <- dats[[fit]]$yp[fit.order]
    yv <- dats[[fit]]$yv[fit.order]
    # plotfunc <- if (i==1) plot else points
    # plotfunc(x,sqrt(yv),col=i,type='l',lty=i,lwd=2,ylab='ysd')
    not_do_axes_separately <- TRUE
    if (i==1) {
      plot(x,sqrt(yv),col=fits.colors.indices[i],type='l',lty=i,lwd=2,ylab=if (not_do_axes_separately) {''} else {expression('sd'[y])}, ylim=c(0,.54), ann=not_do_axes_separately)
    } else {
      points(x,sqrt(yv),col=fits.colors.indices[i],type='l',lty=i,lwd=2)
    }
    legend(x='topright',legend=error.fits.names,col=error.fits.colors.indices,lty=1:error.fits.length,lwd=2)
    mtext(expression('sd'[y]), side=2, line=2.6)
    if (!not_do_axes_separately) mtext('x', side=1,line=3, font=1) # It looks bold, can't get it not bold
  }
}
compare.RGPP.D1.B1.3(run = 6)
