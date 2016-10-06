#setwd("/sscc/home/c/cbe117/Research/GPC/GPC_Codes/")
qsub <- function() system('qsub /sscc/home/c/cbe117/Research/GPC/GPC_Codes/GPC_Submit.pbs')
pbstat <- function() system('pbstat')

compare.1D.error.predictions <- function(){
  run=1 # 1 and 4
  file0=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS200_R8/RGPP_D1_B2_D1_SS18_PS200_R8_',run,'.csv')
  #  ,
  file1=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS200_R8/RGPP_D1_B2_D1_SS18_PS200_R8_',run,'_Preds_DACEregpoly0corrgauss_post.csv')
  #,
  file2=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS200_R8/RGPP_D1_B2_D1_SS18_PS200_R8_',run,'_Preds_GPy_post.csv')
  #,  
  file3=paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B2/RGPP_D1_B2_D1_SS18_PS200_R8/RGPP_D1_B2_D1_SS18_PS200_R8_',run,'_Preds_Python_post.csv')
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

compare.RGPP.D1.B1.3 <- function(run=2) {
  fits <- c('laGP','DACEregpoly0corrgauss','mlegp','GPfit2','GPy','Python')
  fits.names <- c('laGP','DACE','mlegp','GPfit2','GPy','Python')
  fits.length <- length(fits)
  ltys <- c(1,2,1,1,2,2)
  truedat=read.csv(paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B1.3/RGPP_D1_B1.3_D1_SS6_PS200_R8/RGPP_D1_B1.3_D1_SS6_PS200_R8_',run,'.csv'))
  dats <- list()
  for (fit in fits) {
    dats[[fit]] <- read.csv(paste0('/sscc/home/c/cbe117/Research/GPC/GPC_Output/RGPP_D1_B1.3/RGPP_D1_B1.3_D1_SS6_PS200_R8/RGPP_D1_B1.3_D1_SS6_PS200_R8_',run,'_Preds_',fit,'_post.csv'))
  }
  plot(truedat$x,truedat$y,xlim=c(0,1),ylim=c(-.05,1.05),pch=19,cex=1.5,xlab='x',ylab='Predicted y')
  legend(x='bottomright',legend=fits.names,col=1:fits.length,lty=ltys,lwd=2)
  for (i in 1:fits.length){
    fit <- fits[i]
    plotfunc <- points#if(i==1) plot else points
    fit.order <- order(dats[[fit]]$x)
    x <- dats[[fit]]$x[fit.order]
    yp <- dats[[fit]]$yp[fit.order]
    plotfunc(x,yp,type='l',col=i,lty=ltys[i],lwd=2)
  }
  
  for (i in 1:2) {
    fit <- ifelse(i==1,'laGP','DACEregpoly0corrgauss')
    #plot(truedat$x,truedat$y,xlim=c(0,1),ylim=c(-.05,1.05),pch=19,cex=1.5,xlab='x',ylab='Predicted y')
    
    fit.order <- order(dats[[fit]]$x)
    x <- dats[[fit]]$x[fit.order]
    yp <- dats[[fit]]$yp[fit.order]
    yv <- dats[[fit]]$yv[fit.order]
    plotfunc <- ifelse(i==1,plot,points)
    plotfunc(x,yv,col=i,type='l',lty=i,lwd=2)
    legend(x='topright',legend=fits.names[1:2],col=1:2,lty=1:2,lwd=2)
  }
  for (i in 1:2) {
    fit <- ifelse(i==1,'laGP','DACEregpoly0corrgauss')
    #plot(truedat$x,truedat$y,xlim=c(0,1),ylim=c(-.05,1.05),pch=19,cex=1.5,xlab='x',ylab='Predicted y')
    
    fit.order <- order(dats[[fit]]$x)
    x <- dats[[fit]]$x[fit.order]
    yp <- dats[[fit]]$yp[fit.order]
    yv <- dats[[fit]]$yv[fit.order]
    plotfunc <- ifelse(i==1,plot,points)
    plotfunc(x,sqrt(yv),col=i,type='l',lty=i,lwd=2,ylab='ysd')
    legend(x='topright',legend=fits.names[1:2],col=1:2,lty=1:2,lwd=2)
  }
}
compare.RGPP.D1.B1.3()