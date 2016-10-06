require(plyr)
setwd("~/Research/GPC/GPC_Output")
fit.colors <- c('black','red','green','blue','cyan','magenta','magenta4',
                'chartreuse','gray40','orange','tomato4','navy','olivedrab',
                'firebrick','goldenrod','mediumspringgreen')
if(F) {
  beta0 <- paste0('SSCCRun0',c(rep('0',3),rep('',3)),7:12,'_',1:6,'D_0B')
  beta1 <- paste0('SSCCRun0',13:18,'_',1:6,'D_1B')
  beta2 <- paste0('SSCCRun00',1:6,'_',1:6,'D')
  betamixsix <- c('SSCCRun019_6D_000111B','SSCCRun020_6D_000222B','SSCCRun021_6D_111222B','SSCCRun022_6D_001122B')
  betamixsix2 <- c('SSCCRun023_6D_000111B','SSCCRun024_6D_000222B','SSCCRun025_6D_111222B','SSCCRun026_6D_001122B')
  #folders <- c(betamixsix2)
  #folders <- c(beta0,beta1,beta2)
  #folders <- folders[c(4:6,10:12,16:18)]
}

# Gets data from selected data, puts in data.frame
get.data.from.folders.df <- function(folders) {
  for(i in 1:length(folders)) {
    folder <- folders[i]
    fd <- read.csv(paste0(folder,'/OutputTable.csv'),stringsAsFactors=F) # MAYBE factorsasstrings=F ???
    print(dim(fd))
    print(names(fd))
    if (i==1) {
      ad <- fd[-grep('beta',names(fd))] #all data, folder data, exclude betas since not all have same
    } else {
      ad <- rbind(ad,fd[-grep('beta',names(fd))])
    }
  }
  return(ad)
}

# Plots stripchart, strips, colors, and pchs as told
stripcolor <- function(td,toplot,stripby,colorby,pchby=NULL,...) {
  # td is data frame
  # toplot is column to plot
  # stripby is column to strip on
  # colorby is column to use as colors
  # pchby is column to use as pch
  if(is.null(pchby))pchby=colorby
  stripchart(dlply(td,stripby,function(xx){xx[,toplot]}),col='white',las=1,...) # plot blank, ... lets you set titles, vertical, etc
  ucs <- unique(td[,colorby])
  ups <- unique(td[,pchby])
  for(i in 1:length(ucs)) {
    uc <- ucs[i]
    for(j in 1:length(ups)) {
      up <- ups[j]
      stripchart(dlply(td,stripby,function(xx){xx[,toplot][xx[,colorby]==uc & xx[,pchby]==up]}),
                 col=fit.colors[i],pch=j,add=T)
    }
  }
}


# Plots stripchart, strips, colors, and pchs as told
scattercolor <- function(td,toplotx,toploty,colorby,pchby=NULL,
                         filterby=NULL,filteris=NULL,bold=NULL,
                         exclude.scale=NULL,
                         spliton=NULL,mf=NULL,splitsamelims=F,
                         legendbyself=F,legendinclude=T,
                         xlab=NULL,ylab=NULL,
                         ...) {
  # td is data frame
  # toplot is column to plot
  #     stripby is column to strip on
  # colorby is column to use as colors
  # pchby is column to use as pch
  # bold must be list: [[1]] is which column, [[2]] is what value in that column, [[3]] is what thickness
  
  # change mf if splitting
  if(!is.null(mf)) {par(mfrow=mf)}
  
  # first find lims, maybe use in splits
  if(is.null(pchby))pchby=colorby
  if(!is.null(filterby)) {td <- td[,td[,filterby==filteris]]}
  xlim.data <- td[,toplotx]
  ylim.data <- td[,toploty]
  #exclude.scale.pts <- c()
  if(is.list(exclude.scale)) {
    xlim.data <- td[!td[,exclude.scale[[1]]]%in%exclude.scale[[2]],toplotx]
    ylim.data <- td[!td[,exclude.scale[[1]]]%in%exclude.scale[[2]],toploty]
    #exclude.scale.pts <- td[,exclude.scale[[1]]]%in%exclude.scale[[2]]
  }
  
  if(!is.null(spliton)) {
    splits <- unique(td[,spliton])
    print(paste('splits are',splits))
    for (isplit in splits) {
      scattercolor(td[td[,spliton]==isplit,],toplotx=toplotx,toploty=toploty,colorby=colorby,pchby=pchby,
                   filterby=filterby,filteris=filteris,bold=bold,
                   exclude.scale=exclude.scale,
                   main=paste(spliton,'=',isplit),
                   xlim=if(splitsamelims) c(min(xlim.data),max(xlim.data)) else NULL,
                   ylim=if(splitsamelims) c(min(ylim.data),max(ylim.data)) else NULL,
                   legendinclude=F,
                   ...=...)
    }
  }
  
  # now plot scatter
  #browser()
  #plot(td[,toplotx],td[,toploty],col='white',
  plot(xlim.data,ylim.data,col='white',
       xlab=ifelse(is.null(xlab),toplotx,xlab),ylab=ifelse(is.null(ylab),toploty,ylab),
       ...) # plot blank, ... lets you set titles, vertical, etc
  ucs <- unique(td[,colorby]);ucs.legend <- replace(ucs,ucs=='DACEregpoly0corrgauss','DACE')
  ups <- unique(td[,pchby]);  ups.legend <- replace(ups,ups=='DACEregpoly0corrgauss','DACE')
  #ups <- ups[c(1,4,2,3)]
  if (!legendbyself & legendinclude){
    print(c(legendbyself,legendinclude))
    legend('topright',legend=ucs.legend,fill=fit.colors[1:length(ucs)])
    legend('bottomright',legend=ups.legend,pch=as.character(1:length(ups)))
  }
  for(i in 1:length(ucs)) {
    uc <- ucs[i]
    for(j in 1:length(ups)) {
      up <- ups[j]
      #browser()
      points(td[,toplotx][td[,colorby]==uc & td[,pchby]==up],
             td[,toploty][td[,colorby]==uc & td[,pchby]==up],
             col=fit.colors[i],pch=as.character(j)
             #,cex=ifelse(is.list(bold),ifelse(td[,bold[[1]]]==bold[[2]],1,ifelse(length(bold)>2,bold[[3]],2)),1)
             ,cex=ifelse(is.list(bold) ,ifelse( (bold[[1]]==colorby & bold[[2]]==uc) | (bold[[1]]==pchby & bold[[2]]==up),bold[[3]],1),1)
             )
    }
  }
  if(legendbyself) {
    plot(1:2,xlab='',ylab='',col='white',xaxt='n',yaxt='n')
    legend('right',legend=ucs.legend,fill=fit.colors[1:length(ucs)])
    legend('left',legend=ups.legend,pch=as.character(1:length(ups)))
  }
  
  if(!is.null(mf)) {par(mfrow=c(1,1))}
  return(list(ucs=ucs,ups=ups))
}







if (F) {
  ad <- get.data.from.folders.df(betamixsix)
  ad <- get.data.from.folders.df(c('RGPP_D1_B0','RGPP_D2_B0','RGPP_D3_B0','RGPP_D4_B0','RGPP_D5_B0','RGPP_D6_B0'))
  ad <- get.data.from.folders.df(c('RGPP_D1_B0','RGPP_D1_B.7','RGPP_D1_B1','RGPP_D1_B1.3','RGPP_D1_B2'))
  ad <- ad[ad$fit!='GPy',]
}

if (F) {
  stripcolor(ad,'poarmse','fit','batch.name','input.ss')
  stripcolor(ad,'pwbrmse','fit','batch.name','input.ss',xlim=c(0,2))
  abline(v=1,col='red')
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name')
}

if (F) {
  # Test bold
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name',bold=list('fit','GPy',2))
  # test mf and split
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name',bold=list('fit','GPy',2), mf=c(2,2),spliton='input.ss',splitsamelims=T)
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name',bold=list('fit','GPy',2), mf=c(2,2),spliton='input.ss',splitsamelims=T,legendbyself=T)
  scattercolor(ad,'pwbrmse','poarmse','input.ss','batch.name',bold=list('fit','GPy',2), mf=c(3,4),spliton='fit',splitsamelims=T,legendbyself=T)
  scattercolor(ad,'pwbrmse','poarmse','input.ss','batch.name',bold=list('fit','GPy',2), mf=c(3,4),spliton='fit',splitsamelims=T,legendbyself=T,exclude.scale = list('fit',c('LM','PredictMean','GPy')))
  
}


if (F) {
  stripcolor(ad,'rmse','fit','rep','input.dim',las=1)
  
  # betamixsix2
  stripcolor(ad,'poarmse','fit','batch.name','input.ss')
  abline(v=1,col='red')
  stripcolor(ad,'pwbrmse','fit','batch.name','input.ss')
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name')
  
  
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name',xlim=c(0,1))
  scattercolor(ad[ad[,'fit']=='mlegp',],'pwbrmse','poarmse','fit','input.ss',xlim=c(0,1))
  scattercolor(ad[ad[,'fit']=='DACEregpoly0corrgauss',],'pwbrmse','poarmse','fit','input.ss',xlim=c(0,1))
  
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name',xlim=c(0,1)) -> us
  legend(x=.6,y=1.6,legend=us$ucs,fill=1:length(us$ucs),cex=.3)
  legend(x=.6,y=.8,legend=us$ups,pch=1:length(us$ups),cex=.3)
  scattercolor(ad[ad[,'fit']=='mlegp',],'pwbrmse','poarmse','fit','batch.name',xlim=c(0,1)) -> us
  
  scattercolor(ad,'pwbrmse','poarmse','fit','input.ss')
  
  stripcolor(bh,'poarmse','fit','input.ss')
  scattercolor(bh,'pwbrmse','poarmse','fit','input.ss')
  
  
  
  # all 18 betas
  
  scattercolor(ad,'pwbrmse','poarmse','fit','batch.name',xlim=c(0,1)) -> us
  scattercolor(ad,'rmse','prmse','fit','batch.name') -> us
  legend(x=.6,y=1.6,legend=us$ucs,fill=1:length(us$ucs),cex=.3)
  legend(x=.6,y=.8,legend=us$ups,pch=1:length(us$ups),cex=.3)
  scattercolor(ad[ad[,'fit']=='mlegp',],'pwbrmse','poarmse','fit','batch.name',xlim=c(0,1)) -> us
}



Borehole_vis <- function() {
  #folder.names <- c()
  bore <- get.data.from.folders.df('Borehole01')
  scattercolor(bore,'pwbrmse','poarmse','fit','input.ss',exclude.scale = list('fit',c('LM','PredictMean','GPy'))
               ,ylab='Predicted / Average RMSE',xlab='% Worse Than Best')
}

RGPP_vis <- function(d,...) {
  folder.names <- c()
  for (dd in d) {
    folder.names <- c(folder.names,paste0('RGPP_D',d,'_B',c('0','.7','1','1.3')))#,'RGPP_D1_B2')
  }
  rgpp <- get.data.from.folders.df(folder.names)
  rgpp <- rgpp[!(rgpp$fit %in% c('GPfit1.95-2','GPfit1.95-A','GPfit2-2','GPfit2-A','LM','PredictMean','mlegp')),]
  print(dim(rgpp))
  print(names(rgpp))
  plot(rgpp$pwbrmse,rgpp$poarmse)
  scattercolor(rgpp,'pwbrmse','poarmse','fit','input.dim',exclude.scale = list('fit',c('LM','PredictMean','GPy','mlegp'))#,'GPfit2','laGP','DACEregpoly0corrgauss'))
               ,ylab='Predicted / Average RMSE',xlab='Proportion Worse Than Best',...)
}
if (F) {
  RGPP_vis(5:6,spliton='fit',mf=c(2,3),splitsamelims=T,legendbyself=T)
}
if (F) {
  # 5/19/16 trying to find ratio of emrmse to pmrmse
  foldd <- c('Detpep108d1')
  detpep <- get.data.from.folders.df(foldd)
  names(detpep)
  detpep$prmseoverrmse <- detpep$prmse / detpep$rmse
  detpep$prmseoverrmse[detpep$input.ss==200]
  stripchart(detpep$prmseoverrmse[detpep$input.ss==200])
  hist(log(detpep$prmseoverrmse[detpep$input.ss==200]),breaks=50)
  hist((detpep$prmseoverrmse[detpep$input.ss==200]),breaks=50,xlim=c(0,2))
  
  bores <- 'Borehole02'
  bore <- get.data.from.folders.df(bores)
  bore1 <- bore[!(bore$fit %in% c('LM','PredictMean','QM')),]
  bore2 <- bore1[bore1$input.ss %in%c(200,500),]
  
  # first we get prmse over rmse
  bore2p <- plyr::ddply(bore2,.(rep,fit,input.ss),function(xx){c(xx$rmse/xx$prmse)})
  bore2q <- plyr::dlply(bore2p,.(input.ss),function(xx){xx})
  bore200 <- bore2q[[1]]
  bore500 <- bore2q[[2]]
  hist(bore200$V1,breaks=50) # 
  bore200$V1[bore200$fit=='GPfit2']
  
  # now for pi and xi reductions
  bore3 <- plyr::ddply(bore2,.(rep,fit),function(xx){c(xx$rmse[xx$input.ss==500]/xx$rmse[xx$input.ss==200])})
  hist(bore3$V1,breaks=30)
  mean(bore3$V1) # xi reduction
  bore4 <- plyr::ddply(bore2,.(rep,fit),function(xx){c(xx$prmse[xx$input.ss==500]/xx$prmse[xx$input.ss==200])})
  hist(bore4$V1,breaks=30)
  mean(bore4$V1) # pi reduction
}