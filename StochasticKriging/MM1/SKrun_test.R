SKrun1 <- function(include.mlegp=T, include.DiceKriging=T, include.sklearn=F, include.LM=T,
                   n0reps=5, n2=500) {
  namns <- c()
  rmses <- c()
  prmses <- c()
  runtimes <- c()
  
  X0 <- matrix(c(.3,.4,.5,.6,.7,.8,.9), ncol=1)
  MM1.mean <- function(x) {
    x/(1-x)# + rnorm(length(x), 0, sqrt(x)/(1-x))
  }
  MM1.sample <- function(x) {
    #x/(1-x) + rnorm(length(x), 0, sqrt(x)/(1-x))
    sapply(x, function(xx)rgeom(1, 1-xx))
  }
  I1 <- sort(rep(1:length(X0), n0reps))
  #X1 <- rbind(X0, X0, X0, X0)
  X1 <- X0[I1,, drop=F]
  Z1 <- MM1.sample(X1)
  plot(X1, Z1)
  #plyr::ddply(X, 'X')
  vars1 <- sapply(split(x=Z1, f=as.factor(X1)), var)
  n2props <- sqrt(vars1)/sum(sqrt(vars1))
  #n2 <- 500
  n2s <- round(n2*n2props)
  sum(n2s)
  I2 <- rep(1:length(X0), n2s)
  #X2 <- matrix(rep(X0, n2s), ncol=1)
  X2 <- X0[I2,, drop=F]
  Z2 <- MM1.sample(X2)
  I <- c(I1, I2)
  X <- rbind(X1, X2)
  Z <- c(Z1, Z2)
  vars <- sapply(split(x=Z, f=as.factor(X)), var)
  vars <- pmax(1e-8, vars)
  nugprop <- vars[I]
  plot(X, Z)
  
  
  XP <- matrix(seq(.3,.9, l=300), ncol=1)
  ZP <- MM1.mean(XP)
  
  write.csv(XP, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/XP.csv', row.names=F)
  write.csv(ZP, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/ZP.csv', row.names=F)
  write.csv(Z, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Z.csv', row.names=F)
  write.csv(X, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/X.csv', row.names=F)
  write.csv(nugprop, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/nugprop.csv', row.names=F)
  write.csv(vars, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/vars.csv', row.names=F)
  write.csv(X0, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Xmean.csv', row.names=F)
  Zmean <- sapply(split(Z, I), mean)
  write.csv(Zmean, '/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/Zmean.csv', row.names=F)
  #browser()
  if (include.LM) {#browser()
    # Get predictions for mlegp
    LM_runtime <- system.time({
      mod.LM <- lm(Z ~ X, data = data.frame(X=X, Z=Z))
      XPdf <- data.frame(X=XP)
      P.LM <- predict(mod.LM, XPdf, se.fit = T)
      ZP.LM <- P.LM$fit# LM::predict.gp(mod.LM,XP, se.fit = T)
      ZPU.LM <- P.LM$fit + 2*P.LM$se.fit
      ZPL.LM <- P.LM$fit - 2*P.LM$se.fit
      plot(X, Z, main="LM M/M/1", ylim=c(0,18))
      legend(x='topleft', legend=(c('True mean', 'Est. mean', '95%')), fill=c(2,4,3))
      points(XP, ZPU.LM, col=3, type='l', lwd=3)
      points(XP, ZPL.LM, col=3, type='l', lwd=3)
      points(XP, ZP.LM, col=4, type='l', lwd=5)
      points(XP, ZP, col=2, type='l', lwd=5)
      RMSE.LM <- sqrt(mean((ZP.LM - ZP)^2))
      PRMSE.LM <- sqrt(mean((P.LM$se.fit)^2))
      namns <- c(namns, "LM")
      rmses <- c(rmses, RMSE.LM)
      prmses <- c(prmses, PRMSE.LM)
    })
    runtimes <- c(runtimes, LM_runtime[1])
  }
  
  if (include.mlegp) {
    # Get predictions for mlegp
    mlegp_runtime <- system.time({
      mod.mlegp <- mlegp::mlegp(X, Z, nugget=nugprop*100)
      P.mlegp <- mlegp::predict.gp(mod.mlegp,XP, se.fit = T)
      ZP.mlegp <- P.mlegp$fit# mlegp::predict.gp(mod.mlegp,XP, se.fit = T)
      ZPU.mlegp <- P.mlegp$fit + 2*P.mlegp$se.fit
      ZPL.mlegp <- P.mlegp$fit - 2*P.mlegp$se.fit
      plot(X, Z, main="mlegp M/M/1", ylim=c(0,18))
      legend(x='topleft', legend=(c('True mean', 'Est. mean', '95%')), fill=c(2,4,3))
      points(XP, ZPU.mlegp, col=3, type='l', lwd=3)
      points(XP, ZPL.mlegp, col=3, type='l', lwd=3)
      points(XP, ZP.mlegp, col=4, type='l', lwd=5)
      points(XP, ZP, col=2, type='l', lwd=5)
      RMSE.mlegp <- sqrt(mean((ZP.mlegp - ZP)^2))
      PRMSE.mlegp <- sqrt(mean((P.mlegp$se.fit)^2))
      namns <- c(namns, "mlegp")
      rmses <- c(rmses, RMSE.mlegp)
      prmses <- c(prmses, PRMSE.mlegp)
    })
    runtimes <- c(runtimes, mlegp_runtime[1])
  }
  
  
  if (include.DiceKriging) {
    # Get predictions for DiceKriging
    DK_runtime <- system.time({
      mod.DiceKriging <- DiceKriging::km(~1, X, Z, noise.var=nugprop)
      P.DiceKriging <- DiceKriging::predict.km(mod.DiceKriging,XP, type="SK", se.compute = T)
      ZP.DiceKriging <- P.DiceKriging$mean# DiceKriging::predict.gp(mod.DiceKriging,XP, se.fit = T)
      ZPU.DiceKriging <- P.DiceKriging$mean + 2*P.DiceKriging$sd
      ZPL.DiceKriging <- P.DiceKriging$mean - 2*P.DiceKriging$sd
      plot(X, Z, main="DiceKriging M/M/1", ylim=c(0,18))
      legend(x='topleft', legend=(c('True mean', 'Est. mean', '95%')), fill=c(2,4,3))
      points(XP, ZPU.DiceKriging, col=3, type='l', lwd=3)
      points(XP, ZPL.DiceKriging, col=3, type='l', lwd=3)
      points(XP, ZP.DiceKriging, col=4, type='l', lwd=5)
      points(XP, ZP, col=2, type='l', lwd=5)
      RMSE.DiceKriging <- sqrt(mean((ZP.DiceKriging - ZP)^2))
      PRMSE.DiceKriging <- sqrt(mean((P.DiceKriging$sd)^2))
      namns <- c(namns, "DiceKriging")
      rmses <- c(rmses, RMSE.DiceKriging)
      prmses <- c(prmses, PRMSE.DiceKriging)
    })
    runtimes <- c(runtimes, DK_runtime[1])
  }
  
  
  if (include.sklearn) { 
    system('python ~/Research/GPC/StochasticKriging/MM1/sklearnStochasticTest.py')
    ZP.sklearn <- read.csv('./StochasticKriging//MM1//sklearn_YP.csv', header = F)[,1]
    ZP.std.sklearn <- read.csv('./StochasticKriging//MM1//sklearn_stdP.csv', header = F)[,1]
    ZPU.sklearn <- ZP.sklearn + 2*ZP.std.sklearn
    ZPL.sklearn <- ZP.sklearn - 2*ZP.std.sklearn
    plot(X, Z, main="sklearn M/M/1")
    legend(x='topleft', legend=(c('True mean', 'Est. mean', '95%')), fill=c(2,4,3))
    points(XP, ZP, col=2, type='l')
    points(XP, ZPU.sklearn, col=3, type='l')
    points(XP, ZPL.sklearn, col=3, type='l')
    points(XP, ZP.sklearn, col=3, type='l', lwd=3)
    RMSE.sklearn <- sqrt(mean((ZP.sklearn - ZP)^2))
    PRMSE.sklearn <- sqrt(mean((ZP.std.sklearn)^2))
    namns <- c(namns, "sklearn")
    rmses <- c(rmses, RMSE.sklearn)
    prmses <- c(prmses, PRMSE.sklearn)
  }
  #browser()
  data.frame(namns, rmses, prmses, runtimes)
}
SKrun <- function(reps=5, n0reps=5, n2=500) {# browser()
  output <- data.frame()
  for (i in 1:reps) {
    outputi <- SKrun1(n0reps=n0reps, n2=n2)
    outputi <- cbind(outputi, repi=i)
    output <- if (i==1) outputi else rbind(output, outputi)
  }
  output
}
# SKout1 <- SKrun(reps=20, n0reps=5, n2=500)

library(ggplot2)
plot_rmseprmse = function(outputdf) {#browser()
  com2 <- reshape::melt.data.frame(outputdf, measure.var=c('rmses', 'prmses'), id.vars=c('namns','repi'), variable_name="rmseprmse")
  
  rmseprmse_plot <- (ggplot(com2, ggplot2::aes(x=value, y=rmseprmse, color=namns, shape=as.factor(repi)))
                     + geom_point(aes(shape=as.factor(repi)),size=3) + facet_grid(namns ~ .)
                     + guides(shape=F,color=F)
                     + ylab(NULL) + xlab(NULL)
                     +  geom_line(ggplot2::aes(x=value, y=rmseprmse, group=repi)))
  print(rmseprmse_plot)
}
# plot_rmseprmse(SKout1)

library(plyr)
plot_rmseprmse_strip = function(outputdf, saveplot=F, post='') {browser()
                                                       
  fit.colors <- c('magenta4','olivedrab') # c(2,4) magenta4 for mlegp, olivedrab for DK
  yoffset <- .3
  
  outputdf$namns <- as.character(outputdf$namns)
  outputdf <- plyr::ddply(outputdf, .(repi), function(d){d$xi <- d$rmses/d$rmses[which(d$namns=='LM')]; d$pi <- d$prmses/d$rmses[which(d$namns=='LM')]; d})
  
  com2 <- reshape::melt.data.frame(outputdf, measure.var=c('rmses', 'prmses'), id.vars=c('namns','repi'), variable_name="rmseprmse")
  com3 <- reshape::melt.data.frame(outputdf, measure.var=c('xi', 'pi'), id.vars=c('namns','repi'), variable_name="rmseprmse")
  RMSE_on_PRMSE_stripchart_filename <- paste0("/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1//MM1_RMSE_on_PRMSE_stripchart",post,".png")
  
  if (saveplot) png(filename=RMSE_on_PRMSE_stripchart_filename,width = 640,height = 427,units = "px")
  
  default.par.mar <- par('mar')
  par(mar=c(2.5,6,.4,2)) # bottom left top right
  
  #par(mfrow=c(1,2))
  #par(mar=c(5.1,6,4.1,1))
  
  outputdf$plotnamns <- outputdf$namns
  outputdf$plotnamns[outputdf$plotnamns == "DiceKriging"] <- 'Dice'
  com2$plotnamns <- com2$namns
  com2$plotnamns[com2$plotnamns == "DiceKriging"] <- 'Dice'
  com3$plotnamns <- com3$namns
  com3$plotnamns[com3$plotnamns == "DiceKriging"] <- 'Dice'
  
  outputdf_noLM <- outputdf[outputdf$namns != 'LM',]
  
  stripchart(xi ~ plotnamns, data=outputdf_noLM,pch=4,cex.axis=1.8,las=1,
             xlim=c(min(outputdf_noLM$xi,outputdf_noLM$pi), max(outputdf_noLM$xi,outputdf_noLM$pi)),
             #xlab=paste0('RMSE'),
             #main=paste0('RMSE for ','M/M/1'),col='white',
             #group.names=fits.plot.names.cut,
             ylim=c(.9,2 + .1 + yoffset),
            col='white', xlab=''
  )
  abline(h=1:2,col='gray51')


  # First add lines so they are behind markers
  for(ifit in 1:2) {
    fit <- c('DiceKriging', 'mlegp')[ifit]
    for (ii in unique(outputdf$repi)) {
      repiidf <- outputdf[outputdf$repi==ii & outputdf$namns==fit,]
      #lines(x=c(repiidf$rmses, repiidf$prmses), y=c(ifit, ifit+yoffset))
      lines(x=c(repiidf$xi, repiidf$pi), y=c(ifit, ifit+yoffset))
    }
  }
  
  for(ifit in 1:2) {
    fit <- c('DiceKriging', 'mlegp')[ifit]
    for (ii in unique(outputdf$repi)) {
      repiidf <- outputdf[outputdf$repi==ii & outputdf$namns==fit,]
      #stripchart(rmses ~ plotnamns,data=repiidf, add=T,at=ifit,
      stripchart(xi ~ plotnamns,data=repiidf, add=T,at=ifit,
                 pch=20+((ii-1)%%5+1),
                 col=fit.colors[ifit],bg=fit.colors[ifit],cex=2)
    }
  }
  
  # Add PRMSEs offset up
  for(ifit in 1:2) {
    fit <- c('DiceKriging', 'mlegp')[ifit]
    for (ii in unique(outputdf$repi)) {
      repiidf <- outputdf[outputdf$repi==ii & outputdf$namns==fit,]
      #stripchart(prmses ~ plotnamns,data=repiidf, add=T,at=ifit+yoffset,
      stripchart(pi ~ plotnamns,data=repiidf, add=T,at=ifit+yoffset,
                 pch=20+((ii-1)%%5+1),
                 col=fit.colors[ifit],bg='gray76',cex=2)
    }
  }

  if (saveplot) dev.off()
  ###### End of RMSE and PRMSE on same stripchart
  
  par(mar=default.par.mar)
  
}

save_supplementary_table <- function(outdf, SS, save_file=TRUE) {browser()
  names(outdf) <- c("Fit", "EMRMSE", "PMRMSE", "Runtimes", "Rep")
  
  outdf <- plyr::ddply(outdf, .(Rep), function(dd) {dd$best_rmse <- min(dd$EMRMSE); dd$LM_rmse <- dd$EMRMSE[which(dd$Fit == "LM")]; dd})
  
  outdf$POARMSE <- outdf$PMRMSE / outdf$EMRMSE
  outdf$PWBRMSE <- (outdf$EMRMSE - outdf$best_rmse) / outdf$EMRMSE
  
  outdf$xi <- outdf$EMRMSE / outdf$LM_rmse
  outdf$pi <- outdf$PMRMSE / outdf$LM_rmse
  
  outdf <- outdf[,c(1,2,3,8,9,10,11,5,4)]
  
  outdf <- outdf[order(outdf$Fit), ]
  
  write.csv(outdf, 
            paste(
              "/sscc/home/c/cbe117/Research/GPC/StochasticKriging/MM1/SK-MM1_D1_SS",
              SS,
              "_R5_SupplementaryTable.csv"
            ),
            row.names = FALSE
  )
  outdf
}