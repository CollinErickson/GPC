stripchart(dats$df$rmse)
plot(dats$df$fit,dats$df$rmse)
plot(rmse,fit,data=dats$df)
with(dats$df,plot(rmse,fit))


stripchart(dats$rmse,pch=20,col='white')
stripchart(dats$prmse,add=T,pch=3,col='red')
for(ii in 1:reps) {
  stripchart(sapply(dats$rmse,function(xx){xx[ii]}),add=T,pch=as.character(ii),col=ii)
}


# R2 get
dats$df$R2 <- NA
dats$df$R <- NA
dats$df$PBPM
for(rowind in 1:(dim(dats$df)[1])){
  predictmeanind <- which(dats$df$fit=='PredictMean' & dats$df$rep==dats$df$rep[rowind])
  dats$df$R2[rowind] <- 1 - ( dats$df$rmse[rowind] / dats$df$rmse[predictmeanind] )^2 #square for R2 to get MSE not RMSE
  dats$df$R[rowind] <- sqrt(dats$df$R2[rowind])
  dats$df$PBPM[rowind] <- 1 - dats$df$rmse[rowind] / dats$df$rmse[predictmeanind]
}