irep <- 2
x_discr <- read.csv(paste0("/sscc/home/c/cbe117/Research/GPC/GPC_Output/Discrepancy1DGP/Discrepancy1DGP_D1_SS6_PS2000_R5/Discrepancy1DGP_D1_SS6_PS2000_R5_",irep,".csv"))
x <- x_discr$x
y <- x_discr$y
xp_discr <- read.csv(paste0("/sscc/home/c/cbe117/Research/GPC/GPC_Output/Discrepancy1DGP/Discrepancy1DGP_D1_SS6_PS2000_R5/Discrepancy1DGP_D1_SS6_PS2000_R5_",irep,"_PredPts.csv"))
xp <- xp_discr$x
ypa <- xp_discr$y
xp_order <- order(xp)
xp <- xp[xp_order]
ypa <- ypa[xp_order]

plot(xp, ypa, type='l')
points(x, y, cex=2, pch=19)


dice2_out <- read.csv(paste0("/sscc/home/c/cbe117/Research/GPC/GPC_Output/Discrepancy1DGP/Discrepancy1DGP_D1_SS6_PS2000_R5/Discrepancy1DGP_D1_SS6_PS2000_R5_",irep,"_Preds_Dice2.csv"))
points(dice2_out$x[xp_order], dice2_out$yp[xp_order], type='l', col=3)

dice2_out <- read.csv(paste0("/sscc/home/c/cbe117/Research/GPC/GPC_Output/Discrepancy1DGP/Discrepancy1DGP_D1_SS6_PS2000_R5/Discrepancy1DGP_D1_SS6_PS2000_R5_",irep,"_Preds_DACEregpoly0corrgauss.csv"))
points(dice2_out$x[xp_order], dice2_out$yp[xp_order], type='l', col=4)

dice2_out <- read.csv(paste0("/sscc/home/c/cbe117/Research/GPC/GPC_Output/Discrepancy1DGP/Discrepancy1DGP_D1_SS6_PS2000_R5/Discrepancy1DGP_D1_SS6_PS2000_R5_",irep,"_Preds_laGP6.csv"))
points(dice2_out$x[xp_order], dice2_out$yp[xp_order], type='l', col=5)
