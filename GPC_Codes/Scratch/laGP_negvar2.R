dat <- read.csv("//sscc//home//c//cbe117//Research//GPC//GPC_Output//RGPP_D1_B0//RGPP_D1_B0_D1_SS6_PS200_R8//RGPP_D1_B0_D1_SS6_PS200_R8_2.csv")
x <- dat$x
y <- dat$y
datp <- read.csv("//sscc//home//c//cbe117//Research//GPC//GPC_Output//RGPP_D1_B0//RGPP_D1_B0_D1_SS6_PS200_R8//RGPP_D1_B0_D1_SS6_PS200_R8_2_PredPts.csv")
xp <- datp$x
ypa <- datp$y
set.seed(302)
da <- laGP::darg(list(mle=TRUE), X=as.matrix(x))
mod <- laGP::newGP(X=as.matrix(x), Z=y, d=da$start, g=0, dK = TRUE) # no nugget
mleGP.out <- laGP::mleGP(mod,  verb=1)
modp <- laGP::predGP(mod, as.matrix(xp),lite=T)
summary(modp$s2)
table(modp$s2<0)
laGP::deleteGP(mod)
plot(x,y,main='Green predictions have negative variance',cex=3)
points(xp,modp$mean,col='red',pch=19,cex=.3)
points(xp,modp$mean + 3*modp$s2,col='blue',pch=19,cex=.3)
points(xp[modp$s2<0],modp$mean[modp$s2<0],col='green',cex=1)
plot((modp$mean-ypa)^2,modp$s2,main='Squared errors vs predicted s2')
curve(1*x,add=T,col='red')
curve(3*x,add=T,col='blue')
curve(10*x,add=T,col='green')
plot(abs(modp$mean-ypa)[modp$s2>=0],sqrt(modp$s2[modp$s2>=0]),
    main='Absolute errors vs predicted s\n (purple circles have neg var)')
points(abs(modp$mean-ypa)[modp$s2<0],rep(0,sum(modp$s2<0)),col='purple',cex=2)
curve(1*x,add=T,col='red')
curve(3*x,add=T,col='blue')
curve(10*x,add=T,col='green')





