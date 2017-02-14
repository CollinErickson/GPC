X0 <- matrix(c(.3,.4,.5,.6,.7,.8,.9), ncol=1)
MM1.mean <- function(x) {
  x/(1-x)# + rnorm(length(x), 0, sqrt(x)/(1-x))
}
MM1.sample <- function(x) {
  #x/(1-x) + rnorm(length(x), 0, sqrt(x)/(1-x))
  sapply(x, function(xx)rgeom(1, 1-xx))
}
I1 <- sort(rep(1:length(X0), 5))
#X1 <- rbind(X0, X0, X0, X0)
X1 <- X0[I1,, drop=F]
Z1 <- MM1.sample(X1)
plot(X1, Z1)
#plyr::ddply(X, 'X')
vars1 <- sapply(split(x=Z1, f=as.factor(X1)), var)
n2props <- sqrt(vars1)/sum(sqrt(vars1))
n2 <- 500
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


# Get predictions for mlegp
mod.mlegp <- mlegp::mlegp(X, Z, nugget=nugprop*100)
P.mlegp <- mlegp::predict.gp(mod.mlegp,XP, se.fit = T)
ZP.mlegp <- P.mlegp$fit# mlegp::predict.gp(mod.mlegp,XP, se.fit = T)
ZPU.mlegp <- P.mlegp$fit + 2*P.mlegp$se.fit
ZPL.mlegp <- P.mlegp$fit - 2*P.mlegp$se.fit
plot(X, Z)
points(XP, ZP, col=2, type='l')
points(XP, ZP.mlegp, col=3, type='l')
points(XP, ZPU.mlegp, col=3, type='l')
points(XP, ZPL.mlegp, col=3, type='l')
RMSE.mlegp <- sqrt(mean((ZP.mlegp - ZP)^2))



# Get predictions for DiceKriging
mod.DiceKriging <- DiceKriging::km(~1, X, Z, noise.var=nugprop)
P.DiceKriging <- DiceKriging::predict.km(mod.DiceKriging,XP, type="SK", se.compute = T)
ZP.DiceKriging <- P.DiceKriging$mean# DiceKriging::predict.gp(mod.DiceKriging,XP, se.fit = T)
ZPU.DiceKriging <- P.DiceKriging$mean + 2*P.DiceKriging$sd
ZPL.DiceKriging <- P.DiceKriging$mean - 2*P.DiceKriging$sd
plot(X, Z)
points(XP, ZP, col=2, type='l')
points(XP, ZP.DiceKriging, col=3, type='l')
points(XP, ZPU.DiceKriging, col=3, type='l')
points(XP, ZPL.DiceKriging, col=3, type='l')
RMSE.DiceKriging <- sqrt(mean((ZP.DiceKriging - ZP)^2))


# 
system('python ~/Research/GPC/StochasticKriging/MM1/sklearnStochasticTest.py')
ZP.sklearn <- read.csv('./StochasticKriging//MM1//sklearn_YP.csv', header = F)[,1]
ZP.std.sklearn <- read.csv('./StochasticKriging//MM1//sklearn_stdP.csv', header = F)[,1]
ZPU.sklearn <- ZP.sklearn + 2*ZP.std.sklearn
ZPL.sklearn <- ZP.sklearn - 2*ZP.std.sklearn
plot(X, Z)
points(XP, ZP, col=2, type='l')
points(XP, ZP.sklearn, col=3, type='l')
points(XP, ZPU.sklearn, col=3, type='l')
points(XP, ZPL.sklearn, col=3, type='l')
RMSE.sklearn <- sqrt(mean((ZP.sklearn - ZP)^2))
