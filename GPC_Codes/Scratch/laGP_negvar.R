library(laGP)
x  <-  c(0.7843581, 0.4429286, 0.7518785, 0.5220036, 0.1914810, 0.2311136, 0.9931869, 0.1086712, 0.6474116)
y  <- c(0.838690735, 0.329711617, 0.798546936, 0.451856475, 0.048418318, 0.077427461, 0.998183732, 0.009650393, 0.649970429)
set.seed(305)

x <- c(0.78435811625483131682,0.44292859875390100388,0.75187854724936187267,0.52200356751887333484,0.19148100708197388076,0.23111364845600393392,0.99318692962535548574,0.10867117474683457568,0.64741164883081281367)
y <- c(0.83869073457593268373,0.32971161698946427343,0.79854693627797856958,0.45185647471824080812,0.04841831770010144448,0.07742746102269898456,0.99818373193966403178,0.00965039311920488051,0.64997042931735726778)


da <- laGP::darg(list(mle=TRUE), X=as.matrix(x))
ga <- laGP::garg(list(mle=TRUE), y=y)
mod <- laGP::newGPsep(X=as.matrix(x), Z=y, d=da$start, g=ga$start, dK = TRUE)
mleGPsep.out <- laGP::jmleGPsep(mod, drange=c(da$min, da$max), grange=c(ga$min, ga$max), dab=da$ab, gab=ga$ab, verb=1)

#set.seed(06)
xp <- c(0.4873780, 0.4940855, 0.5326270)#lhs::maximinLHS(1000,1)
xp <- c(0.48737802859977802372,0.49408549573971000113,0.53262701862957295695)
modp <- laGP::predGPsep(mod, as.matrix(xp),lite=T)
summary(modp$s2)

xp <- runif(100)

da <- laGP::darg(list(mle=TRUE), X=as.matrix(x))
mod <- laGP::newGPsep(X=as.matrix(x), Z=y, d=da$start, g=0, dK = TRUE)
mleGPsep.out <- laGP::mleGPsep(mod, verb=1)
modp <- laGP::predGPsep(mod, as.matrix(xp),lite=T)
points(xp,modp$mean,col='red')
