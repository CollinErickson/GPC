rastrigin <- function(x,A=10) {
  A*length(x) + sum(x^2-A*cospi(2*x))
}

rastrigin.unit <- function(x,A=10,shift=.5,scale=2.5) {
  rastrigin((x-shift)*scale,A)
}

weldnot <- function(x) {
  if (length(x) !=2) {
    stop("weldnot function only works in two dimensions")
  }
  a <- (x[1])*4-2
  b <- (x[2])*4-2
  sqrt(a^2+2*a*b*cos(a)+b^2)
}

ackley <- function(x) {
  if (length(x) !=2) {
    stop("Ackley function only works in two dimensions")
  }
  a <- (x[1])*8-4
  b <- (x[2])*8-4
  -20*exp(-.2*sqrt(.5*(a^2+b^2))) - exp(.5*cos(2*pi*a)+cos(2*pi*b))+exp(1)+20
}

ackley2 <- function(x) {
  if (length(x) !=2) {
    stop("Ackley2 function only works in two dimensions")
  }
  a <- (x[1]-.5)*4
  b <- (x[2]-.5)*4
  -20*exp(-.2*sqrt(.5*(a^2+b^2))) - exp(.5*cos(2*pi*a)+cos(2*pi*b))+exp(1)+20
}

phughes <- function(x) {
   .63 + sum(  (.2*exp(x)+sin(8*pi*x)-x^2-.5*cos(20*pi*x))*sin(pi*x)  )
 }

if (FALSE) {
  curve(sapply(x,phughes),from=0,to=1)
  x <- y <- seq(0,1,.1)
  z <- matrix(-1,length(x),length(y))
  for(i in 1:length(x)){for(j in 1:length(y)) {z[i,j] <- phughes(c(x[i],y[j]))}}
  filled.contour(x,y,z)
}

borehole <- function(xx)
{
  # copied from http://www.sfu.ca/~ssurjano/Code/boreholer.html
  
  ##########################################################################
  #
  # BOREHOLE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # OUTPUT AND INPUT:
  #
  # y  = water flow rate
  # xx = c(rw, r, Tu, Hu, Tl, Hl, L, Kw)
  #
  ##########################################################################
  
  if (length(xx)!=8) {
    stop("Borehole function only takes 8-D input")
  }
  
  # Used scaling instead
  #rw <- xx[1]
  #r  <- xx[2]
  #Tu <- xx[3]
  #Hu <- xx[4]
  #Tl <- xx[5]
  #Hl <- xx[6]
  #L  <- xx[7]
  #Kw <- xx[8]
  
  shiftscale <- function(a,b,c) {
    # assuming a is between 0 and 1, moves it into range from b to c
    a*(c-b)+b
  }
  rw <- qnorm (xx[1] , 0.10, 0.0161812 )
  r  <- qlnorm (xx[2] , 7.71, 1.0056 )
  Tu <- shiftscale(xx[3] , 63070, 115600 ) 
  Hu <- shiftscale(xx[4] , 990, 1110 )
  Tl <- shiftscale(xx[5] , 63.1, 116 )
  Hl <- shiftscale(xx[6] , 700, 820 )
  L  <- shiftscale(xx[7] , 1120, 1680 )
  Kw <- shiftscale(xx[8] , 9855, 12045 )
  
  frac1 <- 2 * pi * Tu * (Hu-Hl)
  
  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+frac2a+frac2b)
  
  y <- frac1 / frac2
  return(y)
}

otlcircuit <- function(xx)
{
  # 1/28/16 Copied from http://www.sfu.ca/~ssurjano/Code/otlcircuitr.html, http://www.sfu.ca/~ssurjano/otlcircuit.html
  # changed to scale inputs from 0 to 1
  
  ##########################################################################
  #
  # OTL CIRCUIT FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # OUTPUT AND INPUT:
  #
  # Vm = midpoint voltage
  # xx = c(Rb1, Rb2, Rf, Rc1, Rc2, beta)
  #
  ##########################################################################
  
  if (length(xx)!=6) {
    stop("OTL circuit function only takes 6-D input")
  }
  
  # Used shifted instead
  #Rb1  <- xx[1]
  #Rb2  <- xx[2]
  #Rf   <- xx[3]
  #Rc1  <- xx[4]
  #Rc2  <- xx[5]
  #beta <- xx[6]
  
  shiftscale <- function(a,b,c) {
    # assuming a is between 0 and 1, moves it into range from b to c
    a*(c-b)+b
  }
  
  Rb1  <- shiftscale(xx[1] , 50 , 150 )
  Rb2  <- shiftscale(xx[2] , 25 , 70 )
  Rf   <- shiftscale(xx[3] , .5 , 3 )
  Rc1  <- shiftscale(xx[4] , 1.2 , 2.5 )
  Rc2  <- shiftscale(xx[5] , .25 , 1.2 )
  beta <- shiftscale(xx[6] , 50 , 300 )
  
  Vb1 <- 12*Rb2 / (Rb1+Rb2)
  term1a <- (Vb1+0.74) * beta * (Rc2+9)
  term1b <- beta*(Rc2+9) + Rf
  term1 <- term1a / term1b
  
  term2a <- 11.35 * Rf
  term2b <- beta*(Rc2+9) + Rf
  term2 <- term2a / term2b
  
  term3a <- 0.74 * Rf * beta * (Rc2+9)
  term3b <- (beta*(Rc2+9)+Rf) * Rc1
  term3 <- term3a / term3b
  
  Vm <- term1 + term2 + term3
  return(Vm)
}
braninsc <- function(xx)
{
  # taken from http://www.sfu.ca/~ssurjano/branin.html 1/28/16
  
  ##########################################################################
  #
  # BRANIN FUNCTION, RESCALED
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  if (length(xx)!=2) {
    stop("Braninsc function only takes 2-D input")
  }
  x1 <- xx[1]
  x2 <- xx[2]
  
  x1bar <- 15*x1 - 5
  x2bar <- 15 * x2
  
  term1 <- x2bar - 5.1*x1bar^2/(4*pi^2) + 5*x1bar/pi - 6
  term2 <- (10 - 10/(8*pi)) * cos(x1bar)
  
  y <- (term1^2 + term2 - 44.81) / 51.95
  return(y)
}


fried <- function(xx)
{
  # taken 1/28/16 http://www.sfu.ca/~ssurjano/fried.html
  
  ##########################################################################
  #
  # FRIEDMAN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, x3, x4, x5)
  #
  ##########################################################################
  
  if (length(xx)!=5) {
    stop("Friedman function only takes 5-D input")
  }
  
  x1 <- xx[1]
  x2 <- xx[2]
  x3 <- xx[3]
  x4 <- xx[4]
  x5 <- xx[5]
  
  term1 <- 10 * sin(pi*x1*x2)
  term2 <- 20 * (x3-0.5)^2
  term3 <- 10*x4
  term4 <- 5*x5
  
  y <- term1 + term2 + term3 + term4
  return(y)
}

detpep108d <- function(xx)
{
  # taken on 1/28/16 from http://www.sfu.ca/~ssurjano/detpep108d.html
  
  ##########################################################################
  #
  # DETTE & PEPELYSHEV (2010) 8-DIMENSIONAL FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., x8)
  #
  #########################################################################
  
  if (length(xx)!=8) {
    stop("Dette Pepelyshev function only takes 8-D input")
  }
  
  x1 <- xx[1]
  x2 <- xx[2]
  x3 <- xx[3]
  ii <- c(4:8)
  
  term1 <- 4 * (x1 - 2 + 8*x2 - 8*x2^2)^2
  term2 <- (3 - 4*x2)^2
  term3 <- 16 * sqrt(x3+1) * (2*x3-1)^2
  
  xxmat <- matrix(rep(xx[3:8],times=6), 6, 6, byrow=TRUE)
  xxmatlow <- xxmat
  xxmatlow[upper.tri(xxmatlow)] <- 0
  
  inner <- rowSums(xxmatlow)
  inner <- inner[2:6]
  outer <- sum(ii*log(1+inner))
  
  y <- term1 + term2 + term3 + outer
  return(y)
}

franke2d <- function(xx)
{ # taken 1/28/16 from http://www.sfu.ca/~ssurjano/franke2d.html
  ##########################################################################
  #
  # FRANKE'S FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  if (length(xx)!=2) {
    stop("Franke 2D function only takes 2-D input")
  }
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- 0.75 * exp(-(9*x1-2)^2/4 - (9*x2-2)^2/4)
  term2 <- 0.75 * exp(-(9*x1+1)^2/49 - (9*x2+1)/10)
  term3 <- 0.5 * exp(-(9*x1-7)^2/4 - (9*x2-3)^2/4)
  term4 <- -0.2 * exp(-(9*x1-4)^2 - (9*x2-7)^2)
  
  y <- term1 + term2 + term3 + term4
  return(y)
}


limetal02pol <- function(xx)
{ # Taken 1/28/16 from http://www.sfu.ca/~ssurjano/limetal02pol.html
  ##########################################################################
  #
  # LIM ET AL. (2002) FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  if (length(xx)!=2) {
    stop("Lim et al polynomial function only takes 2-D input")
  }
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- (5/2)*x1 - (35/2)*x2
  term2 <- (5/2)*x1*x2 + 19*x2^2
  term3 <- -(15/2)*x1^3 - (5/2)*x1*x2^2
  term4 <- -(11/2)*x2^4 + (x1^3)*(x2^2)
  
  y <- 9 + term1 + term2 + term3 + term4
  return(y)
}











RGP <- function(d,npd,seed=NULL,return.all=F,use.GPfit=T,corr.power=2,betas=NULL,...) {
  # Created by Collin Erickson Nov 2015
  require('lhs')
  require('mlegp')
  nn <- npd^d # d*npd  ## nn is number of points
  if (!is.null(seed)) {set.seed(seed)}  # set seed if given, used to replicate
  xx <- lhs::maximinLHS(n = nn,k = d)  # select design pts from lhs
  if(is.null(betas)) betas <- rep(2,d)#runif(d,-1,3)  # correlation coefficients, no clue how to pick them best
  correl <- GPfit::corr_matrix(X=xx,beta = betas,
            corr=list(type='exponential',power=corr.power))  # find correlation matrix
  ch <- chol(correl)  # Find Cholesky facorization
  uu <- runif(nn)  # Generate independent standard normal
  yy <- t(ch)%*% uu  # Convert to multinormal using Cholesky
  #   require(MASS) #DONT USE MASS mvrnorm, my way is 12 times faster for d=6 npd=4
  #   yy <- mvrnorm(1,rep(0,nn),correl)
  #   system.time(yy <- mvrnorm(1,rep(0,nn),correl))
  #   system.time({ch <- chol(correl)  ;    uu <- runif(nn)  ;           yy <- t(ch)%*% uu  })
  #yy <- uu
  minyy <- min(yy)
  maxyy <- max(yy)
  yy <- (yy-minyy)/(maxyy-minyy) # Scale yy to be in [0,1]
  if (use.GPfit) {  # create model based on which is specified
    #mod <- GPfit::GP_fit(xx,yy,control=c(25*d,10*d,2*d),corr=list(type='exponential',power=corr.power))
    # Fit quickly, then change data and parameters of model.
    mod <- GPfit::GP_fit(xx[1:min(10,nn),],yy[1:min(10,nn)],control=c(10,4,2),corr=list(type='exponential',power=corr.power))
    mod$X <- xx
    mod$Y <- yy
    mod$beta <- betas
    mod$delta <- 0
    return.function <- function(aa) {
      aa <- matrix(aa,nrow = 1)
      GPfit::predict.GP(mod,aa)$Y
    }
  } else {
    mod <- mlegp::mlegp(xx,yy,verbose=0,...) 
    #mod <- mlegp::mlegp(xx[1:min(10,nn),],yy[1:min(10,nn),],verbose=0,...) 
    #mlegp::predict.gp(mod,c(.1,2.,3))
    #mod$Z <- yy
    #mod$X <- xx
    #mod$beta <- betas
    #mod$numObs <- length(yy)
    #mod$mu <- rep(mod$mu[1],length(yy))
    return.function <- function(aa) {mlegp::predict.gp(mod,aa)}
  }
  if (return.all) {
    RGP.string <- paste('RGP','d =',d,', npd = ',npd,', nn = ',nn,', use.GPfit = ',use.GPfit,
                        ', betas = ',paste(betas,collapse=', '),
                        ', seed = ',seed,', corr.power = ',corr.power)
    return(
      list(  get=return.function,
             d=d,npd=npd,nn=nn,xx=xx,yy=yy,mod=mod,
             betas=betas,seed=seed,corr.power=corr.power,
             use.GPfit=use.GPfit,
             string=RGP.string)
    )
  } else {
    return(return.function)
  }  
}

RGP.points <- function(d,n,np,betas,corr.power=NULL,seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  if(is.null(corr.power)) corr.power=2
  if(length(betas)==1 & d!=1) betas <- rep(betas,d)
  x <- lhs::maximinLHS(n,d)
  xp <- lhs::maximinLHS(np,d)
  xx <- rbind(x,xp)
  nn <- n+np
  correl <- GPfit::corr_matrix(X=xx,beta = betas,
                               corr=list(type='exponential',power=corr.power))  # find correlation matrix
  yy <- MASS::mvrnorm(1,rep(0,nn),correl)
  minyy <- min(yy)
  maxyy <- max(yy)
  yy <- (yy-minyy)/(maxyy-minyy) # Scale yy to be in [0,1]
  plot(xx[(n+1):nn],yy[(n+1):nn],pch=19,col='red')
  points(xx[1:n],yy[1:n],pch=19)
  return(list(x=x,y=yy[1:n],xp=xp,ypa=yy[(n+1):nn]))
}

contour.function <- function(functouse,npts=50) {
  x <- seq(from=0,to=1,len=npts)
  y <- seq(from=0,to=1,len=npts)
  z <- matrix(0,npts,npts)
  for (i in 1:npts) {
    for (j in 1:npts) {
      z[i,j] <- functouse(c(x[i],y[j]))
    }
  }
  contour(x,y,z)
}



# 1/13/17 Adding 20D Morris function
Morris <- function(x) {
  beta1 <- (-1)^(1:20)
  beta1[1:10] <- 20
  beta2 <- outer(1:20,1:20,Vectorize(function(i,j) {if (i<j) (-1)^(i+j) else 0}))
  beta2[1:6, 1:6] <- -15
  w <- 2*(x-.5)
  w[c(3,5,7)] <- 2*(1.1*x[c(3,5,7)]/(x[c(3,5,7)]+.1) - .5)
  t3 <- -10 * sum(w[1]*(w[2]*w[3] + w[2]*w[4] + w[2]*w[5] + w[3]*w[4] + w[3]*w[5] + w[4]*w[5]) +
                    w[2]*(w[3]*w[4] + w[3]*w[5] + w[4]*w[5]) +
                    w[3]*w[4]*w[5])
  sum(beta1 * w) + sum(beta2 * outer(w, w)) + t3 + 5*prod(w[1:4])
}
