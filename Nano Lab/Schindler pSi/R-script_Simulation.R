library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

### simulation of bragg mirror
### INPUT!!!
na <- 1; nb <- 1.52; nH <- 2.32; nL <- 1.38;  ## refractive indices
LH <- 0.25; LL <- 0.25                        ## optical thicknesses in units of lambda0
la0 <- 500                                    ## lambda0 in units of nm
rho <- (nH-nL)/(nH+nL)                        ## reflection coefficient rho
la2 <- pi*(LL+LH)*1/acos(rho) * la0       ## right bandedge
la1 <- pi*(LL+LH)*1/acos(-rho) * la0      ## left bandedge
Dla <- la2-la1                            ## bandwidth
N <- 10                                    ## number of bilayers
aliassimulation <- "Simulation of Bragg mirror"


# mutlidiel fuction refectance as a funcition of lampda -> scrol down!!!!! maped from matlap
# multidiel.m - reflection response of isotropic or birefringent multilayer structure
#
#          na | n1 | n2 | ... | nM | nb
# left medium | L1 | L2 | ... | LM | right medium 
#   interface 1    2    3     M   M+1
#
# Usage: [Gamma,Z] = multidiel(n,L,lambda,theta,pol)
#        [Gamma,Z] = multidiel(n,L,lambda,theta)       (equivalent to pol='te')
#        [Gamma,Z] = multidiel(n,L,lambda)             (equivalent to theta=0)
#
# n      = isotropic 1x(M+2), uniaxial 2x(M+2), or biaxial 3x(M+2), matrix of refractive indices
# L      = vector of optical lengths of layers, in units of lambda_0
# lambda = vector of free-space wavelengths at which to evaluate the reflection response
# theta  = incidence angle from left medium (in degrees)
# pol    = for 'tm' or 'te', parallel or perpendicular, p or s, polarizations
#
# Gamma = reflection response at interface-1 into left medium evaluated at lambda 
# Z     = transverse wave impedance at interface-1 in units of eta_a (left medium)
#
# notes: M is the number of layers (M >= 0)
#
#        n = [na, n1, n2, ..., nM, nb]        = 1x(M+2) row vector of isotropic indices
#
#            [ na1  n11  n12  ...  n1M  nb1 ]   3x(M+2) matrix of birefringent indices, 
#        n = [ na2  n21  n22  ...  n2M  nb2 ] = if 2x(M+2), it is extended to 3x(M+2)
#            [ na3  n31  n32  ...  n3M  nb3 ]   by repeating the top row
#
#        optical lengths are in units of a reference free-space wavelength lambda_0:
#        for i=1,2,...,M,  L(i) = n(1,i) * l(i), for TM, 
#                          L(i) = n(2,i) * l(i), for TE,
#        TM and TE L(i) are the same in isotropic case. If M=0, use L=[].
#
#        lambda is also in units of lambda_0, that is, lambda/lambda_0 = f_0/f
#
#        reflectance = |Gamma|^2, input impedance = Z = (1+Gamma)./(1-Gamma)
#
#        delta(i) = 2*pi*[n(1,i) * l(i) * sqrt(1 - (Na*sin(theta))^2 ./ n(3,i).^2))]/lambda, for TM
#        delta(i) = 2*pi*[n(2,i) * l(i) * sqrt(1 - (Na*sin(theta))^2 ./ n(2,i).^2))]/lambda, for TE
#
#        if n(3,i)=n(3,i+1)=Na, then will get NaN's at theta=90 because of 0/0, (see also FRESNEL)
# S. J. Orfanidis - 2000 - www.ece.rutgers.edu/~orfanidi/ewa

multidiel <- function(n,L,lambda,theta,pol){ # [Gamma,Z] = multidiel
  #if nargs()==0, help multidiel; return; end
  if (nargs()<=4){pol <- 'te'}
  if (nargs()==3){theta <- 0}
  if (ncol(n)==1){n <- t(n)}                               # in case n is entered as column 
  K <- nrow(n)                                          # birefringence dimension
  M <- ncol(n)-2                                        # number of layers
  if (K==1){ n <- rbind(n, n, n)}                             # isotropic case
  if (K==2){ n <- rbind(n[1,], n)}                           # uniaxial case
  if (M==0){ L <- c()}                                    # single interface, no slabs
  theta <- theta * pi/180
  if (pol=='te'){
    Nsin2 <- (n[2,1]*sin(theta))^2                      # (Na*sin(tha))^2              
    c <- sqrt(1 - Nsin2 / n[2,] ^ 2)                   # coefficient ci, or cos(th(i)) in isotropic case
    nT <- n[2,] * c                                   # transverse refractive indices
    r <- -diff(nT) / (2*nT[1:(length(nT)-1)] + diff(nT))                                        # r(i) = (nT(i-1)-nT(i)) / (nT(i-1)+nT(i))
  }  else {
    Nsin2 <- (n[1,1]*n[3,1]*sin(theta))^2 / (n[3,1]^2*cos(theta)^2 + n[1,1]^2*sin(theta)^2)
    c <- sqrt(1 - Nsin2 / n[3,] ^ 2)
    nTinv <- c / n[1,]                                # nTinv(i) = 1/nT(i) to avoid NaNs
    r <- -1*(-diff(nTinv) / (2*nTinv[1:(length(nTinv)-1)] + diff(nTinv)))                                    # minus sign because n2r(n) = -n2r(1./n)
  }
  
  if (M>0){
    L <- L * c[2:M+1]                                  # polarization-dependent optical lengths
  }
  Gamma <- r[M+1] * matrix(1,1,length(lambda))                # initialize Gamma at right-most interface
  for (i in seq(M,1)) {                                         # forward layer recursion 
    delta <- 2 * pi * L[i] / lambda                          # phase thickness in i-th layer
    z <- exp(-2 * 1i * delta)                          
    Gamma <- (r[i] + Gamma * z) / (1 + r[i] * Gamma * z);
  }
  Z <- (1 + Gamma) / (1 - Gamma)
#  return(c(Gamma,Z))
  return(data.frame(c(Gamma), c(Z)))
}

  
  
### calc  
n <- data.frame(c(na, nH, rep(c(nL,nH), times = N), nb))  ## indices for the layers A|H(LH)N|G
L <- c(LH, rep(c(LL,LH), times = N))          ## lengths of the layers H(LH)N
la <- seq(300,800, length=501)               ## plotting range is 300 ≤ lambda ≤ 800 nm
Gla <- 100*abs(multidiel(n,L,la / la0)) ^ 2  ## reflectance as a function of lambda
f <- seq(0,6, length = 1201)                   ## frequency plot over 0 ≤ f ≤ 6f0
Gf <- 100*abs(multidiel(n,L, 1 / f )) ^ 2     ## reflectance as a function of f


## ploting
Gammaplot <- data.frame("x"=la, "y"= Gla[[1]])
Zplot <- data.frame("x"=f, "y"= Gf[[1]])

titel <- paste(aliassimulation, "by Wavelenght", sep = " ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott10 <- ggplot(Gammaplot) + 
#  geom_point(aes(x, y)) +
  geom_line(aes(x, y), size = 1) +
  ylab("Reflectivity [%]") + xlab("Wavelenght [nm]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel) +
  ylim(0, 100)
print(plott10)
dev.off()


titel <- paste(aliassimulation, "by freq", sep = " ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott11 <- ggplot(Zplot) + 
#  geom_point(aes(x, y)) +
  geom_line(aes(x, y), size = 1) +
  ylab("Reflectivity [%]") + xlab("freq [1/s]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel) +
  ylim(0, 100)
print(plott11)
dev.off()
