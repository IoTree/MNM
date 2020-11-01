
library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

### INPUT !!!
#For bragg mirror
nair <- 1 # refractive ubdex 
nsupstrate <- 4.2 # refractive ubdex
nH <- 2.13 # refractive ubdex high layer complex if posible
nL <- 1.82 #  refractive ubdex low layer complex if posible
N <- 10 # number of bie layers
lam <- 500 #center wavelength for ploting or target wavelength for calc thickness in [nm]
dlam <- 0.25 #thikness of of wavelenght like 1/4 or 1/2  -> e.g. lambda/4
angle <- 0 #angle of incidence
polarization <- 0
startw <- 300 #start wavelenght in nm
stopw <-800 #end wavelengt ub nm
plotrange <- seq(startw,stopw, length= (stopw - startw +1)) 
aliassimulation <- "Simulation of Bragg mirror"


### General calc -> its posibile to do diffrent desings biside bragg mirror just uncommend part befor and fill out below
# array of complex refractive ibdex
n <- c(nair,rep(c(nH,nL), times = N), nsupstrate)
# array of thicknesses
d <- c(NaN, rep(c((lam/nH*dlam),(lam/nL*dlam)), times = N), NaN)


# jreftran_rt.m
# Reflection and transmission coefficient calculator of lossy multilayer 
# media for optical scientists
# Shawn Divitt
# ETH Zurich
# Photonics Group
# Ver. 2.1, 15 Nov 2016
#-iwt time dependence
#This program produces the complex reflection and transmission coefficients
#of a multilayer stack given the angle of incidence, polarization,
#wavelength, complex-refractive index of each layer, and thickness of each
#layer. The program assumes lossless dielectric incident and exit media. 
#The program also assumes that all layers are non-magnetic. Magnetic media
#can be handled by more general theory (see technical report cited below). 
#This program was inspired by the technical report written by K. Pascoe, 
#"Reflectivity and Transmissivity through Layered, Lossy Media: A
#User-Friendly Approach," 2001. See the following link for the technical
#paper:
#http://oai.dtic.mil/oai/oai?verb=getRecord&metadataPrefix=html&identifier=ADA389099
#####   function [r,t,R,T,A]=


jreftran_rt <- function (l,d,n,t0,polarization){
  #l = free space wavelength, nm
  #d = layer thickness vector, nm
  #n = layer complex refractive index vector
  #t0= angle of incidence
  #polarization should be 0 for TE (s-polarized), otherwise TM (p-polarized)
  
  Z0 <- 376.730313 #impedance of free space, Ohms

  #the line below had mistakenly been a Z0/n instead of a n/Z0 in version 1!
  Y <- n / Z0 #admittance in terms of impedance of free space and refractive index, assuming non-magnetic media

  g <- 1i*2*pi*n / l #propagation constant in terms of free space wavelength and refractive index

  #all of the calculations rely on cosine of the complex angle, but we can
  #only find the sine of the complex angle from snells law. So we use the
  #fact that cos(asin(x))=sqrt(1-x^2)
  #t=asin(n(1)./n*sin(t0)), complex theta for each layer

  ct <- sqrt(1-(n[1] / n*sin(t0)) ^ 2) #cosine theta
  if (polarization==0){
    eta <- Y * ct #tilted admittance, TE case
  }
  else {
    eta <- Y / ct #tilted admittance, TM case
  }
  delta <- 1i * g * d * ct
  M <- array(0L,c(2,2,length(d)))
  for (j in 1:length(d)){
    M[1,1,j]=cos(delta[j])
    M[1,2,j]=1i / eta[j] * sin(delta[j])
    M[2,1,j]=1i * eta[j] * sin(delta[j])
    M[2,2,j]=cos(delta[j])
  }
  M_t <- diag(2) # M total
  for (j in 2:((length(d)-1))){
    M_t <- M_t %*% M[,,j]
#    M_t <-t(M[,,j])
  }
  r <- (eta[1]*(M_t[1,1]+M_t[1,2]*eta[length(eta)])-(M_t[2,1]+M_t[2,2]*eta[length(eta)]))/(eta[1]*(M_t[1,1]+M_t[1,2]*eta[length(eta)])+(M_t[2,1]+M_t[2,2]*eta[length(eta)]))
  t <- 2*eta[1]/(eta[1]*(M_t[1,1]+M_t[1,2]*eta[length(eta)])+(M_t[2,1]+M_t[2,2]*eta[length(eta)]))
  R <- abs(r)^2
  Tk <- Re(eta[length(eta)]/eta[1])*abs(t)^2
  A <- (4*eta[1]*Re((M_t[1,1]+M_t[1,2]*eta[length(eta)])*Conj(M_t[2,1]+M_t[2,2]*eta[length(eta)])-eta[length(eta)]))/abs(eta[1]*(M_t[1,1]+M_t[1,2]*eta[length(eta)])+(M_t[2,1]+M_t[2,2]*eta[length(eta)]))^2
  return(data.frame(r,t,R,Tk,A))
}

o <- list()

for (j in 1:length(plotrange)) {
  o[[j]] <- jreftran_rt(plotrange[j],d,n,angle,polarization)
}
alldata <- do.call(rbind, o)

resultforplot <- data.frame("x"= plotrange, "y" = alldata[["R"]]*100)


titel <- paste(aliassimulation, "by Wavelenght", sep = " ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott10 <- ggplot(resultforplot) + 
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
