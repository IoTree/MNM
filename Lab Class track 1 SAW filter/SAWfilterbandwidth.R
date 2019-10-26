
# Note for calc sub: f/f0=ff0 

tau <- 0.7*(10^-6)
N <- 18
f0 <- 95.6*10^6
#fm <- 95.6*10^6
#fm <- 114.7*10^6
#fm <- seq(40, 150, 1)*10^6
#fm <- 95.6*10^6

#preperation
n <- seq(1, N, 1)
f <- seq(65, 125, 0.1)*10^6
y <- c()

# for normalized transfer function
resultofsum <- sum(((-1)^(n-1))*exp(complex(imag = -((2*pi*f0*(n-1)/(2*f0))))))
Hw <- (((1/(sqrt(2*pi)))*(resultofsum))^2)*exp(complex(imag = (2*pi*f0*tau)))
Hwf0abssquert <- (abs(Hw))^2
Hwf0dB <- 10*log10(Hwf0abssquert)

# for calc
for (fm in f) {
  resultofsum <- sum(((-1)^(n-1))*exp(complex(imag = -((2*pi*fm*(n-1)/(2*f0))))))
  Hw <- (((1/(sqrt(2*pi)))*(resultofsum))^2)*exp(complex(imag = (2*pi*fm*tau)))
  Hwabssquert <- (abs(Hw))^2
  HwdB <- 10*log10(Hwabssquert)
  #y <- c(y, (Hwabssquert/Hwf0abssquert))
  y <- c(y, 10*log10(Hwabssquert/Hwf0abssquert))
  
}

# some cleaning
y[y< -50] <- NA

#ploting
plot(f/1000000, y, main="D25", ylab = "(|H(w)|^2)dB-(|H(w0)|^2)dB", xlab = "f[MHz]", type="l")
grind ()
