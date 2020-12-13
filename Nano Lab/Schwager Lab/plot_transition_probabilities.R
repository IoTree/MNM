library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)



aliasmeasdata <- 'plot_transition_probabilities'  ## name alias 

dataf <- data.frame(x = 2:20, y = 2:20)


for (n in df[["x"]]) {
#  dataf[["y"]][[n-1]] <- round((((2*(1.602176565*10^-19)*(2*n*139*10^-12))/pi^2)^2)/(10^-59),2)
  dataf[["y"]][[n-1]] <- (((2*(1.602176565*10^-19)*(2*n*139*10^-12))/pi^2)^2)/(10^-56)
}





## plot and print to img the scan Data 
titel <- aliasmeasdata
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott10 <- ggplot(dataf) + 
  geom_point(aes(x, y)) +
  geom_line(aes(x, y)) +
  expand_limits(x = 0, y = 0) +
  ylab("transition probabiltiy *10^-56") + xlab("conjugated double bond") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
#  annotate("text", x = x1-45, y = halfmax, label = paste(x1, " THz", sep=""), size = 5) +
#  annotate("text", x = x2+45, y = halfmax, label = paste(x2, " THz", sep=""), size = 5) +
#  annotate("text", x = x1-120, y = 0.2, label = paste("sum(ODdv)= ", ODsum, " THz", sep=""), size = 5) +
  ggtitle(titel)
print(plott10)
dev.off()
