library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

folder <- 'C:\\<-----   your path to measuerment data ----->\\measuerment data' # put your folder desitination hier where all the xlsx from lab class are stored
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.csv", full.names = T)


# saving all data form folder in one data.frame
alldata4 <- c()
for (n in files) {
  tblname = substr(basename(n),1,nchar(basename(n))-4)
  alldata4[[tblname]] <- read.csv(n, header = TRUE)
}

measdata <- '3_Hexylthiophen_polymer_absorption'  ## name of file without .csv
aliasmeasdata <- 'polymer_absorption'  ## name alias 


max <- alldata4[[measdata]][which.max(alldata4[[measdata]][["y"]]),]
xmax <- max[["x"]]
halfmax <- max[["y"]]/2
x1 <- alldata4[[measdata]][["x"]][alldata4[[measdata]][["x"]] < xmax][which.min(abs(alldata4[[measdata]][["y"]][alldata4[[measdata]][["x"]] < xmax]-max(alldata4[[measdata]][["y"]])/2))]
x2 <- alldata4[[measdata]][["x"]][alldata4[[measdata]][["x"]] > xmax][which.min(abs(alldata4[[measdata]][["y"]][alldata4[[measdata]][["x"]] > xmax]-max(alldata4[[measdata]][["y"]])/2))]
speedlight <- 299792458000000000 # nm / s
FWHM <- round(abs((speedlight/x1)-(speedlight/x2))/1000000000000, 2)


## plot and print to img the scan Data 
titel <- aliasmeasdata
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott9 <- ggplot(alldata4[[measdata]]) + 
  geom_point(aes(x, y)) +
  geom_point(aes(x1, halfmax), colour = "red", size = 3)+ 
  geom_point(aes(x2, halfmax), colour = "red", size = 3)+ 
  geom_point(aes(max[["x"]], max[["y"]]), colour = "blue", size = 3)+ 
  geom_line(aes(x, y)) +
  ylab("Absobiton") + xlab("Wavelenght [nm]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  annotate("text", x = x2+50, y = halfmax+0.05, label = paste("FWHM = ", FWHM, " THz", sep=""), size = 5) +
  annotate("text", x = x1-30, y = halfmax, label = paste(x1, " nm", sep=""), size = 5) +
  annotate("text", x = x2+30, y = halfmax, label = paste(x2, " nm", sep=""), size = 5) +
  annotate("text", x = max[["x"]]+75, y = max[["y"]], label = paste("max = ", max[["y"]], " Abs at ",max[["x"]]," nm", sep=""), size = 5) +
  ggtitle(titel)
print(plott9)
dev.off()
