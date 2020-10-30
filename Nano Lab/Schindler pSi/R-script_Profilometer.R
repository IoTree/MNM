
library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

folder <- 'C:\\<---- please change accordingly ---->\\processed_data_for_calc\\Profilometer' # put your folder desitination hier where all the xlsx from lab class are stored
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.csv", full.names = T)


# saving all data form folder in one data.frame
alldata2 <- c()
for (n in files) {
  tblname = substr(basename(n),1,nchar(basename(n))-4)
  alldata2[[tblname]] <- read.csv(n, header = TRUE)
}

analyticalresults <- 'analyticalresults'  ## name of file without .csv
scandata <- 'scandata'  ## name of file without .csv
scanparameters <- 'scanparameters'

aliasscandata <- 'Profilomentrie Scan Data'  ## name alias 
aliasanalyticalresults <- 'Profilomentrie Analytical results'  ## name alias 
aliasscanparameters <- 'Profilomentrie Scan Parameters'  ## name alias 

movingarrow <- 9000


## erase spikes -> smoothing list of raw values -> to get average  of low and high values
#plot(Raw.Nanometer ~ Lateral.um, data = alldata2[[scandata]])  ## plot to see outline points
span_parameter <- 0.01
outlier_threshold <- 1.4
lo <- loess.smooth(alldata2[[scandata]]$Lateral.um, alldata2[[scandata]]$Raw.Nanometer, span = span_parameter)
#lines(lo$x, lo$y, lwd = 3)
#lines(lo$x, lo$y * outlier_threshold, lwd = 3, col = 2)
#lines(lo$x, lo$y / outlier_threshold, lwd = 3, col = 2)
f1 <- approxfun(lo$x, lo$y * outlier_threshold)
(wh1 <- which(alldata2[[scandata]]$Raw.Nanometer > f1(alldata2[[scandata]]$Lateral.um)))
f2 <- approxfun(lo$x, lo$y / outlier_threshold)
(wh2 <- which(alldata2[[scandata]]$Raw.Nanometer < f2(alldata2[[scandata]]$Lateral.um)))
dely1 <- alldata2[[scandata]][[2]][c(wh1)]
#delx <- alldata2[[scandata]][[1]][c(wh1)]
dely2 <- alldata2[[scandata]][[2]][-c(wh1)]
delx <- alldata2[[scandata]][[1]][-c(wh1)]
lowmean <- mean(dely1[dely1 <= -1000])
highmean <- mean(dely2[dely2 >= -100])
arrowlabel <- paste("height:", as.character(round(-1*lowmean - highmean)), sep = " ")

## plot and print to img the scan Data
titel <- aliasscandata
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott5 <- ggplot(alldata2[[scandata]]) + 
  geom_point(aes(Lateral.um, Raw.Nanometer)) +
  geom_line(aes(Lateral.um, Raw.Nanometer)) +
  ylab("movment in y [nm]") + xlab("Travel path in x [um]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel)
print(plott5)
dev.off()



## plot and print to img the scan Data with arrow and lines
titel <- paste(aliasscandata, "arrowline", sep = " ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott6 <- ggplot(alldata2[[scandata]]) + 
  geom_point(aes(Lateral.um, Raw.Nanometer)) +
  geom_line(aes(Lateral.um, Raw.Nanometer)) +
  geom_segment(
    x = movingarrow, y = lowmean,
    xend = movingarrow, yend = highmean,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1.5, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "#EC7014" # Also accepts "red", "blue' etc
  ) +
  geom_segment(
    x = movingarrow, y = highmean,
    xend = movingarrow, yend = lowmean,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1.5, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "#EC7014" # Also accepts "red", "blue' etc
  ) +
  annotate("label", x =movingarrow , y = round(lowmean + highmean)/2, label = arrowlabel, fill="white", label.size = NA) +
  geom_hline(yintercept=lowmean, linetype="dashed", color = "red", size=2) +
  geom_hline(yintercept=highmean, linetype="dashed", color = "blue", size=2) +
  ylab("movment in y [nm]") + xlab("Travel path in x [um]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel)
print(plott6)
dev.off()



