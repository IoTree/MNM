
library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

folder <- 'C:\\<---- please change accordingly ---->\\processed_data_for_calc\\Profilometer_lens' # put your folder desitination hier where all the xlsx from lab class are stored
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.csv", full.names = T)


# saving all data form folder in one data.frame
alldata3 <- c()
for (n in files) {
  tblname = substr(basename(n),1,nchar(basename(n))-4)
  alldata3[[tblname]] <- read.csv(n, header = TRUE)
}

analyticalresults <- 'analyticalresults'  ## name of file without .csv
scandata <- 'scandata'  ## name of file without .csv
scanparameters <- 'scanparameters'

aliasscandata <- 'Profilomentrie lens Scan Data'  ## name alias 
aliasanalyticalresults <- 'Profilomentrie lens Analytical results'  ## name alias 
aliasscanparameters <- 'Profilomentrie lens Scan Parameters'  ## name alias 

movingarrow <- 600


## erase spikes -> smoothing list of raw values -> to get average  of low and high values
span <- 0.1
fit <- loess(Raw.Nanometer ~ Lateral.um, degree=1, span = span, data=alldata3[[scandata]])
smoothline = fit$fitted
p <- abs(c(smoothline[-1], smoothline[[length(smoothline)]])-smoothline)
filter <- which(p > 0.25)
dely <- alldata3[[scandata]]$Raw.Nanometer[-c(filter)]
lowmean <- mean(dely[dely < 200])
highmean <- mean(dely[dely > 200])
arrowlabel <- paste("height:", as.character(round(highmean - lowmean)), sep = " ")

## plot and print to img the scan Data 
titel <- aliasscandata
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott7 <- ggplot(alldata3[[scandata]]) + 
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
print(plott7)
dev.off()


## plot and print to img the scan Data with arrow and mean lines
titel <- paste(aliasscandata, "arrowline", sep = " ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott8 <- ggplot(alldata3[[scandata]]) + 
  geom_point(aes(Lateral.um, Raw.Nanometer)) +
  geom_line(aes(Lateral.um, Raw.Nanometer)) +
  #  geom_line(aes(Lateral.um, smoothline), color="red") +
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
print(plott8)
dev.off()
