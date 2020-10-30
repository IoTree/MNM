library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

folder <- 'C:\\Users\\Gosmo\\OneDrive\\Documents\\Studium-LED-PC\\3. Semseter\\Praktikum Private\\Schindler\\Berechungen\\processed_data_for_calc\\Reflektrometer' # put your folder desitination hier where all the xlsx from lab class are stored
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.csv", full.names = T)


# saving all data form folder in one data.frame
alldata4 <- c()
for (n in files) {
  tblname = substr(basename(n),1,nchar(basename(n))-4)
  alldata4[[tblname]] <- read.csv(n, header = TRUE)
}

measdata <- 'measdata'  ## name of file without .csv
aliasmeasdata <- 'Reflektrometer'  ## name alias 



## plot and print to img the scan Data 
titel <- aliasmeasdata
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott9 <- ggplot(alldata4[[measdata]]) + 
  geom_point(aes(x, y)) +
  geom_line(aes(x, y)) +
  ylab("Reflectivity [%/100]") + xlab("Wavelenght [nm]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel)
print(plott9)
dev.off()