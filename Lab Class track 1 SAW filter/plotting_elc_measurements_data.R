# importend note: for a good fuction of this programm please add a "p" to the end of the Phase files!!!
# also do use ".csv" not ".CSV"
# use is on own risk
# comperight @ Sebastian Stadler

library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
options(digits = 15)

folder <- '****' #folder where all csv files are saved. for ex.: "C:/Users/Dummy/folder1/folder2/allfiles"
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.csv", full.names = T)

for (n in files) {
  file <- n
  tbl <- sapply(file, read.csv, simplify=FALSE, header = TRUE, skip = 2, sep = ",")
  dta <- tbl[[1]]
  x <- dta[1]/10^6
  y <- dta[2]
  x2 <- data.frame(x,y)
  colnames(x2) <- c("freq", "PdBm")
  
  titel <- substring(file, folders_len+2)
  titel <- substr(titel, 1, nchar(titel)-4)
  if(substr(titel, nchar(titel), nchar(titel)) == "p"){
    pdf_titel <- paste(titel, ".png", sep = "")
    png(pdf_titel, width =1600, height = 1200) 
    plott <- ggplot(x2, aes(freq, PdBm)) 
    plott <- plott + geom_point(color='darkblue') 
    plott <- plott +  xlab("Frequency [MHz]") + ylab("deg[°]") 
    plott <- plott + theme_light()
    plott <- plott + theme(
      axis.text=element_text(size=25), 
      axis.title=element_text(size=28,face="bold"),
      plot.title =element_text(size=28),
                           )
    plott <- plott + theme(panel.grid.minor = element_line(size = 1)) +
      scale_x_continuous(breaks = seq(0, 200, 0.2)) +
      scale_y_continuous(breaks = seq(-180, 180, 20))
    plott <- plott +ggtitle(titel)
    print(plott)
    dev.off()
  }
  else{
    pdf_titel <- paste(titel, ".png", sep = "")
    png(pdf_titel, width =1600, height = 1200) 
    plott <- ggplot(x2, aes(freq, PdBm)) 
    plott <- plott + geom_line(color='darkblue', size = 1) 
    plott <- plott +  xlab("Frequency [MHz]") + ylab("P[dBm]") 
    plott <- plott + theme_light()
    plott <- plott + theme( 
                           axis.text=element_text(size=25), 
                           axis.title=element_text(size=28,face="bold"),
                           plot.title =element_text(size=28),
                           )
    plott <- plott + theme(panel.grid.minor = element_line(size = 1)) +
                             scale_x_continuous(breaks = seq(0, 200, 5)) +
      scale_y_continuous(breaks = seq(-100, 0, 5))
    plott <- plott + expand_limits(y = 0)
    plott <- plott +ggtitle(titel)
    print(plott)
    dev.off()
  }
}
