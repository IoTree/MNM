
library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

folder <- 'C:\\Users\\Gosmo\\OneDrive\\Documents\\Studium-LED-PC\\3. Semseter\\Praktikum Private\\Schindler\\Berechungen\\processed_data_for_calc\\Ellipsometer' # put your folder desitination hier where all the xlsx from lab class are stored
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.csv", full.names = T)


# saving all data form folder in one data.frame
alldata1 <- c()
for (n in files) {
  tblname = substr(basename(n),1,nchar(basename(n))-4)
  alldata1[[tblname]] <- read.csv(n, header = TRUE)
}

namemessung1 <- '110mA_measdata'  ## name of file without .csv
namemessung2 <- '160mA_measdata'  ## name of file without .csv
aliasmessung1 <- 'Ellipsometrie 110 mA, 5,11 s'  ## name alias for messung 1
aliasmessung2 <- 'Ellipsometrie 160 mA, 5,00 s'  ## name alias for messung 2

## points to delete 
dellast <- 4
delfirst <- 5

## plot and print to img only "d" and "n" add plotting for other values if needed!
## measuerment 1 
## thickness d
titel <- paste (aliasmessung1, "thickness", sep = ";   ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott1 <- ggplot(alldata1[[namemessung1]][delfirst:(length(alldata1[[namemessung1]][[1]])-dellast),]) + 
  geom_point(aes(X, d)) +
  geom_line(aes(X, d)) +
  ylab("Thickness d [nm]") + xlab("Travel path x [mm]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
    ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel)
print(plott1)
dev.off()

## refractive index n
titel <- paste (aliasmessung1, "refractive index", sep = ";   ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott2 <- ggplot(alldata1[[namemessung1]][delfirst:(length(alldata1[[namemessung1]][[1]])-dellast),]) + 
  geom_point(aes(X, n)) +
  geom_line(aes(X, n)) +
  ylab("refractive index n") + xlab("Travel path x [mm]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel)
print(plott2)
dev.off()

## measurement 2
## thickness d
titel <- paste (aliasmessung2, "thickness", sep = ";   ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott1 <- ggplot(alldata1[[namemessung2]][delfirst:(length(alldata1[[namemessung2]][[1]])-dellast),]) + 
  geom_point(aes(X, d)) +
  geom_line(aes(X, d)) +
  ylab("Thickness d [nm]") + xlab("Travel path x [mm]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel)
print(plott1)
dev.off()

## refractive index n
titel <- paste (aliasmessung2, "refractive index", sep = ";   ")
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott4 <- ggplot(alldata1[[namemessung2]][delfirst:(length(alldata1[[namemessung2]][[1]])-dellast),]) + 
  geom_point(aes(X, n)) +
  geom_line(aes(X, n)) +
  ylab("refractive index n") + xlab("Travel path x [mm]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  ggtitle(titel)
print(plott4)
dev.off()
