library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

folder <- 'C:\\ <---- path to measeruemtn data ----> \\measuerment data' # put your folder desitination hier where all the xlsx from lab class are stored
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.csv", full.names = T)


# saving all data form folder in one data.frame
alldata4 <- c()
for (n in files) {
  tblname = substr(basename(n),1,nchar(basename(n))-4)
  alldata4[[tblname]] <- read.csv(n, header = TRUE)
}

measdata <- '3_Hexylthiophen_monomer_absorption'  ## name of file without .csv
aliasmeasdata <- 'absorbance_integral_monomer'  ## name alias 

speedlight <- 299792458000000000 # nm / s

alldata4[[measdata]][["y"]] <- log10(1/(1-alldata4[[measdata]][["y"]]))
alldata4[[measdata]][["x"]] <- round((speedlight/(alldata4[[measdata]][["x"]]))/1000000000000)

max <- alldata4[[measdata]][which.max(alldata4[[measdata]][["y"]]),]
xmax <- max[["x"]]
halfmax <- max[["y"]]/2
x1 <- alldata4[[measdata]][["x"]][alldata4[[measdata]][["x"]] < xmax][which.min(abs(alldata4[[measdata]][["y"]][alldata4[[measdata]][["x"]] < xmax]-max(alldata4[[measdata]][["y"]])/2))]
x2 <- alldata4[[measdata]][["x"]][alldata4[[measdata]][["x"]] > xmax][which.min(abs(alldata4[[measdata]][["y"]][alldata4[[measdata]][["x"]] > xmax]-max(alldata4[[measdata]][["y"]])/2))]
FWHM <- round(abs((speedlight/x1)-(speedlight/x2))/1000000000000, 2)

rightsecond <- 0
leftsecond <- 0
if (x1 > x2) {
  rightsecond <- alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x1)-1,]
  leftsecond <- alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x2)+1,]
  slopeleft <- leftsecond[["y"]] - alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x2),][["y"]]
  sloperight <- alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x1),][["y"]] - rightsecond[["y"]]
}else{
  leftsecond <- alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x1)+1,]
  rightsecond <- alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x2)-1,]
  slopeleft <- leftsecond[["y"]] - alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x1),][["y"]]
  sloperight <- alldata4[[measdata]][which(alldata4[[measdata]][["x"]]==x2),][["y"]] - rightsecond[["y"]]
}



leftline <- rev(seq(0, halfmax , by=abs(slopeleft)))
rightline <- rev(seq(0, halfmax , by=abs(sloperight)))

datacopy <- alldata4[[measdata]]

n <- 0
for (i in leftline) {
  datacopy[as.numeric(rownames(leftsecond))+n,][["y"]] <- i
  n <- n+1
}

datacopy[(as.numeric(rownames(leftsecond))+1+n-1):length(datacopy[["y"]]),][["y"]] <-0

n <- 0
for (i in rightline) {
  datacopy[as.numeric(rownames(rightsecond))-n,][["y"]] <- i
  n <- n+1
}

datacopy[(as.numeric(rownames(rightsecond))-1-n+1):0,][["y"]] <- 0

ODsum <- 0
for (i in seq(length(datacopy[["y"]]), 1)) {
  j <- i+1
  if (j > length(datacopy[["y"]])) {
    j <- i - 1
  }
  ODsum <- ODsum + (datacopy[["y"]][[i]]*(datacopy[["x"]][[i]] - datacopy[["x"]][[j]]))
  print(ODsum)
}

ODsum <- round(ODsum, 2)

## plot and print to img the scan Data 
titel <- aliasmeasdata
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width = 800, height = 600)
plott9 <- ggplot(alldata4[[measdata]]) + 
  geom_line(aes(datacopy[["x"]], datacopy[["y"]]), colour = "green", size = 2) +
  geom_point(aes(x, y)) +
  geom_point(aes(x1, halfmax), colour = "red", size = 3)+ 
  geom_point(aes(x2, halfmax), colour = "red", size = 3)+ 
  geom_point(aes(max[["x"]], max[["y"]]), colour = "blue", size = 3)+ 
  geom_line(aes(x, y)) +
  ylab("OD=log(I0/I)") + xlab("Frequency [THz]") +
  theme_light() +
  theme(
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
  ) +
  theme(panel.grid.minor = element_line(size = 1)) +
  annotate("text", x = x1-45, y = halfmax, label = paste(x1, " THz", sep=""), size = 5) +
  annotate("text", x = x2+45, y = halfmax, label = paste(x2, " THz", sep=""), size = 5) +
  annotate("text", x = x1-120, y = 0.2, label = paste("sum(ODdv)= ", ODsum, " THz", sep=""), size = 5) +
  ggtitle(titel)
print(plott9)
dev.off()
