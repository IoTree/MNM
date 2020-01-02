#NOTE: for a well-functioning program, put all xlsx files in on folder and specific it in the programm. 
#   Also if data is missing in the xlsx pleas delete this data for this specific design. 
#   Also define the uncertainties!!!

# use is on own risk
# comperight @ Sebastian Stadler



library(readxl)
library(magrittr)
library(dplyr)
library(anytime)
library(ggplot2)
library(gridExtra)

folder <- '***Your folder***' # put your folder desitination hier where all the xlsx from lab class are stored
folders_len <- nchar(folder)
files <- list.files(path = folder, pattern = "*.xlsx", full.names = T)


# saving all data form folder in one data.frame
alldata <- c()
i <- 1
for (n in files) {
  file <- n
  tbl <- sapply(file, read_xlsx, simplify=FALSE)
  dta <- tbl[[1]]
  d1 <- as.data.frame.matrix(dta)
  alldata[[i]] <- d1
  i <- i + 1
}


##6. a)calc mean Velocity from all designs(xlsx)
overallv <- c()
for (n in alldata) {
  i <- 3
  for (m in c(1:5)) {
    if (is.numeric(n[[i]][[1]][[9]][[1]])){
      overallv <- c(overallv, n[[i]][[1]][[9]][[1]])
    }
    i <- i + 1
  }
}
meanv <- sum(overallv)/length(overallv)

##6. b) calc uncertanty for mean Velocity
#uncrtanty for center frequency
name_all <- c()
center_freq_error <- c()
for (n in alldata) {
  name_all <- c(name_all, substr(colnames(n[[3]]), start = 1, stop = 2))
  i_list_1 <- n[[8]][[1]][[5]][[1]]
  i_list_2 <- n[[8]][[1]][[6]][[1]]
  # calc gausian error propagation for supptration
  center_freq_error <- c(center_freq_error, round(sqrt(((0.5*i_list_1)^2)+((0.5*i_list_2)^2)), digits = 1))
}


##7. calc and graph with 4 diffrened lamda
#mean frequency for each design
name <- c()
x <- c()
y <- c()
design_number_each <- c()
for (n in alldata) {
  i <- 3
  j <- 0
  i_list <- c()
  for (m in c(1:5)) {
    if (is.numeric(n[[i]][[1]][[7]][[1]])){
      i_list <- c(i_list, n[[i]][[1]][[7]][[1]])
      j <- j + 1
    }
    i <- i + 1
  }
  name <- c(name, substr(colnames(n[[3]]), start = 1, stop = 2))
  y <- c(y, round(sum(i_list)/length(i_list), digits = 1))
  x <- c(x, round(n[[3]][[1]][[1]], digits = 1))
  design_number_each <- c(design_number_each, j)
}
#uncertanty for each velocity per wavelength diffrenc
v_error_for_each <- center_freq_error*8*x

#uncertanty for mean velocity
v_error_overall <- (1/sum(design_number_each))*sqrt(sum(rep(v_error_for_each, design_number_each)^2))

#plot and print
meanfreq <- data.frame(name = c(name), x = c(x), y = c(y))
plotdata <- data.frame(1/(8*x),y)
lamda <- x
colnames(plotdata) <- c("x", "y")
titel <- "f0_1overLamda"
png_titel <- paste(titel, ".png", sep = "")
png(png_titel, width =800, height = 600)
plott <- ggplot(plotdata, aes(x, y))
plott <- plott + geom_point(color='darkblue', shape = 8, size = 2.5) + 
  geom_text(aes(label=name), vjust=-1, size = 6) + 
  geom_text(aes(label=paste("lam.:", round(lamda*8, digits = 1), sep = " ")), vjust=1.5, size = 6) +
  geom_errorbar(aes(x = x , ymin=y-center_freq_error , ymax=y+center_freq_error), 
                width = 0.0005, 
                position=position_dodge(0.05)
                )
plott <- plott +  xlab("1/wavelength [1/um]") + ylab("center frequency f0 [MHz]") 
plott <- plott + theme_light()
plott <- plott + geom_smooth(method='lm', 
                             #se = FALSE
                             )
plott <- plott + theme(
  axis.text=element_text(size=14), 
  axis.title=element_text(size=16,face="bold"),
  plot.title =element_text(size=16),
)
plott <- plott + theme(panel.grid.minor = element_line(size = 1))
plott <- plott + ggtitle(titel)
print(plott)
dev.off()


error_zero_bandwidth_all <- c()
error_theo_bandwidth_all <- c()
error_phase_bandwidth_all <- c()
error_spacing_all <- c()
overall_name <- c()
j <- 1
for (n in alldata) {
  i <- 3
  # collecting data from alldata frame
  theofbandwidth <- c()
  name <- c()
  freq_f0 <- c()
  bandwid <- c()
  N_fingers <- c()
  spacing <- c()
  spacing_design <- c()
  finger_width <- c()
  phase_bandwidth <- c()
  for (m in c(1:5)) {
    if (is.numeric(n[[i]][[1]][[7]][[1]])){
      freq_f0 <- c(freq_f0, round(n[[i]][[1]][[7]][[1]], digits = 1))
      name <- c(name, substr(colnames(n[[i]]), start = 1, stop = 3))
      bandwid <- c(bandwid, round(n[[i]][[1]][[11]], digits = 1))
      N_fingers <- c(N_fingers, round(n[[i]][[1]][[2]], digits = 1))
      spacing <- c(spacing, round(n[[i]][[1]][[21]], digits = 1))
      spacing_design <- c(spacing_design, round(n[[i]][[1]][[3]], digits = 1))
      finger_width <- c(finger_width, round(n[[i]][[1]][[1]], digits = 1))
      phase_bandwidth <- c(phase_bandwidth, round(n[[i]][[1]][[18]], digits = 1))
    }
    i <- i + 1
  }
  name2 <- substr(name[[1]], start = 1, stop = 2)
  overall_name <- c(overall_name, name)
  theofbandwidth <- c(theofbandwidth, (4*freq_f0)/N_fingers)
  
  ##9. a) zero-bandwidth and theoretical expected values. (To calculate the theoretical expected zero-point bandwidths, use the center frequency f0 determined under 5
  ## and uncertanty
  i_1 <- n[[8]][[1]][[5]][[1]]
  i_2 <- n[[8]][[1]][[6]][[1]]
  error_zero_bandwidth <- sqrt(i_1^2 + i_2^2)
  
  j_1 <- n[[8]][[1]][[16]][[1]]
  j_2 <- n[[8]][[1]][[17]][[1]]
  error_phase_bandwidth <- sqrt(j_1^2 + j_2^2)
  error_phase_bandwidth_all <- c(error_phase_bandwidth_all, error_phase_bandwidth)
  
  error_theo_bandwidth <- (4*center_freq_error[[j]])/N_fingers
  
  #error_mean_phase_band <- (1/length(phase_bandwidth)*error_phase_bandwidth*sqrt(length(phase_bandwidth)))
  #mean_phase_band <- sum(phase_bandwidth)/length(phase_bandwidth)
  #error_spacing <- sqrt((v_error_overall/meanv)^2 +(error_mean_phase_band/mean_phase_band)^2)*(meanv/mean_phase_band)
  #error_spacing_all <- c(error_spacing_all, error_spacing)
  error_spacing <- sqrt((rep(v_error_overall, length(phase_bandwidth))/rep(meanv, length(phase_bandwidth)))^2 +(error_phase_bandwidth/phase_bandwidth)^2)*(meanv/phase_bandwidth)
  error_spacing_all <- c(error_spacing_all, error_spacing)
  
  error_zero_bandwidth_all <- c(error_zero_bandwidth_all, error_zero_bandwidth)
  error_theo_bandwidth_all <- c(error_theo_bandwidth_all, error_theo_bandwidth)
  
  
  #plot
  df2 <- data.frame(label=rep(c("measured values", "calculated values"), each=length(name)),
                    dose=rep(name,2),
                    len=c(bandwid, theofbandwidth))
  
  titel <- paste("zero_bandwidth_comperison", name2, sep="_")
  png_titel <- paste(titel, ".png", sep = "")
  png(png_titel, width = 800, height = 600)
  
  plott <- ggplot(data=df2, aes(x=dose, y=len, fill=label)) +
    geom_bar(stat="identity",color="black", position=position_dodge())
  plott <- plott + 
    xlab("Design") + 
    ylab("zero-bandwidth [MHz]") 
  plott <- plott + 
    theme_light() +
    scale_fill_brewer(palette="Greens") + 
    geom_text(aes(label=round(len, digits = 1)), vjust=1.6, color="black",position = position_dodge(0.9), size=5) +
    ggtitle(titel) + 
    geom_errorbar(aes(ymin=len-c(rep(error_zero_bandwidth, length(bandwid)), error_theo_bandwidth), 
                      ymax=len+c(rep(error_zero_bandwidth, length(bandwid)), error_theo_bandwidth)), 
                  width = 0.5, 
                  position=position_dodge(0.9),
                  color = "blue"
                  )
  plott <- plott + theme(
    legend.key.size = unit(1, "cm"),
    legend.position="bottom",
    legend.text = element_text(size=16),
    legend.title = element_blank(),
    axis.text=element_text(size=14), 
    axis.title=element_text(size=16,face="bold"),
    plot.title =element_text(size=16),
    panel.grid.minor = element_line(size = 1)
    )
  print(plott)
  dev.off()
  
  
  ##9. b) plot for diffrened number of fingers
  N_fingers2 <- N_fingers[!duplicated(N_fingers)]
  if (length(N_fingers2)>1){
    plotdata <- data.frame(1/N_fingers2, bandwid)
    colnames(plotdata) <- c("x", "y")
    titel <- paste("zero_bandwidth_to_Number", name2, sep="_")
    png_titel <- paste(titel, ".png", sep = "")
    png(png_titel, width = 800, height = 600)
    plott <- ggplot(plotdata, aes(x, y))
    plott <- plott + geom_point(color='darkblue', shape = 16, size = 2.8) + 
      geom_text(aes(label=name), vjust=-1, hjust = - 0.2, size = 6) + 
      xlim(c(plotdata[[1]][[1]]- 0.001, plotdata[[1]][[length(plotdata[[1]])]]+ 0.004)) +
      geom_text(aes(label=paste("N:", N_fingers2, sep = " ")), vjust=1.5, hjust = - 0.2, size = 6) + 
      geom_errorbar(aes(x = x, ymin=y-rep(error_zero_bandwidth, length(bandwid)), 
                        ymax=y+rep(error_zero_bandwidth, length(bandwid))), 
                    width = 0.0005, 
                    #position=position_dodge(0.9),
                    color = "blue",
                    size = 1
      )
    plott <- plott +  xlab("1/Number of Finger [1/N]") + ylab("zero-bandwidth [MHz]") 
    plott <- plott + theme_light()
    plott <- plott + geom_smooth(method='lm')
    plott <- plott + theme(
      axis.text=element_text(size=14), 
      axis.title=element_text(size=16,face="bold"),
      plot.title =element_text(size=16),
    )
    plott <- plott + theme(panel.grid.minor = element_line(size = 1))
    plott <- plott + ggtitle(titel)
    print(plott)
    dev.off()
  }
  
  ##12. plot of diffrend spacing to design only where there are diffrendsis in the design
  spacing_design2 <- spacing_design[!duplicated(spacing_design)]
  if (length(spacing_design2)>1){
    #IDT spacing from middle to middle)
    spacing_design2 <- spacing_design2 + N_fingers*finger_width*4
    #plot
    plotdata <- data.frame(spacing_design2, spacing)
    colnames(plotdata) <- c("x", "y")
    titel <- paste("sol_to_ist_spacing", name2, sep="_")
    png_titel <- paste(titel, ".png", sep = "")
    png(png_titel, width = 800, height = 600)
    plott <- ggplot(plotdata, aes(x, y))
    plott <- plott + geom_point(color='darkblue', shape = 8, size = 2.8) + 
      geom_text(aes(label=name), vjust=-1, size = 6) + 
      expand_limits(x = 0, y = 0) #+ 
      #geom_errorbarh(aes(y = y, 
      #                   xmin=x-error_spacing, 
      #                  xmax=x+error_spacing),
      #               height = 10,
      #)
    plott <- plott +  ylab("spacing by measurements [um]") + xlab("spacing by design [um]") 
    plott <- plott + theme_light()
    plott <- plott + geom_smooth(method='lm', fullrange=TRUE)
    plott <- plott + theme(
      axis.text=element_text(size=14), 
      axis.title=element_text(size=16,face="bold"),
      plot.title =element_text(size=16),
    )
    plott <- plott + theme(panel.grid.minor = element_line(size = 1))
    plott <- plott + ggtitle(titel)
    print(plott)
    dev.off()
  }
  j <- j + 1
}


#make table with all calculated values and save it as an excel sheet


# overallv; overall v error; error zero bandwidth; error phase bandwidth; centra frequency error; spacing error;
print(meanv)
print(v_error_overall)
print(name_all)
print(v_error_for_each)
print(error_zero_bandwidth_all)
print(error_phase_bandwidth_all)
print(overall_name)
print(error_spacing_all)

frame_1 <- round(data.frame(meanv, v_error_overall), digits = 1)
colnames(frame_1) <- c("velocity [m/s]", "velocity error [m/s]")
rownames(frame_1) <- c("over all given designs")
frame_2 <- round(data.frame(v_error_for_each,error_zero_bandwidth_all,error_phase_bandwidth_all), digits = 1)
colnames(frame_2) <- c("velocity error [m/s]", "zero bandwidth error [MHz]", "phase delta error [MHz]")
rownames(frame_2) <- name_all
frame_3 <- round(data.frame(error_spacing_all), digits = 1)
colnames(frame_3) <- c("spacing error in [um]")
rownames(frame_3) <- overall_name

write.csv(frame_1, file = "data1.csv", row.names = TRUE)
write.csv(frame_2, file = "data2.csv", row.names = TRUE)
write.csv(frame_3, file = "data3.csv", row.names = TRUE)

