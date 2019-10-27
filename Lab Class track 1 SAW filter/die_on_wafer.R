#install.packages("rlist")
#install.packages("plotrix")
library(rlist)
library("plotrix")

#input
wafersize <- 100 #mm
boundery <- 5 #mm
diex <- 3.9 #mm
diey <- 2.9 #mm
accuracy <- 0.1 #mm
id <- "D25"

#preperation
wafer_r <- (100 - boundery*2)/2
gloabllist <- list()
centerpoint <- list()

#making the grid
#max space in x and y
maxx <- 100 + diex*4
maxy <- 100 + diey*4
#max points in x and y
pointsx <- round(maxx / diex)
if (pointsx %% 2 != 0) {
  pointsx <- pointsx +1
}
pointsy <- round(maxy / diey)
if (pointsy %% 2 != 0) {
  pointsy <- pointsy +1
}
pointsxtimes <- seq(-pointsx/2, pointsx/2)
pointsytimes <- seq(-pointsy/2, pointsy/2)
# making vector table
for (m in seq(1, pointsx)) {
  rowlist <- list()
  for (n in seq(1, pointsy)) {
    rowlist[[n]] <- list(diex*pointsxtimes[[m]], pointsytimes[[n]]*diey)
  }
  gloabllist[[m]] <- rowlist
}

# Circle center point variations
centerpoint[[1]] <- seq(0, diex, accuracy)
centerpoint[[2]] <- seq(0, diey, accuracy)

## processing ##
##point in squer in circal check  #later
#ahalf <- (wafer_r/sqrt(2))/2     #later
# copy of gloaballist
variation <- list()
variation[[1]] <- 0
for (i in seq(1, length(centerpoint[[2]]))) {
  for (k in seq(1, length(centerpoint[[1]]))) {
    nanlist <- gloabllist
    counter <- 0
    vectorsx <- c()
    vectorsy <- c()
    for (n in nanlist) {
      for (m in n) {
        rn <- sqrt(((m[[1]]-centerpoint[[1]][[k]])^2)+((m[[2]]-centerpoint[[2]][[i]])^2))
        if (rn < wafer_r){
          counter <- counter + 1
          vectorsx <- list.append(vectorsx, m[[1]])
          vectorsy <- list.append(vectorsy, m[[2]])
        }
      }
    }
    if (counter > variation[[1]]){
      variation[[1]] <- counter
      variation[[2]] <- k
      variation[[3]] <- i
      finalpointsx <- vectorsx
      finalpointsy <- vectorsy
    }
  }
}

#number of good die calculation
dienum <- as.data.frame(table(finalpointsx))
dienum[[2]] <- dienum[[2]] -1
summi <- 0
m <- 0
for (n in dienum[[2]]) {
  summi <- summi + n
  if (n < m) {
    summi <- summi-m
    summi <- summi+n
  }
  m <- n
}
summi <- summi - dienum[[2]][[length(dienum[[2]])]]

# display results
totalpoints <- paste(c("included points:", variation[[1]]), collapse = " ")
xshift <- paste(c("shift in x:", centerpoint[[1]][[variation[[2]]]]), collapse = " ")
yshift <- paste(c("shift in y:", centerpoint[[1]][[variation[[3]]]]), collapse = " ")
gooddie <- paste(c("number of good dies:", summi), collapse = " ")


par(pty="s")
plot(finalpointsx, finalpointsy, asp = 1, xlim = c(wafersize/-2-10, wafersize/2+10), pch = 20, main = id, xlab = "x", ylab = "y")
draw.circle(centerpoint[[1]][[variation[[2]]]], centerpoint[[1]][[variation[[3]]]], wafersize/2, border="black", lwd=2)
draw.circle(centerpoint[[1]][[variation[[2]]]], centerpoint[[1]][[variation[[3]]]], wafersize/2 - boundery, border="purple", lwd=2)
abline(v = seq(-diex*100, diex*100, diex), h = seq(-diey*100, diey*100, diey), col = "lightgray", lty = 3)
legend("topleft", bg="transparent", bty = "n", c(gooddie, totalpoints, xshift, yshift))
points(centerpoint[[1]][[variation[[2]]]], centerpoint[[1]][[variation[[3]]]], pch = 10)
