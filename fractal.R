# Random fractals
dir.create(mainDir <- "/atlas_scratch/mfleonawicz/projects/fractals", showWarnings=FALSE)
setwd(mainDir)
dir.create(file.path(mainDir, "images/stills"), recursive=TRUE, showWarnings=FALSE)
outDir <- "images"
library(parallel)

plotFractals <- function(g, outDir, n.lim=1000, n=100000, color="blue", opacity=99, m.list=NULL, prewhiten=FALSE, postwhiten=FALSE, postwhiten.opacity=25, stills=FALSE, n.gap=1000){
  if(is.null(m.list))	m <- array(sample(seq(-2.2/3, 1.9/3, by=1/6), 24, replace=TRUE), dim=c(4,6))
  if(is.list(m.list)) len <- length(m.list) else len <- 1
  col.vec <- paste0(colorRampPalette(color)(len), opacity)
  x <- y <- x1 <- y1 <- 0
  last.x <- last.y <- c()
  # Find plot limits
  for(k in 1:len){
    if(!is.null(m.list)) m <- m.list[[k]]
    for(i in 1:n.lim){
      x2 <- x
      i <- sample(1:4, 1, prob=c(0.7,0.1,0.1,0.1))
      x <- m[i,1]*x + m[i,2]*y + m[i,5]
      y <- m[i,3]*x + m[i,4]*y + m[i,6]
      x1 <- range(x1, x)
      y1 <- range(y1, y)
    }
    last.x <- c(last.x, x)
    last.y <- c(last.y, y)
    print(k)
  }

  .addPoints <- function(clr, stills=FALSE, n.gap=1000){
    if(length(clr)==1) clr <- rep(clr, len)
    for(k in 1:len){
      if(!is.null(m.list)) m <- m.list[[k]]
      x <- last.x[k]
      y <- last.y[k]
      x.hold <- y.hold <- rep(NA, n)
      for(j in 1:n){
        x3=x
        i=sample(1:4,1,prob=c(0.3,0.2,0.3,0.2))
        x=m[i,1]*x + m[i,2]*y + m[i,5]
        y=m[i,3]*x3 + m[i,4]*y + m[i,6]
        x.hold[j] <- x
        y.hold[j] <- y
        #points(x,y,pch=".",cex=1, col=clr[k])
        #points(x,y,pch=".",cex=1, col=col.vec[j])
      }
      if(stills){
        n.seq <- c(0, seq(n.gap, n, by=n.gap))
        nc <- nchar(1:length(n.seq))
        mc <- max(nc)
        padZeros <- function(x, n, m) if(n[x]==m) "" else paste0(rep(0, m-n[x]), collapse="")
        n.seq.lab <- paste0(sapply(1:length(n.seq), padZeros, n=nc, m=mc), 1:length(n.seq))
        for(j in 2:length(n.seq)){
          if(is.null(m.list)) png(paste0(outDir, "/stills/RandFract", g, "_frame", n.seq.lab[j], ".png"), width=3200, height=2400, res=300, bg = "black")
          if(!is.null(m.list)) png(paste0(outDir, "/stills/RandFractGroup", g, "_frame", n.seq.lab[j], ".png"), width=3200, height=2400, res=300, bg = "black")
          par(bg="black", mar=c(0,0,0,0))
          plot(0,0,xlim=range(x1),ylim=range(y1),col="black")
          #ind <- (n.seq[j-1]+1):n.seq[j]
          ind <- 1:n.seq[j]
          if(prewhiten) points(x.hold[ind], y.hold[ind], pch=".",cex=1, col="white")
          points(x.hold[ind], y.hold[ind], pch=".",cex=1, col=clr[k])
          if(postwhiten) points(x.hold[ind], y.hold[ind], pch=".",cex=1, col=paste0("#FFFFFF", postwhiten.opacity))
          dev.off()
        }
      } else {
        points(x.hold,y.hold,pch=".",cex=1, col=clr[k])
      }
      print(k)
    }
  }

  if(stills){
    .addPoints(clr=col.vec, stills=stills, n.gap=n.gap)
  } else {
    if(is.null(m.list)) png(paste0(outDir, "/RandFract", g, ".png"), width=3200, height=2400, res=300, bg="black")
    if(!is.null(m.list)) png(paste0(outDir, "/RandFractGroup", g, ".png"), width=3200, height=2400, res=300, bg="black")
    par(bg="black", mar=c(0,0,0,0))
    plot(0, 0, xlim=range(x1), ylim=range(y1), col="black")
    if(prewhiten) .addPoints(clr="white")
    .addPoints(clr=col.vec)
    if(postwhiten) .addPoints(clr=paste0("#FFFFFF", postwhiten.opacity))
    dev.off()
  }
  if(is.null(m.list)) return(m) else return(m.list)
}

set.seed(147)
out <- mclapply(1:128, plotFractals, outDir=outDir, n.lim=1000000, n=4000000, mc.cores=32)

keep <- c(1:3, 5, 8)
plotFractals(1, outDir=outDir, color="purple", n.lim=1000000, n=4000000, m.list=out[keep], prewhiten=FALSE, postwhiten=TRUE)

keep <- c(18, 24, 25, 55)
plotFractals(2, outDir=outDir, color=c("darkorange", "coral", "chocolate", "goldenrod"), n.lim=1000000, n=4000000, m.list=out[keep], prewhiten=FALSE, postwhiten=TRUE)

keep <- c(18)
plotFractals(3, outDir=outDir, color=c("green"), n.lim=1000000, n=4000000, m.list=out[keep], prewhiten=FALSE, postwhiten=TRUE)

keep <- c(26,29,30)
for(p in 1:length(keep)) plotFractals(p+3, outDir=outDir, color=c("orangered", "magenta", "gold")[p], n.lim=1000000, n=4000000, m.list=out[keep[p]], prewhiten=FALSE, postwhiten=TRUE, opacity=50, postwhiten.opacity=50)
n <- length(keep)

save.image("data.RData")
