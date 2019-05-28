sapply(dir("Code/", pattern = "0[0-9]*", full.names = T), source)
trials <- trialFiles()
prey.bead.list <- loadPreyTrajectories()

# correlate maximum velocity with:
  # prey handling time
  # path complexity during handling

# look specifically at X translation:
  # where relative to NC does prey handling begin? how consistent is it?
  # does handling time relate to lateral displacement of prey when handling begins?

par(mfrow=c(4, 1), mar=c(2, 4, 2, 2))
n <- 1
for (i in 1:nrow(trials)) {
  trial.name <- trials$trial[i]
  plotPreyXYZ(trial.name, prey.bead.list, xlab="")
  
  vel <- plotPreyVelocity(trial.name, prey.bead.list,
                   plotting=FALSE, return.vec=TRUE)
  plot(abs(prey.bead.list[[i]][,n]), vel, col="orange", pch=20,
       ylab="Velocity", xlab="Z coordinate",
       panel.first=abline(h=0, lwd=1, col="darkgrey"),
       type='l')
  
  acc <- plotPreyAcceleration(trial.name, prey.bead.list,
                       plotting=FALSE, return.vec=TRUE)
  plot(abs(prey.bead.list[[i]][,n]), c(NA, acc), col="#b665ce", pch=20,
       ylab="Acceleration", xlab="Z coordinate",
       panel.first=abline(h=0, lwd=1, col="darkgrey"), type='l')
  
  delta <- preyPathComplexity(trial.name, prey.bead.list,
                              w=5, sliding.window = F, plotting=FALSE)
  plot(delta, col="blue", pch=20, type='l',
       ylab="Path complexity", xlab="Z coordinate",
       panel.first=abline(h=1, lwd=1, col="darkgrey"))
  
  colordistance:::pause()
  
}











# get path complexities and velocities










deltas <- vector("list", length=nrow(trials)) 
velocities <- vector("list", length=nrow(trials)) 

for (i in 1:nrow(trials)) {
  velocities[[i]] <- plotPreyVelocity(trials$trial[i], prey.bead.list, return.vec = TRUE)
  deltas[[i]] <- preyPathComplexity(trials$trial[i], prey.bead.list, 
                                    xlab="Time (s)")
}

# get max velocities
max.velocities <- unlist(lapply(velocities, max))

prey.handling.times <- prey.handling.time[-which(is.na(prey.handling.time))]/300 # frames/(300 frames/sec) = seconds

trial <- trials[22,]
w <- 10
delta <- preyPathComplexity(trial$trial, prey.bead.list, w=w)


find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
peaks <- find_peaks(delta, m=w)



for (i in 1:nrow(trials)) {
  prey.bead <- prey.bead.list[[i]]
  plot(prey.bead$x, deltas[[i]], pch=19)
}


plot(delta, type='l')
abline(h=mean(delta))
abline(v=peaks[1], lty=2, col="red")


velocities <- plotPreyVelocity(trial.name, prey.bead.list, plotting=F, return.velocities=T)

windows <- seq(5, 50, by=5)

par(mfrow=c(3,1), mar=c(4, 5, 2, 2) + 0.1)
plotPreyXYZ(trial.name, prey.bead.list, xlab="")
plotPreyVelocity(trial.name, prey.bead.list, col="goldenrod", xlab="", plotting = T)
plotPreyAcceleration(trial.name, prey.bead.list, col="#b665ce", xlab="Time (s)")

delta <- preyPathComplexity(trial.name, prey.bead.list, col="orchid", w=5, sliding.window = T)
plot(diff(delta), type='l')


par(mfrow=c(2, 1), mar=c(4, 5, 2, 2))
for (w in windows) {
  delta <- preyPathComplexity(trial.name, prey.bead.list, w=w, sliding.window = T)
  plot(delta, type='l', main=paste("Window size:", w), ylab="Complexity")
  plot(diff(delta), type='l', ylab="Change in complexity", 
       panel.first=abline(h=0, lwd=1, col="darkgrey"))
}

plot(diff(delta), type='l', 
     panel.first=c(abline(h=0, lty=2, col="red"), 
                   abline(h=2*sd(diff(delta)), col="blue"),
                   abline(h=-2*sd(diff(delta)), col="blue")))

which(diff(delta) > 2*sd(diff(delta)))

delta <- preyPathComplexity(trial.name, prey.bead.list, w=5, sliding.window = T)
plot(log(delta), x=log(velocities[1:length(delta)]))
plot(delta, type='l')
abline(h=mean(delta), col="red", lty=2, lwd=2)
abline(h=(mean(delta)+2*sd(delta)), col="blue", lty=2)
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
find_peaks(delta, m=10)


xyz.col <- c("tomato", "#66e699", "cornflowerblue")

# plot trials:
  # 3D prey bead trajectory
  # XYZ coordinates in NC space
  # prey bead velocity over time
    # = distances between points/time elapsed between points
  # prey bead acceleration over time

#prey.bead.list[names(prey.bead.list)==trials$trial[1]]
prey.bead.plots <- function(prey.bead, start.frame=NA, esophagus.frame=NA, main) {
  par(mfrow=c(3, 1), mar=c(4, 5, 2, 2) + 0.1)

  if (!is.na(esophagus.frame)) {
    esophagus <- prey.bead[esophagus.frame, 4]
  } else {
    esophagus <- NULL
  }


  # XYZ coordinates in NC space
  plot(y=prey.bead$x, x=prey.bead$Seconds,
       type='l', lwd=2, col=xyz.col[1],
       ylab="Prey position (mm)", xlab="", main=main,
       ylim=c(-40, 80))
  points(y=prey.bead$y, x=prey.bead$Seconds, type='l', lwd=2, col=xyz.col[2])
  points(y=prey.bead$z, x=prey.bead$Seconds, type='l', lwd=2, col=xyz.col[3])
  abline(v=esophagus, lty=2, lwd=2)

  legend("topleft", legend = LETTERS[24:26], fill=xyz.col, bty="n")

  # prey bead velocity over time
  # = distances between points/time elapsed between points
  # come back and make this faster later
  d <- c()
  for (i in 2:nrow(prey.bead)) {
    d <- c(d, sqrt(sum(prey.bead[i-1, 1:3]-prey.bead[i, 1:3])^2))
  }
  plot(y=d*180, x=prey.bead$Seconds[-1], type='l', lwd=1,
       xlab="", ylab=expression("Prey velocity " (mm ~ sec^{-1})),
       col="goldenrod",
       ylim=c(-10, 200))
  abline(v=esophagus, lty=2, lwd=2)
  abline(h=0, lty=2, col="darkgrey")

  # prey bead acceleration over time
  plot(y=diff(d*180), x=prey.bead$Seconds[-c(1:2)], type='l', lwd=1,
       xlab="Time (s)", ylab=expression("Prey acceleration " (mm ~ sec^{-2})),
       col="#b665ce",
       ylim=c(-100, 100))
  abline(v=esophagus, lty=2, lwd=2)
  abline(h=0, lty=2, col="darkgrey")
}

for (i in 1:nrow(trials)) {
  trial <- trials[i, ]

  # load the prey bead trajectory
  prey.bead <- read.csv(paste(prey.dir, trial$trial, ".csv", sep=""))

  # add seconds column (1 frame * 1 sec/300 frames = 1/300 second per row)
  prey.bead <- prey.bead[,1:3]
  colnames(prey.bead) <- letters[24:26]
  prey.bead$Seconds <- (c(1:nrow(prey.bead))-1)/trial$frame.rate

  prey.bead.plots(prey.bead, start.frame=trial$start.frame, esophagus.frame=trial$esophagus.frame, main=trial$trial)
  #colordistance:::pause()
}

# load the prey bead trajectory
prey.bead <- read.csv(paste(prey.dir, trials$trial[22], ".csv", sep=""))
prey.bead <- prey.bead[-(1:trials$start.frame[22]), ]

# add seconds column (1 frame * 1 sec/300 frames = 1/300 second per row)
prey.bead <- prey.bead[,1:3]
colnames(prey.bead) <- letters[24:26]
prey.bead$Seconds <- (c(1:nrow(prey.bead))-1)/trial$frame.rate

# path complexity:
# for a window size of 'w' frames

w <- 50
par(mfrow=c(5, 1))
window.sizes <- seq(100, 10, by=-10)
plot.colors <- colorRampPalette(colors=c("cornflowerblue", "tomato"))(length(window.sizes))

# slide window
for (w in window.sizes) {
  r <- c()
  for (i in 1:(nrow(prey.bead)-w)) {
    bin <- prey.bead[i:(i+w-1), ]

    # find the distance between the start and end points
    sqrt(sum(bin[1, 1:3]-tail(bin[ , 1:3], 1))^2)

    # find the sum of distances between every subsequent pair of points in the sequence
    dmat <- as.matrix(dist(bin[, 1:3]))

    # overall distance
    i <- c(1:(w-1))
    j <- c(1:(w-1))+1
    v <- sum(dmat[cbind(i, j)])
    r <- c(r, v/dmat[1, w]) # actual distance traveled/minimum possible distance

  }

  plot(r, type='l',
       xlab=paste("Window size:", w),
       ylab="Path length/total distance",
       col=match(w, window.sizes))
  abline(v=398, lty=2)
  # if (match(w, window.sizes)==1) {
  #   plot(r, type='l',
  #        xlab=paste("Window size:", w),
  #        ylab="Path length/total distance",
  #        col=match(w, window.sizes))
  # } else {
  #   points(r, type='l',
  #          col=match(w, window.sizes))
  # }
  #
}

bins <- seq(1, nrow(prey.bead), by=w)

r <- c()
# sequential non-overlapping bins
for (i in 1:(length(bins)-1)) {
  bin <- prey.bead[bins[i]:bins[i+1], ]

  # find the distance between the start and end points
  sqrt(sum(bin[1, 1:3]-tail(bin[ , 1:3], 1))^2)

  # find the sum of distances between every subsequent pair of points in the sequence
  dmat <- as.matrix(dist(bin[, 1:3]))

  # overall distance
  i <- c(1:(w-1))
  j <- c(1:(w-1))+1
  v <- sum(dmat[cbind(i, j)])
  r <- c(r, v/dmat[1, w]) # actual distance traveled/minimum possible distance

}
plot(r, type='l')


# ratio of total distance traveled/minimum distance traveled

# maybe express prey "progress" as % length of NC traveled?
end.x <- tail(prey.bead, 1)[1]
esophagus.frame <- trial$esophagus.frame - trial$start.frame
NC.length <- prey.bead[esophagus.frame, 1] -  prey.bead[1, 1] # x axis ~ AP axis of neurocranium

# % progress as a function of time:
NC.pct <- c()
for (i in 1:nrow(prey.bead)) {
  NC.pct <- c(NC.pct, (prey.bead[esophagus.frame, 1]-prey.bead[i, 1])/NC.length)
}
plot(NC.pct, type='l')
# do path complexity on each of x, y, z axes?

# handling time vs. peak velocity?
# vs. prey type?
