library(plotly)

# read in trial files/notes
trialFiles <- function() {
  
  # read the big guy
  trials <- read.csv("Notes/Prey_trajectory_notes.csv")
  return(trials)
}

# read all of the prey trajectories into a big list
loadPreyTrajectories <- function() {
  
  # get trial files
  trials <- trialFiles()
  
  # set directory in which to look for prey beads
  prey.dir <- "Data/RMA/3D Points smoothed RT Neurocranium/"
  
  # initalize empty list with one element for each trial
  prey.bead.list <- vector("list", length=nrow(trials))
  names(prey.bead.list) <- trials$trial
  
  # for every trial, read in prey bead trajectories
  for (i in 1:nrow(trials)) {
    
    # read in CSV
    prey.bead <- read.csv(paste(prey.dir, trials$trial[i], ".csv", sep=""))
    
    # chop off rows before 'start.frame' row in trials
    prey.bead <- prey.bead[-(1:trials$start.frame[i]), ]
    frames <- prey.bead$frame
    
    # add seconds column (1 frame * 1 sec/300 frames = 1/300 second per row)
    prey.bead <- prey.bead[ ,1:3]
    colnames(prey.bead) <- letters[24:26]
    prey.bead$Seconds <- (c(1:nrow(prey.bead))-1)/trials$frame.rate[i]
    prey.bead$frame <- frames
    
   # store in corresponding list element
    prey.bead.list[[i]] <- prey.bead
    
  }
  
  # Find proportion of head length (mouth to esophagus) traveled for each
  # trial
  full.trials <- which(!is.na(trials$esophagus.frame))
  x.length <- c()
  z.length <- c()
  for (i in full.trials) {
    start.x <- prey.bead.list[[i]][1, 1]
    end.x <- prey.bead.list[[i]][(trials$esophagus.frame[i] - trials$start.frame[i]), 1]
    x.length <- c(x.length, (end.x-start.x))
    z.length <- c(z.length, prey.bead.list[[i]][(trials$esophagus.frame[i] - trials$start.frame[i]), 3])
  }
  
  # Find the average X translation for each catfish (very consistent)
  # This will serve as a marker for the trials in which the prey bead does not
  # pass through the esophagus but in which handling does occur
  nc.length <- data.frame(x.length=x.length,
                          z.length=z.length,
                          catID=trials$catID[full.trials])
  nc.list <- vector("list", length=length(levels(nc.length$catID)))
  names(nc.list) <- levels(nc.length$catID)
  
  for (i in 1:length(levels(nc.length$catID))) {
    nc.list[[i]] <- c(mean(nc.length$x.length[nc.length$catID==levels(nc.length$catID)[i]]),
                      mean(nc.length$z.length[nc.length$catID==levels(nc.length$catID)[i]]))
  }
  
  nc.length <- list(Cat01 = mean(c(69.698898, 69.874955, 69.773262)),
                    Cat02 = mean(c(69.809575, 69.501256, 70.172114)),
                    Cat05 = mean(c(74.742032, 75.916118, 75.325379)))
  
  
  # get X position as a proportion of NC length for each trial
  for (i in 1:length(prey.bead.list)) {
    # get nc length to be traveled for that catfish
    nc <- nc.length[[trials$catID[i]]][1]
    z.dev <- nc.list[[trials$catID[i]]][2]
    
    prey.bead <- prey.bead.list[[i]]
    prey.bead$NC.pct <- (prey.bead$x - prey.bead$x[1]) / nc
    prey.bead$Z.deviation <- (prey.bead$z - prey.bead$z[1]) / nc
    prey.bead$Y.deviation <- (prey.bead$y - prey.bead$y[1]) / nc
    prey.bead.list[[i]] <- prey.bead
    
  }
  
  
  
  return(prey.bead.list)
  
}

# plot the X, Y, and Z positions of the prey bead over time
# dashed vertical line at esophageal plane if present
plotPreyXYZ <- function(trial.name, prey.bead.list, ...) {
  
  # load trial CSV
  trials <- trialFiles()

  # grab relevant trial row
  trial.row <- trials[trials$trial==trial.name, ]
  
  # grab relevant prey trial
  prey.bead.idx <- which(names(prey.bead.list)==trial.name)
  prey.bead <- prey.bead.list[[prey.bead.idx]]
  
  # find esophagus frame
  esophagus.frame <- trial.row$esophagus.frame - trial.row$start.frame
  
  # if NA, set esophagus to NULL so no vertical line is plotted
  if (!is.na(esophagus.frame)) {
    esophagus <- prey.bead[esophagus.frame, 4]
  } else {
    esophagus <- NULL
  }
  
  # XYZ coordinates in NC space
  plot(y=prey.bead$x, x=prey.bead$Seconds,
       type='l', lwd=2, col=xyz.col[1],
       ylab="Prey position (mm)", main=trial.name,
       ylim=c(-40, 80), ...)
  points(y=prey.bead$y, x=prey.bead$Seconds, type='l', lwd=2, col=xyz.col[2])
  points(y=prey.bead$z, x=prey.bead$Seconds, type='l', lwd=2, col=xyz.col[3])
  abline(v=esophagus, lty=2, lwd=2)
  
  legend("topleft", legend = LETTERS[24:26], fill=xyz.col, bty="n")
  
}

# plot the frame-to-frame prey velocities (mm/sec)
# additional arguments are passed to plotting function
plotPreyVelocity <- function(trial.name, prey.bead.list, return.vec=FALSE, plotting=TRUE, ...) {
  
  # load trial CSV
  trials <- trialFiles()
  
  # grab relevant trial row
  trial.row <- trials[trials$trial==trial.name, ]
  
  # grab relevant prey trial
  prey.bead.idx <- which(names(prey.bead.list)==trial.name)
  prey.bead <- prey.bead.list[[prey.bead.idx]]
  
  # find esophagus frame
  esophagus.frame <- trial.row$esophagus.frame - trial.row$start.frame
  
  # if NA, set esophagus to NULL so no vertical line is plotted
  if (!is.na(esophagus.frame)) {
    esophagus <- prey.bead[esophagus.frame, 4]
  } else {
    esophagus <- NULL
  }
  
  # find prey velocities over time
  velocities <- vector(length = (nrow(prey.bead)-1))
  
  # get distance traveled between frames
  # = mm/frame
  for (i in 2:nrow(prey.bead)) {
    velocities[i] <- sqrt(sum(prey.bead[i-1, 1:3]-prey.bead[i, 1:3])^2)
  }
  
  # mm/frame * 300 frames/sec = mm/sec
  velocities <- velocities*trial.row$frame.rate
  
  if (plotting) {
    plot(y=velocities, x=prey.bead$Seconds[-1], 
         type='l', ylab=expression("Prey velocity " (mm ~ sec^{-1})),
         xlim=c(0, max(prey.bead$Seconds)), 
         panel.first=abline(h=0, lwd=1, col="darkgrey"),
         ...)
    abline(v=esophagus, lty=2, lwd=2)
  }

  if (return.vec) {
    return(velocities)
  }
}

# plot the changes in velocity over time (mm/sec^2)
plotPreyAcceleration <- function(trial.name, prey.bead.list, return.vec=FALSE, plotting=TRUE, ...) {
  
  # load trial CSV
  trials <- trialFiles()
  
  # grab relevant trial row
  trial.row <- trials[trials$trial==trial.name, ]
  
  # grab relevant prey trial
  prey.bead.idx <- which(names(prey.bead.list)==trial.name)
  prey.bead <- prey.bead.list[[prey.bead.idx]]
  
  # find esophagus frame
  esophagus.frame <- trial.row$esophagus.frame - trial.row$start.frame
  
  # if NA, set esophagus to NULL so no vertical line is plotted
  if (!is.na(esophagus.frame)) {
    esophagus <- prey.bead[esophagus.frame, 4]
  } else {
    esophagus <- NULL
  }
  
  # find prey velocities over time
  velocities <- plotPreyVelocity(trial.name, prey.bead.list, 
                                 return.vec=TRUE, plotting=FALSE)
  
  accelerations <- diff(velocities)
  times <- prey.bead$Seconds[-c(1:2)]
  
  if (plotting) {
    plot(y=accelerations, x=times, 
         type='l', xlim=c(0, tail(times, 1)), 
         ylab=expression("Prey acceleration " (mm ~ sec^{-2})),
         panel.first=abline(h=0, lwd=1, col="darkgrey"),
         ...)
    abline(v=esophagus, lty=2, lwd=2)
  }


  if (return.vec) {
    return(accelerations)
  }
}

# get prey path complexity
# = actual distance traveled/minimum possible distance over a window
# sliding window defines complexity window for each frame as that frame +/- w frames in front of and behind it
# ex: if w=10, delta window around frame 17 is 7-27
# if sliding.window=FALSE, just chop frames into chunks of window size w
preyPathComplexity <- function(trial.name, prey.bead.list, w=10, sliding.window=TRUE, plotting=TRUE, ylab="Path complexity", return.vec=TRUE, ...) {
  
  # load trial CSV
  trials <- trialFiles()
  
  # grab relevant trial row
  trial.row <- trials[trials$trial==trial.name, ]
  
  # grab relevant prey trial
  prey.bead.idx <- which(names(prey.bead.list)==trial.name)
  prey.bead <- prey.bead.list[[prey.bead.idx]]
  
  # find esophagus frame
  esophagus.frame <- trial.row$esophagus.frame - trial.row$start.frame
  
  # if NA, set esophagus to NULL so no vertical line is plotted
  if (!is.na(esophagus.frame)) {
    esophagus <- prey.bead[esophagus.frame, 4]
  } else {
    esophagus <- NULL
  }
  
  if (sliding.window) {
    
    delta <- vector(length=nrow(prey.bead))
    
    for (i in 1:nrow(prey.bead)) {
      
      # if there are not enough frames behind, just start at first frame
      if (i <= w) {
        first.frame <- 1
      } else {
        first.frame <- i - w
      }
      
      # if not enough in front, just use the remainder
      if ((i + w) > nrow(prey.bead)) {
        last.frame <- nrow(prey.bead)
      } else {
        last.frame <- i + w
      }
      
      bin <- prey.bead[first.frame:last.frame, ]
      
      # find the sum of distances between every subsequent pair of points in the sequence
      dmat <- as.matrix(dist(bin[, 1:3]))
      
      # overall distance
      k <- c(1:(nrow(dmat)-1))
      j <- k+1
      v <- sum(dmat[cbind(k, j)])
      delta[i] <- v/dmat[1, nrow(dmat)] # actual distance traveled/minimum possible distance
    }
    
    if (plotting) {
      plot(y=delta, x=prey.bead$Seconds, 
           type='l', 
           ylab=ylab,
           panel.first=abline(h=1, lwd=1, col="darkgrey"),
           ...)
      abline(v=esophagus, lty=2, lwd=2)
    }
    
  } else {
    
    bins <- seq(1, nrow(prey.bead), by=w)
    
    delta <- vector(length=length(bins))
    
    for (i in 1:(length(bins)-1)) {
      bin <- prey.bead[bins[i]:bins[i+1], ]
      
      # find the distance between the start and end points
      sqrt(sum(bin[1, 1:3]-tail(bin[ , 1:3], 1))^2)
      
      # find the sum of distances between every subsequent pair of points in the sequence
      dmat <- as.matrix(dist(bin[, 1:3]))
      
      # overall distance
      k <- c(1:(w-1))
      j <- c(1:(w-1))+1
      v <- sum(dmat[cbind(k, j)])
      delta[i] <- v/dmat[1, w] # actual distance traveled/minimum possible distance
      
    }
    if (plotting) {
      plot(delta, 
           type='l', 
           ylab=ylab,
           panel.first=abline(h=1, lwd=1, col="darkgrey"),
           ...)
    }
    
  }
  

  
  if (return.vec) {
    return(delta)
  }
  
}

trial.name <- trials$trial[22]
prey.bead.list <- loadPreyTrajectories()

