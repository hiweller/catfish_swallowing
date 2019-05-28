## THINGS TO DO
# Define 'efficiency' function: the length of the path traveled divided by X displacement only, in fraction of head length

# Find start and end X points for pharyngeal jaws in each fish as % of head length:
  # drop markers at anterior tip of neurocranium, approx. start of pharyngeal jaws (midline), approx. end of pharyngeal jaws
  # transform into NC space
  # get X position in terms of head length

# get velocities for prey in terms of head length

# use start and end points of pharyngeal jaws (avg. values?)
  # fit models of whole trajectory; before/middle/after jaws
  # see whether model error improves, and if we can combine phases

# repeat above analysis for efficiency?

# re-animate operculum in trials and note motions


# velocity: test breakpoints at pharyngeal jaws - is prey moving differently before, during, after?

trials <- trialFiles()
prey.bead.list <- loadPreyTrajectories()

combined.df <- data.frame(
  X.Pct = unlist(lapply(prey.bead.list, function(i) i$NC.pct)),
  Z.Pct = unlist(lapply(prey.bead.list, function(i) i$Z.deviation)),
  Velocity = unlist(lapply(prey.bead.list, function(i) i$Velocity / max(i$Velocity, na.rm = T))),
  Complexity = unlist(lapply(prey.bead.list, function(i) i$Complexity)),
  Acceleration = unlist(lapply(prey.bead.list, function(i) i$Acceleration)))

preyEfficiency <- function(trial.name, prey.bead.list, w = 5) {
  
    # load trial CSV
    trials <- trialFiles()
    
    # grab relevant trial row
    trial.row <- trials[trials$trial == trial.name, ]
    
    # grab relevant prey trial
    prey.bead.idx <- which(names(prey.bead.list) == trial.name)
    prey.bead <- prey.bead.list[[prey.bead.idx]]
    
    # find esophagus frame
    esophagus.frame <- trial.row$esophagus.frame - trial.row$start.frame
    
    # if NA, set esophagus to NULL so no vertical line is plotted
    if (!is.na(esophagus.frame)) {
      esophagus <- prey.bead[esophagus.frame, 4]
    } else {
      esophagus <- NULL
    }
    
    # for each w length of neurocranium, take all frames within length and get efficiency (by head length not frame #)
    
    # for 0-5%: in dataframe, go from frame 1 to the first frame where NC.pct > 0.05
    # get efficiency for all of those frames (IN ORDER)
    # then, from the last frame, go forward until you find the first frame where NC.pct > 0.1, etc, until you hit the end
    # THIS MEANS THAT YOU WILL SOMETIMES INCLUDE THE PREY BEAD GOING BACKWARDS (i.e., NC.pct drops below the value that the first frame in the chunk had)
    # this is ok! it will just shoot the efficiency down even more, which is reflective of what's happening
    
    start.frame <- 1
    efficiency <- c()
    for (i in c(seq((w / 100), max(prey.bead$NC.pct, na.rm = T), by = (w / 100)), max(prey.bead$NC.pct))) {
      
      end.frame <- which(prey.bead$NC.pct >= i)[1]
      
      if (start.frame == end.frame) {
        
        efficiency <- c(efficiency, tail(efficiency, 1))
        
      } else {
        
        bin <- prey.bead[c(start.frame:end.frame), 1:3]
        dmat <- as.matrix(dist(bin))
        
        # overall distance
        k <- c(1:(nrow(dmat)-1))
        j <- k+1
        v <- sum(dmat[cbind(k, j)])
        
        efficiency <- c(efficiency, (prey.bead$x[end.frame] - prey.bead$x[start.frame]) / v )
        
        start.frame <- end.frame
        
      }
      
    }
    
    head.length.vec <- c(seq((w / 100), max(prey.bead$NC.pct), by = (w / 100)), 
      max(prey.bead$NC.pct))
    
    return(data.frame(Head.length = head.length.vec,
                      Efficiency = efficiency))
  
}

par(mfrow = c(5, 1), mar=rep(1,4)+0.1)

for (i in 1:nrow(trials)) {
  trial <- trials$trial[i]
  catID <- gsub("Cat", "", trials$catID[i])
  
  PJs <- loadPharyngealMarkers(catID)
  plot(preyEfficiency(trial, prey.bead.list, w=2), pch=20, type='b',
       main = trial, ylim = c(0, 1), xlim = c(0, 1.2),
       panel.first = abline(v = 1, col = "red",
                            lty=2))
  abline(v = PJs[1], col="darkgrey")
  abline(v = PJs[2], col="darkgrey")
  
}

# efficiency: how quickly did you move in the X direction vs. the path traveled to get there

# same test: before, during, after pharyngeal jaws

# are pellets vs. worms handled differently in terms of velocity, efficiency?