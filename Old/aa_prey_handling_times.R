sapply(dir("Code/", pattern = "0[0-9]*", full.names = T), source)
trials <- trialFiles()
prey.bead.list <- loadPreyTrajectories()

# get path complexities and velocities
deltas <- vector("list", length=nrow(trials)) 
velocities <- vector("list", length=nrow(trials)) 

for (i in 1:nrow(trials)) {
  velocities[[i]] <- plotPreyVelocity(trials$trial[i], prey.bead.list, return.vec = TRUE)
  deltas[[i]] <- preyPathComplexity(trials$trial[i], prey.bead.list, 
                                    xlab="Time (s)")
}

for (i in 1:length(velocities)) {
  
  plot(velocities[[i]], type='l')
  abline(v=trials$prey.handling.start.frame[i], lty=2, col="red")
  abline(v=trials$esophagus.frame[i], lty=2, col="blue")
  
}

for (i in 1:length(velocities)) {
  plot(y=velocities[[i]], x=prey.bead.list[[i]][1:length(velocities[[i]]), 1], 
       col="tomato", pch=19, main=names(prey.bead.list)[i])
}

trial.name <- trials$trial[22]
par(mfrow=c(5, 1), mar=c(2, 4, 1, 1))
plotPreyXYZ(trial.name, prey.bead.list)
velocity <- plotPreyVelocity(trial.name, prey.bead.list, return.vec=T)

plot(y=velocity, x=prey.bead.list[[22]][1:length(velocity), 1], 
     col="tomato", pch=19)
points(y=velocity, x=prey.bead.list[[22]][1:length(velocity), 2], col="lightgreen")
points(y=velocity, x=prey.bead.list[[22]][1:length(velocity), 3], col="cornflowerblue")

# # get maximum velocities in mm/sec:
# max.velocities <- unlist(lapply(velocities, max))
# 
# # get prey handling time:
# # first peak of path complexity to esophagus frame
# prey.handling.start <- unlist(lapply(deltas, function(i) 
#   find_peaks(i, m=2*w)[1]))
# 
# prey.handling.end <- trials$esophagus.frame
# 
# prey.handling.time <- trials$esophagus.frame-prey.handling.start

#trials$handling.time.seconds <- prey.handling.time/300
#trials$prey.handling.start.frame <- prey.handling.start
#trials$max.velocity.mmsec <- max.velocities
#write.csv(trials, file = "Notes/Prey_trajectory_notes.csv")
