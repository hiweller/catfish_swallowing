# path complexities relative to X axis (AP axis of the animal)
# get path complexities and velocities
source("Code/04_prey_bead_trajectory_analyses.R")
library(magrittr)
trials <- trialFiles()
prey.bead.list <- loadPreyTrajectories()
color <- rep(RColorBrewer::brewer.pal(n=12, name="Paired"), 3)

# Pharyngeal jaw coordinates

# get velocities and path complexities ####
for (i in 1:nrow(trials)) {
  prey.bead.list[[i]]$Velocity <- 
    plotPreyVelocity(trials$trial[i], prey.bead.list, 
                     return.vec = TRUE, plotting=F)
  prey.bead.list[[i]]$Acceleration <- 
    c(NA, plotPreyAcceleration(trials$trial[i], prey.bead.list, 
                         return.vec = TRUE, plotting=F))
  prey.bead.list[[i]]$Complexity <- 
    preyPathComplexity(trials$trial[i], prey.bead.list, plotting=F, w = 100)

}

# two ways of finding the start of handling time:
# 1) length of NC where velocity plummets (where in NC does this happen?)
# 2) length of NC where handling rockets (where in NC does this happen?)


#### Velocity and complexity as functions of NC length ####
# plot velocities relative to % NC
par(mfrow=c(1, 1), mar=rep(4, 4)+0.1)
plot(prey.bead.list[[1]]$NC.pct, prey.bead.list[[1]]$Velocity, pch=20,
     col=scales::alpha(color[1], 0.12), bg="lightgrey",
     panel.first = abline(h=0, col="darkgrey", lwd=2),
     xlab="Proportion of head length", ylab="Velocity (mm/sec)")
for (i in 2:length(prey.bead.list)) {
  points(prey.bead.list[[i]]$NC.pct, prey.bead.list[[i]]$Velocity, 
         col=scales::alpha(color[i], 0.12), pch=20,
       panel.first = abline(h=0, col="darkgrey", lwd=2))
}

hist(prey.bead.list[[7]]$NC.pct)

# plot delta and velocity relative to each other?
# Not sure whether this is informative
plot(deltas[[1]], velocities[[1]], pch=19, col=scales::alpha(color[1], 0.5),
     xlab="Path complexity",
     ylab="Velocity (mm/sec)",
     main="Complexity and velocity are inversely proportional",
     xlim=c(1, lapply(deltas, max) %>% unlist %>% max),
     ylim=c(0, lapply(velocities, max) %>% unlist %>% max))
for (i in 1:length(deltas)) {
  points(deltas[[i]], velocities[[i]], pch=19, 
         col=scales::alpha(color[i], 0.5),
       main=trials$trial[i], xlab="Path complexity",
       ylab="Velocity (mm/sec)")
}

plot(deltas[[1]], velocities[[1]], pch=19, col=color[1],
     xlab="Path complexity",
     ylab="Velocity (mm/sec)",
     main="Complexity and velocity are inversely proportional",
     xlim=c(1, 10),
     ylim=c(0, 1000))
for (i in 1:length(deltas)) {
  points(deltas[[i]], velocities[[i]], pch=19, 
         col=scales::alpha(color[i], 0.7),
         main=trials$trial[i], xlab="Path complexity",
         ylab="Velocity (mm/sec)")
}



#### Generate distributions/histograms for complexity and velocity  ####
# When in the head are velocity/complexity higher, lower, super variable?

# Generate a massive dataframe of head length, velocity, and complexity
combined.df <- data.frame(
  X.Pct = unlist(lapply(prey.bead.list, function(i) i$NC.pct)),
  Z.Pct = unlist(lapply(prey.bead.list, function(i) i$Z.deviation)),
  Velocity = unlist(lapply(prey.bead.list, function(i) i$Velocity)),
  Complexity = unlist(lapply(prey.bead.list, function(i) i$Complexity)))
#combined.df$Pct <- round(combined.df$Pct, 2)

# Remove any values that are outside of the head (Pct is negative)
combined.df <- combined.df[-which(combined.df$X.Pct < 0),]

Lab.palette <- colorRampPalette(c("ghostwhite", "cornflowerblue", "darkblue"), space = "Lab")
smoothScatter(x=combined.df$X.Pct, y=combined.df$Velocity, 
              nbin=c(132, 100), nrpoints = 0,
              colramp=Lab.palette)
smoothScatter(x=combined.df$X.Pct, y=combined.df$Acceleration, 
              nbin=c(132, 100), nrpoints = 0,
              colramp=Lab.palette)
smoothScatter(x=combined.df$X.Pct, y=combined.df$Complexity, 
              nbin=c(132, 100), nrpoints = 0,
              colramp=Lab.palette)



#### Find individual 'handling time' start points ####
for (i in 1:length(prey.bead.list)) {
  prey <- prey.bead.list[[i]]
  plot(prey$NC.pct, prey$Velocity,
       xlab="Proportion of head length",
       ylab="Velocity (mm/sec)",
       main=names(prey.bead.list)[i],
       pch=20, col=color[i], xlim=c(0, 1))
  abline(v=prey$NC.pct[which(prey$Velocity==max(prey$Velocity))], lty=2, col="red")
}

# Maximum velocity always occurs in the anterior half of the oral cavity, and plummets by like 400% in the back half


# Point 1: you can move prey quickly or carefully, but not both ####
# Not sure whether this is informative
e <- 0.0000000001
summary(lm(log(combined.df$Velocity + e) ~ log(combined.df$Complexity + e)))

test <- lm(log(combined.df$Velocity + e) ~ log(combined.df$Complexity + e))
plot(log(combined.df$Complexity + e), log(combined.df$Velocity + e), pch=19, 
     col=scales::alpha("purple", 0.1),
     xlab="Path complexity",
     ylab="Velocity (mm/sec)",
     main="Complexity and velocity are inversely related",
     #xlim=c(1, 3),
     ylim=c(-10, 10)
     )
for (i in 1:length(prey.bead.list)) {
  points(log(prey.bead.list[[i]]$Complexity+e), log(prey.bead.list[[i]]$Velocity+e), pch=19, 
         col=scales::alpha(color[i], 0.1),
         main=trials$trial[i], xlab="Path complexity",
         ylab="Velocity (mm/sec)")
}

# vs. acceleration
plot(prey.bead.list[[1]]$Complexity, prey.bead.list[[1]]$Acceleration,
     pch=19, 
     col=scales::alpha(color[1], 0.1),
     xlab="Path complexity",
     ylab=expression("Acceleration " (mm ~ sec^{-2})),
     main="Complexity and velocity are inversely related")
for (i in 1:length(prey.bead.list)) {
  points(prey.bead.list[[i]]$Complexity, prey.bead.list[[i]]$Acceleration, pch=19, 
         col=scales::alpha(color[i], 0.1),
         main=trials$trial[i], xlab="Path complexity",
         ylab="Velocity (mm/sec)")
}



# Point 2: prey moves fastest as it's entering the mouth, then dramatically slows down, and past 50% of head length is moving glacially ####
Lab.palette <- colorRampPalette(c("ghostwhite", "lightblue", "darkblue"), space = "Lab")
smoothScatter(x=combined.df$X.Pct, y=combined.df$Velocity, 
              nbin=c(10, 10), nrpoints = 0,
              colramp=Lab.palette)

par(mfrow=c(3, 1), mar=c(4, 4, 1, 1))

{ plot(combined.df$Z.Pct, combined.df$Velocity, 
     col=scales::alpha("royalblue", 0.12), pch=20,
     ylim=c(0, 1000), xlab="",
     ylab="Velocity (mm/sec)",
     panel.first = c(abline(v=0.5, col="red", lty=2,
                            abline(h=0, col="grey", lwd=1.5))))

plot(combined.df$Z.Pct, combined.df$Acceleration, 
     col=scales::alpha("olivedrab4", 0.12), pch=20,
     ylim=range(combined.df$Acceleration, na.rm=T),
     xlab="",
     ylab="Acceleration",
     panel.first = c(abline(v=0.5, col="red", lty=2,
                            abline(h=0, col="grey", lwd=1.5))))

plot(combined.df$Z.Pct, combined.df$Complexity, 
     col=scales::alpha("darkgoldenrod1", 0.12), pch=20, 
     ylim=c(1, 10), 
     xlab="% Head length",
     ylab="Path complexity",
     panel.first = c(abline(v=0.5, col="red", lty=2,
                            abline(h=1, col="grey", lwd=1.5))))
}


# Plot average and median velocity, complexity, acceleration ####
pcts <- sort(unique(round(combined.df$Pct, 2)))
{ avg.vel <- c()
v.sd <- c()

avg.acc <- c()
a.sd <- c()

avg.comp <- c()
c.sd <- c()

for (i in 1:length(pcts)) {
  idx <- which(round(combined.df$Pct, 2)==pcts[i])
  
  avg.vel <- c(avg.vel, mean(combined.df$Velocity[idx]))
  avg.acc <- c(avg.acc, mean(combined.df$Acceleration[idx]))
  avg.comp <- c(avg.comp, mean(combined.df$Complexity[idx]))
  
  v.sd <- c(v.sd, sd(combined.df$Velocity[idx]))
  a.sd <- c(a.sd, sd(combined.df$Acceleration[idx]))
  c.sd <- c(c.sd, sd(combined.df$Complexity[idx]))
}


plus <- avg.vel+v.sd
minus <- avg.vel-v.sd

plot(pcts, minus, ylim=c(min(minus, na.rm = T), max(plus, na.rm = T)), col="grey",
     type='l', xlab="% Head length", ylab="Average velocity",
     panel.first = abline(h=0, col="darkgrey"))
points(pcts, plus, type='l', col="grey")
points(pcts, avg.vel, type='l', col="royalblue", lwd=2)

plus <- avg.acc+a.sd
minus <- avg.acc-a.sd

plot(pcts, minus, ylim=c(min(minus, na.rm = T), max(plus, na.rm = T)), col="grey",
     type='l', xlab="% Head length", ylab="Average acceleration",
     panel.first = abline(h=0, col="darkgrey"))
points(pcts, plus, type='l', col="grey")
points(pcts, avg.acc, type='l', col="olivedrab4", lwd=2)

plus <- avg.comp+c.sd
minus <- avg.comp-c.sd
plot(pcts, minus, ylim=c(min(minus, na.rm = T), max(plus, na.rm = T)), col="grey",
     type='l', xlab="% Head length", ylab="Average complexity",
     panel.first = abline(h=1, col="darkgrey"))
points(pcts, plus, type='l', col="grey")
points(pcts, avg.comp, type='l', col="darkgoldenrod1", lwd=2) }




## Plot each complete trial X and Z pct ####
complete.trials <- prey.bead.list[which(!is.na(trials$esophagus.frame))]
complete.trials.md <- trials[which(!is.na(trials$esophagus.frame)), ]

# plot according to food type
for (i in levels(complete.trials.md$food)) {
  food.trials <- complete.trials[which(complete.trials.md$food==i)]
  if (length(food.trials) > 0) {
    plot(food.trials[[1]]$NC.pct, food.trials[[1]]$Z.deviation, 
         col=scales::alpha(color[1], 0.4), pch=19,
         xlab="% Head length", ylab="Medial-lateral deviation", main=i,
         ylim=c(-0.5, 0.5), panel.first = abline(v=1, col="darkgrey", lwd=2))
    for (j in 2:length(food.trials)) {
      points(food.trials[[j]]$NC.pct, food.trials[[j]]$Z.deviation, 
             col=scales::alpha(color[j], 0.4), pch=19)
      #colordistance:::pause()
    }
  }

}

# plot all
plot(complete.trials[[1]]$NC.pct, complete.trials[[1]]$Z.deviation, 
     col=scales::alpha(color[1], 0.4), pch=19,
     xlab="% Head length", ylab="Medial-lateral deviation",
     ylim=c(-0.5, 0.5), panel.first = abline(v=1, col="darkgrey", lwd=2))
for (i in 2:length(complete.trials)) {
  points(complete.trials[[i]]$NC.pct, complete.trials[[i]]$Z.deviation, 
       col=scales::alpha(color[i], 0.4), pch=19)
  #colordistance:::pause()
}


# Handling start times ####
for (i in 1:length(prey.bead.list)) {
  plot(prey.bead.list[[i]]$NC.pct, prey.bead.list[[i]]$Complexity, 
       pch=20, col=color[i],
       ylab="Complexity", xlab="% Head length",
       main=names(prey.bead.list)[i],
       panel.first = abline(h=1, col="darkgrey"))
}
for (i in 1:length(prey.bead.list)) {
  plot(prey.bead.list[[i]]$NC.pct, prey.bead.list[[i]]$Velocity, 
       pch=20, col=color[i],
       ylab="Velocity", xlab="% Head length",
       main=names(prey.bead.list)[i],
       panel.first = abline(h=1, col="darkgrey"))
}


