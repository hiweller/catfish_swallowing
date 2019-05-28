source("Code/04_prey_bead_trajectory_analyses.R")
library(magrittr)
trials <- trialFiles()
prey.bead.list <- loadPreyTrajectories()
color <- rep(RColorBrewer::brewer.pal(n=12, name="Set3"), 3)

# Get velocities and path complexities ####
for (i in 1:nrow(trials)) {
  prey.bead.list[[i]]$Velocity <- 
    plotPreyVelocity(trials$trial[i], prey.bead.list, 
                     return.vec = TRUE, plotting=F)
  prey.bead.list[[i]]$Acceleration <- 
    c(NA, plotPreyAcceleration(trials$trial[i], prey.bead.list, 
                               return.vec = TRUE, plotting=F))
  prey.bead.list[[i]]$Complexity <- 
    preyPathComplexity(trials$trial[i], prey.bead.list, plotting=F, w = 10)
  
}


# Generate a massive dataframe of head length, velocity, and complexity ####
combined.df <- data.frame(
  X.Pct = unlist(lapply(prey.bead.list, function(i) i$NC.pct)),
  Z.Pct = unlist(lapply(prey.bead.list, function(i) i$Z.deviation)),
  Velocity = unlist(lapply(prey.bead.list, function(i) i$Velocity / max(i$Velocity, na.rm = T))),
  Complexity = unlist(lapply(prey.bead.list, function(i) i$Complexity)),
  Acceleration = unlist(lapply(prey.bead.list, function(i) i$Acceleration)))

# Remove any values that are outside of the head (Pct is negative)
combined.df <- combined.df[-which(combined.df$X.Pct < 0),]

# Point 1: you can move prey quickly or carefully, but not both ####
# Not sure whether this is informative
plot(prey.bead.list[[1]]$Complexity, prey.bead.list[[1]]$Velocity, pch=19, 
     col=scales::alpha(color[1], 0.3),
     xlab="Path complexity",
     ylab="Velocity (mm/sec)",
     main="Complexity and velocity are inversely related",
     xlim=c(1, 5),
     ylim=c(0, 500))
for (i in 1:length(prey.bead.list)) {
  points(prey.bead.list[[i]]$Complexity, prey.bead.list[[i]]$Velocity, pch=19, 
         col=scales::alpha(color[i], 0.3),
         main=trials$trial[i], xlab="Path complexity",
         ylab="Velocity (mm/sec)")
}

# Plotting logs (I do not care for this very much): ####
log.v <- log(combined.df$Velocity)
log.v[which(is.infinite(log.v))] <- NA

log.c <- log(combined.df$Complexity)
log.c[which(is.infinite(log.c))] <- NA

plot(combined.df$Complexity, log.v, col=scales::alpha("darkblue", 0.15), pch=20)


# Point 2: prey moves fastest as it's entering the mouth, then dramatically slows down, and past 50% of head length is moving glacially ####
par(mfrow=c(2, 1), mar=c(4, 4, 0.1, 1))
{ plot(combined.df$X.Pct, log(combined.df$Velocity), 
       col=scales::alpha("darkblue", 0.12), pch=20,
       xlab="", xlim=c(0, 1.3),
       ylab="Log of Normalized Velocity (mm/sec)",
       panel.first = c(abline(v=0.6, col="red", lty=2),
                       abline(h=0, col="grey", lty=2,lwd=1.5),
                       abline(v=0.8, col="red", lty=2),
                       abline(v=0, col="darkgrey", lwd=2),
                       abline(v=1, col="darkgrey", lwd=2)))
  
  # plot(combined.df$X.Pct, combined.df$Acceleration, 
  #      col=scales::alpha("olivedrab4", 0.12), pch=20,
  #      xlab="",
  #      ylab="Acceleration",
  #      panel.first = c(abline(v=0.5, col="red", lty=2,
  #                             abline(h=0, col="grey", lwd=1.5))))
  
  plot(combined.df$X.Pct, combined.df$Complexity, 
       col=scales::alpha("darkgoldenrod1", 0.12), pch=20, 
       xlab="% Head length", xlim=c(0, 1.3),
       ylab="Path complexity",
       panel.first = c(abline(v=0.5, col="red", lty=2,
                              abline(h=1, col="grey", lwd=1.5))))
}

par(mfrow=c(2, 1), mar=c(4, 4, 0.1, 1))
{ smoothScatter(combined.df$X.Pct, combined.df$Velocity, 
       col=scales::alpha("royalblue", 0.12), pch=20,
       xlab="", xlim=c(0, 1.3),
       ylab="Normalized Velocity (mm/sec)",
       panel.first = c(abline(v=0.5, col="red", lty=2,
                              abline(h=0, col="grey", lwd=1.5))))
  
  # plot(combined.df$X.Pct, combined.df$Acceleration, 
  #      col=scales::alpha("olivedrab4", 0.12), pch=20,
  #      xlab="",
  #      ylab="Acceleration",
  #      panel.first = c(abline(v=0.5, col="red", lty=2,
  #                             abline(h=0, col="grey", lwd=1.5))))
  
  smoothScatter(combined.df$X.Pct, combined.df$Complexity, 
       col=scales::alpha("darkgoldenrod1", 0.12), pch=20, 
       xlab="% Head length",
       ylab="Path complexity",
       panel.first = c(abline(v=0.5, col="red", lty=2,
                              abline(h=1, col="grey", lwd=1.5))))
}


smoothScatter(combined.df$X.Pct, combined.df$Velocity)
# Plot each complete trial X and Z pct ####
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


# Plot each trial X and Z pct ####

# for each trial in prey.bead.list,
# plot X-Z and X-Y
# color according to food type
food.color.upper <- rev(c("orangered2", "cornflowerblue", "orchid1"))
food.color.lower <- rev(c("goldenrod1", "darkblue", "mediumorchid4"))

food.types <- levels(trials$food)
catID.pch <- c(15, 17, 19)
opacity <- 0.25
sf <- 2
png("Manuscript/Figures/Fig02_all_trajectories.png", width = 2000 * sf, height = 2400 * sf, res = 300, bg = "transparent")

par(mfrow = c(2, 1), mar=c(5, 6, 3, 2))
for (i in 1:length(food.types)) {
  food.trials <- prey.bead.list[which(trials$food==food.types[i])]
  food.catID <- trials$catID[which(trials$food==food.types[i])]
  food.color <- colorRampPalette(c(food.color.upper[i], food.color.lower[i]))(length(food.trials))
  food.type <- levels(trials$food)[i]
  
  if (i == 1) {
    
    if (length(food.trials) > 0) {
      plot.pch <- catID.pch[which(levels(trials$catID) %in% food.catID[1])]
      plot(food.trials[[1]]$NC.pct, food.trials[[1]]$Y.deviation, 
           col=scales::alpha(food.color[1], opacity),
           ylab="Dorsal-ventral deviation", 
           xlab="Proportion of head length",
           main="Food trajectories",
           ylim=c(-0.4, 0.4), xlim=c(0, 1.2),
           panel.first = c(abline(v=1, col="darkgrey", lwd=2),
                           abline(v=0, col="darkgrey", lwd=2)),
           pch = plot.pch,
           cex.main = 3,
           cex.lab = 3,
           xaxt = "n",
           yaxt = "n",
           asp = 1)
      axis(2, at = c(-0.4, 0, 0.4), cex.axis = 2)
      axis(1, at = c(0, 0.5, 1), cex.axis = 2)
      
      if (length(food.trials) > 1) {
        for (j in 2:length(food.trials)) {
          plot.pch <- catID.pch[which(levels(trials$catID) %in% food.catID[j])]
          points(food.trials[[j]]$NC.pct, food.trials[[j]]$Y.deviation, 
                 col=scales::alpha(food.color[j], opacity), 
                 pch=plot.pch) 
        }
        
      }
    }
  } else {
    if (length(food.trials) >= 1) {
      for (j in 1:length(food.trials)) {
        plot.pch <- catID.pch[which(levels(trials$catID) %in% food.catID[j])]
        points(food.trials[[j]]$NC.pct, food.trials[[j]]$Y.deviation, 
               col=scales::alpha(food.color[j], opacity), 
               pch = plot.pch) 
      }
      
    }
  }
}
for (i in 1:length(food.types)) {
  food.trials <- prey.bead.list[which(trials$food==food.types[i])]
  food.catID <- trials$catID[which(trials$food==food.types[i])]
  food.color <- colorRampPalette(c(food.color.upper[i], food.color.lower[i]))(length(food.trials))
  food.type <- levels(trials$food)[i]
  
  if (i == 1) {
    
    if (length(food.trials) > 0) {
      plot.pch <- catID.pch[which(levels(trials$catID) %in% food.catID[1])]
      plot(food.trials[[1]]$NC.pct, food.trials[[1]]$Z.deviation, 
           col=scales::alpha(food.color[1], opacity),
           ylab="Medial-lateral deviation", 
           xlab="Proportion of head length",
           main="Food trajectories",
           ylim=c(-0.4, 0.4), xlim=c(0, 1.2),
           panel.first = c(abline(v=1, col="darkgrey", lwd=2),
                           abline(v=0, col="darkgrey", lwd=2)),
           pch = plot.pch,
           cex.main = 3,
           cex.lab = 3,
           xaxt = "n",
           yaxt = "n",
           asp = 1)
      axis(2, at = c(-0.4, 0, 0.4), cex.axis = 2)
      axis(1, at = c(0, 0.5, 1), cex.axis = 2)
      
      if (length(food.trials) > 1) {
        for (j in 2:length(food.trials)) {
          plot.pch <- catID.pch[which(levels(trials$catID) %in% food.catID[j])]
          points(food.trials[[j]]$NC.pct, food.trials[[j]]$Z.deviation, 
                 col=scales::alpha(food.color[j], opacity), 
                 pch=plot.pch) 
        }
    
      }
    }
  } else {
        if (length(food.trials) >= 1) {
          for (j in 1:length(food.trials)) {
            plot.pch <- catID.pch[which(levels(trials$catID) %in% food.catID[j])]
            points(food.trials[[j]]$NC.pct, food.trials[[j]]$Z.deviation, 
                   col=scales::alpha(food.color[j], opacity), 
                   pch = plot.pch) 
          }
          
        }
  }
  
}
legend(x = 0.1, y = -0.25, 
       legend = c("Sinking pellet", "Worm", "Squid piece"), 
       fill = c("orange", "orchid", "Cornflowerblue"))
dev.off()

for (i in c(1, 3)) {
  
  food.type <- levels(trials$food)[i]
  
  food.trials <- prey.bead.list[which(trials$food==food.type)]
  
  food.color <- colorRampPalette(c(food.color.upper[i], food.color.lower[i]))(length(food.trials))
  
    if (length(food.trials) > 0) {
      plot(food.trials[[1]]$NC.pct, food.trials[[1]]$Z.deviation, 
           col=scales::alpha(food.color[1], 0.4), pch=20,
           xlab="Proportion of head length", 
           ylab="Medial-lateral deviation", main=levels(trials$food)[i],
           ylim=c(-0.4, 0.4), xlim=c(0, 1.2),
           panel.first = c(abline(v=1, col="darkgrey", lwd=2),
                           abline(v=0, col="darkgrey", lwd=2)),
           cex.main = 3,
           cex.lab = 2)
      
      if (length(food.trials) > 1) {
        for (j in 2:length(food.trials)) {
          points(food.trials[[j]]$NC.pct, food.trials[[j]]$Z.deviation, 
                 col=scales::alpha(food.color[j], 0.4), pch=20) 
        }
      }
    }
}

# plot according to food type
for (i in levels(trials$food)) {
  food.trials <- complete.trials[which(complete.trials.md$food==i)]
  if (length(food.trials) > 0) {
    
    plot(food.trials[[1]]$NC.pct, food.trials[[1]]$Z.deviation, 
         col=scales::alpha(color[1], 0.4), pch=19,
         xlab="% Head length", ylab="Medial-lateral deviation", main=i,
         ylim=c(-0.2, 0.4), panel.first = abline(v=1, col="darkgrey", lwd=2),
         cex.main=2)
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


# Plot velocity
