# HOUSEKEEPING (colors, libraries, loading CSVs, mini-functions) ####
library(ggplot2)
library(magrittr)
source("Code/functions.R")
prey.bead.list <- loadPreyTrajectories()
trials <- trialFiles()
colors <- rep(RColorBrewer::brewer.pal(8, "Set2"), 4)

# 1:11: cat2, teal
# 12:22: cat1, purple
# 23:25: cat5, orange
oranges <- colorRampPalette(c("#e8790b", "#ffe27c"))(11)
teals <- colorRampPalette(c("#8de0bd", "#00727a"))(11)
purples <- colorRampPalette(c("#d5aaed", "#6d1482"))(11)

mround <- function(x, base) {
  base * round(x / base)
}

# set first trial as test for plotting
trial <- prey.bead.list[[22]]


## FIGURE 2: REPRESENTATIVE TRIAL ####
sf <- 1
png("Manuscript/Figures/Fig02_representative_trial.png",
    width = 2500 * sf, 
    height = 2200 * sf,
    res = 300)
par(mar = c(5, 8, 4, 1), mfrow = c(2, 1))
# Plot progress thru the head

{
  lwd <- 6
  
  # plot(trial$Seconds, trial$NC.pct, # make sure X-axis is correct length
  #    type = 'l', lwd = lwd,
  #    col = "goldenrod1",
  #    ylim = range(trial$NC.pct),
  #    panel.first = abline(h = c(0, 1.0),
  #                         col = "lightgrey",
  #                         lwd = 3),
  #    xlab = "",
  #    ylab = "Head proportion",
  #    #main = "Food location",
  #    cex.main = 3,
  #    cex.lab = 2.2,
  #    cex.axis = 2)
  # 
# Plot velocity thru the head

total.displacement <- c()
for (i in 2:nrow(trial)) {
  total.displacement <- c(total.displacement,
                          eucDist(trial[(i-1), 6:8], 
                                  trial[i, 6:8]))
}
#plot(total.displacement,panel.first = abline(h = 0))

plot(trial$Seconds,
     trial$NC.pct, #%>% diff * 300,
     ylim = c(-.3, 1.2),
     type = 'l', lwd = lwd,
     col = "tomato",
     xlab = "",
     panel.first = abline(h = c(0, 1.0),
                          col = "lightgrey",
                          lwd = 3),
     ylab = expression(paste("Head lengths")),
     main = "Displacement on body axes",
     cex.main = 2,
     cex.lab = 2.2,
     cex.axis = 2)
points(trial$Seconds,
       trial$Y.deviation, #%>% diff * 300,
       #c(NA, diff(trial$y)),
       type = 'l', lwd = lwd,
       col = "palegreen3")
points(trial$Seconds,
       trial$Z.deviation, #%>% diff * 300,
       type = 'l', lwd = lwd,
       col = "cornflowerblue")
legend(x = 2, y = 0.9, 
       fill = c("tomato",
                "palegreen3",
                "cornflowerblue"),
       legend = c("AP",
                  "DV",
                  "ML"))

plot(trial$Seconds[-1],
     total.displacement * 300,
     #ylim = range(diff(trial$NC.pct)) * 300,
     type = 'l', lwd = lwd,
     col = "goldenrod1",
     panel.first = abline(h = c(0),
                          col = "lightgrey",
                          lwd = 3),
     ylab = expression("Head lengths * s"^"-1"),
     xlab = "Seconds", 
     main = "Speed",
     cex.main = 2,
     cex.lab = 2.2,
     cex.axis = 2)

}
dev.off()

## FIGURE 3: PREY TRAJECTORIES FOR ALL TRIALS ####
# for each trial in prey.bead.list,
# plot X-Z and X-Y
# color according to food type
food.color.upper <- rev(c("orangered2", "cornflowerblue", "orchid1"))
food.color.lower <- rev(c("goldenrod1", "darkblue", "mediumorchid4"))

food.types <- levels(trials$food)
catID.pch <- c(15, 17, 19)
opacity <- 0.25
sf <- 0.5

png("Manuscript/Figures/Fig03_all_trajectories.png", width = 2000 * sf, height = 2400 * sf, res = 72, bg = "transparent")
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
           cex.axis = 2,
           asp = 1)
      axis(1, at = seq(0, 1.25, by = 0.25), cex.axis = 2)
      
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
           cex.axis = 2, 
           asp = 1)
      axis(1, at = seq(0, 1.25, by = 0.25), cex.axis = 2)
      
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

## FIGURE 4: AVERAGE VELOCITIES/EFFICIENCIES ####
# Velocity
# Get all of the velocities within that chunk and group them
binned.data <- data.frame(X.d = numeric(),
                          Y.d = numeric(),
                          Z.d = numeric(),
                          Tot.d = numeric(),
                          Velocity = numeric(),
                          Position = numeric(),
                          catID = character(),
                          Food = character(),
                          Trial = character())


for (i in 1:length(prey.bead.list)) {
  
  trial <- prey.bead.list[[i]]
  
  Velocity <- diff(trial$NC.pct) * 300 # in head lengths / second
  
  # Normalize?
  X.d <- diff(trial$x)
  Y.d <- diff(trial$y)
  Z.d <- diff(trial$z)
  Tot.d <- sqrt(X.d^2 + Y.d^2 + Z.d^2)
  
  xyz.norm <- t(apply(cbind(X.d, Y.d, Z.d), 1, function(i) abs(i) / sum(abs(i))))
  
  Velocity <- Velocity / max(Velocity)
  Position <- mround(trial$NC.pct[-1], n)
  
  temp.df <- aggregate(cbind(xyz.norm, Tot.d, Velocity), list(factor(Position)), mean)
  
  catID <- rep(trials$catID[i], nrow(temp.df))
  Food <- rep(trials$food[i], nrow(temp.df))
  Trial <- rep(trials$trial[i], nrow(temp.df))
  
  temp.df <- data.frame(X.d = temp.df$X.d,
                        Y.d = temp.df$Y.d,
                        Z.d = temp.df$Z.d,
                        Tot.d = temp.df$Tot.d,
                        Velocity = temp.df$Velocity,
                        Position = temp.df$Group.1,
                        catID = catID,
                        Food = Food,
                        Trial = Trial)
  
  binned.data <- rbind(binned.data, temp.df)
  
  
}

sf <- 1

png("Manuscript/Figures/Fig03_avg_velocities.png", width = 2000 * sf,
    height = 1200 * sf, res = 150, bg = "transparent")
{p <- ggplot(binned.data, aes(x = Position, y = Velocity)) +
    ggtitle("Velocity") + 
    xlab("Proportion of head length") + 
    ylab("Velocity (normalized)")
  p <- p + geom_jitter(aes(x = factor(Position), 
                           y = Velocity, 
                           color = Food), alpha = 0.8,
                       width = 0.15, height = 0.05,
                       size = 3) + 
    geom_boxplot(aes(x = factor(Position), 
                     y = Velocity), alpha = 0.4,
                 fill = "grey",
                 outlier.alpha = 0) +
    scale_color_manual(values = c("orange", 
                                  "cornflowerblue", 
                                  "mediumorchid"),
                       guide = FALSE)
  p <- p + scale_x_discrete(labels = c("0", "0.1", "0.2", "0.3", "0.4",
                                       "0.5", "0.6", "0.7", "0.8", "0.9",
                                       "1", "1.1", "1.2", "1.3", "1.4"),
                            breaks = c("0", "0.1", "0.2", "0.3", "0.4",
                                       "0.5", "0.6", "0.7", "0.8", "0.9",
                                       "1", "1.1", "1.2", "1.3", "1.4"))
  p <- p + scale_y_continuous(labels = seq(0, 1, by  = 0.2),
                              breaks = seq(0, 1, by  = 0.2))
  p + theme_bw(base_size = 32) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5))
}
dev.off()

# X, Y, Z displacement
sf <- 1
png("Manuscript/Figures/Fig03_all_transport_efficiencies.png", width = 2000 * sf, height = 1200 * sf, res = 150, bg = "transparent")
{p <- ggplot(binned.data, aes(x = Position, y = X.d)) +
    ylab("Relative displacement") +
    xlab("Proportion of head length") + 
    ggtitle("Food transport efficiency")
  p <- p + geom_boxplot(aes(x = factor(Position),
                            y = X.d),
                        fill = "tomato", 
                        outlier.color = "tomato",
                        alpha = 0.6)
  p <- p + geom_boxplot(aes(x = factor(Position),
                            y = Y.d),
                        fill = "palegreen3",
                        outlier.color = "palegreen3",
                        alpha = 0.6)
  p <- p + geom_boxplot(aes(x = factor(Position),
                            y = Z.d),
                        fill = "cornflowerblue", 
                        outlier.color = "cornflowerblue",
                        alpha = 0.6)
  p <- p + scale_x_discrete(labels = c("0", "0.1", "0.2", "0.3", "0.4",
                                       "0.5", "0.6", "0.7", "0.8", "0.9",
                                       "1", "1.1", "1.2", "1.3", "1.4"),
                            breaks = c("0", "0.1", "0.2", "0.3", "0.4",
                                       "0.5", "0.6", "0.7", "0.8", "0.9",
                                       "1", "1.1", "1.2", "1.3", "1.4"))
  p <- p + scale_y_continuous(labels = seq(0, 1, by  = 0.2),
                              breaks = seq(0, 1, by  = 0.2))
  p + theme_bw(base_size = 32) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5))
  
}
dev.off()



## FIGURE 5: MOTION CORRELATIONS ####

# plot NC.pct and major axes of motion for the whole trial
# then plot 

# read in rigid body motions from Aaron Olsen
df <- read.csv("Catfish_motion_analysis/Fit joint models/Results by trial with metadata/Nov 17 2016 Cat 01 Trial 01.csv")
start <- 513
esophagus <- 910
end <- 1228
df <- df[start:nrow(df), ]

X.d <- diff(df$Prey_bead.x)
Y.d <- diff(df$Prey_bead.y)
Z.d <- diff(df$Prey_bead.z)
Tot.d <- sqrt(X.d^2 + Y.d^2 + Z.d^2)

xyz.norm <- t(apply(cbind(X.d, Y.d, Z.d), 1, function(i) abs(i) / sum(abs(i))))


# rigid body colors 
pg <- "#C479E4"
hy <- "#EBBD3E"
lj <- "#68AD6B"
op <- "#009E97"

lwd <- 5
cex.main <- 4
cex.axis <- 2.5
sf <- 0.5
png("Manuscript/Figures/Fig04_correlations.png",
    width = 4000 * sf, height = 5000 * sf, res = 150)
par(mfrow = c(5, 1),
    mar = c(5, 8, 4, 1))
# {plot(df$time[30:nrow(df)] - df$time[1],
#       trial$NC.pct[29:nrow(df)] %>% diff * 300,
#       type = 'l', lwd = lwd, col = "darkblue",
#       xlab = "",
#       ylab = "",
#       main = "Food velocity",
#       cex.main = cex.main,
#       cex.lab = 2.2,
#       cex.axis = cex.axis,
#       panel.first = c(abline(h = 0, col = "grey", lwd = 3),
#                       abline(v = (367/300), col = "darkgrey",
#                              lwd = 3, lty = 2)))
  {plot(df$time[30:nrow(df)] - df$time[1],
       trial$NC.pct[30:nrow(df)],
       type = 'l', lwd = lwd, col = "darkblue",
       xlab = "",
       ylab = "",
       main = "Food velocity",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3),
                       abline(v = (367/300), col = "darkgrey",
                              lwd = 3, lty = 2)))
  # plot(df$time[30:nrow(df)] - df$time[1],
  #      xyz.norm[29:nrow(xyz.norm), 1],
  #      type = 'l', lwd = lwd, col = "tomato")

  plot(df$time[30:nrow(df)]- df$time[1],
       df$PostTemporalL.PectoralGirdleL.1[30:nrow(df)] * 180/(2*pi),
       ylim = c(min(df$PostTemporalL.PectoralGirdleL.1) * 180/(2*pi), 0),
       col = pg,
       type = 'l', 
       lwd = lwd,
       xlab = "",
       ylab = "",
       main = "Pectoral girdle retr.",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3),
                       abline(v = (367/300), col = "darkgrey",
                              lwd = 3, lty = 2)))
  #axis(2, at = c(-5, 0), cex.axis = cex.axis)

  plot(df$time[30:nrow(df)]- df$time[1],
       df$SuspensoriumL.HyoidL.1[30:nrow(df)] * 180/(2*pi),
       type = 'l', 
       col = hy,
       lwd = lwd,
       xlab = "",
       ylab = "",
       ylim = c(-16, 0),
       main = "Hyoid retraction",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3),
                       abline(v = (367/300), col = "darkgrey",
                              lwd = 3, lty = 2)))
  #axis(2, at =  c(-15, 0), cex.axis = cex.axis)

  plot(df$time[30:nrow(df)]- df$time[1],
       df$SuspensoriumL.LowerJawL.1[30:nrow(df)] * 180/(2*pi),
       col = lj,
       type = 'l', 
       lwd = lwd,
       xlab = "",
       ylab = "",
       ylim = c(-15, 0),
       main = "Lower jaw depression",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3),
                       abline(v = (367/300), col = "darkgrey",
                              lwd = 3, lty = 2)))
  #axis(2, at =  c(-15, 0), cex.axis = cex.axis)

  plot(df$time[30:nrow(df)]- df$time[1],
       df$SuspensoriumL.OperculumL.1[30:nrow(df)] * 180/(2*pi),
       col = op,
       type = 'l', 
       lwd = lwd,
       ylab = "",
       ylim = c(-3, 7),
       main = "Opercular abduction",
       xlab = "Time (s)",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3),
                       abline(v = (367/300), col = "darkgrey",
                              lwd = 3, lty = 2)))
  #axis(2, at =  c(-3, 0, 7), cex.axis = cex.axis)
}
dev.off()









# PLOT PHARYNGEAL PHASE
png("Manuscript/Figures/Fig05_pharyngeal_correlations.png",
    width = 4000 * sf, height = 5000 * sf, res = 150)
par(mfrow = c(5, 1),
    mar = c(5, 8, 4, 1))
{plot(df$time[30:(esophagus - start)] - df$time[1],
      trial$NC.pct[29:(esophagus - start)] %>% diff * 300,
      type = 'l', lwd = lwd, col = "darkblue",
      xlab = "",
      ylab = "Food velocity",
      cex.main = cex.main,
      cex.lab = 2.2,
      cex.axis = cex.axis,
      panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  
  plot(df$time[30:(esophagus - start)]- df$time[1],
       df$PostTemporalL.PectoralGirdleL.1[30:(esophagus - start)] * 180/(2*pi),
       ylim = c(min(df$PostTemporalL.PectoralGirdleL.1) * 180/(2*pi), 0),
       col = pg,
       type = 'l', 
       lwd = lwd,
       xlab = "",
       ylab = "Pectoral girdle retr.",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at = c(-5, 0), cex.axis = cex.axis)
  
  plot(df$time[30:(esophagus - start)]- df$time[1],
       df$SuspensoriumL.HyoidL.1[30:(esophagus - start)] * 180/(2*pi),
       type = 'l', 
       col = hy,
       lwd = lwd,
       xlab = "",
       ylim = c(-16, 0),
       ylab = "Hyoid retraction",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at =  c(-15, 0), cex.axis = cex.axis)
  
  plot(df$time[30:(esophagus - start)]- df$time[1],
       df$SuspensoriumL.LowerJawL.1[30:(esophagus - start)] * 180/(2*pi),
       col = lj,
       type = 'l', 
       lwd = lwd,
       xlab = "",
       ylim = c(-15, 0),
       ylab = "Lower jaw depression",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at =  c(-15, 0), cex.axis = cex.axis)
  
  plot(df$time[30:(esophagus - start)]- df$time[1],
       df$SuspensoriumL.OperculumL.1[30:(esophagus - start)] * 180/(2*pi),
       col = op,
       type = 'l', 
       lwd = lwd,
       ylim = c(-3, 7),
       ylab = "Opercular abduction",
       xlab = "Time (s)",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at =  c(-3, 0, 7), cex.axis = cex.axis)
}
dev.off()

# PLOT ESOPHAGEAL PHASE
png("Manuscript/Figures/Fig05_esophageal_correlations.png",
    width = 4000 * sf, height = 5000 * sf, res = 150)
par(mfrow = c(5, 1),
    mar = c(5, 8, 4, 1))
{plot(df$time[(esophagus - start):nrow(df)] - df$time[1],
      trial$NC.pct[(esophagus - start - 1):nrow(df)] %>% diff * 300,
      type = 'l', lwd = lwd, col = "darkblue",
      xlab = "",
      ylab = "Food velocity",
      cex.main = cex.main,
      cex.lab = 2.2,
      cex.axis = cex.axis,
      panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  
  plot(df$time[(esophagus - start):nrow(df)]- df$time[1],
       df$PostTemporalL.PectoralGirdleL.1[(esophagus - start):nrow(df)] * 180/(2*pi),
       ylim = c(min(df$PostTemporalL.PectoralGirdleL.1) * 180/(2*pi), 0),
       col = pg,
       type = 'l', 
       lwd = lwd,
       xlab = "",
       ylab = "Pectoral girdle retr.",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at = c(-5, 0), cex.axis = cex.axis)
  
  plot(df$time[(esophagus - start):nrow(df)]- df$time[1],
       df$SuspensoriumL.HyoidL.1[(esophagus - start):nrow(df)] * 180/(2*pi),
       type = 'l', 
       col = hy,
       lwd = lwd,
       xlab = "",
       ylim = c(-16, 0),
       ylab = "Hyoid retraction",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at =  c(-15, 0), cex.axis = cex.axis)
  
  plot(df$time[(esophagus - start):nrow(df)]- df$time[1],
       df$SuspensoriumL.LowerJawL.1[(esophagus - start):nrow(df)] * 180/(2*pi),
       col = lj,
       type = 'l', 
       lwd = lwd,
       xlab = "",
       ylim = c(-15, 0),
       ylab = "Lower jaw depression",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at =  c(-15, 0), cex.axis = cex.axis)
  
  plot(df$time[(esophagus - start):nrow(df)]- df$time[1],
       df$SuspensoriumL.OperculumL.1[(esophagus - start):nrow(df)] * 180/(2*pi),
       col = op,
       type = 'l', 
       lwd = lwd,
       ylim = c(-3, 7),
       ylab = "Opercular abduction",
       xlab = "Time (s)",
       #yaxt = "n",
       cex.main = cex.main,
       cex.lab = 2.2,
       cex.axis = cex.axis,
       panel.first = c(abline(h = 0, col = "grey", lwd = 3)))
  #axis(2, at =  c(-3, 0, 7), cex.axis = cex.axis)
}
dev.off()
