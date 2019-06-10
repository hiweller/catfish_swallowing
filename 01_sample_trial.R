# Plot XYZ trajectories and velocity (discrete derivative) for a sample trial

# BOOKKEEPING ####
require(strucchange)
require(magrittr)
source("Code/00_functions.R")

# load in data
trials <- trialFiles()
motion.list <- loadMotion()

# set idx for sample trial
trial.idx <- 22
trial <- motion.list[[trial.idx]]

# hardcoded vars/plotting aesthetics
xyz_col <- c("tomato", "mediumseagreen", "cornflowerblue")
lwd <- 4
fps <- 300

## Phase transitions
swallow <- which(trial$Prey_bead.x >= 1)[1] / fps
handling <- findBreakpoint(trial, method = "all")$breakpoint / fps

## Plot XYZ on the same plot ####

# plot with legend:
{
# Plot X (anterior-posterior)
plot(trial$time,
     trial$Prey_bead.x, #%>% diff * fps,
     ylim = c(-.5, 1.2),
     type = 'l', lwd = lwd, col = xyz_col[1],
     panel.first = list(abline(h = c(0, 1.0), col = "lightgrey", lwd = 3),
                        abline(v = c(swallow, handling), lty = 2, col = "darkgrey", lwd = 3)),
     xlab = "",
     ylab = expression(paste("Head lengths")),
     main = "Displacement on body axes",
     cex.main = 2,
     cex.lab = 2,
     cex.axis = 1.5)

# Add Y (dorsoventral)
points(trial$time,
       trial$Prey_bead.y,
       type = 'l', lwd = lwd, col = xyz_col[2])

# Then Z (mediolateral)
points(trial$time,
       trial$Prey_bead.z,
       type = 'l', lwd = lwd, col = xyz_col[3])

# Add a legend so I don't have to remember that
legend(x = 3.8, y = 0.9, 
       fill = xyz_col,
       legend = c("AP", "DV", "ML"))
}

## Plot speed ####

# First, calculate speed
# Extract only prey beads and time vector
food.df <- filterCols(trial, "Prey|time")

# speed is total displacement (in XYZ) over time per frame

# make new column
food.df$speed <- rep(NA, nrow(food.df))

# id prey columns
prey.idx <- grep("Prey", colnames(food.df))

# for every frame, take displacement/time
for (i in 2:nrow(food.df)) {
  
  # displacement per frame
  d <- eucDist(food.df[(i-1), prey.idx], food.df[i, prey.idx])
  
  # convert from head lengths/frame to head lengths/second
  # head lengths/frame * 300 frames/sec = head lengths/sec
  d <- d * fps
  
  food.df$speed[i] <- d
}


plot(food.df$time,
     food.df$speed,
     type = 'l', lwd = lwd,
     col = "black",
     ylab = expression("Head lengths * s"^"-1"),
     xlab = "Seconds", 
     main = "Speed",
     cex.main = 2,
     cex.lab = 2,
     cex.axis = 1.5,
     panel.first = list(abline(h = 0, col = "lightgrey", lwd = 3),
                        abline(v = c(swallow, handling), lty = 2, col = "darkgrey", lwd = 3)),
)