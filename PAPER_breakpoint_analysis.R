## Breakpoint analysis for pharyngeal phase
source("Code/functions.R")
library(magrittr)
library(strucchange)

# HEAD LENGTHS (3 measurements per fish using distance measurement tool in Maya) ####
# tip of lower jaw to esophagus
nc.length <- list(Cat01 = mean(c(69.698898, 69.874955, 69.773262)),
                  Cat02 = mean(c(69.809575, 69.501256, 70.172114)),
                  Cat05 = mean(c(74.742032, 75.916118, 75.325379)))


motion.dir <- dir("Catfish_motion_analysis/Fit joint models/Results by trial with metadata/",
                  full.names = TRUE)
motion.list <- vector("list", length = nrow(trials))
trials <- trialFiles()
names(motion.list) <- trials$trial
# FIND BREAKPOINTS ####
breakpoints <- c()
p.vals <- c()

for (i in 1:nrow(trials)) {
  
  # read in CSV and chop off frames before feeding starts
  df <- read.csv(motion.dir[grep(trials$trial[i], motion.dir)])
  df <- df[trials$start.frame[i]:nrow(df), ]
  
  # get head length
  nc <- nc.length[[grep(trials$catID[i], names(nc.length))]]
  
  # scale everything to head length
  df$Prey_bead.x <- (df$Prey_bead.x - df$Prey_bead.x[1]) / nc
  df$Prey_bead.y <- (df$Prey_bead.y) / nc
  df$Prey_bead.z <- (df$Prey_bead.z) / nc
  
  motion.list[[i]] <- df

  # get velocity on anterior-posterior axis
  velocity <- diff(df$Prey_bead.x)
  # plot(diff(df$Prey_bead.x), type = 'l', col = 'red',
  #      main = trials$trial[i])
  # points(diff(df$Prey_bead.y), type = 'l', col = 'green')
  # points(diff(df$Prey_bead.z), type = 'l', col = 'blue')
  
  # use Fstats (applies Chow's test for breakpoints in a sliding window)
  # to find breaks in velocity (i.e. suction to pharyngeal manipulation)
  fs.velocity <- Fstats(velocity ~ df$frame[2:nrow(df)],
                        from = 0.01)
  # abline(v = fs.velocity$breakpoint, lty = 2, lwd = 2, col = "grey")
  # message(trials$trial[i], "\nBreakpoint frame: ", 
  #         fs.velocity$breakpoint, "\nP-val: ", 
  #         sctest(fs.velocity)$p.value)
  
  # add to breakpoints vector
  breakpoints <- c(breakpoints, fs.velocity$breakpoint)
  p.vals <- c(p.vals, sctest(fs.velocity)$p.value)
}

# add breakpoints column to trials
trials$breakpoints <- breakpoints + 1
# Add 1 because the frames for the analysis were from velocity, which shifts the
# frame by 1 (i.e. index 1 is the velocity at frame 2, etc)

# Where in head do breakpoints occur? ####

head.bp <- data.frame(BP.x = c(),
                      BP.y = c(),
                      BP.z = c())
for (i in 1:nrow(trials)) {
  
  col.idx <- grep("Prey_bead", colnames(motion.list[[i]]))
  new.row <- c(motion.list[[i]][trials$breakpoints[i], col.idx])
  head.bp <- rbind(head.bp, new.row)
  
}

colnames(head.bp) = c("Breakpoint.X.HL",
                      "Breakpoint.Y.HL",
                      "Breakpoint.Z.HL")
#trials <- cbind(trials, head.bp)
#write.csv(trials, file = "Notes/Prey_trajectory_notes_BREAKPOINTS.csv")
plot(head.bp$Prey_bead.x, head.bp$Prey_bead.z,
     xlim = c(0, 1.2),
     asp = 1)

plot(head.bp, ylim = c(0, 1), ylab = "Pharyngeal phase onset (head length)",
     pch = 19)
abline(v = mean(head.bp$Prey_bead.x), col = "darkgrey")
abline(h = mean(head.bp) - 2*sd(head.bp), col = "red", lty = 2)
abline(h = mean(head.bp) + 2*sd(head.bp), col = "red", lty = 2)

# Controlling for individual & food type - insignificant
lm(head.bp ~ trials$catID + trials$food + 1) %>% summary

# 

# Building our table:
row.names <-  c("Time", "Head length", "Complexity", "Speed")
summary.list <- vector("list", length = length(row.names))
names(summary.list) <- row.names

phase.list <- list(summary.list, summary.list)
names(phase.list) <- c("Suction", "Handling")
{
# TIME #

# bp frame * 1 s/300 frames = bp in seconds
suction.bp.sec <- trials$breakpoints * 1/300
phase.list$Suction$Time <- suction.bp.sec

# to get handling...
esoph.frames <- unlist(lapply(motion.list, function(i) which(i$Prey_bead.x > 1)[1]))
trials$esophagus.frame <- esoph.frames
write.csv(trials, file = "Notes/Prey_trajectory_notes_BREAKPOINTS.csv")

handling.times <- c()
for (i in 1:nrow(trials)) {
  
  motion.df <- motion.list[[i]]

  start <- trials$breakpoints[i]

  if (is.na(esoph.frames[i])) {
    end <- nrow(motion.df)
  } else {
    end <- esoph.frames[i]
  }
  
  handling.times <- c(handling.times, (end-start)*1/300)
  
}
phase.list$Handling$Time <- handling.times

# LOCATION #
phase.list$Suction$`Head length` <- head.bp$Breakpoint.X.HL
phase.list$Handling$`Head length`<- head.bp$Breakpoint.X.HL

# SPEED #
# average speed
suction.speed <- c()
handling.speed <- c()
for (i in 1:length(trials$breakpoints)) {
  
  suction.df <- motion.list[[i]][1:trials$breakpoints[i], ]
  
  if (is.na(esoph.frames[i])) {
    end <- nrow(motion.list[[i]])
  } else {
    end <- esoph.frames[i]
  }
  handling.df <- motion.list[[i]][trials$breakpoints[i]:end, ]
  
  # head lengths / frame * 300 frames/s
  
  handling.speeds <- c()
  
  for (i in 2:nrow(handling.df)) {
    
    p1 <- handling.df[(i-1), 16:18]
    p2 <- handling.df[i, 16:18]
    
    if (!any(is.na(c(p1, p2)))) {
      handling.speeds <- c(handling.speeds,
                             eucDist(p1, p2))
    }
  }
  
  suction.speeds <- c()
  
  for (i in 2:nrow(suction.df)) {
    
    p1 <- suction.df[(i-1), 16:18]
    p2 <- suction.df[i, 16:18]
    
    if (!any(is.na(c(p1, p2)))) {
      suction.speeds <- c(suction.speeds,
                           eucDist(p1, p2))
    }
  }
  
  suction.speed <- c(suction.speed,
                     suction.speeds %>% mean * 300)
  handling.speed <- c(handling.speed,
                      handling.speeds %>% mean * 300)
}
phase.list$Suction$Speed <- suction.speed
phase.list$Handling$Speed <- handling.speed

# COMPLEXITY #
# Instead of using a sliding window, we'll just do it overall:
# Ratio of distance to esophagus covered to total distance traveled
for (i in 1:length(trials$breakpoints)) {
  
  suction.df <- motion.list[[i]][1:trials$breakpoints[i], ]
  
  if (is.na(esoph.frames[i])) {
    end <- nrow(motion.list[[i]])
  } else {
    end <- esoph.frames[i]
  }
  handling.df <- motion.list[[i]][trials$breakpoints[i]:end, ]
  
  prey.idx <- grep("Prey", colnames(suction.df))
  
  # SUCTION efficiency
  # distance to esophagus
  displacement <- suction.df$Prey_bead.x[nrow(suction.df)]
  d <- 0
  for (i in 2:nrow(suction.df)) {
    d <- d + eucDist(suction.df[i-1, prey.idx], suction.df[i, prey.idx])
  }
  phase.list$Suction$Complexity <- c(phase.list$Suction$Complexity, d / displacement)
  
  
  # HANDLING efficiency
  displacement <- handling.df$Prey_bead.x[nrow(handling.df)] - handling.df$Prey_bead.x[1]
  
  d <- 0
  for (i in 2:nrow(handling.df)) {
    d <- d + eucDist(handling.df[i-1, prey.idx], handling.df[i, prey.idx])
  }
  phase.list$Handling$Complexity <- c(phase.list$Handling$Complexity, d / displacement)
  
}
}

# values for the table...
(phase.list$Suction$Time * 1000) %>% mean
(phase.list$Suction$Time * 1000) %>% sd

(phase.list$Handling$Time * 1000) %>% mean
(phase.list$Handling$Time * 1000) %>% sd

phase.list$Suction$`Head length` %>% mean
phase.list$Suction$`Head length` %>% sd


phase.list$Handling$Speed %>% mean
phase.list$Suction$Speed %>% mean

phase.list$Handling$Speed %>% mean / phase.list$Suction$Speed %>% mean

phase.list$Handling$Speed %>% sd
phase.list$Suction$Speed %>% sd

phase.list$Suction$Complexity %>% mean
phase.list$Suction$Complexity %>% sd

phase.list$Handling$Complexity %>% mean
phase.list$Handling$Complexity %>% sd

phase.list$Handling$Complexity %>% mean / phase.list$Suction$Complexity %>% mean

