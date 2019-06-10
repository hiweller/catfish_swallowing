# Identify breakpoints in each trial and attach them to motion.list

## BOOKKEEPING ####
library(magrittr)
source("Code/00_functions.R")
phases <- c("capture", "handling", "swallowing")

# BREAKPOINTS ####

# load trial data
motion.list <- loadMotion()

# create empty breakpoint dataframe
trial.names <- lapply(motion.list, function(i) i$trial[1]) %>% unlist
bp.df <- data.frame(trial = trial.names,
                    breakpoint = rep(NA, length(motion.list)),
                    p.val = rep(NA, length(motion.list)),
                    row.names = NULL)

# for each trial, find breakpoints & associated p-values
for (i in 1:length(motion.list)) {
  bp <- findBreakpoint(motion.list[[i]], method = "all")
  bp.df$breakpoint[i] <- bp$breakpoint
  bp.df$p.val[i] <- bp$p.val
}

# Add in phase behaviors as a column to dataframes
for (i in 1:length(motion.list)) {
  df <- motion.list[[i]]
  df$phase <- rep(NA, nrow(df))
  
  # phase 1 -- lasts until the frame before breakpoint
  p1.len <- bp.df$breakpoint[i]-1
  
  swallow.idx <- which(df$Prey_bead.x >= 1)
  
  if (length(swallow.idx) == 0) {
    p2.len <- nrow(df) - bp.df$breakpoint[i] + 1
    p3.len <- 0
  } else {
    p2.len <- swallow.idx[1] - bp.df$breakpoint[i]
    p3.len <- length(swallow.idx)
  }
  
  df$phase <- c(rep(phases[1], p1.len),
                rep(phases[2], p2.len),
                rep(phases[3], p3.len))
  
}
