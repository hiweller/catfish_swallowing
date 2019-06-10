# General functions for reading, formatting, and analyzing XROMM data

# BOOKKEEPING ####
# HEAD LENGTHS (avg of 3 measurements per fish using distance measurement tool
# in Maya)
# tip of lower jaw to esophagus
nc.length <- read.csv("Manuscript/Data/NC_lengths_Maya.csv")

# Euclidean distance
eucDist <- function(a, b) {
  return(sqrt(sum((a-b)^2)))
}

# READING IN DATA ###
# read in trial files/notes
trialFiles <- function() {
  # read the big guy
  trials <- read.csv("Notes/Prey_trajectory_notes.csv", row.names = 1)
  return(trials)
}

# read in RB and prey bead motion files into a list
loadMotion <- function(filepath = "Manuscript/Data/Motion data") {
  
  # get trial files
  trials <- trialFiles()
  
  # initalize empty list with one element for each trial
  motion.list <- vector("list", length=nrow(trials))
  names(motion.list) <- trials$trial
  
  # grab all motion trials (more than trial metadata -- some don't contain
  # handling or swallowing)
  motion.dir <- dir(filepath, pattern = "*.csv", full.names = TRUE)
  
  for (i in 1:nrow(trials)) {
    
    # find the right motion.dir file
    idx <- grep(trials$trial[i], tools::file_path_sans_ext(motion.dir))
    
    # read in motion file, chopping off frames before food enters mouth
    motion.file <- read.csv(motion.dir[idx])[-c(1:trials$start.frame[i]-1), ]
    motion.file$time <- motion.file$time - motion.file$time[1]
    
    # get head length from nc.length file
    hl <- nc.length[1, match(trials$catID[i], colnames(nc.length))]
    
    # divide prey beads by head length to get measurements in head length
    motion.file$Prey_bead.x <- (motion.file$Prey_bead.x - 
                                  motion.file$Prey_bead.x[1]) / hl
    motion.file$Prey_bead.y <- motion.file$Prey_bead.y / hl
    motion.file$Prey_bead.z <- motion.file$Prey_bead.z / hl
    
    # add into list
    motion.list[[i]] <- motion.file
  }
  
  return(motion.list)
}

# SORTING DATA ####

# Pull out columns matching a naming pattern
# Ex: filterCols(loadMotion()[[i]], "Prey")
filterCols <- function(df, pattern) {
  cols <- grep(names(df), pattern=paste(pattern))
  return(df[, cols])
}

# ANALYZING DATA ###

# Find breakpoints
findBreakpoint <- function(df = loadMotion()[[22]],
                            method = "x") {
  
  # if method is "x", take velocity in x direction
  if (method == "x") {
    v <- diff(df$Prey_bead.x)
  } else if (method == "all") { # otherwise do speed
    prey.idx <- grep("Prey", colnames(df))
    v <- sapply(2:nrow(df), function(i) eucDist(df[(i-1), prey.idx],
                                                df[i, prey.idx]))
  } else {
    stop("Method must be either 'x' (for X velocity alone) or 'all' (for overall speed)")
  }
  
  # find breakpoint
  bp <- strucchange::Fstats(v ~ df$frame[2:nrow(df)],
                            from = 0.01)
  
  # return list with breakpoint and associated p value
  list(breakpoint = bp$breakpoint + 1,
             p.val = strucchange::sctest(bp)$p.value)
  
  
}
