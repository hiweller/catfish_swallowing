



metadata_files <- dir("Catfish_motion_analysis/Fit joint models/Results by trial with metadata/", 
                      pattern = ".csv",
                      full.names = TRUE)







for (i in 1:length(metadata_files)) {
  
  trial <- read.csv(metadata_files[i])
  
  
  
} 






esoph.idx <- which(!is.na(trials$esophagus.frame))






par(mfrow = c(3, 1))
for (i in 1:length(esoph.idx)) {
  
  trial.row <- trials[esoph.idx[i], ]
  
  file.idx <- grep(trial.row$trial, metadata_files)
  
  trial <- read.csv(metadata_files[file.idx])
  
  # plot(trial$PostTemporalL.PectoralGirdleL.1[trial.row$esophagus.frame:nrow(trial)],
  #      type = 'l',
  #      main = trial.row$trial,
  #      xlab = "Frame",
  #      ylab = "PG motion")
  # abline(v = trial.row$esophagus.frame, lty = 2)
  # plot(trial$Prey_bead.x[trial.row$esophagus.frame:nrow(trial)] %>% diff,
  #      type = 'l',
  #      col = "cornflowerblue",
  #      xlab = "Frame",
  #      ylab = "Food (AP axis)")
  # abline(v = trial.row$esophagus.frame, lty = 2)
  # 
  ccf(trial$PostTemporalL.PectoralGirdleL.1[trial.row$esophagus.frame:nrow(trial)] %>% diff,
      trial$Prey_bead.x[trial.row$esophagus.frame:nrow(trial)] %>% diff, 
      na.action = na.pass)
  
  ccf(trial$PostTemporalL.PectoralGirdleL.1[(trial.row$start.frame + trial.row$prey.handling.start.frame):trial.row$esophagus.frame] %>% diff,
      trial$Prey_bead.x[(trial.row$start.frame + trial.row$prey.handling.start.frame):trial.row$esophagus.frame] %>% diff, 
      na.action = na.pass)
  
  # ccf(trial$SuspensoriumL.HyoidL.1[trial.row$esophagus.frame:nrow(trial)] %>% diff,
  #     trial$Prey_bead.x[trial.row$esophagus.frame:nrow(trial)] %>% diff, 
  #     na.action = na.pass)
  
  ccf(trial$SuspensoriumL.LowerJawL.2[trial.row$esophagus.frame:nrow(trial)] %>% diff,
      trial$Prey_bead.x[trial.row$esophagus.frame:nrow(trial)] %>% diff, 
      na.action = na.pass)
  
  
  colordistance:::pause()
  
}

for (i in 1:length(metadata_files)) {
  df <- read.csv(metadata_files[i])
  plot(df$PostTemporalL.PectoralGirdleL.1,
       type = 'l',
       col = 'black',
       lwd = 2,
       main = basename(metadata_files[i]),
       xlab = "Frame",
       ylab = "Motion")
  plot(df$Prey_bead.x,
         type = 'l',
         col = 'tomato',
         lwd = 2,
       xlab = "Frame",
       ylab = "Motion")
  colordistance:::pause()
}

df <- read.csv("Catfish_motion_analysis/Fit joint models/Results by trial with metadata/Nov 17 2016 Cat 01 Trial 01.csv")
trials <- trialFiles()

# find the trials with an esophageal phase
# find length of the longest esophageal phase (NA padding for later)
# stack esophageal phases together with NA padding
# run correlation matrix between prey X (AP), Y (DV), and Z (ML) coordinates
  # and on velocities
  # and accelerations
# look at distribution of correlations in different phases

for(i in 1:sum(!is.na(trials$esophagus.frame))) {
  # for every esophageal phase
  # concatenate 
}
