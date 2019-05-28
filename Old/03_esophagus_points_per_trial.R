# for each trial:


source("Code/02_esophagus_prey_bead_alignment.R")

trials <- read.table("Data/Housekeeping/trial_names.txt", sep = "\n", stringsAsFactors = FALSE)

animationDir <- dir("Data/Animations/")

for (i in 1:nrow(trials)) {
  
  trial_name <- trials[i,]
  dir_name <- gsub(trial_name, pattern = " ", replacement="_")
  
  # identify the catID
  catID <- strsplit(trial_name, split = " ")[[1]][5]
  
  # align esophagus points to NC space
  trial.aligned.pts <- alignEsophagusToPrey(trial_name)
  
  # save esophagus points to appropriate animations folder
  write.csv(x=trial.aligned.pts$esophagus[11:nrow(trial.aligned.pts$esophagus), ], file=paste("Data/Animations/", dir_name, "/NC_space_coordinates/", "Esophagus_", dir_name, ".csv", sep=""))
}
