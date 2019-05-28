# overall path to project
project_directory <- '~/Dropbox/Brainerd_Lab/Projects/Catfish_swallowing/'

# source of combined rigid body transformation matrix files
origin <- paste(project_directory, 'Data/RMA/3D Points smoothed RT Neurocranium/', sep="")

# eventual destination for separated transforms
destination <- paste(project_directory, "Data/Trials/", sep="")

run <- function(origin, destination) {
  origin_dir <- dir(origin, full.names=F)
  
  for (i in origin_dir) {
    prey_bead <- read.csv(paste(origin, "/", i, sep=""))[,1:3]
    
    # get the trial name and replace spaces with underscores to be nice to bash later
    # i don't have a good explanation for why i named it foo
    foo <- gsub(" ", "_", unlist(strsplit(i, "[.]"))[1])
    
    # define home directory
    home <- paste(destination, foo, "/NC_space_coordinates/", sep="")
    
    # if any part of the path does not exist, generate folders
    if (!dir.exists(home)) {
      dir.create(home, recursive = T)
    }
    
    write.csv(x=prey_bead, file=paste(home, "Prey_bead_", foo, ".csv", sep=""), row.names = FALSE)
  }
}

run(origin, destination)

for (i in dir(destination, full.names = F)) {
  new_file <- paste(destination, i, "/Maya_animations/", i, "_nc_space.ma", sep="")
  if (!dir.exists(new_file)) {
    #dir.create(new_folder, recursive=TRUE)
    file.create(new_file)
  }
}
