# overall path to project
project_directory <- '~/Dropbox/Brainerd_Lab/Projects/Catfish_swallowing/'

# source of combined rigid body transformation matrix files
origin <- paste(project_directory, 'Data/RMA/Transformations RT Neurocranium/', sep="")

# eventual destination for separated transforms
destination <- paste(project_directory, "Data/Trials/", sep="")

run <- function(origin, destination) {
  origin_dir <- dir(origin, full.names=F)
  
  # for every file in origin folder
  for (i in origin_dir) {
    
    # get the trial name and replace spaces with underscores to be nice to bash later
    # i don't have a good explanation for why i named it foo
    foo <- gsub(" ", "_", unlist(strsplit(i, "[.]"))[1])
    
    # define home directory
    home <- paste(destination, foo, "/NC_space_coordinates/", sep="")
    
    # if any part of the path does not exist, generate folders
    if (!dir.exists(home)) {
      dir.create(home, recursive = T)
    }
    
    # load combined rigid body transform CSV
    raw <- read.csv(paste(origin, "/", i, sep=""))
    ids <- colnames(raw)
    
    # find RBT names ("Hyoid*", etc)
    names <- 
      unique( # find all of the unique instances
        unlist( # in a repeating vector
          lapply( # of the strings the precede the underscores in each column name
            strsplit(ids, "_"), 
            function(x) x[1])))
    
    # for every element, find the columns that start with that rigid body name,
    # and store them as a separate matrix
    for (i in 1:length(names)) {
      write.csv(x=raw[ , grep(names[i], ids)], file=paste(home, "/", names[i], "_RigidBodyMatrix_", foo, ".csv", sep=""), row.names = FALSE)
    }
  }
}

run(origin, destination)