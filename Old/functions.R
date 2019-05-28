# what it says on the tin (n-dimensional)
eucDist <- function(a, b) {
  return(sqrt(sum((a-b)^2)))
}

# isolate the X, Y, Z coordinates of a bead or beads matching a name pattern
# ex: get_bead(Cb01_strike01, 'LowerJawL_bead_cra')
get_bead <- function(df, pattern) {
  cols <- grep(names(df), pattern=paste(pattern, '*'))
  return(df[, cols])
}

# list by bead name (1 element per bead; each element contains coordinates for
# every frame of that bead) 
# ex: result <- list_bead(read.csv('path/to/marker/csv.csv'), framescol=TRUE, nan.flag=TRUE)
list_bead <- function(df, framescol=FALSE, nan.flag=TRUE) {
  
  # if there's a frames column, nix it
  if (framescol) { 
    df <- df[,-1]
    }
  
  # get unique bead names (colnames minus last two characters)
  bead_names <- unique(substr(names(df), 1, nchar(names(df))-2))
  
  # put every three columns into one element of a list
  output_list <- lapply(seq(3, ncol(df), 3), function(i) df[,(i-2):i])
  names(output_list) <- bead_names
  
  # if nan.flag is on, search for elements with any NaNs and report back
  if (nan.flag) {
    # do a search for NaNs
    all_nans <- c()
    some_nans <- c()
    for (i in 1:length(output_list)) {
      
      # count number of NaNs for bead
      nan_count <- length(which(is.nan(as.matrix(output_list[[i]]))))
      if (nan_count != 0) {
        
        # if all beads are NaN, add to all_nans pile
        # otherwise, if only some are NaN, add to some_nans
        if (nan_count == length(as.matrix(output_list[[i]]))) {
          all_nans <- c(all_nans, names(output_list)[i])
        } else { some_nans <- c(some_nans, names(output_list[i])) }
      } 
      
    }
    
    # provide info
    if (length(all_nans) == 0 & length(some_nans) == 0) {
      message("No NaNs in marker coordinates.")
    } else {
      
      message(paste("All coordinates missing:\n", paste(all_nans, collapse="\n")), sep="")
      message("\n")
      message(paste("Some coordinates missing:\n", paste(some_nans, collapse="\n")), sep="")
      
    }
    
  }
  
  return(output_list)

}

# list by frame (1 element per list; each element contains coordinates for every
# bead in that frame)
# ex: result <- list_frame(read.csv('path/to/marker/csv.csv'), framescol=FALSE, nan.flag=TRUE)
list_frame <- function(df, framescol=FALSE, nan.flag=TRUE) {
  
  # if there's a frames column, nix it
  if (framescol) { 
    df <- df[,-1]
  }
  
  # get unique bead names (colnames minus last two characters)
  bead_names <- unique(substr(names(df), 1, nchar(names(df))-2))
  
  # list by frame (takes considerably longer than going by bead since it requires matrix reshaping)
  output_list <- lapply(c(1:nrow(df)), function(i) matrix(data = df[i,], ncol=3, byrow=TRUE, dimnames=list(bead_names, c("X", "Y", "Z"))))
  
  # if nan.flag is on, search for elements with any NaNs and report back
  if (nan.flag) {
    
    bead_list <- suppressMessages(list_bead(df))
    
    # do a search for NaNs
    all_nans <- c()
    some_nans <- c()
    for (i in 1:length(bead_list)) {
      
      # count number of NaNs for bead
      nan_count <- length(which(is.nan(as.matrix(bead_list[[i]]))))
      if (nan_count != 0) {
        
        # if all bead in frame are NaN, add to all_nans pile
        # otherwise, if only some are NaN, add to some_nans
        if (nan_count == length(as.matrix(bead_list[[i]]))) {
          all_nans <- c(all_nans, names(bead_list)[i])
        } else { some_nans <- c(some_nans, names(bead_list[i])) }
      } 
      
    }
    
    # provide info
    if (length(all_nans) == 0 & length(some_nans) == 0) {
      message("No NaNs in marker coordinates.")
    } else {
      
      message(paste("All coordinates missing: \n", paste(all_nans, collapse="\n")), sep="")
      message("\n")
      message(paste("Some coordinates missing: \n", paste(some_nans, collapse="\n")), sep="")
      
    }
    
  }
  
  return(output_list)
  
  
}

# translate all coordinates in a single frame to a reference bead
# ex: list_frame_object <- list_frame(read.csv('path/to/csv.csv'))
# translated_lfo <- translate_frame(list_frame_object[[1]], "Urohyal_bead")
translate_frame <- function(frame_object, ref_bead_name) {
  
  # make sure only one frame is provided
  if (length(dim(frame_object)) != 2 | dim(frame_object)[2] != 3) {
    stop("frame_object must be a single frame with beads as rows and X Y Z as columns (i.e. a single element from list_frame output).")
  }
  
  frame_object <- as.data.frame(frame_object)
  
  # get reference bead coordinates from frame
  idx <- grep(ref_bead_name, rownames(frame_object))
  
  # only 1 bead can be used as reference (change later to allow for centroids?)
  if (length(idx) != 1) {
    if (length(idx)==0) {
      stop(paste("No bead names in frame contain ", ref_bead_name, "."))
    } else if (length(idx) > 1) {
      stop(paste("Multiple bead names match pattern:\n", paste(rownames(frame_object)[idx], collapse="\n")), sep="")
    }
    
  }
  
  # get reference row
  ref_bead <- frame_object[idx, ]
  
  # subtract ref_bead from every row
  # this is 100% the worst way to do this
  TF_beads <- matrix(unlist(apply(frame_object, 1, function(x) unlist(x)-unlist(ref_bead))), ncol=3, byrow=TRUE, dimnames=dimnames(frame_object))
  
  return(TF_beads)
  
}

# translate all coordinates for a single bead to a reference bead across all frames
translate_bead <- function(list_bead_object, translate_bead_name, ref_bead_name) {
  
  # get index of beads to be translated (can be multiple)
  t_idx <- grep(translate_bead_name, names(list_bead_object))
  r_idx <- grep(ref_bead_name, names(list_bead_object))
  
  # only 1 bead can be used as reference (change later to allow for centroids?)
  if (length(r_idx) != 1) {
    if (length(r_idx)==0) {
      stop(paste("No bead names in provided list contain ", ref_bead_name, "."))
    } else if (length(r_idx) > 1) {
      stop(paste("Multiple bead names match pattern:\n", paste(names(list_bead_object)[r_idx], collapse="\n")), sep="")
    }
  }
  
  # return list of translated beads
  if (length(t_idx) > 1) {
    translated_beads <- lapply(t_idx, function(i) list_bead_object[[i]] - list_bead_object[[r_idx]])
  } else {
    translated_beads <- list_bead_object[[t_idx]] - list_bead_object[[r_idx]]
  }
  
  return(translated_beads)
  
}

# plot a specific bead's trajectory statically in plotly

