library(matools)

# catfish <- alignCatfishEsophagus(catID="01"); catfish$plot
alignCatfishEsophagus <- function(catID) {
  
  # read in ROI points
  NC.pts <- readROISeries(paste("Data/CT_locators/Point_ROIs/Cat", 
                                catID, "_esophagus_NC_landmarks.rois_series", sep=""))
  
  NC.ref <- readROISeries(paste("Data/CT_locators/Point_ROIs/Cat",
                          catID, "_reference_NC_landmarks.rois_series", sep=""))
  
  esophagus.pts <- readROISeries(paste("Data/CT_locators/Point_ROIs/Cat",
                                 catID, "_esophagus_landmarks.rois_series", sep=""),
                                 name.min.char = 6)
  
  # get rid of any rows that don't include the word 'Point'
  esophagus.pts <- esophagus.pts[grep("Point", rownames(esophagus.pts)), ]
  
  # change rownames to something i can tolerate
  rownames(esophagus.pts) <- c(1:nrow(esophagus.pts))
  
  # align coordinates from esophagus scan to the points from the OBJ scan
  alignment <- bestAlign(NC.ref, rbind(NC.pts, esophagus.pts))
  
  # put all points into a single dataframe for plotting, including a column for
  # origin and type of point
  all.pts <- as.data.frame(cbind(rbind(NC.ref, alignment$mat),
                                 c(rep("Reference", nrow(NC.ref)), 
                                   rep("NC", nrow(NC.pts)),
                                   rep("Esophagus", nrow(esophagus.pts)))))
  
  colnames(all.pts) <- c("x", "y", "z", "PointType")
  rownames(all.pts)[1:(nrow(NC.ref))] <- sapply(c(1:nrow(NC.ref)), function(i) paste("NC.ref", i, sep="."))
  rownames(all.pts)[(nrow(NC.ref)+1):(nrow(NC.ref) + nrow(NC.pts))] <- sapply(c(1:nrow(NC.pts)), function(i) paste("NC", i, sep="."))
  
  # plot reference points, aligned points, and best-fit plane
  x <- alignment$mat[(nrow(NC.pts)+1):nrow(alignment$mat),1]
  y <- alignment$mat[(nrow(NC.pts)+1):nrow(alignment$mat),2]
  z <- alignment$mat[(nrow(NC.pts)+1):nrow(alignment$mat),3]
  ff <- lm(z~x+y)
  cf.mod <- coef(ff)
  x.seq <- seq(min(x),max(x),length.out=231)
  y.seq <- seq(min(y),max(y),length.out=231)
  z.mtx <- t(outer(x.seq, y.seq, function(x,y) 
    cf.mod[1]+cf.mod[2]*x+cf.mod[3]*y))
  
  p <- plot_ly(x=~x.seq, y=~y.seq, z=~z.mtx,
               type="surface",
               opacity = 0.7, colors="cornflowerblue") %>%
    add_trace(data=all.pts,
              x=~x, y=~y, z=~z,
              mode="markers",
              color=~PointType,
              type="scatter3d")

  return(list(plot=p,
              NC.ref=NC.ref,
              aligned.pts=alignment$mat))
}

loadPharyngealMarkers <- function(catID) {
  
  # read in ROI points
  pts <- readROISeries(paste("Data/CT_locators/Point_ROIs/Cat", 
                                catID, "_Pharyngeal_landmarks.rois_series", sep=""),
                                name.min.char = 9)
  
  NC <- pts[grep("neurocranium", rownames(pts)), ]
  Esoph <- pts[grep("Esophagus", rownames(pts)), ]
  
  head.length <- dist(rbind(NC, Esoph))
  
  PJ <- pts[grep("PJ", rownames(pts)), ]
  d <- c()
  for (j in 1:nrow(PJ)) {
    
    d <- c(d, (dist(rbind(NC, PJ[j, ])) / head.length))
    
  }
  
  return(range(d))
}

loadPharyngealMarkers("01")
