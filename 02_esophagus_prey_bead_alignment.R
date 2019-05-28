source("Code/01_catfish_esophagus_NC_alignment.R")

trials <- read.table("Data/Housekeeping/trial_names.txt", sep = "\n", stringsAsFactors = FALSE)
tmats <- "Data/RMA/Transformations RT Neurocranium/"
prey.beads <- "Data/RMA/3D Points smoothed RT Neurocranium/"
prey.tank.space <- "Data/RMA/3D Points smoothed/"

# aligns points to prey bead space:
# 1) load prey points in NC space
# 2) load transformation matrix for NC
# 3) load esophagus points in NC reference space
# 4) transform esophagus points from NC reference space to NC space by applying the same transformation matrix that was applied to NC itself
par(mfrow=c(2, 1), mar=rep(2, 4))
alignEsophagusToPrey <- function(trial_name) {
  
  # get catID
  catID <- strsplit(trial_name, " ")[[1]][5]
  
  # get esophagus in NC space
  esophagus <- alignCatfishEsophagus(catID=catID)$aligned.pts
  
  # read in transformation matrix of neurocranium from a file
  tmat <- readMotion(paste(tmats, trial_name, ".csv", sep=""), nrow=2)
  tmat <- tmat$tmat[,,"Neurocranium",1]
  
  # read in prey bead coordinates
  prey.pts <- read.csv(paste(prey.beads, trial_name, ".csv", sep=""))
  
  # apply transformation matrix to esophagus points
  # esophagus points in CT space --> NC space for trial
  esophagus.NC.space <- matools::applyTransform(esophagus, tmat = tmat)
  
  # convert to animation by turning into an array with one slice/frame
  # library(svgViewR)
  # svg.new(file=paste(trial_name, ".html", sep=""), mode="webgl")
  # svg.spheres(prey.pts, col=colorRampPalette(c("green", "red"))(nrow(prey.pts)))
  # svg.spheres(esophagus.NC.space[1:10,], col="cornflowerblue")
  # svg.spheres(esophagus.NC.space[11:nrow(esophagus.NC.space),], col="orchid")
  # svg.close() # ~=dev.off()
  
  # # find best-fit plane for esophagus using coefficient of linear model
  # esophagus.pts <- esophagus.NC.space[11:nrow(esophagus.NC.space),]
  # es.plane <- lm(esophagus.pts[,1]~esophagus.pts[,2]+esophagus.pts[,3])
  # es.cf <- coef(es.plane)
  # normal.vector <- matrix(ncol=1, c(es.cf[2], es.cf[3], 1))
  # q <- predict
  # # find centroid of points
  # es.ctr <- apply(esophagus.pts, 2, mean)
  # 
  # # find plane distance and centroid distance
  # plane.dist <- apply(prey.pts[,1:3], 1, function(i) distPointToPlane(i, normal.vector, es.ctr))
  # centroid.dist <- apply(prey.pts[,1:3], 1, function(i) sqrt(sum(i-es.ctr)^2))
  # 
  # # plot them
  # plot(plane.dist,  pch=20, col="cornflowerblue",
  #      xlab="Frame", ylab="Distance from esophagus (mm)",
  #      main=paste(trial_name, "(plane)"))
  # abline(h=0, col="red", lty=2, lwd=1.5)
  # 
  # plot(centroid.dist,  pch=20, col="goldenrod",
  #      xlab="Frame", ylab="Distance from esophagus (mm)",
  #      main=paste(trial_name, "(centroid)"))
  # abline(h=0, col="red", lty=2, lwd=1.5)
  
  return(list(esophagus=esophagus.NC.space,
              prey=prey.pts[,1:3]))
}

#for (i in 1:nrow(trials)) {
#  temp <- alignEsophagusToPrey(trials[i,])
#}