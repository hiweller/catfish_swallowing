







#### UTILITIES ####
setwd('~/Dropbox/Brainerd_Lab/Projects/Catfish_swallowing/Analysis/Output/')
source('~/Dropbox/Brainerd_Lab/Projects/Catfish_swallowing/Code/functions.R')
source("/Users/hannah/Dropbox/Brainerd_Lab/Projects/Catfish_swallowing/Code/Hannah_code/R functions.R")
# graph parameters that suit my typical window layout
par(mar=c(3, 5, 3, 5))

#### LIBRARIES ####
library(RColorBrewer)
library(scatterplot3d)
library(plotly)
library(strucchange)

#### LOAD MARKER COORDINATES INTO LIST ####

markers <- read.csv('../Knifefish/XMAtrials/Marker_coordinates/20171110_Cb01_strike03_prey_body_beads_only.csv')

kf_frame <- list_frame(markers, framescol = T)
kf_bead <- list_bead(markers, framescol = T)

kf_first_coords <- translate_frame(kf_frame[[1]], ref_bead_name="Body_bead_01")
prey_bead_ref <- translate_bead(kf_bead, "Prey_bead", "Body_bead_01")

colorGradient <- colorRampPalette(c("green", "red"))

beadnames <- c(rownames(kf_first_coords), as.character(c(1:nrow(prey_bead_ref))))

plot_ly(as.data.frame(kf_first_coords), type="scatter3d", x=~X, y=~Y, z=~Z, mode="markers", marker=list(size=4), text=beadnames[1:10]) %>%
  add_markers(x=prey_bead_ref$Prey_bead_X, y=prey_bead_ref$Prey_bead_Y, z=prey_bead_ref$Prey_bead_Z, marker=list(color=colorGradient(2000)), text=beadnames[11:2010]) %>% 
  layout(xaxis=list(), yaxis=list(scaleanchor="x"))

## plot every catfish


# list of marker files
CSVdir <- dir('XYZcoordinate_CSVs/', full.names = T)

# read in every file into a big list named by file
marker_list <- lapply(1:length(CSVdir), function(x) read.csv(CSVdir[x]))
names(marker_list) <- lapply(strsplit(CSVdir, split = "/"), function(x) x[3])

for (i in 1:length(marker_list)) {
  
  df <- marker_list[[i]]
  print(names(marker_list)[i])
  df_frame <- list_frame(df, framescol=T, nan.flag = F)
  df_bead <- list_bead(df, framescol=T, nan.flag = F)
  
  first_coords <- translate_frame(df_frame[[1]], ref_bead_name="Neurocranium_bead_cra_L")
  
  if(length(which(is.nan(unlist(df_bead$Prey_bead[,1]))))==nrow(df_bead$Prey_bead)) {message("No prey bead; skipping")} else {
    
    prey_bead_ref <- translate_bead(df_bead, "Prey_bead", "Neurocranium_bead_cra_L")
    
    colorGradient <- colorRampPalette(c("green", "red"))
    
    beadnames <- c(rownames(first_coords), as.character(c(1:nrow(prey_bead_ref))))
    
    p <- plot_ly(as.data.frame(first_coords), type="scatter3d", x=~X, y=~Y, z=~Z, mode="markers", marker=list(size=4), text=beadnames[1:length(rownames(first_coords))]) %>%
      add_markers(x=prey_bead_ref$Prey_bead_X, y=prey_bead_ref$Prey_bead_Y, z=prey_bead_ref$Prey_bead_Z, marker=list(color=colorGradient(nrow(prey_bead_ref))), text=beadnames[(length(rownames(first_coords))+1):length(beadnames)]) %>% 
      layout(xaxis=list(), yaxis=list(scaleanchor="x"))
    
    print (p)
    pause()
  }
}

#### GET PREY LOCATIONS AND PLOT IN 3D ####


# goal: plot a set of head marker coordinates and the path that the prey bead takes relative to all of them
# 1: pick a reference point to move to the origin; get translation matrix for first set of coordinates (all from frame 1)

test <- marker_list[[1]][,-1]
test <- read.csv('../Knifefish/XMAtrials/Marker_coordinates/20171110_Cb01_strike03_prey_body_beads_only.csv')[,-1]

# reference point coordinates:
# should be 1752 x 3
refname <- "Body_bead_01"
ref <- get_bead(test, pattern=refname)

# first set of head coordinates
# get list of all unique beads for naming sanity
bead_names <- unique(substr(names(test), 1, nchar(names(test))-2))
first_coords <- as.data.frame(matrix(data = unlist(test[1,]), ncol=3, byrow=TRUE))
rownames(first_coords) <- bead_names

# translation matrix should just be coordinate of reference bead subtracted from every bead in set?

# reference vector (1x3)
firstref <- first_coords[which(rownames(first_coords) %in% refname), ]

# translation matrix for first set of markers
# if row is not NaN (no coords), subtract marker reference from point; otherwise leave it to maintain row order
# reshape into matrix with 3 columns for plotting and take names
TM_markers <- matrix(unlist(apply(first_coords, 1, function(x) if (!is.nan(x[1])){x-firstref} else{x})), ncol=3, byrow=TRUE)
colnames(TM_markers) <- colnames(first_coords)
rownames(TM_markers) <- rownames(first_coords)
TM_markers <- TM_markers[!is.nan(TM_markers[,1]),]
# color by rigid body
# haha bodycount
rigidBodyCount <- unlist(unique(lapply(strsplit(rownames(TM_markers), split="_"), function(x) x[1])))

# vector of who gets what color
colorList <- cbind(rigidBodyCount, colorRampPalette(brewer.pal(11, "Spectral"))(length(rigidBodyCount)))

# count how many times we need each one/in what order for plotting
vec <- sapply(rigidBodyCount, function (i) grep(pattern = i, x = rownames(TM_markers)))

# make color vector and append it to marker list
colorVector <- unlist(sapply(c(1:length(rigidBodyCount)), function(i) rep(colorList[i, 2], length(vec[[i]]))))

colSums(test)/ncol(test)

# 2: get translation matrix for prey bead relative to each frame of reference point
prey_bead <- get_bead(test, pattern='Prey_bead')
ref <- get_bead(test, pattern="Body_bead_01")
prey_bead_ref <- prey_bead-ref

plot_ly(as.data.frame(TM_markers), type="scatter3d", x=~V1, y=~V2, z=~V3, mode="markers", marker=list(color=colorVector, size=4)) %>%
  add_markers(x=prey_bead_ref$Prey_bead_X, y=prey_bead_ref$Prey_bead_Y, z=prey_bead_ref$Prey_bead_Z, marker=list(color=colorGradient(2000))) %>% 
  layout(xaxis=list(), yaxis=list(scaleanchor="x"))

plot_ly(as.data.frame(first_coords), type="scatter3d", x=~V1, y=~V2, z=~V3, mode="markers", marker=list(color=colorVector, size=4), text=rownames(first_coords)) 

plot_ly(prey_bead_ref, type="scatter3d", x=~Prey_bead_X, y=~Prey_bead_Y, z=~Prey_bead_Z, mode="markers", marker=list(color=colorGradient(1752), size=4))

plot_ly(prey_bead, type="scatter3d", x=~Prey_bead_X, y=~Prey_bead_Y, z=~Prey_bead_Z, mode="markers", marker=list(color=colorGradient(1752), size=4))



## COMPOST HEAP!!! recycle this garbage code at will
# haha bodycount
rigidBodyCount <- unlist(unique(lapply(strsplit(bead_names, split="_"), function(x) x[1])))
colorList <- cbind(rigidBodyCount, colorRampPalette(brewer.pal(8, "Spectral")(length(rigidBodyCount))))
vec <- sapply(rigidBodyCount, function (i) grep(pattern = i, x = bead_names))
colorVector <- unlist(sapply(c(1:length(rigidBodyCount)), function(i) rep(colorList[i, 2], length(vec[[i]]))))

bead_list <- vector("list", length(bead_names))
names(bead_list) <- bead_names
# plot first of each coordinate for bead markers
for (i in 1:length(bead_names)) {
  bead_coords <- grep(paste(bead_names[i], '_*', sep=''), names(test))
  bead_list[[i]] <- test[, bead_coords]
}

first_coords <- test[1,]
test2 <- as.data.frame(matrix(data = first_coords, nrow = length(bead_names), ncol=3, byrow=TRUE))
rownames(test2) <- bead_names
test2$color <- colorVector


p <- plot_ly(prey_bead, type="scatter3d", x=~Prey_bead_X, y=~Prey_bead_Y, z=~Prey_bead_Z, mode="markers", marker=list(color=colorGradient(1752))) 

# find prey columns
prey_cols <- grep(pattern="Prey_bead_*", names(test))
epax_cols <- grep(pattern="Epaxial_bead_mid1_dor_*", names(coords))
prey_bead <- test[ , prey_cols]
epax_bead <- test[ , epax_cols]

prey <- prey_bead# - epax_bead

plot_ly(test2, type="scatter3d", x=~V1, y=~V2, z=~V3, mode="markers", marker=list(color=test2$color, size=5)) %>%
  add_markers(x=prey$Prey_bead_X, y=prey$Prey_bead_Y, z=prey$Prey_bead_Z, marker=list(color=colorGradient(1752)))



# Vector storage facility
prey_distance_vectors <- vector("list", length(marker_list))
names(prey_distance_vectors) <- names(marker_list)
gape <- vector("list", length(marker_list))

# calculate distance of prey to selected marker
for (i in 1:length(marker_list)) {
  coords <- marker_list[[i]]
  
  # find the column numbers for prey and epax beads
  prey_cols <- grep(pattern="Prey_bead_*", names(coords))
  epax_cols <- grep(pattern="Epaxial_bead_mid2_ven_*", names(coords))
  
  # if bead coordinates exist for both points, get euclidean distances
  if (length(prey_cols)==3 & length(epax_cols)==3) {
    prey <- cbind(coords[, prey_cols])
    ep <- cbind(coords[, epax_cols])
    
    prey_ep_vec <- sapply(c(1:dim(prey)[1]), function(x) eucDist(prey[x,], ep[x,]))
    
  } else {message(paste("No prey bead for", names(marker_list)[i], "; skipping"))}
  
  prey_distance_vectors[[i]] <- prey_ep_vec
}

# clean up list - get rid of empty elements, NaNs
prey_distance_vectors[sapply(prey_distance_vectors, is.null)] <- NULL
preydist <- prey_distance_vectors
preydist[sapply(preydist, function(x) length(x)==0)] <- NULL
preydist <- lapply(preydist, function(x) x <- x[!is.nan(x)])

# get difference/velocity vectors
preydiff <- lapply(preydist, function(x) diff(x)*300)

# index
idx <- unlist(lapply(preydiff, function(x) match(min(x, na.rm = T), x)))

par(mfrow=c(2, 1), mar=c(2, 5, 2, 5))
plot(preydist[[1]][275:900], col='coral', type='l', lwd=2, ylab='Prey distance from epaxial (cm)', xlab='Frame')
abline(v=22, lty=2)
abline(v=35, lty=2)
abline(v=180, lty=2)
abline(v=220, lty=2)
plot(smooth.spline(preydiff[[1]][275:900], spar=0.0001), col='royalblue', lwd=2, type='l', ylab='Prey velocity (cm/s)', ylim=c(-110, 50))
abline(h=0, lty=2, col='red')
abline(v=22, lty=2)
abline(v=35, lty=2)
abline(v=180, lty=2)
abline(v=220, lty=2)
names(preydiff)[1]

par(mfrow=c(2, 1), mar=c(3, 5, 3, 5))
p <- ggplot(data=data.frame(Prey=preydist[[1]][275:900], Frame=c(1:626)), aes(x=Frame, y=Prey))
p <- p + geom_line(col='coral', lwd=1.1) + ylab('Prey distance from epaxial (cm)')
p <- p + geom_vline(xintercept=c(22, 35, 180, 220), col='darkgrey'); p

p <- ggplot(data=data.frame(Prey=smooth.spline(preydiff[[1]][274:899], spar=0.0001)$y, Frame=c(1:626)), aes(x=Frame, y=Prey)) + ylim(low=-110, high=50)
p <- p + geom_line(col='royalblue', lwd=1.1) + ylab('Prey velocity (cm/s)')
p <- p + geom_vline(xintercept=c(22, 35, 180, 220), col='darkgrey') + geom_hline(yintercept=0, lty=2, col='red', lwd=1.2); p


test <- smooth.spline(preydiff[[1]][274:899], spar=0.0001)
plot(smooth.spline(preydiff[[1]], spar=0.0001), type='l')

fake_vector <- c(rep(NA, 309), preydiff[[1]][310:900])
plot(fake_vector, col='royalblue', type='b', pch=20, ylab='Prey velocity (cm/s)')
points(preydiff[[1]][1:310], type='l', col='royalblue')














for (i in 1:length(preydist)) {
  preydist[[i]] <- preydist[[i]][!is.nan(preydist[[i]])]
}




lapply(preydist, function(x) plot(x, type='l', col='royalblue'))

for (i in 1:length(preydist)) {
  
  t <- idx[i]+5
  par(mfrow=c(2,1))
  
  plot(preydist[[i]][t:length(preydist[[i]])], type='l', col='coral', ylab='Prey distance (cm)', xlab='Frame')
  
  plot(preydiff[[i]][t:length(preydiff[[i]])], type='l', col='royalblue', ylab='Prey velocity (cm/s)', xlab='Frame')
  abline(h=0, lty=2, col='red')
  
  pause()
  #points(preydiff[[i]][t:length(preydiff[[i]])], type='l', col=i)
  
}

test <- as.data.frame(preydiff)

plot(preydiff[[1]][idx[1]:length(preydiff[[1]])], type='l', col='royalblue', ylab='Prey velocity (cm/s)', xlab='Frame')
abline(h=0, lty=2, col='red')

maxlength <- max(unlist(lapply(preydiff, length)))
sapply(c(1:length(preydiff)), function(x) length(preydiff[[x]][idx[x]:length(preydiff[[x]])]))
max(idx)

preydiff_df <- matrix(NA, ncol=maxlength*2, nrow=length(preydiff))




colnames(prey) <- c("Frame", "X", "Y", "Z")
prey$Frame <- prey$Frame/dim(prey[1])

colorGradient <- colorRampPalette(c("#e5f5fc", "#00308c"), bias=T)

plot_ly(prey, type="scatter3d", x=~X, y=~Y, z=~Z, mode="markers", marker=list(color=colorGradient(1752)))


# compared to neurocranium?
prey_nc_ref <- prey[,2:4]-nc[,2:4]
plot_ly(prey_nc_ref, type="scatter3d", x=~X, y=~Y, z=~Z, mode="markers", marker=list(color=colorGradient(1752)))


prey_nc_dist <- sapply(c(1:dim(prey)[1]), function(x) eucDist(prey[x, 2:4], nc[x, 2:4]))

prey_nc_ref <- data.frame(Frame=c(1:1752), Distance=prey_nc_dist)
plot(prey_nc_ref, pch=20, col="royalblue", type='b')

# poor man's derivative = subtract value from subsequent value
prey_nc_ref$Derivative <- c(NA, diff(prey_nc_ref$Distance))
plot(prey_nc_ref$Frame[250:350], prey_nc_ref$Derivative[250:350], type='l')

plot(prey_nc_ref$Frame[350:1752], prey_nc_ref$Derivative[350:1752], type='l')

plot(prey_nc_ref$Frame, prey_nc_ref$Derivative, type='l')

plot(c(1:1752), test, type='l')
fit.loess <- loess.smooth(c(1:1752), test, span=0.01)
plot(fit.loess, type='l', lwd=2, col="red")
points(c(1:1752), test, pch=20, col="blue")


#### METHODS OF PLOTTING PREY TRAJECTORY ####

CSVdir <- dir('XYZcoordinate_CSVs/')

par(mfrow=c(2,1), mar=c(3, 5, 3, 5))
prey_vectors <- vector("list", length=length(CSVdir))
prey_dist <- prey_vectors
names(prey_vectors) <- CSVdir
for (i in 1:length(CSVdir)) {
  df <- read.csv(paste('XYZcoordinate_CSVs/', CSVdir[i], sep=''))
  df_bead <- list_bead(df, framescol=T, nan.flag = F)
  df_frame <- list_frame(df, framescol = T, nan.flag = F)
  prey_bead_ref <- as.data.frame(translate_bead(df_bead, "Prey_bead", "Epaxial_bead_mid1_dor"))
  first_coords <- as.data.frame(translate_frame(df_frame[[1]], ref_bead_name = "Epaxial_bead_mid1_dor"))
  colorGradient <- colorRampPalette(c("green", "red"))
  
  p <- plot_ly(as.data.frame(prey_bead_ref), x=~Prey_bead_X, y=~Prey_bead_Y, z=~Prey_bead_Z, type="scatter3d", mode="markers", marker=list(size=4, color=colorGradient(nrow(prey_bead_ref)))) %>% add_markers(first_coords, x=~first_coords$X, y=~first_coords$Y, z=~first_coords$Z, type="scatter3d", mode="markers", marker=list(size=4, color="royalblue"), text=rownames(first_coords))
  print(p)
  #test <- apply(prey_bead_ref, 1, function(x) eucDist(x, c(0,0,0)))
  
#  plot(test, type='l', col="coral")
#  plot(diff(test), type='l', col="royalblue", lwd=1.5)
#  abline(h=0)
  
  prey_vectors[[i]] <- diff(test)
  prey_dist[[i]] <- test
}

# animations
p <- prey_bead_ref %>% plot_ly(x=~Prey_bead_X, y=~Prey_bead_Y, z=~Prey_bead_Z, type="scatter3d", mode="markers", frame=c(1:nrow(prey_bead_ref)))

# align by peaks?
lapply(prey_vectors, min)

test <- prey_dist[[1]]

window

plot(prey_vectors[[1]], type='l', ylab="Prey velocity", col="royalblue", lwd=1.5)
abline(h=0, col="red", lty=2)


#### STRUCTURAL BREAKS ####
v <- prey_dist[[1]]
plot(v, type='l', col="royalblue", lwd=1.5, main="Prey distance from epax", ylab="Distance", xlab="Frame")
plot(diff(v), type='l', col="tomato")

par(mfrow=c(2,1))
starts <- lapply(prey_vectors, function(x) which(x %in% min(x, na.rm=T)))

# for every vector, take only starting point and forward
chopped_v <- lapply(1:length(starts), function(i) prey_dist[[i]][starts[[i]]:length(prey_vectors[[i]])])
chopped_d <- lapply(1:length(starts), function(i) prey_vectors[[i]][starts[[i]]:length(prey_vectors[[i]])])

for (i in 1:length(prey_vectors)) {
  
  v <- ts(chopped_d[[i]])
  b <- breakpoints(v~c(1:length(v)))
  
  
  plot(chopped_d[[i]], type='l', col="royalblue", lwd=1.5, main=paste(i))
  lines(b)
  #abline(v=which(prey_vectors[[i]] %in% min(prey_vectors[[i]], na.rm=T)), lty=2)
  #plot(prey_vectors[[i]], type='l', col="tomato", lwd=1.5, main=paste(i))
  #abline(v=which(prey_vectors[[i]] %in% min(prey_vectors[[i]], na.rm=T)), lty=2)
  pause()
}

v <- ts(chopped_v[[1]])
b <- breakpoints(v~c(1:length(v)))
plot(b)
lines(b)
plot(v)
lines(b)
plot(diff(v))


#### Feb. 02 2018 ####
colorGradient <- colorRampPalette(c("green", "red"))

readmotion <- readMotion('In vivo motion unified relative to Neurocranium.csv')

for (trial in unique(readmotion$trial)) {
  
  read_motion <- subsetRows(readmotion, list('trial'=trial))
  
  prey <- t(read_motion$xyz[grep("Prey*", rownames(read_motion$xyz)),,])
  
  epax <- t(read_motion$xyz[grep("Epaxial_bead_cra*", rownames(read_motion$xyz)),,])
  
  distances <- c()
  
  for (i in 1:nrow(prey)) {
    distances <- c(distances, eucDist(prey[i,], epax[i,]))
  }
  
  #plot(distances[2:length(distances)], diff(distances), type='l', lwd=1.5, xlab='Distance from esophagus', ylab='Velocity', col='cornflowerblue')
  #abline(h=0, lty=2, lwd=1.5)
  #plot(diff(distances), type='l', lwd=1.6, col="cornflowerblue")
  #abline(h=0, lty=2)
  p <- plot_ly(as.data.frame(prey), type="scatter3d", x=~V1, y=~V2, z=~V3, mode="markers", marker=list(color=colorGradient(nrow(prey)), size=4)) %>%
    layout(title=trial, scene=list(xaxis=list(title='X'), yaxis=list(title='Y'), zaxis=list(title='Z')))
  
  print(p)
   
  #pause()
  
}


