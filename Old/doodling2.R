source("Code/esophagus_prey_bead_alignment.R")

for (i in 1:nrow(trials)) {
  print(i)
  trial_name <- trials[i,]
  coordinates <- alignEsophagusToPrey(trial_name)
  prey.esophagus.pts <- as.data.frame(rbind(coordinates$esophagus[11:nrow(coordinates$esophagus),],
                              as.matrix(coordinates$prey)))
  
  colnames(prey.esophagus.pts) <- letters[24:26]
  
  prey.esophagus.pts$PointType <- c(rep("Esophagus", (nrow(coordinates$esophagus)-10)),
                                    rep("Prey", nrow(coordinates$prey)))
  
  p <- plot_ly(data=prey.esophagus.pts,
          x=~x, y=~y, z=~z, color=~PointType,
          colors=c("tomato", "cornflowerblue"),
          type="scatter3d", mode="markers", opacity=0.5) %>% 
    layout(title=paste(trial_name))
  print(p)
}

write.csv(x=esophagus.pts, file="Data/CT_locators/Cat 01/Cat 01 esophagus.csv")

# redo esophagus points on CT scan (fewer points!)
# measure esophagus as % length of NC
# move plane in maya cams and record approx. crossing frame
# plot bead velocity:
  # overall
  # X, Y, Z dimensions
# figure out distance to plane calculation

coordinates <- alignEsophagusToPrey(trials[22,])
prey.pts <- as.matrix(coordinates$prey)
plot(coordinates$prey[,1], pch=20, col="cornflowerblue")
points(coordinates$prey[,2], pch=20, col="tomato")
points(coordinates$prey[,3], pch=20, col="lightgreen")

par(mfrow=c(1,1))
plot(diff(prey.pts)[,1], pch=20, type="l", lwd=2, col="cornflowerblue")
points(diff(prey.pts)[,2], pch=20, type="l", lwd=2, col="tomato")
points(diff(prey.pts)[,3], pch=20, type="l", lwd=2, col="lightgreen")
abline(h=0, lwd=2, lty=2)

# displacement over time: how far did the bead travel from one frame to the next?
# mm/Frame
# 500 Frames/sec
# multiply by 500 --> mm/sec
d <- c()
for (i in 2:nrow(prey.pts)) {
  d <- c(d, 500*sqrt(sum(prey.pts[(i-1),]-prey.pts[i,])^2))
}

par(mar=rep(2,4))
plot(d, type='l', xlab="Frame", lwd=2, ylab="mm/sec")
plot(diff(d), type='l', main="d(V)")

# distance to esophagus center?
esophagus.pts <- as.data.frame(coordinates$esophagus[11:nrow(coordinates$esophagus),])
colnames(esophagus.pts) <- letters[24:26]
es.ctr <- apply(esophagus.pts, 2, mean)

e <- c()
for (i in 1:nrow(prey.pts)) {
  e <- c(e, sqrt(sum(prey.pts[i,]-es.ctr)^2))
}
plot(e, type='l', xlab="Frame", lwd=2, ylab="mm from esophagus center")


#### PLANE/POINT DISTANCE TROUBLESHOOTING ####

# find best-fit plane for esophagus using coefficient of linear model
library(plotly)
esophagus.pts <- as.data.frame(coordinates$esophagus[11:nrow(coordinates$esophagus),])
colnames(esophagus.pts) <- letters[24:26]
es.plane <- lm(data=esophagus.pts, z~y)
es.cf <- coef(es.plane)

plot.pts <- as.data.frame(rbind(coordinates$esophagus, as.matrix(coordinates$prey)))
colnames(plot.pts) <- letters[24:26]

x.seq <- seq(min(esophagus.pts$x),max(esophagus.pts$x),length.out=231)
y.seq <- seq(min(esophagus.pts$y),max(esophagus.pts$y),length.out=231)
z.mtx <- t(outer(x.seq, y.seq, function(x,y) 
  es.cf[1]+x+es.cf[2]*y))

plot_ly(x=~x.seq, y=~y.seq, z=~z.mtx,
        type="surface",
        opacity = 0.7, colors="cornflowerblue") %>%
  add_trace(data=esophagus.pts,
        x=~x, y=~y, z=~z,
        type="scatter3d", mode="markers")

# normal vector to the plane?
V <- matrix(ncol=1, c(es.cf[2], es.cf[3], -1))
v <- V/sqrt(sum(V^2))


# point in the plane
q <- es.ctr

# point from which to calculate distance in plane
p <- coordinates$prey[1,]

D <- (v[1]*q[1]+v[2]*q[2]+v[3]*q[3]+es.cf[1])/(sqrt(sum(v^2))); D

# find centroid of points
es.ctr <- apply(esophagus.pts, 2, mean)

distPointToPlane(coordinates$prey[1,], normal.vector)

# find plane distance and centroid distance
plane.dist <- apply(prey.pts[,1:3], 1, function(i) distPointToPlane(i, v, es.ctr))
centroid.dist <- apply(prey.pts[,1:3], 1, function(i) sqrt(sum(i-es.ctr)^2))

# plot them
plot(plane.dist,  pch=20, col="cornflowerblue",
     xlab="Frame", ylab="Distance from esophagus (mm)",
     main=paste(trial_name, "(plane)"))
abline(h=0, col="red", lty=2, lwd=1.5)

plot(centroid.dist,  pch=20, col="goldenrod",
     xlab="Frame", ylab="Distance from esophagus (mm)",
     main=paste(trial_name, "(centroid)"))
abline(h=0, col="red", lty=2, lwd=1.5)
