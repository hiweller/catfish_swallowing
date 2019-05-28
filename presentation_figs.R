# HOUSEKEEPING (colors, libraries, loading CSVs, mini-functions) ####
prey.bead.list <- loadPreyTrajectories()
trials <- trialFiles()
test <- prey.bead.list[[1]]
colors <- rep(RColorBrewer::brewer.pal(8, "Set2"), 4)

# 1:11: cat2, teal
# 12:22: cat1, purple
# 23:25: cat5, orange
oranges <- colorRampPalette(c("#e8790b", "#ffe27c"))(11)
teals <- colorRampPalette(c("#8de0bd", "#00727a"))(11)
purples <- colorRampPalette(c("#d5aaed", "#6d1482"))(11)

mround <- function(x, base) {
  base * round(x / base)
}


# plotting for poster ####

# NC.pct
oral.ref <- 25
pharyngeal.ref <- 400
sf <- 1



# oral phase: ####
png("~/Dropbox/Brainerd_Lab/Presentations/SICB_2019/Catfish_talk/oral_phase.png", width = 3000 * sf, height = 3000 * sf, res = 300)

par(mar = c(5, 8, 4, 1), mfrow = c(3, 1))
trial <- test
{plot(c(1:nrow(trial)), # make sure X-axis is correct length
     c(trial$NC.pct[1:oral.ref], 
       rep(NA, nrow(trial) - oral.ref)), # only plot first 33 frames
     type = 'l', lwd = 6,
     col = "goldenrod1",
     ylim = range(trial$NC.pct),
     panel.first = abline(h = c(0, 1.0),
                          col = "lightgrey",
                          lwd = 3),
     xlab = "",
     ylab = "Head proportion",
     main = "Food location",
     yaxt = "n",
     xaxt = "n",
     cex.main = 3,
     cex.lab = 2.2)
  axis(2, at = c(0, 0.5, 1.0), cex.axis = 1.5)
  axis(1, at = c(0, 200, 400, 600), cex.axis = 2)

  # velocity
plot(c(1:nrow(trial)),
     c(NA, diff(trial$NC.pct[1:oral.ref]),
       rep(NA, nrow(trial) - oral.ref)) * 300,
     ylim = range(diff(trial$NC.pct)) * 300,
     type = 'l', lwd = 6,
     col = "darkblue",
     panel.first = abline(h = c(0),
                          col = "lightgrey",
                          lwd = 3),
     ylab = "Velocity (head lengths/sec)",
     xlab = "", 
     main = "Food velocity",
     yaxt = "n",
     xaxt = "n",
     cex.main = 3,
     cex.lab = 2.2)
axis(2, at = c(0, 5, 10, 15), cex.axis = 1.5)
axis(1, at = c(0, 200, 400, 600), cex.axis = 2)

# Motion along body axes
X.d <- smooth.spline(temp.df$X.d, df =  round(0.1 * nrow(trial)))$y
Y.d <- smooth.spline(temp.df$Y.d, df =  round(0.1 * nrow(trial)))$y
Z.d <- smooth.spline(temp.df$Z.d, df =  round(0.1 * nrow(trial)))$y

plot(c(1:nrow(trial)),
     c(X.d[1:oral.ref],
       rep(NA, nrow(trial) - oral.ref)),
     ylim = c(0, 1),
     type = 'l', lwd = 6,
     col = "tomato",
     panel.first = abline(h = c(0, 1.0),
                          col = "lightgrey",
                          lwd = 3),
     ylab = "Motion proportion",
     xlab = "Frame", 
     main = "Food motion on body axes",
     yaxt = "n",
     xaxt = "n",
     cex.main = 3,
     cex.lab = 2.2)
axis(2, at = c(0, 0.5, 1.0), cex.axis = 1.5)
axis(1, at = c(0, 200, 400, 600), cex.axis = 2)
points(c(1:nrow(trial)),
       c(Y.d[1:oral.ref],
         rep(NA, nrow(trial) - oral.ref)),
       type = 'l', lwd = 6,
       col = "cornflowerblue")
points(c(1:nrow(trial)),
       c(Z.d[1:oral.ref],
         rep(NA, nrow(trial) - oral.ref)),
       type = 'l', lwd = 6,
       col = "palegreen3")
legend(x = 600, y = 0.9, 
       legend = c("A-P",
                  "D-V",
                  "M-L"),
       fill = c("tomato",
               "palegreen3",
               "cornflowerblue"),
       cex = 2)
}
dev.off()
# pharyngeal phase: ####
png("~/Dropbox/Brainerd_Lab/Presentations/SICB_2019/Catfish_talk/pharyngeal_phase.png", width = 3000 * sf, height = 3000 * sf, res = 300)
par(mar = c(5, 8, 4, 1), mfrow = c(3, 1))
{plot(c(1:nrow(trial)), # make sure X-axis is correct length
      c(trial$NC.pct[1:pharyngeal.ref], 
        rep(NA, nrow(trial) - pharyngeal.ref)), # only plot first 33 frames
      type = 'l', lwd = 6,
      col = "goldenrod1",
      ylim = range(trial$NC.pct),
      panel.first = abline(h = c(0, 1.0),
                           col = "lightgrey",
                           lwd = 3),
      xlab = "",
      ylab = "Head proportion",
      main = "Food location",
      yaxt = "n",
      xaxt = "n",
      cex.main = 3,
      cex.lab = 2.2)
  axis(2, at = c(0, 0.5, 1.0), cex.axis = 1.5)
  axis(1, at = c(0, 200, 400, 600), cex.axis = 2)
  
  # velocity
  plot(c(1:nrow(trial)),
       c(NA, diff(trial$NC.pct[1:pharyngeal.ref]),
         rep(NA, nrow(trial) - pharyngeal.ref)) * 300,
       ylim = range(diff(trial$NC.pct)) * 300,
       type = 'l', lwd = 6,
       col = "darkblue",
       panel.first = abline(h = c(0),
                            col = "lightgrey",
                            lwd = 3),
       ylab = "Velocity (head lengths/sec)",
       xlab = "", 
       main = "Food velocity",
       yaxt = "n",
       xaxt = "n",
       cex.main = 3,
       cex.lab = 2.2)
  axis(2, at = c(0, 5, 10, 15), cex.axis = 1.5)
  axis(1, at = c(0, 200, 400, 600), cex.axis = 2)
  
  # Motion along body axes
  X.d <- smooth.spline(temp.df$X.d, df =  round(0.1 * nrow(trial)))$y
  Y.d <- smooth.spline(temp.df$Y.d, df =  round(0.1 * nrow(trial)))$y
  Z.d <- smooth.spline(temp.df$Z.d, df =  round(0.1 * nrow(trial)))$y
  
  plot(c(1:nrow(trial)),
       c(X.d[1:pharyngeal.ref],
         rep(NA, nrow(trial) - pharyngeal.ref)),
       ylim = c(0, 1),
       type = 'l', lwd = 6,
       col = "tomato",
       panel.first = abline(h = c(0, 1.0),
                            col = "lightgrey",
                            lwd = 3),
       ylab = "Motion proportion",
       xlab = "Frame", 
       main = "Food motion on body axes",
       yaxt = "n",
       xaxt = "n",
       cex.main = 3,
       cex.lab = 2.2)
  axis(2, at = c(0, 0.5, 1.0), cex.axis = 1.5)
  axis(1, at = c(0, 200, 400, 600), cex.axis = 2)
  points(c(1:nrow(trial)),
         c(Y.d[1:pharyngeal.ref],
           rep(NA, nrow(trial) - pharyngeal.ref)),
         type = 'l', lwd = 6,
         col = "cornflowerblue")
  points(c(1:nrow(trial)),
         c(Z.d[1:pharyngeal.ref],
           rep(NA, nrow(trial) - pharyngeal.ref)),
         type = 'l', lwd = 6,
         col = "palegreen3")
  legend(x = 600, y = 0.9, 
         legend = c("A-P",
                    "D-V",
                    "M-L"),
         fill = c("tomato",
                  "palegreen3",
                  "cornflowerblue"),
         cex = 2)
}
dev.off()

# esophageal phase ####
png("~/Dropbox/Brainerd_Lab/Presentations/SICB_2019/Catfish_talk/esophageal_phase.png", width = 3000 * sf, height = 3000 * sf, res = 300)
par(mar = c(5, 8, 4, 1), mfrow = c(3, 1))
{plot(c(1:nrow(trial)), # make sure X-axis is correct length
      c(trial$NC.pct), 
      type = 'l', lwd = 6,
      col = "goldenrod1",
      ylim = range(trial$NC.pct),
      panel.first = abline(h = c(0, 1.0),
                           col = "lightgrey",
                           lwd = 3),
      xlab = "",
      ylab = "Head proportion",
      main = "Food location",
      yaxt = "n",
      xaxt = "n",
      cex.main = 3,
      cex.lab = 2.2)
  axis(2, at = c(0, 0.5, 1.0), cex.axis = 1.5)
  axis(1, at = c(0, 200, 400, 600), cex.axis = 2)
  
  # velocity
  plot(c(1:nrow(trial)),
       c(NA, diff(trial$NC.pct)) * 300,
       ylim = range(diff(trial$NC.pct)) * 300,
       type = 'l', lwd = 6,
       col = "darkblue",
       panel.first = abline(h = c(0),
                            col = "lightgrey",
                            lwd = 3),
       ylab = "Velocity (head lengths/sec)",
       xlab = "", 
       main = "Food velocity",
       yaxt = "n",
       xaxt = "n",
       cex.main = 3,
       cex.lab = 2.2)
  axis(2, at = c(0, 5, 10, 15), cex.axis = 1.5)
  axis(1, at = c(0, 200, 400, 600), cex.axis = 2)
  
  # Motion along body axes
  X.d <- smooth.spline(temp.df$X.d, df =  round(0.1 * nrow(trial)))$y
  Y.d <- smooth.spline(temp.df$Y.d, df =  round(0.1 * nrow(trial)))$y
  Z.d <- smooth.spline(temp.df$Z.d, df =  round(0.1 * nrow(trial)))$y
  
  plot(c(1:nrow(trial)),
       c(NA, X.d),
       ylim = c(0, 1),
       type = 'l', lwd = 6,
       col = "tomato",
       panel.first = abline(h = c(0, 1.0),
                            col = "lightgrey",
                            lwd = 3),
       ylab = "Motion proportion",
       xlab = "Frame", 
       main = "Food motion on body axes",
       yaxt = "n",
       xaxt = "n",
       cex.main = 3,
       cex.lab = 2.2)
  axis(2, at = c(0, 0.5, 1.0), cex.axis = 1.5)
  axis(1, at = c(0, 200, 400, 600), cex.axis = 2)
  points(c(1:nrow(trial)),
         c(NA, Y.d),
         type = 'l', lwd = 6,
         col = "cornflowerblue")
  points(c(1:nrow(trial)),
         c(NA, Z.d),
         type = 'l', lwd = 6,
         col = "palegreen3")
}
dev.off()
points(c(1:nrow(trial)), # make sure X-axis is correct length
       c(rep(NA,  pharyngeal.ref),
         trial$NC.pct[(pharyngeal.ref+1):nrow(trial)]),
       type = 'l', lwd = 6, col = "#EB6EA0")
points(c(1:nrow(trial)), # make sure X-axis is correct length
       c(trial$NC.pct[1:pharyngeal.ref],
         c(rep(NA, nrow(trial) - pharyngeal.ref))),
       type = 'l', lwd = 6, col = "#EBD2D8")

dev.off()
# just plot NC.pct
trial$NC.pct %>% plot
(diff(trial$NC.pct) * 300) %>% plot(ylab = "Velocity (head lengths/second)",
                                    xlab = "Frame",
                                    type = 'l', 
                                    lwd = 3)
abline(h = 0, col = "lightgrey")
trial$x


# get relevant normalized motions
trial <- prey.bead.list[[1]]

Velocity <- diff(trial$NC.pct) * 300 # in head lengths / second

# Normalize?
n <- 0.1
X.d <- diff(trial$x)
Y.d <- diff(trial$y)
Z.d <- diff(trial$z)
Tot.d <- sqrt(X.d^2 + Y.d^2 + Z.d^2)

xyz.norm <- t(apply(cbind(X.d, Y.d, Z.d), 1, function(i) abs(i) / sum(abs(i))))

Velocity <- Velocity / max(Velocity)
Position <- mround(trial$NC.pct[-1], n)
temp.df <- data.frame(xyz.norm, Tot.d, Velocity, Position)

avg.values <- aggregate(cbind(xyz.norm, Tot.d, Velocity), list(factor(Position)), mean)

catID <- rep(trials$catID[i], nrow(temp.df))
Food <- rep(trials$food[i], nrow(temp.df))
Trial <- rep(trials$trial[i], nrow(temp.df))

temp.df <- data.frame(X.d = temp.df$X.d,
                      Y.d = temp.df$Y.d,
                      Z.d = temp.df$Z.d,
                      Tot.d = temp.df$Tot.d,
                      Velocity = temp.df$Velocity,
                      Position = temp.df$Position,
                      catID = catID,
                      Food = Food,
                      Trial = Trial)

# plot NC pct
plot(temp.df$Position, type = 'l')



geom_boxplot(temp.df$X.d, type = 'p', pch = 19, col="tomato")
points(temp.df$Y.d, type = 'l', col = "palegreen3")
points(temp.df$Z.d, type = 'l', col = "cornflowerblue")

p <- ggplot(temp.df, aes(x = Position, y = X.d)) +
  ylab("Relative X, Y, Z displacement") +
  xlab("Position in head")
p <- p + geom_boxplot(aes(x = factor(Position),
                          y = X.d),
                      fill = "tomato", 
                      outlier.color = "tomato",
                      alpha = 0.6)
p <- p + geom_boxplot(aes(x = factor(Position),
                          y = Y.d),
                      fill = "palegreen3",
                      outlier.color = "palegreen3",
                      alpha = 0.6)
p <- p + geom_boxplot(aes(x = factor(Position),
                          y = Z.d),
                      fill = "cornflowerblue", 
                      outlier.color = "cornflowerblue",
                      alpha = 0.6)
p + theme_bw(base_size = 14)


temp <- prey.bead.list[["Nov 17 2016 Cat 01 Trial 01"]]
plot(temp$NC.pct)
abline(v = 33)


## read in rigid body rotations ####
df <- read.csv("Catfish_motion_analysis/Fit joint models/Results by trial with metadata/Nov 17 2016 Cat 01 Trial 01.csv")


start <- 512
esophagus <- 910
end <- 1228
df <- df[start:end, ]

oral.phase <- df[1:oral.ref, ]
pharyngeal.phase <- df[oral.ref:pharyngeal.ref, ]
esophageal.phase <- df[pharyngeal.ref:nrow(df), ]


# rigid body colors ####
pg <- "#C479E4"
hy <- "#EBBD3E"
lj <- "#68AD6B"
op <- "#009E97"


# esophageal correlations ####
lwd <- 5
cex.main <- 4
cex.axis <- 2.5
png("~/Dropbox/Brainerd_Lab/Presentations/SICB_2019/Catfish_talk/esophageal_correlations.png", width = 4000 * sf, height = 5000 * sf, res = 300)

par(mfrow = c(5, 1),
    mar = c(5, 8, 4, 1))
{plot(c(400:716),
     esophageal.phase$Prey_bead.y %>% diff * 300,
     #ylim = c(-0.1, 0.15),
     ylim = c(-25, 45),
     type = 'l', lwd = lwd, col = "palegreen3",
  xlab = "",
  ylab = "",
  main = "Food coordinates",
  yaxt = "n",
  xaxt = "n",
  cex.main = cex.main,
  cex.lab = 2.2,
  panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at = c(-20, 0, 20, 40, 60), cex.axis = cex.axis)
axis(1, at = c(400, 500, 600, 700), cex.axis = cex.axis)

points(c(400:716),
       esophageal.phase$Prey_bead.x %>% diff * 300,
       type = 'l', 
       lwd = lwd,
       col = "tomato")

points(c(400:716),
       lwd = lwd,
       esophageal.phase$Prey_bead.z %>% diff * 300, 
       type = 'l', col = "cornflowerblue")

plot(c(400:717),
  esophageal.phase$PostTemporalL.PectoralGirdleL.1 * 180/(2*pi),
     ylim = c(min(df$PostTemporalL.PectoralGirdleL.1) * 180/(2*pi), 0),
  col = pg,
     type = 'l', 
     lwd = lwd,
     xlab = "",
     ylab = "",
     main = "Pectoral girdle retr.",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
  panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at = c(-5, 0), cex.axis = cex.axis)
axis(1, at = c(400, 500, 600, 700), cex.axis = cex.axis)

plot(c(400:717),
     esophageal.phase$SuspensoriumL.HyoidL.1 * 180/(2*pi),
     type = 'l', 
     col = hy,
     lwd = lwd,
     xlab = "",
     ylab = "",
     ylim = c(-16, 0),
     main = "Hyoid retraction",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
     panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at =  c(-15, 0), cex.axis = cex.axis)
axis(1, at = c(400, 500, 600, 700), cex.axis = cex.axis)

plot(c(400:717),
     esophageal.phase$SuspensoriumL.LowerJawL.1 * 180/(2*pi),
     col = lj,
     type = 'l', 
     lwd = lwd,
     xlab = "",
     ylab = "",
     ylim = c(-15, 0),
     main = "Lower jaw depression",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
     panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at =  c(-15, 0), cex.axis = cex.axis)
axis(1, at = c(400, 500, 600, 700), cex.axis = cex.axis)

plot(c(400:717),
  esophageal.phase$SuspensoriumL.OperculumL.1 * 180/(2*pi),
  col = op,
     type = 'l', 
     lwd = lwd,
     ylab = "",
     ylim = c(-3, 7),
     main = "Opercular abduction",
     xlab = "Frame",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
  panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at =  c(-3, 0, 7), cex.axis = cex.axis)
axis(1, at = c(400, 500, 600, 700), cex.axis = cex.axis)
}
dev.off()
# pharyngeal correlations ####
lwd <- 5
cex.main <- 4
cex.axis <- 2.5
png("~/Dropbox/Brainerd_Lab/Presentations/SICB_2019/Catfish_talk/pharyngeal_correlations.png", width = 4000 * sf, height = 5000 * sf, res = 300)

par(mfrow = c(5, 1),
    mar = c(5, 8, 4, 1))

{{plot(c(oral.ref:pharyngeal.ref)[-1],
      pharyngeal.phase$Prey_bead.y %>% diff * 300,
      ylim = apply(pharyngeal.phase[ , 16:18], 2, function(i) range(diff(i))) %>% range * 300,
      type = 'l',
      col = "palegreen3",
      lwd = lwd,
      xlab = "",
      ylab = "",
      main = "Food coordinates",
      yaxt = "n",
      xaxt = "n",
      cex.main = cex.main,
      cex.lab = 2.2,
      panel.first = abline(h = 0, col = "grey", lwd = 3))
  axis(2, at = c(-100, 0, 100), cex.axis = cex.axis)
  axis(1, at = c(100, 200, 300, 400), cex.axis = cex.axis)
  
  points(c(oral.ref:pharyngeal.ref)[-1],
         pharyngeal.phase$Prey_bead.x %>% diff * 300,
         type = 'l', col = "tomato",
         lwd = lwd)
  
  points(c(oral.ref:pharyngeal.ref)[-1],
         pharyngeal.phase$Prey_bead.z %>% diff * 300, 
         type = 'l', col = "cornflowerblue",
         lwd = lwd)}

plot(c(oral.ref:pharyngeal.ref),
     pharyngeal.phase$PostTemporalL.PectoralGirdleL.1 * 180/(2*pi),
     ylim = c(min(df$PostTemporalL.PectoralGirdleL.1) * 180/(2*pi), 0),
     col = pg,
     type = 'l', 
     lwd = lwd,
     xlab = "",
     ylab = "",
     main = "Pectoral girdle retr.",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
     panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at = c(-5, 0), cex.axis = cex.axis)
axis(1, at = seq(100, 400, by = 100), cex.axis = cex.axis)

plot(c(oral.ref:pharyngeal.ref),
     pharyngeal.phase$SuspensoriumL.HyoidL.1 * 180/(2*pi),
     type = 'l', 
     col = hy,
     lwd = lwd,
     xlab = "",
     ylab = "",
     ylim = c(-16, 0),
     main = "Hyoid retraction",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
     panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at =  c(-15, 0), cex.axis = cex.axis)
axis(1, at = seq(100, 400, by = 100), cex.axis = cex.axis)

plot(c(oral.ref:pharyngeal.ref),
     pharyngeal.phase$SuspensoriumL.LowerJawL.1 * 180/(2*pi),
     col = lj,
     type = 'l', 
     lwd = lwd,
     xlab = "",
     ylab = "",
     ylim = c(-15, 0),
     main = "Lower jaw depression",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
     panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at =  c(-15, 0), cex.axis = cex.axis)
axis(1, at = seq(100, 400, by = 100), cex.axis = cex.axis)

plot(c(oral.ref:pharyngeal.ref),
     pharyngeal.phase$SuspensoriumL.OperculumL.1 * 180/(2*pi),
     col = op,
     type = 'l', 
     lwd = lwd,
     ylab = "",
     ylim = c(-3, 7),
     main = "Opercular abduction",
     xlab = "Frame",
     yaxt = "n",
     xaxt = "n",
     cex.main = cex.main,
     cex.lab = 2.2,
     panel.first = abline(h = 0, col = "grey", lwd = 3))
axis(2, at =  c(-3, 0, 7), cex.axis = cex.axis)
axis(1, at = seq(100, 400, by = 100), cex.axis = cex.axis)}
dev.off()


# SUMMARY DATA ####
# Velocity
# Get all of the velocities within that chunk and group them
binned.data <- data.frame(X.d = numeric(),
                          Y.d = numeric(),
                          Z.d = numeric(),
                          Tot.d = numeric(),
                          Velocity = numeric(),
                          Position = numeric(),
                          catID = character(),
                          Food = character(),
                          Trial = character())

for (i in 1:length(prey.bead.list)) {
  
  trial <- prey.bead.list[[i]]
  
  Velocity <- diff(trial$NC.pct) * 300 # in head lengths / second
  
  # Normalize?
  X.d <- diff(trial$x)
  Y.d <- diff(trial$y)
  Z.d <- diff(trial$z)
  Tot.d <- sqrt(X.d^2 + Y.d^2 + Z.d^2)
  
  xyz.norm <- t(apply(cbind(X.d, Y.d, Z.d), 1, function(i) abs(i) / sum(abs(i))))
  
  Velocity <- Velocity / max(Velocity)
  Position <- mround(trial$NC.pct[-1], n)
  
  temp.df <- aggregate(cbind(xyz.norm, Tot.d, Velocity), list(factor(Position)), mean)
  
  catID <- rep(trials$catID[i], nrow(temp.df))
  Food <- rep(trials$food[i], nrow(temp.df))
  Trial <- rep(trials$trial[i], nrow(temp.df))
  
  temp.df <- data.frame(X.d = temp.df$X.d,
                        Y.d = temp.df$Y.d,
                        Z.d = temp.df$Z.d,
                        Tot.d = temp.df$Tot.d,
                        Velocity = temp.df$Velocity,
                        Position = temp.df$Group.1,
                        catID = catID,
                        Food = Food,
                        Trial = Trial)
  
  binned.data <- rbind(binned.data, temp.df)
  
  
}

sf <- 2
png("~/Dropbox/Brainerd_Lab/Presentations/SICB_2019/Catfish_talk/velocity_summary.png", width = 2000 * sf, height = 1200 * sf, res = 300, bg = "transparent")

# plot velocity
{p <- ggplot(binned.data, aes(x = Position, y = Velocity)) +
  ggtitle("Velocity") + 
  xlab("Proportion of head length") + 
  ylab("Velocity (normalized)")
p <- p + geom_jitter(aes(x = factor(Position), 
                         y = Velocity, 
                         color = Food), alpha = 0.8,
                     width = 0.15, height = 0.05,
                     size = 3) + 
  geom_boxplot(aes(x = factor(Position), 
                   y = Velocity), alpha = 0.4,
               fill = "grey",
               outlier.alpha = 0) +
  scale_color_manual(values = c("orange", 
                                "cornflowerblue", 
                                "mediumorchid"),
                     guide = FALSE)
p <- p + scale_x_discrete(labels = c("0", "0.5", "1"),
                          breaks = c("0", "0.5", "1"))
p <- p + scale_y_continuous(labels = c(0, 0.5, 1),
                          breaks = c(0, 0.5, 1))
p + theme_bw(base_size = 32) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))
}
dev.off()

# X, Y, Z displacement
sf <- 2
png("~/Dropbox/Brainerd_Lab/Presentations/SICB_2019/Catfish_talk/XYZ_displacement_summary.png", width = 2000 * sf, height = 1200 * sf, res = 300, bg = "transparent")
{p <- ggplot(binned.data, aes(x = Position, y = X.d)) +
    ylab("Relative displacement") +
    xlab("Proportion of head length") + 
    ggtitle("Food motion on body axes")
  p <- p + geom_boxplot(aes(x = factor(Position),
                            y = X.d),
                        fill = "tomato", 
                        outlier.color = "tomato",
                        alpha = 0.6)
  p <- p + geom_boxplot(aes(x = factor(Position),
                            y = Y.d),
                        fill = "palegreen3",
                        outlier.color = "palegreen3",
                        alpha = 0.6)
  p <- p + geom_boxplot(aes(x = factor(Position),
                            y = Z.d),
                        fill = "cornflowerblue", 
                        outlier.color = "cornflowerblue",
                        alpha = 0.6)
  p <- p + scale_x_discrete(labels = c("0", "0.5", "1"),
                            breaks = c("0", "0.5", "1"))
  p <- p + scale_y_continuous(labels = c(0, 0.5, 1),
                              breaks = c(0, 0.5, 1))
  p + theme_bw(base_size = 32) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5))
  
}
dev.off()

# misc ####
names(df)

prey.beads <- c(201:203)

dist(df[1:2, 201:203])

df$Prey_bead.x %>% plot
abline(v = start)
