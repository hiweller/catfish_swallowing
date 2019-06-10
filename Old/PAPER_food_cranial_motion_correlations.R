# QUESTION: how tightly are food and cranial motions correlated during the
# different phases of feeding?

# ANALYSIS: Get cross-correlation coefficients between food motion and all head
# motions for each phase.

# BOOKKEEPING ####
# HEAD LENGTHS (3 measurements per fish using distance measurement tool in Maya)
# tip of lower jaw to esophagus
nc.length <- list(Cat01 = mean(c(69.698898, 69.874955, 69.773262)),
                  Cat02 = mean(c(69.809575, 69.501256, 70.172114)),
                  Cat05 = mean(c(74.742032, 75.916118, 75.325379)))
source("Code/04_prey_bead_trajectory_analyses.R")
trials <- trialFiles()
require(ggplot2)
library(magrittr)

# set lag max
lag.max <- 100

# bone colors that match "colorful bones"
colors <- data.frame("Post temporal" = "#cc1a4c",
                     "Suspensorium" = "#2cb245",
                     "Pectoral girdle" = "#f7bd20",
                     "Hyoid" = "#aa5aed",
                     "Lower jaw" = "#21bbc6",
                     "Operculum" = "#f77027",
                     "Neurocranium" = "#7B91DE")

# Step 1: get seperate mass dataframes for each phase. ####
motion.dir <- dir("Catfish_motion_analysis/Fit joint models/Results by trial with metadata/",
                  full.names = TRUE)


esoph.bps <- c()
for (i in 1:nrow(trials)) {
  
  # for every trial in trial metadata file
  # find which file it goes with in the motion directory
  trial.idx <- grep(trials$trial[i], motion.dir)
  
  # read in motion file
  motion.file <- read.csv(motion.dir[trial.idx])
  motion.file <- motion.file[trials$start.frame[i]:nrow(motion.file), ]
  
  # get prey.bead.x, y, z into head length proportion
  # where lower jaw = 0, esophagus = 1
  hl <- nc.length[[grep(trials$catID[i], names(nc.length))]]
  motion.file$Prey_bead.x <- (motion.file$Prey_bead.x - motion.file$Prey_bead.x[1]) / hl
  motion.file$Prey_bead.y <- motion.file$Prey_bead.y / hl
  motion.file$Prey_bead.z <- motion.file$Prey_bead.z / hl
  
  pharyngeal.bp <- trials$breakpoints[i]
  esoph.bp <- which(motion.file$Prey_bead.x >= 1)
  esoph.bps <- c(esoph.bps, esoph.bp[1])
  if(length(esoph.bp) < 50) {
    esoph.bp <- nrow(motion.file)
  } else {
    esoph.bp <- esoph.bp[1]
  }
  
  if (i == 1) {
    # this is stupid but it works so don't talk shit ok
    temp.df <- motion.file[1, ]
    temp.df <- temp.df[-1, ]
    
    suction.df <- temp.df
    pharyngeal.df <- temp.df
    esophageal.df <- temp.df
  }
  
  # break out "suction" phase:
  # THIS IS ALSO STUPID LEAVE ME ALONE
  suction <- motion.file[1:pharyngeal.bp, ]
  suction.df <- rbind(suction.df, suction)
  suction[,] <- NA
  suction.df <- rbind(suction.df, suction)
  message("Suction: ", nrow(suction), " frames")
  
  # and pharyngeal
  pharyngeal <- motion.file[(pharyngeal.bp + 1):esoph.bp, ]
  pharyngeal.df <- rbind(pharyngeal.df, pharyngeal)
  pharyngeal[,] <- NA
  pharyngeal.df <- rbind(pharyngeal.df, pharyngeal)
  message("Pharyngeal: ", nrow(pharyngeal), " frames")
  
  # and esophageal, if it exists
  if (esoph.bp < nrow(motion.file)) {
    esophageal <- motion.file[esoph.bp:nrow(motion.file), ]
    esophageal.df <- rbind(esophageal.df, esophageal)
    esophageal[,] <- NA
    esophageal.df <- rbind(esophageal.df, esophageal)
    message("Esophageal: ", nrow(esophageal), " frames")
  }
  print(dim(esophageal.df))
  
}

# Step 1.5: get esophageal phases in a list instead of a df
esoph.indiv <- c()
esoph.frames <- c()
for (i in 1:length(motion.list)) {
  
  motion.file <- motion.list[[i]]
  
  esoph.bp <- which(motion.file$Prey_bead.x >= 1)
  
  if (length(esoph.bp) > 1) {
    esoph.indiv <- c(esoph.indiv, as.character(motion.file$individual[[1]]))
    esoph.frames <- c(esoph.frames, length(esoph.bp))
    
  }

  
}

handling.indiv <- c()
handling.frames <- c()
for (i in 1:length(motion.list)) {
  
  motion.file <- motion.list[[i]]
  
  esoph.bp <- which(motion.file$Prey_bead.x >= 1)
  
  if (length(esoph.bp) > 1) {
    esoph.indiv <- c(esoph.indiv, as.character(motion.file$individual[[1]]))
    esoph.frames <- c(esoph.frames, length(esoph.bp))
    
  }
  
  
}

# Step 2: get cross-correlations ####
phase.list <- list("Suction" = suction.df,
                   "Pharyngeal" = pharyngeal.df,
                   "Esophageal" = esophageal.df)

col.idx <- grep(".[1-2]|hypaxial", colnames(phase.list[[1]]))

# for each phase, get a dataframe output of:
  # columns: motion axes
  # rows: 1) max cross-correlation value, 2) lag
df <- matrix(NA, nrow = 2, ncol = length(col.idx))
colnames(df) <- colnames(temp.df)[col.idx]
rownames(df) <- c("Max CCF", "Lag")

ccf.list <- vector("list", 3)
names(ccf.list) <- names(phase.list)

for (i in 1:length(phase.list)) {
  
  # xerox blank df into list
  ccf.list[[i]] <- df
  
  # my usual style: doing too many things on one line for no reason besides
  # laziness
  lag.max <- which(diff(is.na(phase.list[[i]][ , 1])) != 0) %>% diff %>% min
  
  # for each motion axis, get cross-correlation with prey motion
  for (j in 1:length(col.idx)) {
    ci <- col.idx[j]
    temp.ccf <- ccf(diff(phase.list[[i]]$Prey_bead.x, differences = 1), 
        diff(phase.list[[i]][ , ci], differences = 1), 
        na.action = na.pass,
        lag.max = 10*log10(lag.max/2),
        main = paste(names(phase.list)[i], colnames(phase.list[[i]])[ci]), plot = F)
    
    # store cross-correlation coefficient and lag
    ccf.idx <- which(abs(temp.ccf$acf) %in% (max(abs(temp.ccf$acf))))
    ccf.list[[i]][1, j] <- temp.ccf$acf[ccf.idx]
    ccf.list[[i]][2, j] <- temp.ccf$lag[ccf.idx]
  }
}

# Step 3: plot ccfs by bone color ####

# names
bone.names <- c("Post-temporal", "Suspensorium", "Pectoral girdle",
            "Hyoid retr.", "Hyoid depr.", 
            "LJ depression", "LJ roll",
            "Operculum abd.", "Operculum elev.", "Hypaxial")
#bone.names <- unlist(lapply(bone.names, function(i) substr(i, 1, nchar(i)-1)))

ccf.df <- reshape2::melt(ccf.list)
colnames(ccf.df) <- c("Value", "Element", "CCF", "Behavior")
ccf.df <- ccf.df[ccf.df$Value=="Max CCF", ]

sf <- 0.5
png("Manuscript/Figures/Fig05_food_cranial_motion_correlations.png", width = 2000 * sf,
   height = 1200 * sf, res = 200, bg = "white")
{colors2 <- data.frame(colors$Post.temporal, colors$Suspensorium,
                      colors$Pectoral.girdle, colors$Hyoid, colors$Hyoid,
                      colors$Lower.jaw, colors$Lower.jaw, 
                      colors$Operculum, colors$Operculum, "black")
  colnames(colors2) <- colnames(ccf.list[[1]])
  
  #colors2 <- RColorBrewer::brewer.pal(9, "Set1")
p <- ggplot(data = ccf.df, aes(x = Behavior, y = abs(CCF))) +
  ylab("Correlation with food speed (R)") +
  geom_violin(color = "darkgrey", fill = "lightgrey") + 
  geom_jitter(width = 0.2, aes(color = Element), size = 2.5) + 
  #(labels = labels) +
  scale_color_manual(values = t(colors2), labels = bone.names) + 
  xlim("Suction", "Pharyngeal", "Esophageal") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)); p
}
dev.off()



p <- ggplot(data = ccf.df, aes(x = Behavior, y = abs(CCF))) +
  ylab("Correlation with food speed (R)") +
  geom_violin(color = "darkgrey", fill = "lightgrey") + 
  geom_jitter(width = 0.2, aes(color = Element), size = 2.5) + 
  #(labels = labels) +
  #scale_color_manual(values = t(colors2), labels = bone.names) + 
  xlim("Suction", "Pharyngeal", "Esophageal") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)); p

lapply(ccf.list, function(i) mean(abs(i[1,])))
ccf.list$Suction


# Step not a step: get avg speed during pharyngeal
pharyngeal.speeds <- c()

for (i in 2:nrow(pharyngeal.df)) {
  
  p1 <- pharyngeal.df[(i-1), 16:18]
  p2 <- pharyngeal.df[i, 16:18]
  
  if (!any(is.na(c(p1, p2)))) {
    pharyngeal.speeds <- c(pharyngeal.speeds,
                           eucDist(p1, p2))
  }
}

esophageal.speeds <- c()
for (i in 2:nrow(esophageal.df)) {
  p1 <- esophageal.df[(i-1), 16:18]
  p2 <- esophageal.df[i, 16:18]
  
  if (!any(is.na(c(p1, p2)))) {
    esophageal.speeds <- c(esophageal.speeds,
                           eucDist(p1, p2))
  }
}

mean(esophageal.speeds) * 300
# time for pharyngeal handling
pharyngeal.handling <- data.frame (start = trials$breakpoints, end = esoph.bps) 

# i hate that i do this. it's ridiculous
handling.times <- apply(pharyngeal.handling, 1, 
                        function(i)
                          if (!any(is.na(i))) {
                              diff(i)
                          }) %>% unlist * 1/300

mean(handling.times)
sd(handling.times)


# plot pharyngeal prey bead motions separately
pbx <- pharyngeal.df$Prey_bead.x
sep <- c(1, which(diff(which(is.na(pbx))) > 1))

for (i in 1:(length(sep)-1)) {
  
  temp.pbx <- pharyngeal.df[(sep[i]+1):sep[i+1], 16:20]
  
  temp.pbx <- temp.pbx[which(apply(temp.pbx, 1, function (i) sum(is.na(i))) == 0), ]
  
  if (nrow(temp.pbx) > 0) {
    plot(temp.pbx$time,
         temp.pbx$Prey_bead.x, 
         col = "tomato",
         type = 'l', lwd = 2, 
         xlab = "Time (s)",
         ylab = "Oral cavity distance",
         main = temp.pbx$trial[1])}

  
}
