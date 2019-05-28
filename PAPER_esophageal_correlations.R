## Correlation analysis
source("Code/functions.R")
library(magrittr)

# HEAD LENGTHS (3 measurements per fish using distance measurement tool in Maya)
# tip of lower jaw to esophagus
nc.length <- list(Cat01 = mean(c(69.698898, 69.874955, 69.773262)),
                        Cat02 = mean(c(69.809575, 69.501256, 70.172114)),
                        Cat05 = mean(c(74.742032, 75.916118, 75.325379)))

# FOR EVERY TRIAL WHERE FOOD GOES PAST ESOPHAGUS
# read in motion file
# chop off the stuff before feeding begins using start.frame in trials
# divide displacements by nc.pct
# plot nc.pct progress + put dotted line at esophagus
# plot from start.frame in trials to end

esophagus.trials <- trials[!is.na(trials$esophagus.frame), ]

motion.dir <- dir("Catfish_motion_analysis/Fit joint models/Results by trial with metadata/",
                  full.names = TRUE)

# test round

for (i in 1:length(esophagus.trials)) {
  
  df <- read.csv(motion.dir[grep(esophagus.trials$trial[i], motion.dir)])
  df <- df[esophagus.trials$start.frame[i]:nrow(df), ]
  
  nc <- nc.length[[grep(esophagus.trials$catID[i], names(nc.length))]]
  
  df$Prey_bead.x <- (df$Prey_bead.x - df$Prey_bead.x[1]) / nc
  plot(df$Prey_bead.x, type = 'l', lwd = 2,
       ylab = "Head length", main = esophagus.trials$trial[i])
  abline(h = 1, lty = 2, col = "red")

}


par(mfrow = c(4, 1), mar = rep(2, 4))
for (i in 1:length(trials)) {
  
  df <- read.csv(motion.dir[grep(trials$trial[i], motion.dir)])
  df <- df[trials$start.frame[i]:nrow(df), ]
  
  nc <- nc.length[[grep(trials$catID[i], names(nc.length))]]
  
  df$Prey_bead.x <- (df$Prey_bead.x - df$Prey_bead.x[1]) / nc
  
  if (sum(df$Prey_bead.x > 1) > 20) {
    
    esoph.phase <- df[df$Prey_bead.x >= 1, ]
    
    # if it doesn't exist, create it
    if(!exists("all.esophagus.trials")) {
      all.esophagus.trials <- esoph.phase
    } else {
      # otherwise, make NA padding
      na.matrix <- as.data.frame(matrix(data = NA,
                                        nrow = nrow(esoph.phase),
                                        ncol = ncol(esoph.phase)))
      colnames(na.matrix) <- colnames(esoph.phase)
      
      # and glue it all together
      all.esophagus.trials <- rbind(all.esophagus.trials,
                                    na.matrix, 
                                    esoph.phase)
    }
    
    # plot(esoph.phase$Prey_bead.x %>% diff,
    #      type = 'l', lwd = 2,
    #      ylab = "Head length", main = trials$trial[i])
    # abline(h = 1, lty = 2, col = "red")
    # plot(esoph.phase$Neurocranium.SuspensoriumL.1 %>% diff, 
    #      ylab = "Suspensorium", type = 'l')
    # plot(esoph.phase$SuspensoriumL.HyoidL.1 %>% diff,
    #      ylab = "Hyoid", type = 'l')
    # plot(esoph.phase$SuspensoriumL.LowerJawL.1 %>% diff,
    #      ylab = "Lower jaw", type ='l')
    # plot(esoph.phase$SuspensoriumL.OperculumL.1 %>% diff,
    #      ylab = "Operculum", type = 'l')
    
    ccf(esoph.phase$Prey_bead.x %>% diff,
        esoph.phase$PostTemporalL.PectoralGirdleL.1 %>% diff)
    ccf(esoph.phase$Prey_bead.x %>% diff,
        esoph.phase$SuspensoriumL.HyoidL.1 %>% diff)
    ccf(esoph.phase$Prey_bead.x %>% diff,
        esoph.phase$SuspensoriumL.LowerJawL.1 %>% diff)
    ccf(esoph.phase$Prey_bead.x %>% diff,
        esoph.phase$SuspensoriumL.OperculumL.1 %>% diff)
  }
  
}

summary(lm(all.esophagus.trials$Prey_bead.x %>% diff ~ all.esophagus.trials$PostTemporalL.PectoralGirdleL.1 %>% diff))

pg.corr <- ccf(x = all.esophagus.trials$Prey_bead.x %>% diff, 
    y = all.esophagus.trials$PostTemporalL.PectoralGirdleL.1 %>% diff,
    na.action = na.pass)

pg.corr <- ccf(x = all.esophagus.trials$Prey_bead.x %>% diff, 
               y = all.esophagus.trials$SuspensoriumL.HyoidL.1 %>% diff,
               na.action = na.pass)


pg.corr <- ccf(x = all.esophagus.trials$Prey_bead.x %>% diff, 
               y = all.esophagus.trials$SuspensoriumL.LowerJawL.1 %>% diff,
               na.action = na.pass)


pg.corr <- ccf(x = all.esophagus.trials$Prey_bead.x %>% diff, 
               y = all.esophagus.trials$SuspensoriumL.OperculumL.1 %>% diff,
               na.action = na.pass)
max(pg.corr$acf)
