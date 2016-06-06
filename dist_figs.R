## N. Chardon
## Start: 3 Feb 2016
## Aim: Disturbance Figures

## Generated DF:
load('~/Desktop/Research/silene/r_files/statsum.RData')

## code from J. Prevey ##
### make the background black and white
theme_set(theme_bw(24))
#make all text large
theme_set(theme_bw(base_size =35))

rm(list=ls())

#load DFs, summarySE function, and libraries
load('~/Desktop/Research/silene/r_files/size15_trailsubset2.RData')
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData')
load('~/Desktop/Research/silene/r_files/size_lo.RData')
load('~/Desktop/Research/silene/r_files/size_hi.RData')
load('~/Desktop/Research/silene/r_files/cover_lo.RData')
load('~/Desktop/Research/silene/r_files/cover_hi.RData')

load('~/Desktop/Research/silene/r_files/id_numb.RData') #total presences per transect
load('~/Desktop/Research/silene/r_files/lowtrail.RData')
load('~/Desktop/Research/silene/r_files/lownotrail.RData')
load('~/Desktop/Research/silene/r_files/by_elev.RData')
load('~/Desktop/Research/silene/r_files/hitrail.RData')
load('~/Desktop/Research/silene/r_files/hinotrail.RData')
load('~/Desktop/Research/silene/r_files/sums.RData')
#load('Desktop/Research/silene/r_files/summarySE.RData')
library(ggplot2)
library(plotrix) #package for std.error()
#library(Rmisc) #package for summarySE()

################################################################################

#BARE vs. Silene size
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/size_bareT2.eps')
plot(area~soil, data=size15, type='n', xlim=c(0,26), xlab='% Bare Soil Cover',
     ylab=expression(paste('Maximum Cushion Size (cm'^'2',')')))
for(i in nrow(size15)) {
    foo <- size15[which(size15$low_elev==1),]
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==0),]
        points(area~soil, data=bar, cex=3, pch=6)
    }
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==1),]
        points(area~soil, data=bar, col='green', pch=6)
    }
}
for(i in nrow(size15)) {
    foo <- size15[which(size15$low_elev==0),]
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==0),]
        points(area~soil, data=bar, cex=3, pch=2)
    }
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==1),]
        points(area~soil, data=bar, col='blue', pch=2)
    }
}
legend('topright', pch=c(6,2), col=c('green', 'blue'), 
       title='Offtrail = black, Trail = colored',
       legend=c('Elevational Lower Limit', 'Elevational Center'))
dev.off()

#VEG vs. Silene size
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/size_vegT2.eps')
plot(area~veg, data=size15, type='n', xlim=c(0,100), xlab='% Vegetation Cover',
     ylab=expression(paste('Maximum Cushion Size (cm'^'2',')')))
for(i in nrow(size15)) {
    foo <- size15[which(size15$low_elev==1),]
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==0),]
        points(area~veg, data=bar, cex=3, pch=6)
    }
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==1),]
        points(area~veg, data=bar, col='green', pch=6)
    }
}
for(i in nrow(size15)) {
    foo <- size15[which(size15$low_elev==0),]
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==0),]
        points(area~veg, data=bar, cex=3, pch=2)
    }
    for (j in nrow(foo)) {
        bar <- foo[which(foo$trail==1),]
        points(area~veg, data=bar, col='blue', pch=2)
    }
}
legend('topright', pch=c(6,2), col=c('green', 'blue'), 
       title='Offtrail = black, Trail = colored',
       legend=c('Elevational Lower Limit', 'Elevational Center'))
dev.off()


## TRAIL/NO TRAIL FIGS ## ------------------------------------------------------
#add soil data to size15 and make cush col for ALL % COVER FIGS
cover152size15 <- match(size15$unique, cover15$unique)
size15$soil <- cover15$soil[cover152size15] 
for(i in 1:length(cover15$min)) {
    cover15$cush[i] <- sum(cover15$min[i], cover15$phlox[i], cover15$paro[i])
}

#make DFs (no MAM, KIN, NIW nor BIE, GRA)
#most recent figs coded with seT2 to indicate this DF
load('~/Desktop/Research/silene/r_files/size15_trailsubset2.RData') #subsetted for trail2
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') 
size_elev <- split(size15, size15$low_elev)
size_lo <- data.frame(size_elev[2])
size_hi <- data.frame(size_elev[1])
colnames(size_lo) <- colnames(size15)
colnames(size_hi) <- colnames(size15)

cover_elev <- split(cover15, cover15$low_elev)
cover_lo <- data.frame(cover_elev[2])
cover_hi <- data.frame(cover_elev[1])
colnames(cover_lo) <- colnames(cover15)
colnames(cover_hi) <- colnames(cover15)

save(size_lo, file="~/Desktop/Research/silene/r_files/size_lo.RData")
save(size_hi, file="~/Desktop/Research/silene/r_files/size_hi.RData")
save(cover_lo, file="~/Desktop/Research/silene/r_files/cover_lo.RData")
save(cover_hi, file="~/Desktop/Research/silene/r_files/cover_hi.RData")

#-------------------------------------------------------------------------------
# % cover of all categories 
statsum <- data.frame(variable=c('veg', 'sil', 'cush', 'wood', 'rock', 'grav',
                                 'soil'), se_lonotrail=NA, mean_lonotrail=NA,
                      se_lotrail=NA, mean_lotrail=NA, se_hinotrail=NA, mean_hinotrail=NA,
                      se_hitrail=NA, mean_hitrail=NA)

variable <- statsum$variable
for (i in 1:nrow(cover_lo)) {
    foo <- cover_lo[which(cover_lo$trail==0),]
    for (j in 1:length(variable)) {
        x <- statsum$variable[j]
        statsum$se_lonotrail[j] <- std.error(foo[,which(colnames(foo)==x)], na.rm=T)
        statsum$mean_lonotrail[j] <- mean(foo[,which(colnames(foo)==x)], na.rm=T)
    }
    foo <- cover_lo[which(cover_lo$trail==1),]
    for (j in 1:length(variable)) {
        x <- statsum$variable[j]
        statsum$se_lotrail[j] <- std.error(foo[,which(colnames(foo)==x)], na.rm=T)
        statsum$mean_lotrail[j] <- mean(foo[,which(colnames(foo)==x)], na.rm=T)
    }
}
for (i in 1:nrow(cover_hi)) {
    foo <- cover_hi[which(cover_hi$trail==0),]
    for (j in 1:length(variable)) {
        x <- statsum$variable[j]
        statsum$se_hinotrail[j] <- std.error(foo[,which(colnames(foo)==x)], na.rm=T)
        statsum$mean_hinotrail[j] <- mean(foo[,which(colnames(foo)==x)], na.rm=T)
    }
    foo <- cover_hi[which(cover_hi$trail==1),]
    for (j in 1:length(variable)) {
        x <- statsum$variable[j]
        statsum$se_hitrail[j] <- std.error(foo[,which(colnames(foo)==x)], na.rm=T)
        statsum$mean_hitrail[j] <- mean(foo[,which(colnames(foo)==x)], na.rm=T)
    }
}
save(statsum, file='~/Desktop/Research/silene/r_files/statsum2.RData')

#PLOT: all % cover
colv <- rainbow(length(statsum$variable))
theme_set(theme_bw(24)) #set plot background to white
mytheme <- theme(axis.text.x=element_text(colour='black',size=28), 
                 axis.text.y=element_text(colour='black',size=22),
                 axis.title.y=element_text(size=28),
                 plot.title=element_text(size=30),
                 legend.position='none')

#LOnotrail
ymin <- statsum$mean_lonotrail[1:7]-statsum$se_lonotrail[1:7]
ymax <- statsum$mean_lonotrail[1:7]+statsum$se_lonotrail[1:7]
all_cover <- ggplot(statsum, aes(variable, mean_lonotrail)) +
    geom_point(statsum, mapping=aes(variable[1:7], mean_lonotrail[1:7], 
                                    colour=colv[1:7], size=1)) +
    geom_errorbar(statsum, mapping=aes(variable[1:7], ymin=ymin[1:7], 
                                       ymax=ymax[1:7], colour=colv[1:7]),
                  width=0.3, size=1) +
    ggtitle('Elevational Lower Limit') +
    labs(x='', y='Mean % Cover +/- SE') +
    ylim(0,50) +
    mytheme 
#lotrail
ymin2 <- statsum$mean_lotrail[1:7]-statsum$se_lotrail[1:7] #need to assign diff. name
ymax2 <- statsum$mean_lotrail[1:7]+statsum$se_lotrail[1:7]
all_cover2 <- all_cover +
    geom_point(statsum, mapping=aes(variable[1:7], mean_lotrail[1:7], 
                                    colour=colv[1:7], size=1)) +
    geom_errorbar(statsum, mapping=aes(variable[1:7], ymin=ymin2[1:7], 
                                       ymax=ymax2[1:7], colour=colv[1:7]),
                  width=0.3, linetype=2, size=1)
ggsave(all_cover2, 
       file='~/Desktop/Research/silene/disturbance_paper/figs/allT2_coverLO.eps')

#HInotrail
ymin3 <- statsum$mean_hinotrail[1:7]-statsum$se_hinotrail[1:7]
ymax3 <- statsum$mean_hinotrail[1:7]+statsum$se_hinotrail[1:7]
all_cover3 <- ggplot(statsum, aes(variable, mean_hinotrail)) + 
    geom_point(statsum, mapping=aes(variable[1:7], mean_hinotrail[1:7], 
                                    colour=colv[1:7], size=1)) +
    geom_errorbar(statsum, mapping=aes(variable[1:7], ymin=ymin3[1:7], 
                                       ymax=ymax3[1:7], colour=colv[1:7]),
                  width=0.3, size=1) +
    ggtitle('Elevational Center') +
    labs(x='', y='Mean % Cover +/- SE') +
    ylim(0,50) + #keep above plot dimensions
    mytheme
#hitrail
ymin4 <- statsum$mean_hitrail[1:7]-statsum$se_hitrail[1:7] #need to assign diff. name
ymax4 <- statsum$mean_hitrail[1:7]+statsum$se_hitrail[1:7]
all_cover4 <- all_cover3 +
    geom_point(statsum, mapping=aes(variable[1:7], mean_hitrail[1:7], 
                                    colour=colv[1:7], size=1)) +
    geom_errorbar(statsum, mapping=aes(variable[1:7], ymin=ymin4[1:7], 
                                       ymax=ymax4[1:7], colour=colv[1:7]),
                  width=0.3, linetype=2, size=1)
ggsave(all_cover4, 
       file='~/Desktop/Research/silene/disturbance_paper/figs/allT2_coverHI.eps')

#create ggplot settings for SE BAR plots ---------------------------------------
theme_set(theme_bw(24)) #make the background black and white
mytheme <- theme(axis.text.x=element_text(colour='black',size=28), 
                 #axis.title.x=element_text(size=22,vjust=-0.3), #vjust=moves axis label
                 axis.text.y=element_text(colour='black',size=22),
                 axis.title.y=element_text(size=28,vjust=1),
                 legend.position='none',
                 axis.line=element_line(colour='black')) 
colv <- c('grey', 'orange')
#pd <- position_dodge(width=0.4)
#-------------------------------------------------------------------------------
load('~/Desktop/Research/silene/r_files/cover_lo.RData')
load('~/Desktop/Research/silene/r_files/cover_hi.RData')

## PLOT: VEG vs ELEV X TRAIL ##
#create plot components
means1 <- tapply(cover_lo$veg, cover_lo$trail, mean, na.rm=T) 
means2 <- tapply(cover_hi$veg, cover_hi$trail, mean, na.rm=T)
se1 <- tapply(cover_lo$veg, cover_lo$trail, std.error, na.rm=T)
se2 <- tapply(cover_hi$veg, cover_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)

#make ggplot
vegT_se2 <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail]) + #index for color
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=5) +
    labs(x='', y='% Vegetation Cover') +
    ylim(0,50) + #set y-axis limits
    scale_colour_manual(values=colv) +
    mytheme

#save with specified width
ggsave(vegT_se2, width=20, height=20, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/vegT_se2.eps')

## PLOT: BARE VS ELEV X TRAIL ##
#create plot components
means1 <- tapply(cover_lo$bare, cover_lo$trail, mean, na.rm=T) 
means2 <- tapply(cover_hi$bare, cover_hi$trail, mean, na.rm=T)
se1 <- tapply(cover_lo$bare, cover_lo$trail, std.error, na.rm=T)
se2 <- tapply(cover_hi$bare, cover_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)

#make ggplot
bareT_se2 <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail]) +
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=5) +
    labs(x='', y='% Bare Ground Cover') +
    ylim(0,25) + #set y-axis limits
    scale_colour_manual(values=colv) +
    mytheme

#save with specified width (Fig. 1 Biology'16)
ggsave(bareT_se2, width=20, height=20, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/bareT_se2.eps')


## PLOT: SIL COVER vs ELEV X TRAIL ##
#create plot components
cover_lo <- subset(cover_lo, quant_sil > 0) #for density at silene pops
cover_hi <- subset(cover_hi, quant_sil > 0) 
means1 <- tapply(cover_lo$sil, cover_lo$trail, mean, na.rm=T) 
means2 <- tapply(cover_hi$sil, cover_hi$trail, mean, na.rm=T)
se1 <- tapply(cover_lo$sil, cover_lo$trail, std.error, na.rm=T)
se2 <- tapply(cover_hi$sil, cover_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)

#make ggplot
silT_se2 <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail]) +
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=8) +
    labs(x='', y='Silene Density (% cover)') +
    ylim(0,7) +
    scale_colour_manual(values=colv) +
    mytheme

#save with specified width (Fig. 2A Biology'16)
ggsave(silT_se2, width=24.2, height=16.4, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/silT_se2.eps')

## PLOT: AREA vs ELEV x TRAIL ## 
#create plot variables
load('~/Desktop/Research/silene/r_files/cover_lo.RData') #reload for full df
load('~/Desktop/Research/silene/r_files/cover_hi.RData')
means1 <- tapply(size_lo$area, size_lo$trail, mean, na.rm=T) 
means2 <- tapply(size_hi$area, size_hi$trail, mean, na.rm=T)
se1 <- tapply(size_lo$area, size_lo$trail, std.error, na.rm=T)
se2 <- tapply(size_hi$area, size_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)
#make ggplot
areaT_se2 <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail]) + #index for color
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=8) +
    labs(x='', y=expression(paste('Maximum Cushion Size (cm'^'2',')'))) + #superscript
    scale_colour_manual(values=colv) +
    ylim(70,150) + #make axis ticks look good
    mytheme
#save with specified width
ggsave(areaT_se2, width=30.4, height=20.8, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/areaT_se2.eps')


## PLOT: MIN COVER vs ELEV X TRAIL ##
#create plot components
#cover_lo <- subset(cover_lo, min > 0) #for density at min pops
#cover_hi <- subset(cover_hi, min > 0) 
means1 <- tapply(cover_lo$min, cover_lo$trail, mean, na.rm=T) 
means2 <- tapply(cover_hi$min, cover_hi$trail, mean, na.rm=T)
se1 <- tapply(cover_lo$min, cover_lo$trail, std.error, na.rm=T)
se2 <- tapply(cover_hi$min, cover_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)

#make ggplot
minT_se2 <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail]) +
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=8) +
    labs(x='', y='Minuartia Density (% cover)') +
    ylim(0,3) +
    scale_colour_manual(values=colv) +
    mytheme
#save 
ggsave(minT_se2, width=24.2, height=16.4, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/minT_se2.eps')

## PLOT: PHLOX COVER vs ELEV X TRAIL ##
#create plot components
means1 <- tapply(cover_lo$phlox, cover_lo$trail, mean, na.rm=T) 
means2 <- tapply(cover_hi$phlox, cover_hi$trail, mean, na.rm=T)
se1 <- tapply(cover_lo$phlox, cover_lo$trail, std.error, na.rm=T)
se2 <- tapply(cover_hi$phlox, cover_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)

#make ggplot
phloxT_se2 <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail]) +
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=8) +
    labs(x='', y='Phlox Density (% cover)') +
    ylim(0,0.6) +
    scale_colour_manual(values=colv) +
    mytheme
#save 
ggsave(phloxT_se2, width=20, height=20, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/phloxT_se2.eps')


## PLOT: PARO COVER vs ELEV X TRAIL ##
#create plot components
means1 <- tapply(cover_lo$paro, cover_lo$trail, mean, na.rm=T) 
means2 <- tapply(cover_hi$paro, cover_hi$trail, mean, na.rm=T)
se1 <- tapply(cover_lo$paro, cover_lo$trail, std.error, na.rm=T)
se2 <- tapply(cover_hi$paro, cover_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)

#make ggplot
paroT_se2 <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail]) +
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=5) +
    labs(x='', y='Paronychia Density (% cover)') +
    ylim(0,0.6) +
    scale_colour_manual(values=colv) +
    mytheme
#save 
ggsave(paroT_se2, width=20, height=20, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/paroT_se2.eps')

## PLOT: AVGSMSZ vs ELEV x TRAIL ## 
load('~/Desktop/Research/silene/r_files/cover_lo.RData') #subsetted for trail
load('~/Desktop/Research/silene/r_files/cover_hi.RData') 

#create plot components
means1 <- tapply(cover_lo$avgsmsize, cover_lo$trail, mean, na.rm=T) 
means2 <- tapply(cover_hi$avgsmsize, cover_hi$trail, mean, na.rm=T)
se1 <- tapply(cover_lo$avgsmsize, cover_lo$trail, std.error, na.rm=T)
se2 <- tapply(cover_hi$avgsmsize, cover_hi$trail, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Lower Range Limit', 'Lower Range Limit', 'Range Center', 'Range Center'),
                Trail=c('0','1','0','1'),
                mean=means, lower=lowers, upper=uppers)

#make ggplot
avgsmsizeT_se <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location,ymin=lower,ymax=upper,
                                 width=0.1), size=2, colour=colv[d$Trail],
                  position='dodge', width=0.25) +
    geom_point(d, mapping=aes(Location, y=mean, colour=colv[d$Trail]), size=8) +
    labs(x='', y=expression(paste('Average Small Size (cm'^'2',')'))) +
    #ylim(0,3) +
    scale_colour_manual(values=colv) +
    mytheme
#save with specified width
ggsave(avgsmsizeT_se, width=30.4, height=20.8, units=c('cm'),
       file='~/Desktop/Research/silene/disturbance_paper/figs/avgsmszT_se.eps')


#----------------------------------------------------------------------------#

## HIDIST/NO HIDIST FIGS (SE BARS) ##------------------------------------------

#create cover df by disturbance
cover15 <- subset(cover15, veg<=100)
cover_dist <- split(cover15, cover15$hidist)
nodist <- data.frame(cover_dist[1])
dist <- data.frame(cover_dist[2])
colnames(nodist) <- colnames(cover15)
colnames(dist) <- colnames(cover15)

#PLOT: Vegetation cover by disturbance vs. elevation
#set up plot components
#datsum <- summarySE(nodist, measurevar="veg", groupvars="low_elev", na.rm=T)
means1 <- tapply(nodist$veg, nodist$low_elev, mean, na.rm=T)
means2 <- tapply(dist$veg, dist$low_elev, mean, na.rm=T)
se1 <- tapply(nodist$veg, nodist$low_elev, std.error, na.rm=T)
se2 <- tapply(dist$veg, dist$low_elev, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Center', 'Lower Limit'), mean=means, 
                lower=lowers, upper=uppers)
d$Location <- c("Center (ND)", "Lower Limit (ND)", 
                "Center (D)", "Lower Limit (D)")
#make ggplot
veg_se <- ggplot(d, aes(Location, mean)) +
        geom_errorbar(d, mapping=aes(Location, ymin=lower, ymax=upper, width=0.2, 
                              position='dodge')) +
        geom_point(d, mapping=aes(Location, y=mean), size=4) +
        labs(x='Range Location', y='% Vegetation Cover')
ggsave(veg_se, file='Desktop/Research/silene/disturbance_paper/figs/veg_se.pdf')

#PLOT: Bare ground cover by disturbance vs. elevation
#set up plot components
means1 <- tapply(nodist$bare, nodist$low_elev, mean, na.rm=T)
means2 <- tapply(dist$bare, dist$low_elev, mean, na.rm=T)
se1 <- tapply(nodist$bare, nodist$low_elev, std.error, na.rm=T)
se2 <- tapply(dist$bare, dist$low_elev, std.error, na.rm=T)
means <- c(means1,means2)
lowers <- c(means1-se1, means2-se2)
uppers <- c(means1+se1, means2+se2)
d <- data.frame(Location=c('Center', 'Lower Limit'), mean=means, 
                lower=lowers, upper=uppers)
d$Location <- c("Center (ND)", "Lower Limit (ND)", 
                "Center (D)", "Lower Limit (D)")
#make ggplot
bare_se <- ggplot(d, aes(Location, mean)) +
    geom_errorbar(d, mapping=aes(Location, ymin=lower, ymax=upper, width=0.2, 
                                 position='dodge')) +
    geom_point(d, mapping=aes(Location, y=mean), size=4) +
    labs(x='Range Location', y='% Bare Ground Cover')
ggsave(bare_se, file='Desktop/Research/silene/disturbance_paper/figs/bare_se.pdf')

#PLOT: Sil cover by elev vs. disturbance


#PLOT: Area by site vs. disturbance
#set up plot components
means <- tapply(size15$area, size15$hi, mean, na.rm=T)
se <- tapply(size15$area, size15$hi, std.error, na.rm=T)
d <- data.frame(Disturbance=c('No Disturbance', 'Disturbance'), mean=means, 
                lower=c(means-se), upper=c(means+se))
#make ggplot
area_se <- ggplot(d, aes(Disturbance, mean)) +
    geom_errorbar(d, mapping=aes(Disturbance,ymin=lower,ymax=upper,width=0.2),
                  size=2) +
    geom_point(d, mapping=aes(Disturbance, y=mean), size=8) +
    labs(x='', y='Maximum Cushion Size (cm^2)') +
    theme(axis.text.x=element_text(colour='black',size=20,vjust=-1), #fix vjust!!
          axis.text.y=element_text(size=16),
          axis.title.y=element_text(size=20))
ggsave(area_se, file='Desktop/Research/silene/disturbance_paper/figs/area_se.pdf')

## -----------------------------------------------------------------------------
## Lattice Figures ##
## -----------------------------------------------------------------------------
library(lattice)
xyplot(min ~ low_elev | trail, data = cover15)

xyplot(area ~ low_elev | trail, groups=veg, data=size15)
