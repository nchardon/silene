## N. Chardon
## Start: 9 Dec 2015
## Aim: Justify use of 5 largest plants
## Generated dataframes:
load('~/Desktop/Research/silene/r_files/means.RData') #site means
load('~/Desktop/Research/silene/r_files/means_na.RData') #NA site means
load('~/Desktop/Research/silene/r_files/fives.RData') #largest 5 by quad stats

rm(list=ls())
# Load Libraries
library(quantreg)
library(raster)

setwd("~/Desktop/Research/silene/r_files/")
load('size.RData')
load('sums14.RData')
load('cover.Rdata')

#size for nth percentile at each site and other averages ======================
site <- unique(size$site)
means <- data.frame(site)
for(i in 1:length(site)) {
        foo <- size[which(size$site==site[i]),]
        means$lat[i] <- mean(foo$lat, na.rm=T)
        means$long[i] <- mean(foo$long, na.rm=T)
        means$elev[i] <- mean(foo$elev, na.rm=T)
        means$mean[i] <- mean(foo$areacm, na.rm=T)
        means$med[i] <- median(foo$areacm, na.rm=T)
        perc <- quantile(foo$areacm, 
                         c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), na.rm=T)
        means$ten[i] <- perc[1] 
        means$twenty[i] <- perc[2] 
        means$thirty[i] <- perc[3] 
        means$forty[i] <- perc[4] 
        means$fifty[i] <- perc[5] 
        means$sixty[i] <- perc[6] 
        means$seventy[i] <- perc[7] 
        means$eighty[i] <- perc[8] 
        means$ninety[i] <- perc[9] 
        means$hund[i] <- perc[10] 
        means$number[i] <- 0.1 * length(foo$areacm) #obs. for 90th percentile
        means$cv[i] <- cv(foo$areacm, na.rm=T)
        quant <- means$ninety[i]
        chop <- subset(foo, areacm>=quant) #cut off for 90th perc.
        means$cvninety[i] <- cv(chop$areacm, na.rm=T)
        means$cvexp[i] <- means$cvninety[i]/means$cv[i]*100 #% of cv explained
}

sums142means <- match(means$site, sums14$id_names)
means$reldens <- sums14$reldens[sums142means] 
means$estdens <- sums14$estdens[sums142means] 
means$ros <- sums14$ros[sums142means] 
means$sitetotal <- sums14$total[sums142means] 

#pattern seen in biggest plants
fit_ninety <- (lm(ninety~lat+ I(lat^2), data=means_na)) #all terms p<0.05
fit_mean <- (lm(mean~lat + I(lat^2), data=means_na))

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/justify_five.eps')
par(bg='lightgrey')
plot(mean~lat, data=means_na, pch=16, ylim=c(min(means_na$mean), 350),
     xlab='Latitude (º)', ylab='Plant Area (cm^2)')
points(ninety~lat, data=means_na, pch=16, col='hot pink')
xx<-seq(min(means_na$lat),max(means_na$lat))
yy<-coef<-fit_mean$coef %*% rbind(1,xx,xx^2)
lines(xx,yy)
xx<-seq(min(means_na$lat),max(means_na$lat))
yy<-coef<-fit_ninety$coef %*% rbind(1,xx,xx^2)
lines(xx,yy, col='hot pink')
legend('topright', lty=c(1,1), col=c('black', 'hot pink'), 
       legend=c('Site Means', '90th Percentiles*'))
dev.off()

x_90 <- cbind(means$lat, means$lat^2)
y_90 <- cbind(means$mean)
summary(rq(y_90 ~ x_90, tau=0.9))

#5 largest per quad ============================================================
#remove NA values
size = size[is.na(size$areacm) == FALSE,]
id <- unique(size$id) 
fives <- as.data.frame(id)
for(i in 1:length(unique(size$id))) {
        foo <- size[which(size$id==id[i]),]
        fives$quant[i] <- length(foo$areacm) #number of plants in quad
        fives$mean[i] <- mean(foo$areacm)
        bar <- foo$areacm #isolate area of all plants
        ordered <- sort(bar, decreasing=T) #sort plants by size
        if(length(ordered)>5) { #calculations for quads with >5 plants
                fives$min[i] <- ordered[5]
                fives$max[i] <- ordered[1]
                fives$mean_5[i] <- mean(ordered[1:5])
                fives$median_5[i] <- median(ordered[1:5])
                fives$cv_5[i] <- cv(ordered[1:5], na.rm=T)
        }
        else { #calculations for quads with <= 5 plants
                fives$min[i] <- min(ordered)
                fives$max[i] <- max(ordered)
                fives$mean_5[i] <- mean(ordered)
                fives$median_5[i] <- median(ordered)
        }
        fives$two[i] <- ordered[2]
        fives$three[i] <- ordered[3]
        fives$four[i] <- ordered[4]
        fives$cv_all[i] <- cv(bar, na.rm=T)
        fives$cv_expl[i] <- fives$cv_five[i]/fives$cv_all[i]*100 #% of cv explained
}
fives$site <- substr(as.character(fives$id), 1, 3)

#add elev to fives df
size2fives <- match(fives$id, size$id)
fives$elev <- size$elev[size2fives]

#save fives dfs
save(fives, file = '~/Desktop/Research/silene/r_files/fives.RData')

#5 largest vs. mean size patterns: no trend
sites <- unique(fives$site)
sites1 <- c('whe', 'tao', 'niw', 'lat', 'bal')
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/max.mean_elev.eps')
plot(mean~elev, data=fives, type='n',
     xlim=c(3350,4050), ylim=c(0,1000),
     ylab='Silene cushion area (cm^2)', xlab='Elevation (m)')
for(i in 1:length(sites1)) {
    foo <- fives[which(fives$site==sites1[i]),]
    for(j in 1:length(foo$mean)) {
        points(mean~elev, data=foo, pch=i, cex=1.5)
        points(mean_5~elev, data=foo, pch=i, col='red', cex=0.7)
    }
}
legend('topright', pch=1:5, legend=sites1)
dev.off()

#site abundance and max sizes ==================================================
site <- unique(fives$site)
for(i in 1:length(unique(fives$site))) {
        foo <- fives[which(fives$site==site[i]),]
        means$mean_maxs[i] <- mean(foo$max)
        means$quads[i] <- length(unique(foo$id))
}
means$rel_abund <- means$sitetotal/means$quads

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/abundance14.eps')
par(mfrow=c(2,2))
plot(sitetotal~mean_maxs, data=means)
plot(rel_abund~mean_maxs, data=means)
plot(reldens~mean_maxs, data=means)
plot(estdens~mean_maxs, data=means)
dev.off()

#calculate % of plants in diff. size classes ===================================
# Proportion of plants in each size class to match Silene matrices size dist.
# Breaks = size classes from Doak & Morris (pers.comm. Apr. 2015)
# average ros area = 16.9mm^2 = 0.169cm^2
site <- means$site
for(i in 1:length(site)) {
        foo <- size[which(size$site==site[i]),]
        histo <- hist(foo$areacm, plot = F, 
            breaks = c(0, 0.845, 0.169, 3.211, 12.5, 25, 50, 100, 200, 3461.85))
        means$hist1[i] <- histo$counts[1]/means$sitetotal[i]*100 #perc. in class
        means$hist2[i] <- histo$counts[2]/means$sitetotal[i]*100
        means$hist3[i] <- histo$counts[3]/means$sitetotal[i]*100
        means$hist4[i] <- histo$counts[4]/means$sitetotal[i]*100
        means$hist5[i] <- histo$counts[5]/means$sitetotal[i]*100
        means$hist6[i] <- histo$counts[6]/means$sitetotal[i]*100
        means$hist7[i] <- histo$counts[7]/means$sitetotal[i]*100
        means$hist8[i] <- histo$counts[8]/means$sitetotal[i]*100
        means$hist9[i] <- histo$counts[9]/means$sitetotal[i]*100
}

#climate data ================================================================
load('~/Desktop/Research/silene/r_files/ibut_dat.RData') 

#add summer climate stats
site <- means$site
for(i in 1:length(site)) {
        foo <- ibut_dat[which(ibut_dat$site==site[i]),]
        means$junavg[i] <- mean(foo$junavg, na.rm=T)
        means$julavg[i] <- mean(foo$julavg, na.rm=T)
        means$augavg[i] <- mean(foo$augavg, na.rm=T)
        }
#no ibuttons at SUB, so subsitute NIW climate
means[8,]$junavg <- means[10,]$junavg
means[8,]$julavg <- means[10,]$julavg
means[8,]$augavg <- means[10,]$augavg
means[8,]$jja <- means[10,]$jja

#make N. America df and save means dfs
means_na <- means[-c(3:4, 12, 14),]
save(means, file = '~/Desktop/Research/silene/r_files/means.RData')
save(means_na, file = '~/Desktop/Research/silene/r_files/means_na.RData')

#climate and max sizes ========================================================
load('~/Desktop/Research/silene/r_files/ibut_dat.RData') 
load('~/Desktop/Research/silene/r_files/fives.RData')
load('~/Desktop/Research/silene/r_files/size.RData')
load('~/Desktop/Research/silene/r_files/cover.RData')

#match max sizes and cover to clim data
fives2ibut_dat <- match(ibut_dat$quad_id, fives$id) 
ibut_dat$sz_mean <- fives$mean[fives2ibut_dat]
ibut_dat$sz_mean5 <- fives$mean_5[fives2ibut_dat]
ibut_dat$sz_max5 <- fives$max[fives2ibut_dat]
ibut_dat$sz_min5 <- fives$min[fives2ibut_dat]
cover$veg <- cover$forb + cover$grass
cover2ibut_dat <- match(ibut_dat$quad_id, cover$id)
ibut_dat$veg <- cover$veg[cover2ibut_dat]

#save expanded DF
save(ibut_dat, file = '~/Desktop/Research/silene/r_files/ibut_dat.RData')

#mean vs. max size relationship with climate (residuals not great but ok)
fit1 <- lmer(sz_max5~julavg + (1|site), data=ibut_dat)
fit2 <- lmer(sz_mean5~julavg + (1|site), data=ibut_dat)
fit3 <- lmer(sz_mean~julavg + (1|site), data=ibut_dat)

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/sizes_julavg.eps')
plot(sz_max5~julavg, data=ibut_dat, col='red', pch=2, cex=1.6,
     ylim=c(min(ibut_dat$sz_mean, na.rm=T), 900), xlim=c(6,14), #omit 3 outliers
     ylab='Silene cushion area (cm^2)', xlab='July Average Temperature (ºC)')
points(sz_mean5~julavg, data=ibut_dat, col='blue', cex=1.2)
points(sz_mean~julavg, data=ibut_dat, pch=16, cex=0.6)
legend('topleft', pch=c(2,1,16), col=c('red','blue','black'),
       legend=c('Quad Largest Silene','Quad Mean of 5 Largest Silene',
                'Quad Mean'))
dev.off()

## PERCENTILES ##
#size class percentiles to clim data: pattern maybe w/ mid to large size classes
colors <- c('blue','blue','blue','black','black','black','red','red','red')
pch <- c(2,1,16,2,1,16,2,1,16)
cex <- c(1.6,1.2,0.6,1.6,1.2,0.6,1.6,1.2,0.6)
index <- c(19:27)

group1 <- rbind(means_na[2,], means_na[5:6,], means_na[8:10,]) #CO & NM (n=6)
group2 <- rbind(means_na[1,], means_na[3:4,], means_na[7,]) #AK (n=4)

#size class perc to clim: no clear pattern
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/perc_julavg.eps')
plot(means_na[,19]~julavg, data=means_na, type='n', xlim=c(8.5,13),
     ylim=c(min(means_na[19:27]), max(means_na[19:27])),
     ylab='% in Size Class', xlab='July Average Temperature (ºC)')
for(i in 1:length(index)) {
     points(means_na[,index[i]]~means_na$julavg, col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(means_na[19:27]))
dev.off()

#size class perc. by grouped elev vs. clim: bigger response in mid or high classes
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/perc.elev1_julavg.eps')
plot(group1[,19]~group1$julavg, type='n', 
     ylim=c(min(group1[19:27]), max(group1[19:27])), xlim=c(9,13),
     ylab='% in Size Class', xlab='July Average Temperature (ºC)')
for(i in 1:length(index)) {
        points(group1[,index[i]]~group1$julavg, col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(group1[19:27]))
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/perc.elev2_julavg.eps')
plot(group2[,19]~group2$julavg, type='n', 
     ylim=c(min(group2[19:27]), max(group2[19:27])), xlim=c(8.5,13),
     ylab='% in Size Class', xlab='July Average Temperature (ºC)')
for(i in 1:length(index)) {
        points(group2[,index[i]]~group2$julavg, col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(group2[19:27]))
dev.off()

#size class percentiles to elev data: no distinguishable pattern seen
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/perc_elev.eps')
plot(means_na[,19]~elev, data=means_na, type='n', 
     ylim=c(min(means_na[19:27]), max(means_na[19:27])),
     ylab='% in Size Class', xlab='Elevation (m)')
for(i in 1:length(index)) {
        points(means_na[,index[i]]~means_na$elev, col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(means_na[19:27]))
dev.off()

#plot size class perc. by grouped elev: mid to high perc. show biggest response
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/perc_elev1.eps')
plot(group1[,19]~group1$elev, type='n', 
     ylim=c(min(group1[19:27]), max(group1[19:27])), xlim=c(3490,4000),
     ylab='% in Size Class', xlab='Elevation (m)')
for(i in 1:length(index)) {
        points(group1[,index[i]]~group1$elev, col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(group1[19:27]))
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/perc_elev2.eps')
plot(group2[,19]~group2$elev, type='n', 
     ylim=c(min(group2[19:27]), max(group2[19:27])), xlim=c(70, 1650),
     ylab='% in Size Class', xlab='Elevation (m)')
for(i in 1:length(index)) {
        points(group2[,index[i]]~group2$elev, col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(group2[19:27]))
dev.off()

#absolute percentiles (10th-100th) and clim: more var and stronger trend! 
colors <- c('blue','blue','blue','black','black','black','red','red','red')
pch <- c(2,1,16,2,1,16,2,1,16)
cex <- c(1.6,1.2,0.6,1.6,1.2,0.6,1.6,1.2,0.6)
index <- c(7:15)
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/sizepercs_julavg.eps')
plot(ten~julavg, data=means_na, type='n', xlab='July Average Temperature (ºC)',
     ylab='Silene Cushion Area (cm^2)', xlim=c(8.4,13.5),
     ylim=c(min(means_na$ten), max(means_na$ninety))) #scale fig better w/o 100th
for(i in 1:length(index)) {
        points(means_na[,index[i]]~means_na$julavg, 
               col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(means_na[7:15]))
dev.off()

#by region: AK vs. CO&NM
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/sizepercs_julavgCONM.eps')
plot(ten~julavg, data=group1, type='n', main='Colorado & New Mexico',
     xlab='July Average Temperature (ºC)',
     ylab='Silene Cushion Area (cm^2)', xlim=c(8.8,13.5),
     ylim=c(min(means_na$ten), max(means_na$ninety))) #scale fig better w/o 100th
for(i in 1:length(index)) {
        points(group1[,index[i]]~group1$julavg, 
               col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(group1[7:15]))
dev.off()
colors <- c('blue','blue','blue','black','black','black','black','red','red','red')
pch <- c(2,1,16,2,1,16,17,2,1,16)
cex <- c(1.6,1.2,0.6,1.6,1.8,1.2,0.6,1.6,1.2,0.6)
index <- c(7:16)
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/allsizepercs_ppt.eps')
plot(ten~p_jja, data=group1, type='n', main='Colorado & New Mexico',
     ylim=c(min(means_na$ten), max(means_na$hund)),
     xlab='JJA Precipitation (mm)',
     ylab='Silene Cushion Area (cm^2)', xlim=c(56,86))
for(i in 1:length(index)) {
        points(group1[,index[i]]~group1$p_jja, 
               col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(group1[7:16]))
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/sizepercs_julavgAK.eps')
plot(ten~julavg, data=group2, type='n', main='Alaska',
     xlab='July Average Temperature (ºC)',
     ylab='Silene Cushion Area (cm^2)', xlim=c(8.5,13),
     ylim=c(min(means_na$ten), max(means_na$ninety))) #scale fig better w/o 100th
for(i in 1:length(index)) {
        points(group2[,index[i]]~group2$julavg, 
               col=colors[i], pch=pch[i], cex=cex[i])
}
legend('topright', pch=pch, col=colors, 
       legend=colnames(group2[7:15]))
dev.off()