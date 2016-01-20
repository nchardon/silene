## N. Chardon
## Start: 9 Dec 2015
## Aim: Justify use of 5 largest plants
## Generated dataframes:
load('~/Desktop/Research/silene/r_files/means.RData')
load('~/Desktop/Research/silene/r_files/means_na.RData')

rm(list=ls())
# Load Libraries
library(quantreg)
library(raster)

setwd("~/Desktop/Research/silene/r_files/")
load('size.RData')
load('sums14.RData')

#size for 90th percentile at each site and other averages ======================
site <- unique(size$site)
means <- data.frame(site)
for(i in 1:length(site)) {
        foo <- size[which(size$site==site[i]),]
        means$lat[i] <- mean(foo$lat, na.rm=T)
        means$elev[i] <- mean(foo$elev, na.rm=T)
        means$mean[i] <- mean(foo$areacm, na.rm=T)
        means$med[i] <- median(foo$areacm, na.rm=T)
        means$ninety[i] <- quantile(foo$areacm, 0.9, na.rm=T)
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

means_na <- means[-c(3:4, 12, 14),]

#save means dfs
save(means, file = '~/Desktop/Research/silene/r_files/means.RData')
save(means_na, file = '~/Desktop/Research/silene/r_files/means_na.RData')

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
        bar <- foo$areacm #isolate area of all plants
        ordered <- sort(bar, decreasing=T) #sort plants by size
        if(length(ordered)>5) { #calculations for quads with >5 plants
                fives$min[i] <- ordered[5]
                fives$max[i] <- ordered[1]
                fives$mean[i] <- mean(ordered[1:5])
                fives$median[i] <- median(ordered[1:5])
                fives$cv_five[i] <- cv(ordered[1:5], na.rm=T)
        }
        else { #calculations for quads with <= 5 plants
                fives$min[i] <- min(ordered)
                fives$max[i] <- max(ordered)
                fives$mean[i] <- mean(ordered)
                fives$median[i] <- median(ordered)
        }
        fives$two[i] <- ordered[2]
        fives$three[i] <- ordered[3]
        fives$four[i] <- ordered[4]
        fives$cv_all[i] <- cv(bar, na.rm=T)
        fives$cv_expl[i] <- fives$cv_five[i]/fives$cv_all[i]*100 #% of cv explained
}
fives$site <- substr(as.character(fives$id), 1, 3)

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
#ibutton id with lat, long, elev, site, and summer climate averages

#STOP 20 JAN: loop should report NA where no ibuttons, not running through correctly
site <- means$site
for(i in 1:length(site)) {
        foo <- ibut_dat[which(ibut_dat$site==site[i]),]
        means$junavg[i] <- mean(foo$junavg)
}