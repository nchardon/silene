## N. Chardon
## Start: 9 Nov 2015
## Aim: abundance analyses
## Generated Dataframes:

rm(list=ls())

#load datasets
setwd("~/Desktop/Research/silene/r_files")
load('size15.RData')
load('size.RData')
load('sums.RData')
load('sums14.RData')
load('gini.RData')
load('gini15.RData')

############
# 2015 Data
############

#Wilcoxon rank sum tests: sig. diff. at center and limit with dist
wilcox.test(sums$lt_dens, sums$lnt_dens) #p=0.001673
wilcox.test(sums$lt_dens, sums$ht_dens) #p=0.4028
wilcox.test(sums$lnt_dens, sums$hnt_dens) #p=0.0.5548
wilcox.test(sums$ht_dens, sums$hnt_dens) #p=0.000528

wilcox.test(sums$lt_reldens, sums$lnt_reldens) #p=0.001673
wilcox.test(sums$lt_reldens, sums$ht_reldens) #p=0.3653
wilcox.test(sums$lnt_reldens, sums$hnt_reldens) #p=0.5548
wilcox.test(sums$ht_reldens, sums$hnt_reldens) #p=0.0002967

setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/density.eps')
par(mfrow=c(2,1))
boxplot(sums$lt_dens, sums$lnt_dens, ylab='Density of 5 Largest Plants per Quad',
        xlab='Range Limit Disturbed vs. Undisturbed')
boxplot(sums$ht_dens, sums$hnt_dens, ylab='Density of 5 Largest Plants per Quad',
        xlab='Range Center Disturbed vs. Undisturbed')
dev.off()

#Wilcoxon rank sum test on % cover of Silene: higher % cover at disturbed
foo <- cover15[which(cover15$low_elev=='1'),]
foo1 <- foo[which(foo$trail=='1'),]
foo2 <- foo[which(foo$trail=='0'),]
wilcox.test(foo1$sil, foo2$sil) #p-value < 2.2e-16

setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/percover.eps')
par(mfrow=c(2,2))
boxplot(foo1$sil, foo2$sil, xlab='Range Limit Disturbed vs. Undisturbed', 
        ylab='% Cover Silene')

foo <- cover15[which(cover15$low_elev=='0'),]
foo1 <- foo[which(foo$trail=='1'),]
foo2 <- foo[which(foo$trail=='0'),]
wilcox.test(foo1$sil, foo2$sil) #p-value < 2.2e-16
boxplot(foo1$sil, foo2$sil, xlab='Range Center Disturbed vs. Undisturbed', 
        ylab='% Cover Silene')

foo <- cover15[which(cover15$low_elev=='0'),]
foo1 <- foo[which(foo$trail=='1'),]
foo <- cover15[which(cover15$low_elev=='1'),]
foo2 <- foo[which(foo$trail=='1'),]
wilcox.test(foo1$sil, foo2$sil) #p-value = 0.0007644
boxplot(foo1$sil, foo2$sil, 
        xlab='Range Center Disturbed vs. Range Limit Disturbed', 
        ylab='% Cover Silene')

foo <- cover15[which(cover15$low_elev=='0'),]
foo1 <- foo[which(foo$trail=='0'),]
foo <- cover15[which(cover15$low_elev=='1'),]
foo2 <- foo[which(foo$trail=='0'),]
wilcox.test(foo1$sil, foo2$sil) #p-value = 0.005
boxplot(foo1$sil, foo2$sil, 
        xlab='Range Center Undisturbed vs. Range Limit Undisturbed', 
        ylab='% Cover Silene')
dev.off()

############
# 2014 Data
############

#add lat, elev, and gini to sums14
gini2sums14 <- match(sums14$id_names, gini$site_names)
sums14$lat <- gini$lat[gini2sums14] 
sums14$elev <- gini$elev[gini2sums14] 
sums14$gini <- gini$gini[gini2sums14] 

#make NA sums14 df
sums14_na <- sums14[-c(1,3,11,12),]

#linear regression: only sig. relationship with ros
fit <- lm(sums14_na$ros~sums14_na$lat) #p=0.086, r^2=0.32
plot(sums14_na$ros~sums14_na$lat)
abline(fit)

fit <- lm(sums14_na$ros~sums14_na$elev) #p=0.02, r^2=0.56
setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/rosettes.eps')
plot(sums14_na$ros~sums14_na$elev, xlab='Elevation (m)', 
     ylab='Small Plants (<20 rosettes) as % of Population')
abline(fit)
dev.off()