## N. Chardon
## Start: 23 Oct 2015
## Aim: use Lorenz curves, associated estimators, and KS Test to analyze size structure
## Generated Dataframes:
load('~/Desktop/Research/silene/r_files/gini.RData')
load('~/Desktop/Research/silene/r_files/gini15.RData')

rm(list=ls())

#load datasets
setwd("~/Desktop/Research/silene/r_files")
load('size15.RData')
load('size.RData')
load('sites.RData')
load('lowtrail.RData') #lowtrail_df
load('lownotrail.RData') #lownotrail_df
load('hitrail.RData') #hitrail_df
load('hinotrail.RData') #hinotrail_df

#load libraries
library(ineq)

############
#2014 Data
############
#make vector of site names to use in loops
site_names <- names(sites)

#histograms
setEPS() #vectorized image
postscript('~/Desktop/Research/silene/lorenz_figs/hist.eps')
par(mfrow=c(4,4)) #define graph outside loop
for (j in 1:length(sites)) {
        foo <- as.data.frame(sites[which(names(sites)==site_names[j],)])
        main <- site_names[j]
        hist(foo[,14], breaks=20, main=main, xlab='Plant Area (cm^2)')
}
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/loghist.eps')
par(mfrow=c(4,4)) #define graph outside loop
for (j in 1:length(sites)) {
        foo <- as.data.frame(sites[which(names(sites)==site_names[j],)])
        foo[,14] <- log(foo[,14])
        main <- site_names[j]
        hist(foo[,14], breaks=20, main=main, xlab='Log(Plant Area)')
}
dev.off()

#dataframe for estimators
gini <-  data.frame(site_names)
gini$lat <- site.lat
gini$elev <- site.elev
gini$reg <- c('es', 'co', 'ch', 'nm', 'co', 'no.ak', 'co', 'nm', 'so.ak', 
              'no.ak', 'es', 'ch', 'nm', 'so.ak')
gini$reg <- as.character(gini$reg)

#gini index (gini)
for (j in 1:length(sites)) {
        foo <- as.data.frame(sites[which(names(sites)==site_names[j],)])
        gini$gini[j] <- ineq(foo[,14], type='Gini')
}

#pietra index (rs)
for (j in 1:length(sites)) {
        foo <- as.data.frame(sites[which(names(sites)==site_names[j],)])
        gini$rs[j] <- ineq(foo[,14], type='RS')
}

#coefficient of variation (cv)
for (j in 1:length(sites)) {
        foo <- as.data.frame(sites[which(names(sites)==site_names[j],)])
        gini$cv[j] <- ineq(foo[,14], type='var')
}

#lorenz asymmetry coefficient (s)
gini$s <- c(NA)
for (j in 1:length(sites)) {
        foo <- as.data.frame(sites[which(names(sites)==site_names[j],)])
        gini$s[j] <- Lasym(foo[,14], na.rm=T)
}

#save gini dataframe
save(gini, file='~/Desktop/Research/silene/r_files/gini.RData')

#sort gini df by region
gini_sort <- gini[order(gini$reg),]

#splitting continents
gini_na <- gini[-c(1,3,11,12),]
gini_eu <- gini[c(1,3,11,12),]

#splitting by s<1
gini_na_s <- gini_na[-c(4,9,10),]

#linear models, ANOVA, & estimator correlation
summary(lm(gini~lat+I(lat^2), data=gini_na))
summary(lm(rs~lat+I(lat^2), data=gini_na))
summary(lm(cv~lat+I(lat^2), data=gini_na))
summary(lm(s~lat+I(lat^2), data=gini_na))

summary(lm(gini~elev, data=gini_na_s))
summary(lm(rs~elev, data=gini_na_s))
summary(lm(cv~elev, data=gini_na_s))
summary(lm(s~elev, data=gini_na_s))

summary(lm(gini~s, data=gini))

anova(lm(s~reg, data=gini))

cor(gini$gini, gini$rs)
cor(gini$gini, gini$cv)
cor(gini$gini, gini$s)
cor(gini$rs, gini$cv)
cor(gini$rs, gini$s)
cor(gini$cv, gini$s)

#lorenz curves
setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/all_lorenz.eps')
par(mfrow=c(3,2), pty='s') #organizating of figs, and make square figs
plot(Lc(sites$whe[,14]), main='New Mexico')
lines(Lc(sites$tao[,14]), lty=3)
lines(Lc(sites$lat[,14]), lty=6)
plot(Lc(sites$bal[,14]), main='Colorado')
lines(Lc(sites$sub[,14]), lty=3)
lines(Lc(sites$niw[,14]), lty=6)
plot(Lc(sites$tho[,14]), main='Southern Alaska')
lines(Lc(sites$wra[,14]), lty=3)
plot(Lc(sites$too[,14]), main='Northern Alaska')
lines(Lc(sites$pin[,14]), lty=3)
plot(Lc(sites$ais[,14]), main='Spain')
lines(Lc(sites$urd[,14]), lty=3)
plot(Lc(sites$val[,14]), main='Switzerland')
lines(Lc(sites$duc[,14]), lty=3)
dev.off()

#plot all Lc
setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/na_lorenz.eps')
par(pty='s')
plot(Lc(sites$whe[,14]), main='North America Sites', col='red')
lines(Lc(sites$tao[,14]), lty=3, col='red')
lines(Lc(sites$lat[,14]), lty=6, col='red')
lines(Lc(sites$bal[,14]), col=11)
lines(Lc(sites$sub[,14]), lty=3, col=11)
lines(Lc(sites$niw[,14]), lty=6, col=11)
lines(Lc(sites$tho[,14]), col='grey')
lines(Lc(sites$wra[,14]), lty=3, col='grey')
lines(Lc(sites$too[,14]), col='blue')
lines(Lc(sites$pin[,14]), lty=3, col='blue')
xfoo <- seq(0.5, 1, 0.05)
yfoo <- rev(seq(0, 0.5, 0.05))
lines(xfoo, yfoo, lty=5)
legend('topleft', cex=0.6,
       col=c('black', 'black', 'red', 'red', 'red', 11, 11, 11, 'grey', 'grey', 
             'blue', 'blue'),
       lty=c(1,5,1,3,6,1,3,6,1,3,1,3),
       legend=c('LINE OF EQUALITY', 'SKEWNESS SYMMETRY LINE', 'Wheeler', 'Taos', 
                'Latir', 'Bald Mt', 'East Niwot', 'Niwot',
                'Thompson Pass', 'Wrangells', 'Toolik', 'Pingo'))
dev.off()

plot(Lc(sites$ais[,14]), main='Spain')
lines(Lc(sites$urd[,14]), lty=3)
plot(Lc(sites$val[,14]), main='Switzerland')
lines(Lc(sites$duc[,14]), lty=3)
dev.off()

############
#2015 Data
############
#set up df
site_names <- unique(size15$id)
gini15 <-  data.frame(site_names)

#gini and s for dist/undist and lo/hi elev (system time = 35 min)
for(i in 1:length(site_names)) {
        foo <- size15[which(size15$id==site_names[i]),]
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==1) #low elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        gini15$gini_lowtrail[i] <- ineq(subbfoo[,10], type='Gini')
                        gini15$s_lowtrail[i] <- Lasym(subbfoo[,10], na.rm=T)
                }
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        gini15$gini_lownotrail[i] <- ineq(subbfoo[,10], type='Gini')
                        gini15$s_lownotrail[i] <- Lasym(subbfoo[,10], na.rm=T)
                }
        }
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==0) #high elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        gini15$gini_hitrail[i] <- ineq(subbfoo[,10], type='Gini')
                        gini15$s_hitrail[i] <- Lasym(subbfoo[,10], na.rm=T)
                }
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        gini15$gini_hinotrail[i] <- ineq(subbfoo[,10], type='Gini')
                        gini15$s_hinotrail[i] <- Lasym(subbfoo[,10], na.rm=T)
                }
        }
}
#NaN are for transects without plants
save(gini15, file='gini15.RData')

#Wilcoxon rank sum (aka Mann-Whitney U) test: all but one signficant
wilcox.test(gini15$gini_lowtrail, gini15$gini_lownotrail) #p-value = 0.007796
wilcox.test(gini15$gini_hitrail, gini15$gini_hinotrail) #p-value = 0.000329
wilcox.test(gini15$s_lowtrail, gini15$s_lownotrail) #p-value = 0.03766
wilcox.test(gini15$s_hitrail, gini15$s_hinotrail) #p-value = 0.03515

wilcox.test(gini15$gini_lowtrail, gini15$gini_hitrail, paired=F) #p-value = 0.3997
wilcox.test(gini15$s_lowtrail, gini15$s_hitrail) #p-value = 0.0134
wilcox.test(gini15$gini_lownotrail, gini15$gini_hinotrail) #p-value = 0.0134
wilcox.test(gini15$s_lownotrail, gini15$s_hinotrail) #p-value = 0.6193

#PLOT: high transects have higher inequality 
plot(gini15$site_names, gini15$gini_lowtrail)
points(gini15$site_names, gini15$gini_hitrail, pch='+')

#PLOT: lorenz curves
foo <- lownotrail_df[which(lownotrail_df$id=='yal'),] #start low elev plot
setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/low_vs_hi_lorenz.eps')
par(mfrow=c(2,1), pty='s')
plot(Lc(foo$area), main='Low Elevation')
for (i in unique(lownotrail_df$id)) {
        foo <- subset(lownotrail_df, id==i)
        lines(Lc(foo$area))
}
for (i in unique(lowtrail_df$id)) {
        foo <- subset(lowtrail_df, id==i)
        lines(Lc(foo$area), col='red')
}

foo <- hinotrail_df[which(hinotrail_df$id=='yal'),] #start high elev plot
plot(Lc(foo$area), main='High Elevation')
for (i in unique(hinotrail_df$id)) {
        foo <- subset(hinotrail_df, id==i)
        lines(Lc(foo$area))
}
for (i in unique(hitrail_df$id)) {
        foo <- subset(hitrail_df, id==i)
        lines(Lc(foo$area), col='red')
}
dev.off()

#KS TESTS AND CDF: ind. plant level sig.
#low trail vs. no trail: larger prop. of smaller plants off trail
ks.test(lowtrail_df$area, lownotrail_df$area) #p-value = 3.237e-07
x <- lowtrail_df[order(lowtrail_df$area),]
trail.x <- x[13][is.na(x$area)==F,]
trail.y <- (1:length(trail.x))/length(trail.x)
x <- lownotrail_df[order(lownotrail_df$area),]
notrail.x <- x[13][is.na(x$area)==F,]
notrail.y <- (1:length(notrail.x))/length(notrail.x)

setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/hiandlotrailvnotrail.eps')
par(mfrow=c(2,1))
plot(trail.x, trail.y, col='red', pch=16, cex=0.4, ylab='CDF', 
     xlab='Plant Area (cm^2)')
points(notrail.x, notrail.y, pch=16, cex=0.4)
legend('bottomright', c('Low Trail', 'Low No Trail'), pch=c(16, 16),
       col=c('red', 'black'))

#hi trail vs. no trail:
ks.test(hitrail_df$area, hinotrail_df$area) #p-value < 2.2e-16
x <- hitrail_df[order(hitrail_df$area),]
trail.x <- x[13][is.na(x$area)==F,]
trail.y <- (1:length(trail.x))/length(trail.x)
x <- hinotrail_df[order(hinotrail_df$area),]
notrail.x <- x[13][is.na(x$area)==F,]
notrail.y <- (1:length(notrail.x))/length(notrail.x)

plot(trail.x, trail.y, col='red', pch=16, cex=0.4, ylab='CDF', 
     xlab='Plant Area (cm^2)')
points(notrail.x, notrail.y, pch=16, cex=0.4)
legend('bottomright', c('High Trail', 'High No Trail'), pch=c(16, 16),
       col=c('red', 'black'))
dev.off()

#low no trail vs. high no trail:
ks.test(lownotrail_df$area, hinotrail_df$area) #p-value = 0.04203
x <- lownotrail_df[order(lownotrail_df$area),]
lo.x <- x[13][is.na(x$area)==F,]
lo.y <- (1:length(lo.x))/length(lo.x)
x <- hinotrail_df[order(hinotrail_df$area),]
hi.x <- x[13][is.na(x$area)==F,]
hi.y <- (1:length(hi.x))/length(hi.x)

setEPS()
postscript('~/Desktop/Research/silene/lorenz_figs/hivlotrailandnotrail.eps')
par(mfrow=c(2,1))
plot(lo.x, lo.y, pch=16, cex=0.4, ylab='CDF', xlab='Plant Area (cm^2)')
points(hi.x, hi.y, pch=16, cex=0.4, col='blue')
legend('bottomright', c('Low No Trail', 'High No Trail'), pch=c(16, 16),
       col=c('black', 'blue'))

#low vs. high trail: larger prop. of small plants at high
ks.test(lowtrail_df$area, hitrail_df$area) #p-value = 0.001076
x <- lowtrail_df[order(lowtrail_df$area),]
lo.x <- x[13][is.na(x$area)==F,]
lo.y <- (1:length(lo.x))/length(lo.x)
x <- hitrail_df[order(hitrail_df$area),]
hi.x <- x[13][is.na(x$area)==F,]
hi.y <- (1:length(hi.x))/length(hi.x)

plot(lo.x, lo.y, pch=16, cex=0.4, ylab='CDF', xlab='Plant Area (cm^2)')
points(hi.x, hi.y, pch=16, cex=0.4, col='blue')
legend('bottomright', c('Low Trail', 'High Trail'), pch=c(16, 16),
       col=c('black', 'blue'))
dev.off()