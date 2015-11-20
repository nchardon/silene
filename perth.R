## N. Chardon
## Start: 10 Sept 2015
## Aim: Analyses and Figures for Perth III Conference

# clear memory & set directory
rm(list=ls())
setwd("~/Desktop/Research/silene/")

# Load dataframes
load('~/Desktop/Research/silene/r_files/size.RData')
load('~/Desktop/Research/silene/r_files/sites.RData')
load('~/Desktop/Research/silene/r_files/na_df.Rdata')
load('~/Desktop/Research/silene/r_files/size15.RData')
load('~/Desktop/Research/silene/r_files/lowtrail.RData')
load('~/Desktop/Research/silene/r_files/lownotrail.RData')

## Linear + Quadratic Regressions
# North America (FIGURE 1)
fit_elev <- lm(na_df$areacm ~ na_df$elev + I(na_df$elev^2))
#model p-value=0.0039, elev=0.006, elev^2=0.003
png('~/Desktop/Research/silene/perth_figs/na_elev.png', 12, 8, units='cm', res=700)
par(mar=c(4.5,4.5,1,0.5))
plot(na_df$areacm ~ na_df$elev,
     xlab = '', ylab = '',
     pch = 16, cex = 0.5, cex.axis=0.9)
xx<-seq(min(na_df$elev,na.rm=T),max(na_df$elev,na.rm=T)) #span observed values
yy<-coef<-fit_elev$coef %*% rbind(1,xx,xx^2)
lines(xx,yy)
dev.off()

fit_lat <- lm(na_df$areacm ~ na_df$lat +I(na_df$lat^2))
#model p-value=0.00029, lat=0.0001, lat^2=0.0001
png('~/Desktop/Research/silene/perth_figs/na_lat.png', 12, 8, units='cm', res=700)
par(mar=c(4.5,4.5,1,0.5))
plot(na_df$areacm ~ na_df$lat,
     xlab = '', ylab = '',
     pch = 16, cex = 0.5, cex.axis=0.9)
xx<-seq(min(na_df$lat,na.rm=T),max(na_df$lat,na.rm=T))
yy<-coef<-fit_lat$coef %*% rbind(1,xx,xx^2)
lines(xx,yy)
dev.off()

#LM using all points in Val Bercla (FIGURE 2)
sites$val[,10] <- 0.01*(sites$val[,10]) #turn mm^2 to cm^2
fit_val <- lm(sites$val[,10] ~ sites$val[,13])
#p-value < 0.001
png('~/Desktop/Research/silene/perth_figs/size_elev_val.png', 12, 7, units='cm', res=700)
par(mar=c(4.5,4.5,0.5,0.5))
plot(sites$val[,10] ~ sites$val[,13], 
     ylim = c(0, 750), 
     pch = 16,
     #main = 'Elevational Profile at Val Bercla, Switzerland', 
     xlab = '', ylab = '')
abline(fit_val)
dev.off()

## Disturbance Analyses with Summer 2015 Data

#compare sizes at low elev by trail presence
#Wilcoxon rank sum test
wilcox.test(lowtrail_df$area, lownotrail_df$area) #p-value < 2.2e-16
wilcox.test(lowtrail_df$area, hitrail_df$area) #p-value < 2.2e-16
wilcox.test(lownotrail_df$area, hinotrail_df$area) #p-value = 0.233
wilcox.test(hitrail_df$area, hinotrail_df$area) #p-value < 2.2e-16

#quad level stats
trail_means <- tapply(lowtrail_df$area, lowtrail_df$unique, mean, na.rm = TRUE)
notrail_means <- tapply(lownotrail_df$area, lownotrail_df$unique, 
                        mean, na.rm = TRUE)
wilcox.test(trail_means, notrail_means) #p-value < 2.2e-16

#CDF plot with quad means (FIGURE 3) => different fig. than in poster!!!
#need to use ind. plant data (sig.), see 'estimators.R'
trail.x <- sort(trail_means, decreasing = FALSE)
trail.y <- (1:dim(trail.x))/dim(trail.x)
notrail.x <- sort(notrail_means, decreasing = FALSE)
notrail.y <- (1:dim(notrail.x))/dim(notrail.x)
png('~/Desktop/Research/silene/perth_figs/cdf.png', 12, 8, units='cm', res=700)
par(mar=c(4.5,4.5,0.5,0.5)) #set margins(bottom, left, top, right)
plot(trail.x, trail.y,
     type = 'l', lty=2, xlab='', ylab='')
lines(notrail.x, notrail.y)
dev.off()
ks.test(trail_means, notrail_means) #p-value < 2.2e-16

###############################################################################
## Analyses and Figs not used in poster:
# 1. point means sd~elev by region
# 2. point means size~elev by region
# 3. size means ~ site lat and sd
# 4. size means ~ site elev
# 5. 10 largest plants per site
###############################################################################

## 1. Plot regions distinctly for sd ~ elev 
#(run 'Point Level Analyses' from '2014_analyses.R')
                
fit.tho <- lm(tho.sd ~ tho.elev)
fit.wra <- lm(wra.sd ~ wra.elev)
fit.too <- lm(too.sd ~ too.elev)
fit.pin <- lm(pin.sd ~ pin.elev)
fit.niw <- lm(niw.sd ~ niw.elev)
fit.bal <- lm(bal.sd ~ bal.elev)
fit.lat <- lm(lat.sd ~ lat.elev)
fit.tao <- lm(tao.sd ~ tao.elev)
fit.whe <- lm(whe.sd ~ whe.elev)
fit.val <- lm(val.sd ~ val.elev)
fit.duc <- lm(duc.sd ~ duc.elev)
fit.ais <- lm(ais.sd ~ ais.elev)
fit.urd <- lm(urd.sd ~ urd.elev)
png('perth_figs/sd_elev.png')
plot(wra.sd ~ wra.elev,
     ylim = c(0, 80000),
     xlim = c(0, 4000),
     main = "Site Elevation and Standard Deviation",
     xlab = "Elevation of Population (m)",
     ylab = "Standard Deviation of Sizes within Population",
     col = 'green', pch = 15)
points(tho.sd ~ tho.elev,
       col = 'green', pch = 16)
points(too.sd ~ too.elev, col = 'blue', pch = 15)
points(pin.sd ~ pin.elev, col = 'blue', pch = 16)
points(niw.sd ~ niw.elev, col = 'orange', pch = 15)
points(bal.sd ~ bal.elev, col = 'orange', pch = 16)
points(lat.sd ~ lat.elev, col = 'red', pch = 15)
points(tao.sd ~ tao.elev, col = 'red', pch = 16)
points(whe.sd ~ whe.elev, col = 'red', pch = 17)
points(val.sd ~ val.elev, col = 'purple', pch = 15)
points(duc.sd ~ duc.elev, col = 'purple', pch = 16)
points(ais.sd ~ ais.elev, col = 'violet', pch = 15)
points(urd.sd ~ urd.elev, col = 'violet', pch = 15)
abline(fit.tho, col = 'green')
abline(fit.wra, col = 'green')
abline(fit.too, col = 'blue')
abline(fit.pin, col = 'blue')
abline(fit.niw, col = 'orange')
abline(fit.bal, col = 'orange')
abline(fit.lat, col = 'red')
abline(fit.tao, col = 'red')
abline(fit.whe, col = 'red')
abline(fit.val, col = 'purple')
abline(fit.duc, col = 'purple')
abline(fit.ais, col = 'violet')
abline(fit.urd, col = 'violet')
legend("topleft",
       cex = 0.5,
       legend = c('AK North', 'AK South', 'CO', 'NM', 'Spain', 'Switzerland'),
       col = c('blue', 'green', 'orange', 'red', 'violet', 'purple'),
       lty = c(1, 1, 1, 1, 1, 1))
dev.off()

## 2. Plot regions distinctly for size ~ elev
png('perth_figs/size_elev.png')
plot(wra.size ~ wra.elev,
     ylim = c(0, 40000),
     xlim = c(0, 4000),
     main = "Population Elevation and Mean Plant Size",
     xlab = "Elevation of Population (m)",
     ylab = "Mean Plant Size",
     col = 'green', pch = 15)
points(tho.size ~ tho.elev,
       col = 'green', pch = 16)
points(too.size ~ too.elev, col = 'blue', pch = 15)
points(pin.size ~ pin.elev, col = 'blue', pch = 16)
points(niw.size ~ niw.elev, col = 'orange', pch = 15)
points(bal.size ~ bal.elev, col = 'orange', pch = 16)
points(lat.size ~ lat.elev, col = 'red', pch = 15)
points(tao.size ~ tao.elev, col = 'red', pch = 16)
points(whe.size ~ whe.elev, col = 'red', pch = 17)
points(val.size ~ val.elev, col = 'purple', pch = 15)
points(duc.size ~ duc.elev, col = 'purple', pch = 16)
points(ais.size ~ ais.elev, col = 'violet', pch = 15)
points(urd.size ~ urd.elev, col = 'violet', pch = 15)
abline(lm(tho.sd ~ tho.elev), col = 'green')
abline(lm(wra.sd ~ wra.elev), col = 'green')
abline(lm(too.sd ~ too.elev), col = 'blue')
abline(lm(pin.sd ~ pin.elev), col = 'blue')
abline(lm(niw.sd ~ niw.elev), col = 'orange')
abline(lm(bal.sd ~ bal.elev), col = 'orange')
abline(lm(lat.sd ~ lat.elev), col = 'red')
abline(lm(tao.sd ~ tao.elev), col = 'red')
abline(lm(whe.sd ~ whe.elev), col = 'red')
abline(lm(val.sd ~ val.elev), col = 'purple')
abline(lm(duc.sd ~ duc.elev), col = 'purple')
abline(lm(ais.sd ~ ais.elev), col = 'violet')
abline(lm(urd.sd ~ urd.elev), col = 'violet')
legend("topleft",
       cex = 0.5,
       legend = c('AK North', 'AK South', 'CO', 'NM', 'Spain', 'Switzerland'),
       col = c('blue', 'green', 'orange', 'red', 'violet', 'purple'),
       lty = c(1, 1, 1, 1, 1, 1))
dev.off()

# Europe
png('perth_figs/pointsize_elev_europe.png')
plot(val.size ~ val.elev,
     ylim = c(0, 40000),
     xlim = c(2000, 2800),
     main = "Population Elevation and Mean Plant Size (EUROPE)",
     xlab = "Elevation of Population (m)",
     ylab = "Mean Plant Size",
     col = 'purple', pch = 15)
points(duc.size ~ duc.elev, col = 'purple', pch = 16)
points(ais.size ~ ais.elev, col = 'violet', pch = 15)
points(urd.size ~ urd.elev, col = 'violet', pch = 15)
abline(lm(val.sd ~ val.elev), col = 'purple')
abline(lm(duc.sd ~ duc.elev), col = 'purple')
abline(lm(ais.sd ~ ais.elev), col = 'violet')
abline(lm(urd.sd ~ urd.elev), col = 'violet')
dev.off()

# Corresponding Linear Models: N too small for this test?
summary(lm(val.sd ~ val.elev)) #p=0.03
summary(lm(duc.sd ~ duc.elev)) #not sig.
summary(lm(ais.sd ~ ais.elev)) #not sig.
summary(lm(urd.sd ~ urd.elev)) #not sig.

## 3. Plot size means vs. site lat and sd
# (run 'Site Level Analyses' from '2014_analyses.R')
# N. America
plot(site.lat[2], s.size.means[2], 
     main = 'Mean Plant Size at Site Latitude',
     xlab = 'Site Latitude',
     ylab = 'Mean Plant Size',
     xlim = c(30,70), ylim = c(500, 20000))
points(site.lat[4:10], s.size.means[4:10])
points(site.lat[13:14], s.size.means[13:14])

png('mean plant size sd at site lat.png')
plot(site.lat[2], s.size.sd[2], 
     main = 'Mean Plant Size SD at Site Latitude',
     xlab = 'Site Latitude',
     ylab = 'Mean Site Plant Size SD',
     xlim = c(30,70), ylim = c(500, 20000))
points(site.lat[4:10], s.size.sd[4:10])
points(site.lat[13:14], s.size.sd[13:14])
dev.off()

## 4. Plot size means vs. site elev
png('site elev means vs. size means.png')
plot(site.elev[1], s.size.means[1], 
     col = 'violet', pch = 16, cex = 1,
     main = 'Mean Plant Size at Site Elevation',
     xlab = 'Site Elevation (m)',
     ylab = 'Mean Plant Size',
     xlim = c(0,4000), ylim = c(500, 30000))
points(site.elev[2], s.size.means[2], col = 'orange', pch = 16, cex = 1)
points(site.elev[3], s.size.means[3], col = 'purple', pch = 16, cex = 1)
points(site.elev[4], s.size.means[4], col = 'red', pch = 16, cex = 1)
points(site.elev[5], s.size.means[5], col = 'orange', pch = 16, cex = 1)
points(site.elev[6], s.size.means[6], col = 'blue', pch = 16, cex = 1)
points(site.elev[8], s.size.means[8], col = 'red', pch = 16, cex = 1)
points(site.elev[9], s.size.means[9], col = 'green', pch = 16, cex = 1)
points(site.elev[10], s.size.means[10], col = 'blue', pch = 16, cex = 1)
points(site.elev[11], s.size.means[11], col = 'violet', pch = 16, cex = 1)
points(site.elev[12], s.size.means[12], col = 'purple', pch = 16, cex = 1)
points(site.elev[13], s.size.means[13], col = 'red', pch = 16, cex = 1)
points(site.elev[14], s.size.means[14], col = 'green', pch = 16, cex = 1)
legend("topleft",
       cex = 1,
       legend = c('AK North', 'AK South', 'CO', 'NM', 'Spain', 'Switzerland'),
       col = c('blue', 'green', 'orange', 'red', 'violet', 'purple'),
       pch = c(16, 16, 16, 16, 16, 16))
dev.off()

## 5. 10 LARGEST PLANTS
# 10 largest plants at each site
wra_10 <- sites$wra[order(sites$wra[,10], decreasing = TRUE),][1:10,]
too_10 <- sites$too[order(sites$too[,10], decreasing = TRUE),][1:10,]
pin_10 <- sites$pin[order(sites$pin[,10], decreasing = TRUE),][1:10,]
tho_10 <- sites$tho[order(sites$tho[,10], decreasing = TRUE),][1:10,]
bal_10 <- sites$bal[order(sites$bal[,10], decreasing = TRUE),][1:10,]
niw_10 <- sites$niw[order(sites$niw[,10], decreasing = TRUE),][1:10,]
sub_10 <- sites$sub[order(sites$sub[,10], decreasing = TRUE),][1:10,]
whe_10 <- sites$whe[order(sites$whe[,10], decreasing = TRUE),][1:10,]
tao_10 <- sites$tao[order(sites$tao[,10], decreasing = TRUE),][1:10,]
lat_10 <- sites$lat[order(sites$lat[,10], decreasing = TRUE),][1:10,]

duc_10 <- sites$duc[order(sites$duc[,10], decreasing = TRUE),][1:10,]
val_10 <- sites$val[order(sites$val[,10], decreasing = TRUE),][1:10,]
urd_10 <- sites$urd[order(sites$urd[,10], decreasing = TRUE),][1:10,]
ais_10 <- sites$ais[order(sites$ais[,10], decreasing = TRUE),][1:10,]

# Combine NA into one dataframe
data_10 <- rbind(wra_10, too_10, pin_10, tho_10, bal_10, niw_10, 
                     sub_10, whe_10, tao_10, lat_10)
data_10$areacm <- 0.01*(data_10$area)

# NA Plots & LM
pdf('10 Largest Plants per Site.pdf')
plot(data_10$area ~ data_10$lat,
     main = '10 Largest Plants per Site',
     xlab = 'Latitude', ylab = 'Plant Size',
     pch = 16)
dev.off()
summary(lm(data_10$area ~ data_10$lat)) # p = 0.44

pdf('10 Largest Plants and Elevation.pdf')
par(mfrow = c(2,2))
plot(lat_10$elev, lat_10$area, 
     main = '10 Largest Plants (New Mexico)',
     xlab = 'Elevation (m)', ylab = 'Plant Size',
     pch = 16, col = 'gray',
     ylim = c(5000, 120000),
     xlim = c(3500, 4100))
points(whe_10$elev, whe_10$area, pch = 16)
points(tao_10$elev, tao_10$area, pch = 16, col = '637')
legend('topright',
       cex = 0.7,
       legend = c('Latir', 'Wheeler', 'Taos'),
       col = c('gray', 'black', '637'),
       pch = c(16, 16, 16))

plot(bal_10$elev, bal_10$area, 
     main = '10 Largest Plants (Colorado)',
     xlab = 'Elevation (m)', ylab = 'Plant Size',
     pch = 16, col = 'gray',
     ylim = c(5000, 126000),
     xlim = c(3300, 4100))
points(niw_10$elev, niw_10$area, pch = 16)
legend('topleft',
       cex = 0.7,
       legend = c('Bald', 'Niwot'),
       col = c('gray', 'black'),
       pch = c(16, 16))

plot(pin_10$elev, pin_10$area, 
     main = '10 Largest Plants (Northern Alaska)',
     xlab = 'Elevation (m)', ylab = 'Plant Size',
     pch = 16, col = 'gray',
     ylim = c(8000, 172000),
     xlim = c(45, 1000))
points(too_10$elev, too_10$area, pch = 16)
legend('topright',
       cex = 0.7,
       legend = c('Pingo', 'Toolik'),
       col = c('gray', 'black'),
       pch = c(16, 16))

plot(tho_10$elev, tho_10$area, 
     main = '10 Largest Plants (Southern Alaska)',
     xlab = 'Elevation (m)', ylab = 'Plant Size',
     pch = 16, col = 'gray',
     ylim = c(5000, 197000),
     xlim = c(820, 1520))
points(wra_10$elev, wra_10$area, pch = 16)
legend('topleft',
       cex = 0.7,
       legend = c('Thompson Pass', 'Wrangells'),
       col = c('gray', 'black'),
       pch = c(16, 16))
dev.off()

# SWITZERLAND
png('10 Largest Plants and Elevation (Switzerland).png')
duc_10$areacm <- 0.01*(duc_10$area)
val_10$areacm <- 0.01*(val_10$area)
plot(duc_10$elev, duc_10$areacm, 
     main = '10 Largest Plants (Switzerland)',
     xlab = 'Elevation (m)', ylab = 'Plant Size (cm^2)',
     pch = 16, col = 'gray',
     ylim = c(min(duc_10$areacm), max(val_10$areacm)),
     xlim = c(min(duc_10$elev), max(duc_10$elev)))
points(val_10$elev, val_10$areacm, pch = 16)
legend('topright',
       cex = 0.7,
       legend = c('Ducanfurgga', 'Val Berlca'),
       col = c('gray', 'black'),
       pch = c(16, 16))
dev.off()
