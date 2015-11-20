## N. Chardon
## Start: 9 Oct 2015
## Aim: quantile regressions with 2014 data

rm(list=ls())
setwd("~/Desktop/Research/silene/")

# Load Libraries
library(quantreg)
library(ggplot2)
library(latticeExtra)

# load dataframes
load('r_files/na_df.RData')

## Quantile Regression
#elevation: quadratic term significant at all quantiles, 
x <- cbind(as.numeric(na_df$elev), as.numeric((na_df$elev)^2))
y <- cbind(as.numeric(na_df$areacm))
qreg_all <- rq(y ~ x, tau=seq(0.05, 0.95, by=0.05))
qreg_plot <- summary(qreg_all)
png('~/Desktop/Research/silene/perth_figs/quantreg_elev.png')
plot(qreg_plot)
dev.off()

#latitude
x <- cbind(as.numeric(na_df$lat), as.numeric((na_df$lat)^2))
y <- cbind(as.numeric(na_df$areacm))
qreg_all <- rq(y ~ x, tau=seq(0.05, 0.95, by=0.05))
qreg_plot <- summary(qreg_all)
png('~/Desktop/Research/silene/perth_figs/quantreg_lat.png')
plot(qreg_plot)
dev.off()

#plot lower 25% of plant sizes --> doesn't look better than all points
na_df_30 <- na_df[na_df$areacm < quantile(na_df$areacm, 0.30, na.rm=T),]
png('~/Desktop/Research/silene/perth_figs/quantreg_30.png')
plot(na_df_30$elev, na_df_30$areacm)
dev.off()
fit_30 <- lm(na_df_30$areacm ~ na_df_30$elev + I(na_df_30$elev^2))

na_df_20 <- na_df[na_df$areacm < quantile(na_df$areacm, 0.20, na.rm=T),]
plot(na_df_20$elev, na_df_20$areacm)
na_df_10 <- na_df[na_df$areacm < quantile(na_df$areacm, 0.10, na.rm=T),]
plot(na_df_10$elev, na_df_10$areacm)

#plot fitted curve at 90th quantile
x_90 <- cbind(as.numeric(na_df$lat), as.numeric((na_df$lat)^2))
y_90 <- cbind(as.numeric(na_df$areacm))
qreg_90 <- summary(rq(y_90 ~ x_90, tau=0.9))
#all points
plot(na_df$areacm ~ na_df$lat)
xx<-seq(min(na_df$lat,na.rm=T),max(na_df$lat,na.rm=T)) #span observed values
yy<-coef<-qreg_90$coef %*% rbind(1,xx,xx^2)
lines(xx,yy)
#subset of all points [0.89, 0.91]
na_df_90 <- subset(na_df, areacm < quantile(na_df$areacm, 0.91, na.rm=T) &
                           areacm > quantile(na_df$areacm, 0.89, na.rm=T))
points(na_df_90$elev, na_df_90$areacm, col='red', pch=1)

panel.quantile(x_90, y_90, form = y~x+x^2, method = "rq",
               tau = 0.5, ci = T, ci.type = "default", level = 0.95)

