## N. Chardon
## Start: 4 Dec 2015
## Aim: Run LMM with 2015 data

rm(list=ls())
library(lme4)
library(lmerTest)

setwd("~/Desktop/Research/silene/r_files/")
load('size15.RData')
load('cover15.RData')

#################
## SET UP DATA ## -------------------------------------------------------------
#################
#make unqiue trans per site
size15$id_trans <- paste(size15$id, size15$trans, sep = '_')

#convert % dist. to single number
#weighted average
size15$wt_dist <- size15$low_dist*1 + size15$med_dist*2 + size15$hi_dist*3
#disturbed quads => strongest signal here, but almost all quads disturbed
for(i in 1:length(size15$low_dist)) {
        if (size15$low_dist[i]==100) {
                size15$dist[i] <- 0
        } else {
                size15$dist[i] <- 1
        }
}
size15$dist <- as.factor(size15$dist)
# >50% of quad is disturbed
for(i in 1:length(size15$low_dist)) {
        if (size15$low_dist[i]<50) {
                size15$halfdist[i] = 1
        } else {
                size15$halfdist[i] <- 0
        }
}
size15$halfdist <- as.factor(size15$halfdist)
# >50% of quad is highly disturbed
for(i in 1:length(size15$low_dist)) {
        if (size15$hi_dist[i]>=50) {
                size15$halfhi[i] <- 1
        } else {
                size15$halfhi[i] <- 0
        }
}
size15$halfhi <- as.factor(size15$halfhi)
# high disturbance in quad => best measure to use
size15 <- size15[is.na(size15$hi)==F,]
for(i in 1:length(size15$hi_dist)) {
        if (size15$hi_dist[i]>0) {
                size15$hi[i] <- 1
        } else {
                size15$hi[i] <- 0
        }
}
size15$hi <- as.factor(size15$hi)

foo <- cover15[is.na(cover15$hi)==F,]
for(i in 1:length(foo$hi)) {
        if (foo$hi[i]>0) {
                foo$hidist[i] <- 1
        } else {
                foo$hidist[i] <- 0
        }
}
foo$hidist <- as.factor(foo$hidist)

#delete cover data errors from df
size15$foo <- size15$low_dist+size15$med_dist+size15$hi_dist
size15 <- subset(size15, foo==100)
        
#convert to factor
size15$trail <- as.factor(size15$trail)
size15$low_elev <- as.factor(size15$low_elev)
size15$pa <- as.factor(size15$pa)

#####################
## DISTURBANCE LMM ## ----------------------------------------------------------
#####################
fit <- lmer(area ~ trail + low_elev 
            + (1|id/trans/quad), data=size15) #anova p-value(hi)=0.01046

fit <- lmer(area ~ hi + low_elev 
            + (1|id/trans/quad), data=size15) #anova p-value(hi)=0.01623

foo <- foo[is.na(foo$avgsize)==F,] #FIX CODE: NA/NaN/Inf in 'y'
fit <- lmer(avgsize ~ hidist + low_elev 
            + (1|id/trans), data=foo) #p-value=

#plot residuals => model assumptions violated
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
# scatterplot residuals vs. fitted
scatter.smooth(fitted(fit), resid(fit))
abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8)  

# Normal qq-plot of residuals
qqnorm(resid(fit), main="normal qq-plot, residuals")
qqline(resid(fit))

# scatterplot of sqrt of absolute residuals vs. fitted
scatter.smooth(fitted(fit), sqrt(abs(resid(fit))))
mtext("Scale-Location", 3, line = 0.8)

# Normal qq-plot of random effects
qqnorm(ranef(fit)$Ring[,1], main="normal qq-plot, random effects")
qqline(ranef(fit)$Ring[,1]) 
dev.off()

#log-transform size so that residual variances fit model assumptions
size15$logarea <- log(size15$area)
fit <- lmer(logarea ~ hi + low_elev 
            + (1|id/trans/quad), data=size15) #anova p-value(hi)=0.005359

#plot residuals => look good
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
# scatterplot residuals vs. fitted
scatter.smooth(fitted(fit), resid(fit))
abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8)  

# Normal qq-plot of residuals
qqnorm(resid(fit), main="normal qq-plot, residuals")
qqline(resid(fit))

# scatterplot of sqrt of absolute residuals vs. fitted
scatter.smooth(fitted(fit), sqrt(abs(resid(fit))))
mtext("Scale-Location", 3, line = 0.8)

# Normal qq-plot of random effects
qqnorm(ranef(fit)$id[,1], main="normal qq-plot, random effects")
qqline(ranef(fit)$id[,1]) 
dev.off()

#####################
## % COVER LMM ## ----------------------------------------------------------
#####################
#add veg & bare ground to size15
cover15$veg <- cover15$gram + cover15$forb
cover15$bare <- cover15$grav + cover15$soil
cover152size15 <- match(size15$unique, cover15$unique)
size15$veg <- cover15$veg[cover152size15]
size15$bare <- cover15$bare[cover152size15]

#lmm
fit <- lmer(area ~ veg + low_elev 
            + (1|id/trans/quad), data=size15) #anova p-value=0.00608

fit <- lmer(area ~ bare + low_elev 
            + (1|id/trans/quad), data=size15) #anova p-value=0.2111

#plot residuals => model assumptions violated
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
# scatterplot residuals vs. fitted
scatter.smooth(fitted(fit), resid(fit))
abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8)  

# Normal qq-plot of residuals
qqnorm(resid(fit), main="normal qq-plot, residuals")
qqline(resid(fit))

# scatterplot of sqrt of absolute residuals vs. fitted
scatter.smooth(fitted(fit), sqrt(abs(resid(fit))))
mtext("Scale-Location", 3, line = 0.8)

# Normal qq-plot of random effects
qqnorm(ranef(fit)$id[,1], main="normal qq-plot, random effects")
qqline(ranef(fit)$id[,1]) 
dev.off()

#lmm with logarea
fit <- lmer(logarea ~ veg + low_elev 
            + (1|id/trans/quad), data=size15) #anova p-value(veg)=0.03711

#plot residuals => look good
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
# scatterplot residuals vs. fitted
scatter.smooth(fitted(fit), resid(fit))
abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8)  

# Normal qq-plot of residuals
qqnorm(resid(fit), main="normal qq-plot, residuals")
qqline(resid(fit))

# scatterplot of sqrt of absolute residuals vs. fitted
scatter.smooth(fitted(fit), sqrt(abs(resid(fit))))
mtext("Scale-Location", 3, line = 0.8)

# Normal qq-plot of random effects
qqnorm(ranef(fit)$id[,1], main="normal qq-plot, random effects")
qqline(ranef(fit)$id[,1]) 
dev.off()
