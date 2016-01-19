## N. Chardon
## Start: 4 Dec 2015
## Aim: Run LMM with 2015 data
## Generated dataframes:
load('~/Desktop/Research/silene/r_files/size15_lmm2.RData') 
load('~/Desktop/Research/silene/r_files/cover15_lmm2.RData')

load('~/Desktop/Research/silene/r_files/size15_lmm.RData') #old df
load('~/Desktop/Research/silene/r_files/cover15_lmm.RData') #old df

rm(list=ls())
library(lme4)
library(lmerTest)
library(ggplot2)

#################
## SET UP DATA ## -------------------------------------------------------------
#################
setwd("~/Desktop/Research/silene/r_files/")
load('size15.RData')
load('cover15.RData')

#delete cover data errors from df
cover15$foo <- cover15$low+cover15$med+cover15$hi
cover15 <- subset(cover15, foo==100)
cover15$foo <- cover15[!cover15$foo] #length 1200

size15$foo <- size15$low_dist+size15$med_dist+size15$hi_dist
size15 <- subset(size15, foo==100)
size15$foo <- size15[!size15$foo] #length 1200

#convert to factor
size15$trail <- as.factor(size15$trail)
size15$low_elev <- as.factor(size15$low_elev)

#convert % dist. to single number
#weighted average
# size15$wt_dist <- size15$low_dist*1 + size15$med_dist*2 + size15$hi_dist*3

# >50% of quad is disturbed
# n=1143 for halfdist=1
# n=2263 for halfdist=0
for(i in 1:length(size15$low_dist)) {
        if (size15$low_dist[i]<50) {
                size15$halfdist[i] = 1
        } else {
                size15$halfdist[i] <- 0
        }
}
size15$halfdist <- as.factor(size15$halfdist)

# high disturbance in quad
# n=1792 for hi=1
# n=1614 for hi=0

for(i in 1:length(size15$hi_dist)) {
        if (size15$hi_dist[i]>0) {
                size15$hi[i] <- 1
        } else {
                size15$hi[i] <- 0
        }
}
size15$hi <- as.factor(size15$hi)

for(i in 1:length(cover15$hi)) {
        if (cover15$hi[i]>0) {
                cover15$hidist[i] <- 1
        } else {
                cover15$hidist[i] <- 0
        }
}
cover15$hidist <- as.factor(cover15$hidist)

#log(response variable) for better residual patterns in lmm's 
size15$logarea <- log(size15$area)
cover15$logavgsize <- log(cover15$avgsize)

#add pa to cover15 (start df lmm2)
size152cover15 <- match(cover15$unique, size15$unique)
cover15$pa <- size15$pa[size152cover15]

#add veg & bare ground to cover15 and size15
cover15$veg <- cover15$gram + cover15$forb
cover15$bare <- cover15$grav + cover15$soil

cover152size15 <- match(size15$unique, cover15$unique)
size15$veg <- cover15$veg[cover152size15]
size15$bare <- cover15$bare[cover152size15]

#assign pa to min, paro, phlox
for (i in 1:dim(cover15)[1]) {
        if (cover15$min[i] > 0) {
                cover15$min_pa[i] <- '1'
        }
        else {
                cover15$min_pa[i] <- '0'
        }
        if (cover15$paro[i] > 0) {
                cover15$paro_pa[i] <- '1'
        }
        else {
                cover15$paro_pa[i] <- '0'
        }
        if (cover15$phlox[i] > 0) {
                cover15$phlox_pa[i] <- '1'
        }
        else {
                cover15$phlox_pa[i] <- '0'
        }
}
cover15$min_pa <- as.factor(as.numeric(cover15$min_pa))
cover15$paro_pa <- as.factor(as.numeric(cover15$paro_pa))
cover15$phlox_pa <- as.factor(as.numeric(cover15$phlox_pa))

#save expanded dfs
save(size15, file='~/Desktop/Research/silene/r_files/size15_lmm2.RData')
save(cover15, file='~/Desktop/Research/silene/r_files/cover15_lmm2.RData')

#####################
## DISTURBANCE LMM ## ----------------------------------------------------------
#####################
#lmm with logarea so that residual variances fit model assumptions
fit1 <- lmer(logarea ~ low_elev + (1|id/trans/quad), data=size15)
fit2 <- lmer(logarea ~ hi + (1|id/trans/quad), data=size15) #best model
fit3 <- lmer(logarea ~ low_elev + hi + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * hi + (1|id/trans/quad), data=size15)
fit5 <- lmer(logarea ~ (1|id/trans/quad), data=size15)


#glm on Silene presence: more presences with disturbance
fit1 <- glmer(pa ~ low_elev + (1|trans/id), data=cover15, family=binomial)
fit2 <- glmer(pa ~ hidist + (1|trans/id), data=cover15, family=binomial) #best
fit3 <- glmer(pa ~ low_elev + hidist + (1|trans/id), data=cover15, family=binomial)
fit4 <- glmer(pa ~ low_elev * hidist + (1|trans/id), data=cover15, family=binomial) #no convergence
fit5 <- glmer(pa ~ (1|trans/id), data=cover15, family=binomial)


#lmm with logarea on % of hi_dist: no signficance
fit1 <- lmer(logarea ~ low_elev + (1|id/trans/quad), data=size15)
fit2 <- lmer(logarea ~ hi_dist + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ low_elev + hi_dist + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * hi_dist + (1|id/trans/quad), data=size15)
fit5 <- lmer(logarea ~ (1|id/trans/quad), data=size15)

#####################
## % COVER LMM ## ----------------------------------------------------------
#####################

#VEG lmm with logarea so residuals meet model assumptions
fit1 <- lmer(logarea ~ low_elev + (1|id/trans/quad), data=size15)
fit2 <- lmer(logarea ~ veg + (1|id/trans/quad), data=size15) #sig
fit3 <- lmer(logarea ~ low_elev + veg + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * veg + (1|id/trans/quad), data=size15)
fit5 <- lmer(logarea ~ (1|id/trans/quad), data=size15) #best model?


#BARE lmm: bare ground marginally sig
fit1 <- lmer(logarea ~ low_elev + (1|id/trans/quad), data=size15)
fit2 <- lmer(logarea ~ bare + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ low_elev + bare + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * bare + (1|id/trans/quad), data=size15)
fit5 <- lmer(logarea ~ (1|id/trans/quad), data=size15) 


#% COVER: check that veg & bare are influenced by hidist
#subset data
cover15 <- subset(cover15, veg<=100)
cover15 <- subset(cover15, bare<=100)

fit1 <- lmer(veg ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(veg ~ hidist + (1|id/trans), data=cover15)
fit3 <- lmer(veg ~ low_elev + hidist + (1|id/trans), data=cover15)
fit4 <- lmer(veg ~ low_elev * hi + (1|id/trans), data=cover15) #best
fit5 <- lmer(veg ~ (1|id/trans), data=cover15)

#log-transform bare to fit model assumptions
for (i in 1:length(cover15$bare)) {
        if(cover15$bare[i]==0) {
                cover15$bare[i] <- 0.0001
        }
        else {cover15$bare[i] <- cover15$bare[i]}
}
cover15$logbare <- log(cover15$bare)
fit1 <- lmer(bare ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(logbare ~ hidist + (1|id/trans), data=cover15)
fit3 <- lmer(bare ~ low_elev + hidist + (1|id/trans), data=cover15)
fit4 <- lmer(bare ~ low_elev * hi + (1|id/trans), data=cover15) #best
fit5 <- lmer(bare ~ (1|id/trans), data=cover15)


#MIN lmm: IN PROGRESS
#log transform min and give 0 values a number tranformation works
for (i in 1:length(cover15$min)) {
        if(cover15$min[i]==0) {
                cover15$min[i] <- 0.0001
        }
        else {cover15$min[i] <- cover15$min[i]}
}
cover15$logmin <- log(cover15$min)
fit1 <- lmer(min ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(logmin ~ hidist + (1|id/trans), data=cover15) #best
fit3 <- lmer(min ~ low_elev + hidist + (1|id/trans), data=cover15)
fit4 <- lmer(min ~ low_elev * hi + (1|id/trans), data=cover15)
fit5 <- lmer(min ~ (1|id/trans), data=cover15)

#PARO and PHLOX lmm: dist nor elev sig on % cover
fit_paro <- lmer(paro ~ hidist + low_elev + (1|id/trans), data=cover15) #not sig.
fit_phlox <- lmer(phlox ~ hidist + low_elev + (1|id/trans), data=cover15) #not sig.


#MIN glmm: more min presences with disturbance
fit1 <- glmer(min_pa ~ low_elev + (1|trans/id), data=cover15, family=binomial)
fit2 <- glmer(min_pa ~ hidist + (1|trans/id), data=cover15, family=binomial) #best 
fit3 <- glmer(min_pa ~ low_elev + hidist + (1|trans/id), data=cover15, family=binomial)
fit4 <- glmer(min_pa ~ low_elev * hidist + (1|trans/id), data=cover15, family=binomial)
fit5 <- glmer(min_pa ~ (1|trans/id), data=cover15, family=binomial)

#PARO and PHLOX glmm: only increase in phlox presence with disturbance
fit1 <- glmer(phlox_pa ~ low_elev + (1|trans/id), data=cover15, family=binomial)
fit2 <- glmer(phlox_pa ~ hidist + (1|trans/id), data=cover15, family=binomial) #best 
fit3 <- glmer(phlox_pa ~ low_elev + hidist + (1|trans/id), data=cover15, family=binomial)
fit4 <- glmer(phlox_pa ~ low_elev * hidist + (1|trans/id), data=cover15, family=binomial)
fit5 <- glmer(phlox_pa ~ (1|trans/id), data=cover15, family=binomial)


#DENSITY lmm: disturbance determines density
#subset for presence-only (n=763)
cover15 <- subset(cover15, quant_sil > 0)
#log-transform to fit model assumptions
for (i in 1:length(cover15$sil)) {
        if(cover15$sil[i]==0) {
                cover15$sil[i] <- 0.0001
        }
        else {cover15$sil[i] <- cover15$sil[i]}
}
cover15$logsil <- log(cover15$sil)
fit1 <- lmer(logsil ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(logsil ~ hidist + (1|id/trans), data=cover15)
fit3 <- lmer(logsil ~ low_elev + hidist + (1|id/trans), data=cover15)
fit4 <- lmer(logsil ~ low_elev * hi + (1|id/trans), data=cover15)
fit5 <- lmer(logsil ~ (1|id/trans), data=cover15)


#AVGSMSIZE lmm: bigger smaller plants at low elevation (only 31.3% quads > 5 plants)
#remove (-) and Inf avgsmsize values (n=342; 28.5% of all sampled quads)
cover15 <- subset(cover15, avgsmsize>=0)
cover15 <- cover15[is.infinite(cover15$avgsmsize)==F,] 
#log-transform to fit model assumptions
cover15$logavgsmsize <- log(cover15$avgsmsize)
fit1 <- lmer(logavgsmsize ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(logavgsmsize ~ hidist + (1|id/trans), data=cover15)
fit3 <- lmer(logavgsmsize ~ low_elev + hidist + (1|id/trans), data=cover15) #sig. elev
fit4 <- lmer(logavgsmsize ~ low_elev * hi + (1|id/trans), data=cover15)
fit5 <- lmer(logavgsmsize ~ (1|id/trans), data=cover15)


#Summary of models ------------------------------------------------------------
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

#AIC values
AIC(fit1)
AIC(fit2)
AIC(fit3) 
AIC(fit4) 
AIC(fit5) 

######
#PLOTS -------------------------------------------------------------------------
######
#Disturbance increases bare ground
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/bare.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) #set margins to corr. to cex
boxplot(bare~hidist, data=cover15, xaxt='n', cex.axis=1.5, cex.lab=1.5,
        ylab='% Bare Ground')
dev.off()

#Disturbance decreases veg cover, which is higher at low elev
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/veg.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2))
boxplot(veg~low_elev, data=cover15, xaxt='n', cex.axis=1.5, cex.lab=1.5,
        ylab='% Co-Occurring Vegetation')
dev.off()
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/veg_dist.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2))
boxplot(veg~hidist, data=cover15, xaxt='n', cex.axis=1.5, cex.lab=1.5,
        ylab='% Co-Occurring Vegetation')
dev.off()

#Disturbance and lower veg cover increases Silene size
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/area.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
boxplot(area~hi, data=size15, xaxt='n', cex.axis=1.5, cex.lab=1.5,
        ylab='')
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/area_veg.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
plot(area~veg, data=size15, cex.axis=1.5, cex.lab=1.5,
        xlab='% Cover Co-Occurring Vegetation',
        ylab='')
dev.off()

#Disturbance increases Silene presence and density where present
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/pa.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
plot(pa~hidist, data=cover15, cex.axis=1.5, cex.lab=1.5,
     xlab='Absence (0) or Presence (1) of High Disturbance',
     ylab='Proportion of Silene acaulis absence (0) or presence (1) out of all samples')
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/sil_density.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
plot(sil~hidist, data=cover15, cex.axis=1.5, cex.lab=1.5,
     xlab='Absence (0) or Presence (1) of High Disturbance',
     ylab='% Cover of Silene acaulis (where present)')
dev.off()

#Smaller plants are bigger at lower elevation and not affected by dist
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/smarea.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
boxplot(avgsmsize~low_elev, data=cover15, xaxt='n', cex.axis=1.5, cex.lab=1.5,
        ylab='')
dev.off()

#Disturbance allows for more Minuartia cover
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/min.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
boxplot(min~hidist, data=cover15, xaxt='n', cex.axis=1.5, cex.lab=1.5,
        ylab='')
dev.off()

#Disturbance increase presence of MIN and PHLOX
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/min_pa.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
plot(min_pa~hidist, data=cover15, cex.axis=1.5, cex.lab=1.5,
     xlab='Absence (0) or Presence (1) of High Disturbance',
     ylab='Proportion of Minuartia obtusiloba absence (0) or presence (1) out of all samples')
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/phlox_pa.eps')
par(bg='lightgrey', mai=c(1.02,0.82*1.5,0.2,0.2)) 
plot(pa~hidist, data=cover15, cex.axis=1.5, cex.lab=1.5,
     xlab='Absence (0) or Presence (1) of High Disturbance',
     ylab='Proportion of Phlox pulvinata absence (0) or presence (1) out of all samples')
dev.off()

##############
#LMMs NOT USED ----------------------------------------------------------------
##############
fit_trail <- lmer(area ~ trail * low_elev 
                  + (1|id/trans/quad), data=size15) #anova p-value(hi)=0.009691

fit_hi <- lmer(area ~ hi * low_elev 
               + (1|id/trans/quad), data=size15) #anova p-value(hi)=0.005993

fit_hidist <- lmer(avgsize ~ hidist * low_elev 
                   + (1|id/trans), data=cover15) #nonsig.

fit_hidistsm <- lmer(avgsmsize ~ hidist + low_elev 
                     + (1|id/trans), data=cover15) #anova p-value(low_elev)=0.002133

fit_veg <- lmer(area ~ veg + low_elev 
                + (1|id/trans/quad), data=size15) #anova p-value(veg)=0.00608

fit_bare <- lmer(area ~ bare + low_elev 
                 + (1|id/trans/quad), data=size15) #anova p-value(bare)=0.2111

fit_bare_veg <- lmer(area ~ bare * veg * low_elev 
                     + (1|id/trans/quad), data=size15) #none sig., with or w/o int.

########################################
#CHECKING MODEL ASSUMPTIONS OF RESIDUALS ---------------------------------------
########################################
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/logavgsmsize_residuals.eps')
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
# scatterplot residuals vs. fitted
scatter.smooth(fitted(fit1), resid(fit1))
abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8)  

# Normal qq-plot of residuals
qqnorm(resid(fit1), main="normal qq-plot, residuals")
qqline(resid(fit1))

# scatterplot of sqrt of absolute residuals vs. fitted
scatter.smooth(fitted(fit1), sqrt(abs(resid(fit1))))
mtext("Scale-Location", 3, line = 0.8)

# Normal qq-plot of random effects
qqnorm(ranef(fit1)$id[,1], main="normal qq-plot, random effects")
qqline(ranef(fit1)$id[,1]) 
dev.off()

############
#OTHER PLOTS ------------------------------------------------------------------
############
#GGPLOT: work in progress
#create df for plot
df <- data.frame(matrix(NA, nrow=4, ncol=4))
colnames(df) <- c('variable', 'dist', 'mean', 'se')
df$variable <- c('veg', 'veg', 'bare', 'bare')
df$dist <- c('0','1','0','1')
#calculate mean and se
se0 <- std.error(cover15[which(cover15$hidist==0),]$veg)
me0 <- mean(cover15[which(cover15$hidist==0),]$veg)
se1 <- std.error(cover15[which(cover15$hidist==1),]$veg)
me1 <- mean(cover15[which(cover15$hidist==1),]$veg)

se2 <- std.error(cover15[which(cover15$hidist==0),]$bare)
me2 <- mean(cover15[which(cover15$hidist==0),]$bare)
se3 <- std.error(cover15[which(cover15$hidist==1),]$bare)
me3 <- mean(cover15[which(cover15$hidist==1),]$bare)
df$mean <- c(me0, me1, me2, me3)
df$se <- c(se0, se1, se2, se3)
ggplot(df, aes(dist, mean))


x1 <- size15[which(size15$low_elev==1),]
x2 <- size15[which(size15$low_elev==0),]
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/area_dist.eps')
par(mfrow=c(1,2), bg='lightgrey')
boxplot(area~hi, data=x1, xlab='', ylab='Plant Area (cm^2)', 
        main='Range Limit')
boxplot(area~hi, data=x2, xlab='', ylab='',
        main='Range Center')
dev.off()

x1 <- size15[which(size15$hi==1),]
x2 <- size15[which(size15$hi==0),]
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/area_elev.eps')
par(mfrow=c(1,2), bg='lightgrey')
boxplot(area~low_elev, data=x1, xlab='', 
        ylab='Plant Area (cm^2)', main='Disturbed')
boxplot(area~low_elev, data=x2, xlab='', ylab='Plant Area (cm^2)',
        main='Undisturbed')
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/area_veg.eps')
par(bg='lightgrey')
plot(area~veg, data=size15, xlab='% Vegetation Cover',
     ylab='Plant Area (cm^2)')
dev.off()

setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/percover_dist.eps')
par(mfrow=c(1,2), bg='lightgrey')
#boxplot(veg~hidist, data=cover15, ylab='% Vegetation Cover')
boxplot(bare~hidist, data=cover15, ylab='% Bare Ground')
dev.off()