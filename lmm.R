## N. Chardon
## Start: 4 Dec 2015
## Aim: Run LMM with 2015 data
## Generated dataframes:
load('~/Desktop/Research/silene/r_files/size15_trailsubset2.RData') #CURRENT DF
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') 
#most recent figs coded with seT2 to indicate this DF

load('~/Desktop/Research/silene/r_files/size15_lmm2.RData') 
load('~/Desktop/Research/silene/r_files/cover15_lmm2.RData') #updated in climate.R

load('~/Desktop/Research/silene/r_files/size15_trailsubset.RData') #subsetted for trail
load('~/Desktop/Research/silene/r_files/cover15_trailsubset.RData') 
load('~/Desktop/Research/silene/r_files/size15_distsubset.RData') #subsetted for dist
load('~/Desktop/Research/silene/r_files/cover15_distsubset.RData') 

load('~/Desktop/Research/silene/r_files/size15_lmm.RData') #old df
load('~/Desktop/Research/silene/r_files/cover15_lmm.RData') #old df

rm(list=ls())
library(lme4) #lmm and glmm
library(AICcmodavg) #AICc
library(lmerTest) #p-values
library(MASS) #probability distribution of residuals
library(car)
library(ggplot2)

#WD for figs
setwd("~/Desktop/Research/silene/disturbance_paper/figs")

#################
## SET UP DATA ## -------------------------------------------------------------
#################
setwd("~/Desktop/Research/silene/r_files/")
load('size15.RData')
load('cover15.RData')

#delete cover data errors from df (this also deletes sites w/o dist % estimation)
#cover15$foo <- cover15$low+cover15$med+cover15$hi
#cover15 <- subset(cover15, foo==100)
#cover15$foo <- cover15[!cover15$foo] #length 1200

#size15$foo <- size15$low_dist+size15$med_dist+size15$hi_dist
#size15 <- subset(size15, foo==100)
#size15$foo <- size15[!size15$foo] #length 1200

#convert to factor
size15$trail <- as.factor(size15$trail)
size15$low_elev <- as.factor(size15$low_elev)
cover15$trail <- as.factor(cover15$trail)
cover15$low_elev <- as.factor(cover15$low_elev)

#convert % dist. to single number
#weighted average
# size15$wt_dist <- size15$low_dist*1 + size15$med_dist*2 + size15$hi_dist*3

# >50% of quad is disturbed
# n=1143 for halfdist=1
# n=2263 for halfdist=0
for(i in 1:length(size15$low_dist)) {
    if (is.na(size15$low_dist[i])) {
        size15$halfdist[i] <- 'NA'
    }    
    else if (size15$low_dist[i]<50) {
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
    if (is.na(size15$hi_dist[i])) { #label NA values as NA, otherwise get false assigned
        size15$hi[i] <- 'NA'
    }    
    else if (size15$hi_dist[i]>0) {
                size15$hi[i] <- 1
        } else {
                size15$hi[i] <- 0
        }
}
size15$hi <- as.factor(size15$hi)

for(i in 1:length(cover15$hi)) {
    if (is.na(cover15$hi[i])) {
        cover15$hidist[i] <- 'NA'
    }    
    else if (cover15$hi[i]>0) {
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
size15$rock <- cover15$rock[cover152size15]
size15$gram <- cover15$gram[cover152size15]
size15$forb <- cover15$forb[cover152size15]

#assign pa to min, paro, phlox
for (i in 1:dim(cover15)[1]) {
        if (cover15$min[i] > 0) {
                cover15$min_pa[i] <- '1'
        }
        else {
                cover15$min_pa[i] <- '0'
        }
        if (is.na(cover15$paro[i])) {
            cover15$paro_pa[i] <- 'NA'
        }    
        else if (cover15$paro[i] > 0) {
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
cover15$paro_pa <- as.factor(as.numeric(cover15$paro_pa)) #NA where not measured
cover15$phlox_pa <- as.factor(as.numeric(cover15$phlox_pa))

#save expanded dfs
save(size15, file='~/Desktop/Research/silene/r_files/size15_lmm2.RData')
save(cover15, file='~/Desktop/Research/silene/r_files/cover15_lmm2.RData')

## CURRENT DF (sites n=18) ## 
#subset DFs to delete sites w/o trails (NIW, KIN, MAM) 
#& transects 1-4 at BIE and GRA (= pilot study transects)
#AIC_trail2_subset table
load('~/Desktop/Research/silene/r_files/size15_lmm2.RData') #load original DF
load('~/Desktop/Research/silene/r_files/cover15_lmm2.RData') #load original DF
#remove NIW, MAM, KIN
size15 <- size15[!(size15$id %in% c("niw",'kin','mam')),]
cover15 <- cover15[!(cover15$id %in% c("niw",'kin','mam')),]
save(size15, file='~/Desktop/Research/silene/r_files/size15_trailsubset.RData')
save(cover15, file='~/Desktop/Research/silene/r_files/cover15_trailsubset.RData')
#remove BIE and GRA T1-4
load('~/Desktop/Research/silene/r_files/size15_trailsubset.RData') 
load('~/Desktop/Research/silene/r_files/cover15_trailsubset.RData') 
size15 <- size15[-c(183:308, 1934:2062),]
cover15 <- cover15[-c(81:120, 682:721),]
save(size15, file='~/Desktop/Research/silene/r_files/size15_trailsubset2.RData')
save(cover15, file='~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData')

#OR: subset DFs to delete sites w/o % dist estimation (sites n=17)
#NIW, HAN, RED, SUN
#AIC_dist_subset table
load('~/Desktop/Research/silene/r_files/size15_lmm2.RData') #load original DF
load('~/Desktop/Research/silene/r_files/cover15_lmm2.RData') #load original DF
size15 <- size15[!(size15$id %in% c("niw",'han','red', 'sun')),]
cover15 <- cover15[!(cover15$id %in% c("niw",'han','red', 'sun')),]
save(size15, file='~/Desktop/Research/silene/r_files/size15_distsubset.RData')
save(cover15, file='~/Desktop/Research/silene/r_files/cover15_distsubset.RData')

########################################################
## MODELS WITH size15 (RANDOM EFFECTS: id/trans/quad) ## -----------------------
########################################################

#RESIDUALS-ONLY LMM
fit5 <- lmer(logarea ~ (1|id/trans/quad), data=size15)

#LMM: LOGAREA ~ ELEV + DIST
#logarea so that residual variances fit model assumptions
fit1 <- lmer(logarea ~ low_elev + (1|id/trans/quad), data=size15)
fit2 <- lmer(logarea ~ hi + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ low_elev + hi + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * hi + (1|id/trans/quad), data=size15)
#LMM: LOGAREA ~ ELEV + TRAIL
fit2 <- lmer(logarea ~ trail + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ trail + low_elev + + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * trail + (1|id/trans/quad), data=size15)

#LMM: LOGAREA ~ ELEV + VEG
fit2 <- lmer(logarea ~ veg + (1|id/trans/quad), data=size15)
fit3 <- lmer(logarea ~ low_elev + veg + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * veg + (1|id/trans/quad), data=size15)
#add ROCK => higher AIC and no factors sig.
fit4 <- lmer(logarea ~ low_elev * veg * rock + (1|id/trans/quad), data=size15)

#LMM: LOGAREA ~ VEG + TRAIL
fit3 <- lmer(logarea ~ veg + trail + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ trail * veg + (1|id/trans/quad), data=size15)

#LMM: LOGAREA ~ ELEV + BARE
fit2 <- lmer(logarea ~ bare + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ low_elev + bare + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * bare + (1|id/trans/quad), data=size15)

#LMM: LOGAREA ~ DIST + BARE
fit3 <- lmer(logarea ~ hi + bare + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ hi * bare + (1|id/trans/quad), data=size15)
#LMM: LOGAREA ~ TRAIL + BARE
fit3 <- lmer(logarea ~ trail + bare + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ trail * bare + (1|id/trans/quad), data=size15)

#LMM: LOGAREA ~ ROCK + TRAIL
fit2 <- lmer(logarea ~ rock + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ trail + rock + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ trail * rock + (1|id/trans/quad), data=size15)

#LMM: LOGAREA ~ ASP (only nested transects)
#transform aspect
size15$cosasp <- cos(size15$asp) #71 NA values
fit1 <- lmer(logarea ~ cosasp + (1|id/trans), data=size15)
fit5 <- lmer(logarea ~ (1|id/trans), data=size15)

#SEPARATE LMM: LOGAREA ~ VEG | ROCK models for BOTH elevations
#run both model sets for both elevs
fit1 <- lmer(logarea ~ trail + (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])
fit2 <- lmer(logarea ~ veg + (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])
fit3 <- lmer(logarea ~ trail + veg + (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])
fit4 <- lmer(logarea ~ trail * veg + (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])
fit5 <- lmer(logarea ~ (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])

fit2 <- lmer(logarea ~ rock + (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])
fit3 <- lmer(logarea ~ trail + rock + (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])
fit4 <- lmer(logarea ~ trail * rock + (1|id/trans/quad), 
             data=size15[which(size15$low_elev==0),])

#LMM: LOGAREA ~ ELEV + HI_DIST => no signficance (not included in AIC table)
fit2 <- lmer(logarea ~ hi_dist + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ low_elev + hi_dist + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ low_elev * hi_dist + (1|id/trans/quad), data=size15)

#LMM: % GRAM and % FORB on SIZE
#add forb and gram to size15
cover152size15 <- match(size15$unique, cover15$unique)
size15$gram <- cover15$gram[cover152size15]
size15$forb <- cover15$forb[cover152size15]

fit1 <- lmer(logarea ~ trail * gram + (1|id/trans/quad), data=size15) 
fit2 <- lmer(logarea ~ trail * forb + (1|id/trans/quad), data=size15) 
fit3 <- lmer(logarea ~ gram + (1|id/trans/quad), data=size15)
fit4 <- lmer(logarea ~ forb + (1|id/trans/quad), data=size15) #best model

###################################################
## MODELS WITH cover15 (RANDOM EFFECTS: id/trans) ## ----------------------------
###################################################

#LMM: % VEG COVER ~ ELEV + DIST
#subset data
cover15 <- subset(cover15, veg<=100)
fit1 <- lmer(veg ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(veg ~ hidist + (1|id/trans), data=cover15)
fit3 <- lmer(veg ~ low_elev + hidist + (1|id/trans), data=cover15)
fit4 <- lmer(veg ~ low_elev * hi + (1|id/trans), data=cover15) #best
fit5 <- lmer(veg ~ (1|id/trans), data=cover15)
#LMM: % VEG COVER ~ ELEV + TRAIL
fit2 <- lmer(veg ~ trail + (1|id/trans), data=cover15)
fit3 <- lmer(veg ~ low_elev + trail + (1|id/trans), data=cover15)
fit4 <- lmer(veg ~ low_elev * trail + (1|id/trans), data=cover15) #best

#LMM: % BARE COVER ~ ELEV + DIST
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
cover15 <- subset(cover15, bare<=100)
#log-transform bare to fit model assumptions: IN PROGRESS (residuals still don't fit assumptions)
#give 0 values a number so tranformation works
for (i in 1:length(cover15$bare)) {
    if(cover15$bare[i]==0) {
        cover15$bare[i] <- 0.0001
    }
    else {cover15$bare[i] <- cover15$bare[i]}
}
cover15$logbare <- log(cover15$bare)
fit1 <- lmer(bare ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(bare ~ hidist + (1|id/trans), data=cover15)
fit3 <- lmer(bare ~ low_elev + hidist + (1|id/trans), data=cover15)
fit4 <- lmer(bare ~ low_elev * hi + (1|id/trans), data=cover15) #best
fit5 <- lmer(bare ~ (1|id/trans), data=cover15)
#LMM: % BARE COVER ~ ELEV + TRAIL
fit2 <- lmer(bare ~ trail + (1|id/trans), data=cover15)
fit3 <- lmer(logbare ~ low_elev + trail + (1|id/trans), data=cover15)
fit4 <- lmer(bare ~ low_elev * trail + (1|id/trans), data=cover15) #best


#GLMM: SIL PRESENCE ~ ELEV + DIST => more presences with disturbance
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
#make $pa a factor for correct modeling
cover15$pa <- as.factor(cover15$pa)
fit1 <- glmer(pa ~ low_elev + (1|id/trans), data=cover15, family=binomial)
fit2 <- glmer(pa ~ hidist + (1|id/trans), data=cover15, family=binomial) #best
fit3 <- glmer(pa ~ low_elev + hidist + (1|id/trans), data=cover15, family=binomial)
fit4 <- glmer(pa ~ low_elev * hidist + (1|id/trans), data=cover15, family=binomial) #no convergence
fit5 <- glmer(pa ~ (1|id/trans), data=cover15, family=binomial)

#GLMM: SIL PRESENCE BY TRAIL ~ ELEV + VEG
fit1 <- glmer(pa ~ low_elev + (1|id/trans/quad), 
              data=cover15[which(cover15$trail==0),], family=binomial)
fit2 <- glmer(pa ~ veg + (1|id/trans), 
              data=cover15[which(cover15$trail==0),], family=binomial)
fit3 <- glmer(pa ~ low_elev + veg + (1|id/trans), 
              data=cover15[which(cover15$trail==0),], family=binomial)
fit4 <- glmer(pa ~ low_elev * veg +  (1|id/trans), 
              data=cover15[which(cover15$trail==0),], family=binomial)
fit5 <- glmer(pa ~ (1|id/trans), 
             data=cover15[which(cover15$trail==0),], family=binomial)
#GLMM: SIL PRESENCE BY TRAIL ~ ELEV + BARE
fit2 <- glmer(pa ~ bare + (1|id/trans), 
              data=cover15[which(cover15$trail==0),], family=binomial)
fit3 <- glmer(pa ~ low_elev + bare + (1|id/trans), 
              data=cover15[which(cover15$trail==0),], family=binomial)
fit4 <- glmer(pa ~ low_elev * bare +  (1|id/trans), 
              data=cover15[which(cover15$trail==0),], family=binomial)


#LMM: SIL DENSITY ~ ELEV + DIST => disturbance increases density
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
#subset for presence-only (n=760)
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
#LMM: SIL DENSITY ~ ELEV + TRAIL => same pattern
fit2 <- lmer(logsil ~ trail + (1|id/trans), data=cover15)
fit3 <- lmer(logsil ~ low_elev + trail + (1|id/trans), data=cover15)
fit4 <- lmer(logsil ~ low_elev * trail + (1|id/trans), data=cover15)
#LMM: SIL DENSITY ~ VEG + TRAIL
fit1 <- lmer(logsil ~ veg + (1|id/trans), data=cover15)
fit3 <- lmer(logsil ~ veg + trail + (1|id/trans), data=cover15)
fit4 <- lmer(logsil ~ veg * trail + (1|id/trans), data=cover15)

#LMM: AVGSMSIZE ~ ELEV + DIST => bigger smaller plants at low elev (77% quads > 5 plants)
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData')
#log-transform avgsmsize to fit model assumptions
cover15$logavgsmsize <- log(cover15$avgsmsize)
fit1 <- lmer(logavgsmsize ~ low_elev + (1|id/trans), data=cover15) 
fit2 <- lmer(logavgsmsize ~ hidist + (1|id/trans), data=cover15)
fit3 <- lmer(logavgsmsize ~ low_elev + hidist + (1|id/trans), data=cover15) 
fit4 <- lmer(logavgsmsize ~ low_elev * hidist + (1|id/trans), data=cover15)
fit5 <- lmer(logavgsmsize ~ (1|id/trans), data=cover15)
#LMM: AVGSMSIZE ~ ELEV + TRAIL => low elev and trail increases size
fit2 <- lmer(logavgsmsize ~ trail + (1|id/trans), data=cover15)
fit3 <- lmer(logavgsmsize ~ trail + low_elev + (1|id/trans), data=cover15) 
fit4 <- lmer(logavgsmsize ~ low_elev * trail + (1|id/trans), data=cover15)


#LMM: % MIN COVER ~ ELEV + DIST
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
#log-transform min: IN PROGRESS (residuals still don't fit assumptions)
for (i in 1:length(cover15$min)) {
        if(cover15$min[i]==0) {
                cover15$min[i] <- 0.0001
        }
        else {cover15$min[i] <- cover15$min[i]}
}
cover15$logmin <- log(cover15$min)
fit1 <- lmer(min ~ low_elev + (1|id/trans), data=cover15)
fit2 <- lmer(min ~ hidist + (1|id/trans), data=cover15) #best
fit3 <- lmer(min ~ low_elev + hidist + (1|id/trans), data=cover15)
fit4 <- lmer(min ~ low_elev * hi + (1|id/trans), data=cover15)
fit5 <- lmer(min ~ (1|id/trans), data=cover15) 
#LMM: % MIN COVER ~ ELEV + TRAIL more min at trail and at lower elev, but interaction not sig. 
fit2 <- lmer(min ~ trail + (1|id/trans), data=cover15) #best
fit3 <- lmer(logmin ~ low_elev + trail + (1|id/trans), data=cover15) 
fit4 <- lmer(min ~ low_elev * trail + (1|id/trans), data=cover15)

#LMM: MIN DENSITY ~ ELEV + TRAIL
foo <- subset(cover15, min>0) #subset for Min presences (n=608)
foo$logmin <- log(foo$min)
fit1 <- lmer(min ~ low_elev + (1|id/trans), data=foo)
fit2 <- lmer(min ~ trail + (1|id/trans), data=foo) 
fit3 <- lmer(logmin ~ low_elev + trail + (1|id/trans), data=foo) 
fit4 <- lmer(min ~ low_elev * trail + (1|id/trans), data=foo)
fit5 <- lmer(min ~ (1|id/trans), data=foo) 
#LMM: MIN DENSITY ~ BARE + TRAIL
fit2 <- lmer(min ~ bare + (1|id/trans), data=foo) 
fit3 <- lmer(min ~ bare + trail + (1|id/trans), data=foo) 
fit4 <- lmer(min ~ bare * trail + (1|id/trans), data=foo)

#GLMM: MIN PA ~ ELEV + DIST => more min presences with disturbance
fit1 <- glmer(min_pa ~ low_elev + (1|id/trans), data=cover15, family=binomial)
fit2 <- glmer(min_pa ~ hidist + (1|id/trans), data=cover15, family=binomial) #best 
fit3 <- glmer(min_pa ~ low_elev + hidist + (1|id/trans), data=cover15, family=binomial)
fit4 <- glmer(min_pa ~ low_elev * hidist + (1|id/trans), data=cover15, family=binomial)
fit5 <- glmer(min_pa ~ (1|trans/id), data=cover15, family=binomial)
#GLMM: % MIN COVER ~ ELEV + TRAIL => same pattern (more p at trail), but stronger
fit2 <- glmer(min_pa ~ trail + (1|trans/id), data=cover15, family=binomial) #best 
fit3 <- glmer(min_pa ~ low_elev + trail + (1|id/trans), data=cover15, family=binomial)
fit4 <- glmer(min_pa ~ low_elev * trail + (1|id/trans), data=cover15, family=binomial)


#LMM: % PHLOX COVER ~ TRAIL + ELEV 
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
#log-transform phlox: IN PROGRESS - RESIDUALS DON'T FIT MODEL ASSUMPTIONS
for (i in 1:length(cover15$phlox)) {
    if(cover15$phlox[i]==0) {
        cover15$phlox[i] <- 0.0001
    }
    else {cover15$phlox[i] <- cover15$phlox[i]}
}
cover15$logphlox <- log(cover15$phlox)
fit1 <- lmer(phlox ~ low_elev + (1|id/trans), data=cover15) 
fit2 <- lmer(phlox ~ trail + (1|id/trans), data=cover15) 
fit3 <- lmer(logphlox ~ trail + low_elev + (1|id/trans), data=cover15) 
fit4 <- lmer(phlox ~ trail * low_elev + (1|id/trans), data=cover15) 
fit5 <- lmer(phlox ~ (1|id/trans), data=cover15) 

#LMM: PHLOX DENSITY ~ ELEV + TRAIL
foo <- subset(cover15, phlox>0) #subset for Phlox presences (n=189)
foo$logphlox <- log(foo$phlox)
fit1 <- lmer(logphlox ~ low_elev + (1|id/trans), data=foo)
fit2 <- lmer(logphlox ~ trail + (1|id/trans), data=foo) 
fit3 <- lmer(logphlox ~ low_elev + trail + (1|id/trans), data=foo) 
fit4 <- lmer(logphlox ~ low_elev * trail + (1|id/trans), data=foo)
fit5 <- lmer(logphlox ~ (1|id/trans), data=foo) 
#LMM: PHLOX DENSITY ~ BARE + TRAIL
fit2 <- lmer(logphlox ~ bare + (1|id/trans), data=foo) 
fit3 <- lmer(logphlox ~ bare + trail + (1|id/trans), data=foo) 
fit4 <- lmer(logphlox ~ bare * trail + (1|id/trans), data=foo)

#GLMM: PHLOX PRESENCE ~ ELEV + DIST => increase in phlox presence with disturbance
fit1 <- glmer(phlox_pa ~ low_elev + (1|id/trans), data=cover15, family=binomial) #best 
fit2 <- glmer(phlox_pa ~ hidist + (1|id/trans), data=cover15, family=binomial)
fit3 <- glmer(phlox_pa ~ low_elev + hidist + (1|id/trans), data=cover15, family=binomial)
fit4 <- glmer(phlox_pa ~ low_elev * hidist + (1|id/trans), data=cover15, family=binomial)
fit5 <- glmer(phlox_pa ~ (1|trans/id), data=cover15, family=binomial)
#GLMM: PHLOX PRESENCE ~ ELEV + TRAIL => range location determines presence
fit2 <- glmer(phlox_pa ~ trail + (1|trans/id), data=cover15, family=binomial) 
fit3 <- glmer(phlox_pa ~ low_elev + trail + (1|id/trans), data=cover15, family=binomial)
fit4 <- glmer(phlox_pa ~ low_elev * trail + (1|id/trans), data=cover15, family=binomial)

#LMM: % PARO COVER ~ TRAIL + ELEV
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
#log-transform paro: IN PROGRESS, RESIDUALS STILL DONT MEET MODEL ASSUMPTIONS
for (i in 1:length(cover15$paro)) {
    if(cover15$paro[i]==0) {
        cover15$paro[i] <- 0.0001
    }
    else {cover15$paro[i] <- cover15$paro[i]}
}
cover15$logparo <- log(cover15$paro)
fit1 <- lmer(paro ~ low_elev + (1|id/trans), data=cover15) 
fit2 <- lmer(paro ~ trail + (1|id/trans), data=cover15) 
fit3 <- lmer(logparo ~ trail + low_elev + (1|id/trans), data=cover15) 
fit4 <- lmer(paro ~ trail * low_elev + (1|id/trans), data=cover15) 
fit5 <- lmer(paro ~ (1|id/trans), data=cover15) 

#LMM: PARO DENSITY ~ ELEV + TRAIL
foo <- subset(cover15, paro>0) #subset for Phlox presences (n=136)
foo$logparo <- log(foo$paro)
fit1 <- lmer(logparo ~ low_elev + (1|id/trans), data=foo)
fit2 <- lmer(logparo ~ trail + (1|id/trans), data=foo) 
fit3 <- lmer(logparo ~ low_elev + trail + (1|id/trans), data=foo) 
fit4 <- lmer(logparo ~ low_elev * trail + (1|id/trans), data=foo)
fit5 <- lmer(logparo ~ (1|id/trans), data=foo) 
#LMM: PARO DENSITY ~ BARE + TRAIL
fit2 <- lmer(logparo ~ bare + (1|id/trans), data=foo) 
fit3 <- lmer(logparo ~ bare + trail + (1|id/trans), data=foo) 
fit4 <- lmer(logparo ~ bare * trail + (1|id/trans), data=foo)


#GLMM: PARO PRESENCE ~ ELEV + TRAIL
fit1 <- glmer(paro_pa ~ low_elev + (1|id/trans), data=cover15, family=binomial)
fit2 <- glmer(paro_pa ~ trail + (1|id/trans), data=cover15, family=binomial) 
fit3 <- glmer(paro_pa ~ low_elev + trail + (1|id/trans), data=cover15, family=binomial)
fit4 <- glmer(paro_pa ~ low_elev * trail + (1|id/trans), data=cover15, family=binomial)
fit5 <- glmer(paro_pa ~ (1|id/trans), data=cover15, family=binomial)


## POISSON GLMM ## ---------------------------------------------------

# SILENE #
#check probability distribution of residuals
cover15$sil <- as.integer(cover15$sil) #make integer for nbinom
cover15$sil.t <- cover15$sil + 1 #tranform 0s for distributions that can't have 0s
qqp(cover15$sil.t, 'norm') #normal
qqp(cover15$sil.t, 'lnorm') #lognormal
nbinom <- fitdistr(cover15$sil.t, 'negative binomial') #negative binomial
pdf('residuals/sil_nbinom.pdf')
qqp(cover15$sil.t, 'nbinom', size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
dev.off()
pois <- fitdistr(cover15$sil.t, 'Poisson') #poisson
pdf('residuals/sil_pois.pdf')
qqp(cover15$sil.t, 'pois', pois$estimate)
dev.off()
gam <- fitdistr(cover15$sil.t, 'gamma') #gamma
pdf('residuals/sil_gam.pdf')
qqp(cover15$sil.t, 'gamma', shape = gam$estimate[[1]], rate = gam$estimate[[2]])
dev.off()

#Silene cover
fit1 <- glmer(sil ~ trail + (1|id/trans), data=cover15, family=poisson) #link='log'
fit2 <- glmer(sil ~ low_elev + (1|id/trans), data=cover15, family=poisson)
fit3 <- glmer(sil ~ trail + low_elev + (1|id/trans), data=cover15, family=poisson)
fit4 <- glmer(sil ~ trail * low_elev + (1|id/trans), data=cover15, family=poisson)
fit5 <- glmer(sil ~ (1|id/trans), data=cover15, family=poisson)

# MINUARTIA #
#check probability distribution of residuals
cover15$min <- as.integer(cover15$min) 
cover15$min.t <- cover15$min + 1
qqp(cover15$min.t, 'norm') 
qqp(cover15$min.t, 'lnorm') 
nbinom <- fitdistr(cover15$min.t, 'negative binomial')
pdf('residuals/min_nbinom.pdf')
qqp(cover15$min.t, 'nbinom', size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
dev.off()
pois <- fitdistr(cover15$min.t, 'Poisson') 
pdf('residuals/min_pois.pdf')
qqp(cover15$min.t, 'pois', pois$estimate)
dev.off()

#Minuartia cover
fit1 <- glmer(min ~ trail + (1|id/trans), data=cover15, family=poisson)
fit2 <- glmer(min ~ low_elev + (1|id/trans), data=cover15, family=poisson)
fit3 <- glmer(min ~ trail + low_elev + (1|id/trans), data=cover15, family=poisson)
fit4 <- glmer(min ~ trail * low_elev + (1|id/trans), data=cover15, family=poisson)
fit5 <- glmer(min ~ (1|id/trans), data=cover15, family=poisson)

# PHLOX #
#check probability distribution of residuals
cover15$phlox <- as.integer(cover15$phlox) 
cover15$phlox.t <- cover15$phlox + 1
qqp(cover15$phlox.t, 'norm') 
qqp(cover15$phlox.t, 'lnorm') 
nbinom <- fitdistr(cover15$phlox.t, 'negative binomial')
pdf('residuals/phlox_nbinom.pdf')
qqp(cover15$phlox.t, 'nbinom', size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
dev.off()
pois <- fitdistr(cover15$phlox.t, 'Poisson') 
pdf('residuals/phlox_pois.pdf')
qqp(cover15$phlox.t, 'pois', pois$estimate)
dev.off()

#Phlox cover
fit1 <- glmer(phlox ~ trail + (1|id/trans), data=cover15, family=poisson)
fit2 <- glmer(phlox ~ low_elev + (1|id/trans), data=cover15, family=poisson)
fit3 <- glmer(phlox ~ trail + low_elev + (1|id/trans), data=cover15, family=poisson)
fit4 <- glmer(phlox ~ trail * low_elev + (1|id/trans), data=cover15, family=poisson)
fit5 <- glmer(phlox ~ (1|id/trans), data=cover15, family=poisson)

# PARONYCHIA #
#check probability distribution of residuals
cover15$paro <- as.integer(cover15$paro) 
cover15$paro.t <- cover15$paro + 1
qqp(cover15$paro.t, 'norm') 
qqp(cover15$paro.t, 'lnorm') 
nbinom <- fitdistr(cover15$paro.t, 'negative binomial')
pdf('residuals/paro_nbinom.pdf')
qqp(cover15$paro.t, 'nbinom', size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
dev.off()
pois <- fitdistr(cover15$paro.t, 'Poisson') 
pdf('residuals/paro_pois.pdf')
qqp(cover15$paro.t, 'pois', pois$estimate)
dev.off()

#Paronychia cover
fit1 <- glmer(paro ~ trail + (1|id/trans), data=cover15, family=poisson)
fit2 <- glmer(paro ~ low_elev + (1|id/trans), data=cover15, family=poisson)
fit3 <- glmer(paro ~ trail + low_elev + (1|id/trans), data=cover15, family=poisson)
fit4 <- glmer(paro ~ trail * low_elev + (1|id/trans), data=cover15, family=poisson)
fit5 <- glmer(paro ~ (1|id/trans), data=cover15, family=poisson)


## QUASI-POISSON GLMM ## ---------------------------------------------------
## no option for this in lme4
#Silene cover
cover15$sil <- as.integer(cover15$sil) #poisson needs integers
fit2 <- glmer(sil ~ trail + (1|id/trans), data=cover15, family=poisson) 
fit2 <- glmer(sil ~ low_elev + (1|id/trans), data=cover15, family=poisson)
fit3 <- glmer(sil ~ trail + low_elev + (1|id/trans), data=cover15, family=poisson)
fit4 <- glmer(sil ~ trail * low_elev + (1|id/trans), data=cover15, family=poisson)
fit5 <- glmer(sil ~ (1|id/trans), data=cover15, family=poisson)


# MODEL SUMMARIES # ------------------------------------------------------------
#p-values and variances
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

#AIC values
AICc(fit1)
AICc(fit2)
AICc(fit3) 
AICc(fit4)
AICc(fit5) 


########################################
#CHECKING MODEL ASSUMPTIONS OF RESIDUALS ---------------------------------------
########################################
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/residuals/logparo_density.eps')
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
# scatterplot residuals vs. fitted
scatter.smooth(fitted(fit3), resid(fit3))
abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8)  

# Normal qq-plot of residuals
qqnorm(resid(fit3), main="normal qq-plot, residuals")
qqline(resid(fit3))

# scatterplot of sqrt of absolute residuals vs. fitted
scatter.smooth(fitted(fit3), sqrt(abs(resid(fit3))))
mtext("Scale-Location", 3, line = 0.8)

# Normal qq-plot of random effects
qqnorm(ranef(fit3)$id[,1], main="normal qq-plot, random effects")
qqline(ranef(fit3)$id[,1]) 
dev.off()


######
#PLOTS -------------------------------------------------------------------------
######
#SIL histogram
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/hist/sil.eps')
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
breaks <- c(seq(-1, 33, 1))
hist(cover15$sil, breaks=breaks, xlab='% Silene Cover',
     main='Histogram of Silene Cover (breaks = 1%)')
dev.off()

#BARE histogram
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/hist/bare.eps')
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
cover15 <- subset(cover15, bare<=100)
breaks <- c(seq(-1, 80, 1))
hist(cover15$bare, breaks=breaks, xlab='% Bare Ground Cover',
     main='Histogram of Bare Ground Cover (breaks = 1%)')
dev.off()

#MIN histogram
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/hist/min.eps')
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData') #reload for full DF
breaks <- c(seq(-1, 16, 1))
hist(cover15$min, breaks=breaks, xlab='% Minuartia Cover',
     main='Histogram of Minuartia Cover (breaks = 1%)')
dev.off()

#PHLOX histogram
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/hist/phlox.eps')
breaks <- c(seq(-1, 21, 1))
hist(cover15$phlox, breaks=breaks, xlab='% Phlox Cover',
     main='Histogram of Phlox Cover (breaks = 1%)')
dev.off()

#PARO histogram
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/hist/paro.eps')
breaks <- c(seq(-1, 12, 1))
hist(cover15$paro, breaks=breaks, xlab='% Paronychia Cover',
     main='Histogram of Paronychia Cover (breaks = 1%)')
dev.off()

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

#Disturbance increases Silene presence, and density where present
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/pa.eps')
#par(mai=c(1.02,0.82*1.5,0.2,0.2)) 
plot(pa~trail, data=cover15[!is.na(cover15$pa),], cex.axis=1.5, cex.lab=1.5,
     xlab='Absence (0) or Presence (1) of Trail',
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