graphics.off()
rm(list=ls())

setwd("C:/Users/Dan Doak/Desktop/Students/Nathalie/QAIC")
# load libraries
#library(AICcmodavg) #AICc
library(MASS) #glmmPQL
library(lme4)
# library(AICcmodavg)
# library(MuMIn)
library(bbmle)
library(glmmADMB)

# load DFs (no MAM, KIN, NIW, GRA T1-4, BIE T1-4)
load('size15_trailsubset2.RData')
load('cover15_trailsubset2.RData')

cover15 <- subset(cover15, veg<=100) #subset data
cover15$wts=rep(100,times=length(cover15$id))
cover15$veg_p <- cover15$veg/100 #tranform to % for glmmPQL
#---------------------------------------------------

# The documentation for the R betareg package mentions that
# if y also assumes the extremes 0 and 1, a useful transformation in practice is (y * (n???1) + 0.5) / n where n is the sample size.

numcases=length(cover15$veg_p)

#adjust data so that there are not 0 or 1 values: 
cover15$veg_padj = (cover15$veg_p*(numcases-1) +0.5)/numcases

cover15$idfactor=as.factor(cover15$id)
cover15$transfactor=as.factor(cover15$trans)

plot(cover15$veg_p, cover15$veg_padj)


betamod1= glmmadmb(veg_padj ~ as.factor(trail), random =~ 1|idfactor/transfactor,family='beta',data=cover15)


boxplot(cover15$veg_p