## N. Chardon
## Start: 13 May 2016
## End: 6 Jun 2016 (using beta regressions now, see beta_reg.R)
## Aim: GLMM with quasi-binomial residual distribution

# load DFs (no MAM, KIN, NIW, GRA T1-4, BIE T1-4)
load('~/Desktop/Research/silene/r_files/size15_trailsubset2.RData')
load('~/Desktop/Research/silene/r_files/cover15_trailsubset2.RData')

# load libraries
library(AICcmodavg) #AICc
library(MASS) #glmmPQL

# % VEG COVER model
cover15 <- subset(cover15, veg<=100) #subset data
cover15$veg_p <- cover15$veg/100 #tranform to % for glmmPQL
fit_quasibi <- glmmPQL(veg_p ~ trail, random= ~1|id/trans, 
                    family=quasibinomial(link="logit"), data=cover15)

# extract deviance: not stored in model
deviance(fit_quasibi)

# manually extract deviance (only works with GLM)
dfun <- function(object) {
    with(object, sum((weights * residuals^2)[weights > 0])/df.residual)
}


# automated model run: IN PROGRESS
y <- c('veg', ...) #set up DF and run loop through rows and columns?
x <- c('trail', ...)
mods <- data.frame(colnames('y', 'x', 'coeff', ...))
for(i in 1:length(y)) {
    for(j in 1:length(x)) {
        n <- #index for model run
        mods$y[n] <- print(y[i])
        mods$x[n] <- print(x[j])
        fit[n] <- glmmPQL(cover15[,y[i]] ~ cover15[,x[j]], random= ~1|id/trans, 
                          family=quasibinomial(link="logit"))
        mods$coef[n] <- summary(fit[n])$SOMETHING #extract values and write to table
    }
}