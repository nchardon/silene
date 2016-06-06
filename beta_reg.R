## N. Chardon and D. Doak
## Start: 6 June 2016
## Aim: GLMM (mixed model beta regressions) for 2015 % cover data

# install packages
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(R2admb)
library(glmmADMB)
library(AICcmodavg) #AICc

# load cover data (no MAM, KIN, NIW, GRA T1-4, BIE T1-4)
setwd("~/Desktop/Research/silene/")
load('r_files/cover15_trailsubset2.RData')

# transform data
cover15 <- subset(cover15, veg<=100) #subset data
cover15$veg_p <- cover15$veg/100 #tranform to %

# adjust data so that there are not 0 or 1 values
numcases <- length(cover15$veg_p)
cover15$veg_padj <- (cover15$veg_p*(numcases-1) +0.5)/numcases 
plot(cover15$veg_p, cover15$veg_padj) #plot data shift

# random variables as factor
cover15$idfactor=as.factor(cover15$id)
cover15$transfactor=as.factor(cover15$trans)

# test model
betamod2 <- glmmadmb(veg_padj ~ soil, random =~ 1|idfactor/transfactor,
                   family='beta', data=cover15)

# automated model runs
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