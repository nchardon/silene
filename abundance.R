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

#Wilcoxon rank sum tests: sig. diff. across low and mid-elev. sites regardless
#of dist and b/w low elev. dist/undist, but not between dist/undist high sites
wilcox.test(sums$lt_dens, sums$lnt_dens) #p=0.039
wilcox.test(sums$lt_dens, sums$ht_dens) #p=0.022
wilcox.test(sums$lnt_dens, sums$hnt_dens) #p=0.020
wilcox.test(sums$ht_dens, sums$hnt_dens) #not sig.

wilcox.test(sums$lt_reldens, sums$lnt_reldens) #p=0.039
wilcox.test(sums$lt_reldens, sums$ht_reldens) #p=0.022
wilcox.test(sums$lnt_reldens, sums$hnt_reldens) #p=0.018
wilcox.test(sums$ht_reldens, sums$hnt_reldens) #not sig.

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
plot(sums14_na$ros~sums14_na$elev)
abline(fit)