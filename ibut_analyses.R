## N. Chardon
## Start: 18 Sept 2015
## Aim: explore iButton data (mission length Summer 2014 - Summer 2015)

# generated datasets
load('~/Desktop/Research/silene/r_files/ibut_dat.RData') 
#ibutton id with lat, long, elev, site, and summer climate stats

rm(list=ls())

# load datasets
load('~/Desktop/Research/silene/r_files/ibut.RData')
load('~/Desktop/Research/silene/r_files/ibut_table.RData')
load('~/Desktop/Research/silene/r_files/table.RData') 
#ibut_table with correct date format
load('~/Desktop/Research/silene/r_files/ibut_dat.RData') 
#ibutton id with lat, long, elev, site
load('~/Desktop/Research/silene/r_files/means_na.RData') 

# ============================================================================
# start table 5 days after implementation (start at row 28 in [row,1,i])
table_cut <- table[28:dim(table)[1],,]

# temps for growing season: June, July, August
ibut_gro <- array(NA, dim=c(dim(table_cut)[1], 3, dim(table_cut)[3]))
#create empty array to match table array dim.
for (i in 1:dim(table_cut)[3]) {
        for (j in 1:dim(table_cut)[1]) {
                if(table_cut[j,3,i] == '6') {
                        ibut_gro[j,1,i] <- table_cut[j,2,i] #add date row if true
                }
                else if(table_cut[j,3,i] == '7') {
                        ibut_gro[j,2,i] <- table_cut[j,2,i]
                }
                else if(table_cut[j,3,i] == '8') {
                        ibut_gro[j,3,i] <- table_cut[j,2,i] 
                }
        }
}

# convert ibut_gro to numeric
ibut_gro[,1,] <- as.numeric(ibut_gro[,1,]) 
ibut_gro[,2,] <- as.numeric(ibut_gro[,2,])
ibut_gro[,3,] <- as.numeric(ibut_gro[,3,])
class(ibut_gro) <- "numeric"

# June, July, August tbot means
jun_means <- apply(ibut_gro[,1,], MARGIN=2, FUN=mean, na.rm=T)
jul_means <- apply(ibut_gro[,2,], MARGIN=2, FUN=mean, na.rm=T)
aug_means <- apply(ibut_gro[,3,], MARGIN=2, FUN=mean, na.rm=T)
ibut_dat$junavg <- jun_means #add means to ibut_dat
ibut_dat$julavg <- jul_means
ibut_dat$augavg <- aug_means
for (i in 1:nrow(ibut_dat)) {
    jja <- c(ibut_dat$junavg[i], ibut_dat$julavg[i], 
             ibut_dat$augavg[i])
    ibut_dat$jja[i] <- mean(jja, na.rm=T)
}

# max temps per iButton
jun_max <- apply(ibut_gro[,1,], MARGIN=2, FUN=max, na.rm=T) 
jul_max <- apply(ibut_gro[,2,], MARGIN=2, FUN=max, na.rm=T)
aug_max <- apply(ibut_gro[,3,], MARGIN=2, FUN=max, na.rm=T)
ibut_dat$junmax <- jun_max
ibut_dat$julmax <- jul_max
ibut_dat$augmax <- aug_max

# remove unrecorded ibuttons (n=3)
ibut_dat <- ibut_dat[is.na(ibut_dat$quad_id)==F,]
# ibuttons not found
load('~/Desktop/Research/silene/r_files/cover.RData')
cover[!cover$ibutt%in%ibut_dat$id,]

#save df
save(ibut_dat, file='~/Desktop/Research/silene/r_files/ibut_dat.RData')
#ibutton id matched with lat, long, elev, site, and summer climate stats
# ==============================================================================
# STOP 9/24/15: how best to structure array?
# length of growing season (no. of days with daily high temp > 5C)
#create ibut_day array
#for/if loop to get JJA data
#max for ibut_day[]
#mark days: if (max(ibut_day[,,]) > 5) {ibut_day[lastrow,,] == 1}
# else {ibut_day[lastrow,,] == 0}

obs <- OBS PER DAY
days <- AMT DAYS
ibut_day <- array(NA, dim=c(obs, days, dim(table)[3]))
for (i in 1:SOMETHING) {
        ibut_day[TBOT,,]
        ibut_day[,DAYX,] <- ALL TBOT THAT DAY
        ibut_day[,,IBUTTON]
}

# extract date from time stamp
as.Date(as.character(table[1,1,1], '%y %m %d'))

####################
## PLOTS =======================================================================
####################
#temperature changes with site elevation 
sites <- unique(ibut_dat$site)
means_na$lm_jjaelev <- c(NA) #initialize column for lm p-values
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/jjatemp_elev.eps')
plot(jja~elev, data=ibut_dat, type='n',
     xlab='Elevation (m)', ylab='Average JJA Temp (ºC)') #set up blank plot
colors <- c('red', 'grey', 'violet', 'blue', 'black', 'green', 'orange', 'pink')
for(i in 1:length(sites)) {
        foo <- ibut_dat[which(ibut_dat$site==sites[i]),]
        for(j in 1:nrow(foo)){
            points(jja[j]~elev[j], data=foo, col=colors[i],
                       pch=16, cex=0.85)
            fit <- lm(jja~elev, data=foo)
            abline(fit, col=colors[i])
            means_na[which(means_na$site==sites[i]),]$lm_jjaelev <- 
                summary(fit)$coefficients[2,4] #p-value
        }
}
legend('center', pch=c(rep.int(16,8)), col=colors, 
       legend=c(sites))
dev.off()

#temperature changes with site elevation: grouped = more clear pattern
group1 <- sites[1:5] #higher elev sites
group2 <- sites[6:8] #lower elev sites

library(lme4)
library(lmerTest)
foo <- ibut_dat[which(ibut_dat$site==group1[1]),]
foo <- rbind(foo, ibut_dat[which(ibut_dat$site==group1[2]),])
foo <- rbind(foo, ibut_dat[which(ibut_dat$site==group1[3]),])
foo <- rbind(foo, ibut_dat[which(ibut_dat$site==group1[4]),])
foo <- rbind(foo, ibut_dat[which(ibut_dat$site==group1[5]),])
foo <- rbind(foo, ibut_dat[which(ibut_dat$site==group2[1]),])
fit <- lmer(jja~elev + (1|site), data=foo) #non-arctic regions sig (<0.005)

colors1 <- colors[1:5]
colors2 <- colors[6:8]
#group1
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/jja_elev1.eps')
plot(jja~elev, data=ibut_dat, type='n', xlim=c(3350,4100), ylim=c(6,15),
     xlab='Elevation (m)', ylab='Average JJA Temp (ºC)') #set up blank plot
for(i in 1:length(group1)) {
        foo <- ibut_dat[which(ibut_dat$site==group1[i]),]
        for(j in 1:length(foo$id)){
                points(jja[j]~elev[j], data=foo, col=colors1[i],
                       pch=16, cex=0.85)
            fit <- lm(jja~elev, data=foo) #individual site lm
            abline(fit, col=colors[i])
        }
}
legend('topright', pch=c(16,16,16,16,16), col=colors1, 
       legend=c("whe (p=0.007)","bal (p=0.01)","niw","tao","lat"))
dev.off()
#group2
setEPS()
postscript('~/Desktop/Research/silene/disturbance_paper/figs/jultemp_elev2.eps')
plot(julavg~elev, data=ibut_dat, type='n', xlim=c(0,1200),
     xlab='Elevation (m)', ylab='Average July Temp (ºC)')
for(i in 1:length(group2)) {
        foo <- ibut_dat[which(ibut_dat$site==group2[i]),]
        for(j in 1:length(foo$id)){
                points(julavg[j]~elev[j], data=foo, col=colors2[i],
                       pch=16, cex=0.85)
        }
}
legend('topleft', pch=c(16,16,16), col=colors2, legend=group2)
dev.off()