## N. Chardon
## Start: 18 Sept 2015
## Aim: explore iButton data (mission length Summer 2014 - Summer 2015)

# load datasets
load('~/Desktop/Research/silene/r_files/ibut.RData')
load('~/Desktop/Research/silene/r_files/ibut_table.RData')
load('~/Desktop/Research/silene/r_files/table.RData') 
#ibut_table with correct date format
load('~/Desktop/Research/silene/r_files/ibut_dat.RData') 
#ibutton id with lat, long, elev, site, and summer climate averages

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

# max temps per iButton
jun_max <- apply(ibut_gro[,1,], MARGIN=2, FUN=max, na.rm=T) 
jul_max <- apply(ibut_gro[,2,], MARGIN=2, FUN=max, na.rm=T)
aug_max <- apply(ibut_gro[,3,], MARGIN=2, FUN=max, na.rm=T)
ibut_dat$junmax <- jun_max
ibut_dat$julmax <- jul_max
ibut_dat$augmax <- aug_max

#save df
save(ibut_dat, file='~/Desktop/Research/silene/r_files/ibut_dat.RData')
#ibutton id matched with lat, long, elev, and site

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
## PLOTS
####################
plot(ibut_dat$lat, ibut_dat$augavg, 
     xlab = 'Latitude (°)', ylab = 'Mean August Temperature (°C)',
     pch = 16, cex = 0.8)

#plot all ibut data per site with for loop (need to match ID to site and elevs)