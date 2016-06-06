## N. Chardon
## Start: 29 Sept 2015
## Stop: 25 Feb 2016
## Aim: Create usable datasets for cover and size for Summer 2015 data
## Generated Dataframes:
load('~/Desktop/Research/silene/r_files/size15.RData') #new df available (_lmm2; _clim)
load('~/Desktop/Research/silene/r_files/cover15.RData') #new df available (_lmm2; _clim)
load('~/Desktop/Research/silene/r_files/id_numb.RData') #total presences per transect
load('~/Desktop/Research/silene/r_files/lowtrail.RData')
load('~/Desktop/Research/silene/r_files/lownotrail.RData')
load('~/Desktop/Research/silene/r_files/by_elev.RData')
load('~/Desktop/Research/silene/r_files/hitrail.RData')
load('~/Desktop/Research/silene/r_files/hinotrail.RData')
#load('~/Desktop/Research/silene/r_files/sums.RData') #need to re-run (25Feb16)

## Updated DFs:
load('~/Desktop/Research/silene/r_files/size15_lmm2.RData') 
load('~/Desktop/Research/silene/r_files/cover15_lmm2.RData')

#MOUNTAIN RANGE CODES (n=4)
#fr: Front Range (9)
#sa: Sawatch Range (5)
#sj: San Juans (3)
#tm: Ten Mile/Mosquito Range (4)

#SITE CODES (n=20)
#bel: Mt Belford (sa) 
#bie: Mt. Bierstadt (fr)
#dem: Mt. Democrat (tm)
#den: Mt. Democrat North (tm)
#ele: Mt. Elbert East (sa)
#eln: Mt. Elbert North (sa)
#els: Mt. Elbert South (sa)
#eva: Mt. Evans (fr)
#evw: Mt. Evans West (fr)
#gra: Grays Peak (fr)
#grs: Grays Peak South (fr)
#han: Handies Peak (sj)
#kin: Kingston Peak (fr)
#mam: Mammoth Gulch (fr)
#niw: Niwot Ridge (fr)
#qua: Quandary Peak (tm)
#red: Redcloud Mountain (sj)
#she: Mt. Sherman (tm)
#squ: Squaretop Mountain (fr)
#sun: Sunshine Peak (sj)
#yal: Mt. Yale (sa)

rm(list=ls())

#set wd
setwd("~/Desktop/Research/silene/data_2015")

## ORGANIZE DATA ## -----------------------------------------------------------
#read in data
size15 <- read.csv('size_2015b.csv', stringsAsFactors = F)
cover15 <- read.csv('cover_2015d.csv', stringsAsFactors = F)
gps15 <- read.csv('gps_2015b.csv', stringsAsFactors = F)

#create unique id per quad
size15$unique <- paste(size15$id, size15$trans, size15$quad, sep='_')
size15$unique2 <- paste(size15$id, size15$trans, sep='_')
cover15$unique <- paste(cover15$id, cover15$trans, cover15$quad, sep='_')
cover15$unique2 <- paste(cover15$id, cover15$trans, sep='_')
gps15$unique2 <- paste(gps15$id, gps15$trans, sep='_')

#check that dataframes match
cover15[!cover15$unique%in%size15$unique,]
gps15[!gps15$unique2%in%cover15$unique2,] #ele_4, squ_7, squ_8 have no data in cover15 due to bad weather

#match gps to cover (match data to all quads in transect with $unique2)
gps152cover15 <- match(cover15$unique2, gps15$unique2) 
cover15$lat <- gps15$lat[gps152cover15]
cover15$long <- gps15$long[gps152cover15]
cover15$elev <- gps15$elev[gps152cover15]
cover15$ibutt <- gps15$ibutton[gps152cover15]
cover15$slope <- gps15$slope[gps152cover15]
cover15$asp <- gps15$asp[gps152cover15]

#match cover to size
cover152size15 <- match(size15$unique, cover15$unique)
size15$trail <- cover15$trail[cover152size15] #add trail data
size15$low_elev <- cover15$low_elev[cover152size15] #add low_elev data
size15$low_dist <- cover15$low[cover152size15]
size15$med_dist <- cover15$med[cover152size15]
size15$hi_dist <- cover15$hi[cover152size15]
#size15$low_dist[which(size15$low_dist>100)] <- NA #turn >100% cover to NA
cover152size15 <- match(size15$unique2, cover15$unique2) #match transect-level data
size15$lat <- cover15$lat[cover152size15]
size15$long <- cover15$long[cover152size15]
size15$elev <- cover15$elev[cover152size15]
size15$ibutt <- cover15$ibutt[cover152size15]
size15$slope <- cover15$slope[cover152size15]
size15$asp <- cover15$asp[cover152size15]

#calculate cushion area in cm^2 
#A = ((pi * maj/2 * min/2)*(1-(miss/100)))*0.01
size15$maj <- as.numeric(size15$maj)
size15$min <- as.numeric(size15$min)
size15$miss <- as.numeric(size15$miss)
size15$area <- ((3.14*(size15$maj/2)*(size15$min/2))*
                        (1-(size15$miss/100)))*0.01

#make column with presence/absence data
for (i in 1:dim(size15)[1]) {
        if (!is.na(size15$area[i])) {
                size15$pa[i] <- '1'
        }
        else if (is.na(size15$area[i])) {
                size15$pa[i] <- '0'
        }
}
size15$pa <- as.numeric(size15$pa)

## SITE SUMMARIES ## -----------------------------------------------------------
#lnt=low no trail, lt = low trail, hnt=high no trail, ht=high trail
#sum of p/a per transect per site
id_names <- unique(cover15$id)
id_numb <- matrix(NA, length(id_names), 12)
for(j in 1:length(id_names)) {
        sub <- size15[which(size15$id==id_names[j]),]
        for(i in 1:max(sub$trans)) {
                subset <- sub[which(sub$trans==i),]
                temp <- sum(subset$pa)
                id_numb[j,i] <- temp
        }
}
id_numb <- as.data.frame(id_numb)
rownames(id_numb) <- unique(cover15$id)
colnames(id_numb) <- c('1','2','3','4','5','6','7','8','9','10','11','12')
save(id_numb, file='~/Desktop/Research/silene/r_files/id_numb.RData')

#density measures per dist/undist and hi/lo for complete sites (syst=40min)
id_names <- unique(cover15$id)
sums <- data.frame(id_names)
for(i in 1:20) {
        sums$total[i] <- sum(id_numb[i,], na.rm=T)
}
colnames(sums) <- c('site', 'total')

for(i in 1:length(id_names)) {
        foo <- size15[which(size15$id==id_names[i]),]
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==1) #low elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        areatot <- sum(subbfoo$area, na.rm=T)
                        transtot <- length(unique(subbfoo$trans))*100000
                        #total density
                        sums$lt_dens[i] <- areatot/transtot
                        #relative density (area out of quads with plants)
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,] #take out NAs
                        quadstot <- length(unique(subbfoo$unique))*10000
                        sums$lt_reldens[i] <- areatot/quadstot
                        #total plants
                        sums$lt_total[i] <- nrow(nafoo) 
                }
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        areatot <- sum(subbfoo$area, na.rm=T)
                        transtot <- length(unique(subbfoo$trans))*100000
                        sums$lnt_dens[i] <- areatot/transtot
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,] 
                        quadstot <- length(unique(subbfoo$unique))*10000
                        sums$lnt_reldens[i] <- areatot/quadstot
                        sums$lnt_total[i] <- nrow(nafoo) 
                }
        }
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==0) #high elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        areatot <- sum(subbfoo$area, na.rm=T)
                        transtot <- length(unique(subbfoo$trans))*100000
                        sums$ht_dens[i] <- areatot/transtot
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,] 
                        quadstot <- length(unique(subbfoo$unique))*10000
                        sums$ht_reldens[i] <- areatot/quadstot
                        sums$ht_total[i] <- nrow(nafoo) 
                }
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        areatot <- sum(subbfoo$area, na.rm=T)
                        transtot <- length(unique(subbfoo$trans))*100000
                        sums$hnt_dens[i] <- areatot/transtot
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,] 
                        quadstot <- length(unique(subbfoo$unique))*10000
                        sums$hnt_reldens[i] <- areatot/quadstot
                        sums$hnt_total[i] <- nrow(nafoo) 
                }
        }
}

#calculate 20 4m^2 quads per 'transect' site: NIW
foo <- size15[which(size15$id=='niw'),]
areatot <- sum(foo$area, na.rm=T)
quadstot <- length(unique(foo$unique))*40000
sums$hnt_dens[16] <- areatot/quadstot
nafoo <- foo[is.na(foo$area)==F,] 
quadstot <- length(unique(foo$unique))*40000
sums$hnt_reldens[16] <- areatot/quadstot
sums$hnt_total[16] <- nrow(nafoo)

save(sums, file='~/Desktop/Research/silene/r_files/sums.RData')

## QUAD SUMMARIES ## ----------------------------------------------------------
#calculations in size15 to cover15: meas_sil and sum_meas_sil
quads <- unique(cover15$unique) #length(cover15$unique) = length(size15$unique)
for (i in 1:nrow(cover15)) {
        foo <- size15[which(size15$unique==quads[i]),]
        cover15$meas_sil[i] <- nrow(foo)
        #quantity of measured sil per quad
        cover15$sum_meas_sil[i] <- sum(foo$area)
        #area sum of measured sil per quad
}

#calculations in cover15
for(i in 1:nrow(cover15)) {
    #avg size of all sil per quad    
    cover15$avgsize[i] <- (cover15$sil[i]*100)/cover15$quant_sil[i]
    #avg size of non-measured sil per quad  
    if (cover15$quant_sil[i] > 5) { #for quads with non-measured plants
                cover15$avgsmsize[i] <- 
                        (cover15$sil[i]*100-cover15$sum_meas_sil[i])/
                        (cover15$quant_sil[i]-cover15$meas_sil[i])
                
        }
        else {
                cover15$avgsmsize[i] <- NA #NA if no non-measured sil in quad
        }
}

#calculate avgsize and avgsmsize for NIW
for (i in 1:length(cover15$unique)) {
    if (cover15$id[i]=='niw') {
            cover15$avgsize[i] <- (cover15$sil[i]*400)/cover15$quant_sil[i]
            if (cover15$quant_sil[i] > 5) {
                cover15$avgsmsize[i] <- 
                    (cover15$sil[i]*400-cover15$sum_meas_sil[i])/
                    (cover15$quant_sil[i]-cover15$meas_sil[i])
            }
            else {
                cover15$avgsmsize[i] <- NA
            }
    }
}

#make (-) and INF values NA (=100 sampling errors)
cover15[which(cover15$avgsmsize<0),]$avgsmsize <- NA
cover15[which(cover15$avgsmsize==Inf),]$avgsmsize <- NA

## LOW/HI ELEV AND DIST/UNDIST DATAFRAMES ## -----------------------------------
#make low_elev df 
by_elev <- split(size15, size15$low_elev) #$`1` is low elevation
low_elev_df <- data.frame(by_elev$`1`)
hi_elev_df <- data.frame(by_elev$`0`)

#trail vs. no trail df's 
lowtrail <- (split(low_elev_df, low_elev_df$trail))
lowtrail_df <- data.frame(lowtrail$`1`)
lownotrail_df <- data.frame(lowtrail$`0`)

hitrail <- split(hi_elev_df, hi_elev_df$trail)
hitrail_df <- data.frame(hitrail$`1`)
hinotrail_df <- data.frame(hitrail$`0`)

## SAVE DATAFRAMES ## ----------------------------------------------------------
save(lowtrail_df, file= '~/Desktop/Research/silene/r_files/lowtrail.RData')
save(lownotrail_df, file= '~/Desktop/Research/silene/r_files/lownotrail.RData')
save(hitrail_df, file= '~/Desktop/Research/silene/r_files/hitrail.RData')
save(hinotrail_df, file= '~/Desktop/Research/silene/r_files/hinotrail.RData')
save(by_elev, file= '~/Desktop/Research/silene/r_files/by_elev.RData')
save(size15, file='~/Desktop/Research/silene/r_files/size15.RData')
save(cover15, file='~/Desktop/Research/silene/r_files/cover15.RData')
