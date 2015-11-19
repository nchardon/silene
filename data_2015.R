## N. Chardon
## Start: 29 Sept 2015
## Stop: 
## Aim: Create usable datasets for cover and size for Summer 2015 data
## Generated Dataframes:
load('~/Desktop/Research/silene/r_files/size15.RData')
load('~/Desktop/Research/silene/r_files/cover15.RData')
load('~/Desktop/Research/silene/r_files/id_numb.RData') #total presences per transect
load('~/Desktop/Research/silene/r_files/lowtrail.RData')
load('~/Desktop/Research/silene/r_files/lownotrail.RData')
load('~/Desktop/Research/silene/r_files/by_elev.RData')
load('~/Desktop/Research/silene/r_files/hitrail.RData')
load('~/Desktop/Research/silene/r_files/hinotrail.RData')
load('~/Desktop/Research/silene/r_files/sums.RData')

#MOUNTAIN RANGE CODES (n=4)
#fr: Front Range
#sa: Sawatch Range
#sj: San Juans
#tm: Ten Mile/Mosquito Range

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
size15 <- read.csv('size_2015b.csv')
cover15 <- read.csv('cover_2015d.csv')

#create unique id per quad
size15$unique <- paste(size15$id, size15$trans, size15$quad, sep='_')
cover15$unique <- paste(cover15$id, cover15$trans, cover15$quad, sep='_')

#match cover to size
cover152size15 <- match(size15$unique, cover15$unique)
size15$trail <- cover15$trail[cover152size15] #add trail data
size15$low_elev <- cover15$low_elev[cover152size15] #add low_elev data
size15$low_dist <- cover15$low[cover152size15]
size15$med_dist <- cover15$med[cover152size15]
size15$hi_dist <- cover15$hi[cover152size15]
#size15$low_dist[which(size15$low_dist>100)] <- NA #turn >100% cover to NA

#delete NA columns and NA rows
cover15 <- cover15[-(21:26)]
cover15 <- cover15[-(1532:2031),]

#calculate cushion area in cm^2 
#A = ((pi * maj/2 * min/2)*(1-(miss/100)))*0.01
size15$maj <- as.numeric(size15$maj)
size15$min <- as.numeric(size15$min)
size15$miss <- as.numeric(size15$miss)
size15$area <- ((3.14*size15$maj/2*size15$min/2)*(1-(size15$miss/100)))*0.01

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
id_names <- c('bel', 'bie', 'dem', 'den', 'eln', 'els', 'eva', 'evw', 'gra', 
              'qua', 'red', 'she', 'squ', 'sun', 'yal', 
              'niw', 'ele', 'han', 'kin', 'mam')
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
rownames(id_numb) <- c('bel', 'bie', 'dem', 'den', 'eln', 'els', 'eva', 'evw', 'gra', 
                       'qua', 'red', 'she', 'squ', 'sun', 'yal', 
                       'niw', 'ele', 'han', 'kin', 'mam')
colnames(id_numb) <- c('1','2','3','4','5','6','7','8','9','10','11','12')

#density measures per dist/undist and hi/lo for complete sites (long system time)
id_names <- c('bel', 'bie', 'dem', 'den', 'eln', 'els', 'eva', 'evw', 'gra', 
              'qua', 'red', 'she', 'squ', 'sun', 'yal', 
              'niw', 'ele', 'han', 'kin', 'mam')
sums <- data.frame(id_names)
for(i in 1:20) {
        sums$total[i] <- sum(id_numb[i,], na.rm=T)
}
colnames(sums) <- c('site', 'total')

for(i in 1:length(id_names[1:15])) { #this loop only works for first 15 sites
        foo <- size15[which(size15$id==id_names[i]),]
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==1) #low elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        #total density
                        sums$lt_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000) 
                        #relative density (area out of quads with plants)
                        bar <- unique(subbfoo$trans) #id all trans
                        foosum <- data.frame(bar) #create df for trans totals
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,] #take out NAs
                        for (l in 1:length(bar)) { #loop through each trans
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) #trans area
                                foosum$quads[l] <- length(unique(baz$quad)) #trans quads
                        }
                        sums$lt_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$lt_total[i] <- nrow(nafoo) #total plants
                }
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        sums$lnt_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000)
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar) 
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,]
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$lnt_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$lnt_total[i] <- nrow(nafoo)
                }
        }
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==0) #high elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        sums$ht_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000)
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar) 
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,]
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$ht_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$ht_total[i] <- nrow(nafoo)
                }
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        sums$hnt_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000)
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar) 
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,]
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$hnt_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$hnt_total[i] <- nrow(nafoo)
                }
        }
}

#calculate high-only sites with modified loop from above: ELE, HAN
for(i in 17:length(id_names[17:18])) { #loop through only ELE and HAN
        foo <- size15[which(size15$id==id_names[i]),]
        sums$lt_dens[i] <- NA #fill absent areas with NA
        sums$lt_reldens[i] <- NA
        sums$lt_total[i] <- NA
        sums$lnt_dens[i] <- NA
        sums$lnt_reldens[i] <- NA
        sums$lnt_total[i] <- NA
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==0) #high elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        sums$ht_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000)
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar) 
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,]
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$ht_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$ht_total[i] <- nrow(nafoo)
                }
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        sums$hnt_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000)
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar) 
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,]
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$hnt_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$hnt_total[i] <- nrow(nafoo)
                }
        }
}

#calcuate hi undist with modified loop: KIN
for(i in 1:length(id_names[19])) { #loop through only KIN
        foo <- size15[which(size15$id==id_names[i]),]
        sums$lt_dens[i] <- NA #fill absent areas with NA
        sums$lt_reldens[i] <- NA
        sums$lt_total[i] <- NA
        sums$lnt_dens[i] <- NA
        sums$lnt_reldens[i] <- NA
        sums$lnt_total[i] <- NA
        sums$ht_dens[i] <- NA 
        sums$ht_reldens[i] <- NA
        sums$ht_total[i] <- NA
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==0) #high elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==0) #notrail
                        sums$hnt_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000)
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar) 
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,]
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$hnt_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$hnt_total[i] <- nrow(nafoo)
                }
        }
}

#calculate hi and low dist with modified loop: MAM
for(i in 1:length(id_names[20])) { #loop through only MAM
        foo <- size15[which(size15$id==id_names[i]),]
        sums$lnt_dens[i] <- NA
        sums$lnt_reldens[i] <- NA
        sums$lnt_total[i] <- NA
        sums$hnt_dens[i] <- NA 
        sums$hnt_reldens[i] <- NA
        sums$hnt_total[i] <- NA
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==1) #low elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        sums$lt_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000) 
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar)
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,] 
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$lt_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$lt_total[i] <- nrow(nafoo) #total plants
                }
        }
        for(j in 1:nrow(foo)) {
                subfoo <- subset(foo, low_elev==0) #high elev
                for(k in 1:nrow(subfoo)) {
                        subbfoo <- subset(subfoo, trail==1) #trail
                        sums$ht_dens[i] <- sum(subbfoo$area, na.rm=T)/
                                (max(subbfoo$trans, na.rm=T)*100000)
                        bar <- unique(subbfoo$trans) 
                        foosum <- data.frame(bar) 
                        nafoo <- subbfoo[is.na(subbfoo$area)==F,]
                        for (l in 1:length(bar)) { 
                                baz <- subbfoo[which(subbfoo$trans==bar[l]),]
                                foosum$area[l] <- sum(baz$area, na.rm=T) 
                                foosum$quads[l] <- length(unique(baz$quad)) 
                        }
                        sums$ht_reldens[i] <- sum(foosum$area)/(sum(foosum$quads)*10000) 
                        sums$ht_total[i] <- nrow(nafoo)
                }
        }
}

#calculate 20 2m^2 quads per 'transect' site: NIW
## WHY NA FOR TRANS 1? ##
foo <- size15[which(size15$id=='niw'),]
sums[16,]$lnt_dens <- sum(foo$area, na.rm=T)/(2*20*40000)
bar <- unique(foo$trans)
for (i in 1:length(bar)) {
        foo <- foo[which(foo$trans==bar[i]),] #subset by transect
        foo <- foo[is.na(foo$area)==F,] #take out NA areas (and thus quads)
        foosum <- data.frame(unique(foo$trans)) #create df
        foosum$area[i] <- sum(baz$area) #sum area per trans
        foosum$quads[i] <- length(unique(baz$quad)) #numb. of quad per trans
}
sums[16,]$lnt_reldens <- sum(foosum$area)/(sum(foosum$quads)*40000) 
sums[16,]$lnt_total <- sums[16,2] #all plants measured were lnt
#add all area and all quads and divide for rel dens

save(sums, file='sums.RData')

## QUAD SUMMARIES ## ----------------------------------------------------------
#calculations in size15 to cover15
foo <- unique(size15$unique)
for (i in 1:length(foo)) {
        subfoo <- size15[which(size15$unique==foo[i]),]
        cover15$meas_sil[i] <- nrow(subfoo)
        #quantity of measured sil per quad
        cover15$sum_meas_sil[i] <- sum(subfoo$area)
        #area sum of measured sil per quad
        if (is.na(cover15$sum_meas_sil[i])) {
                cover15$meas_sil[i] <- 0
                ## FIX CODE, still returning NA ##
        }
        #if NA area, no sil measured so label as '0'
}

## FIX CODE, if statements not working ##

#calculations in cover15
for(i in 1:length(cover15$unique)) {
        cover15$avgsize[i] <- (cover15$sil[i]*100)/cover15$quant_sil[i]
        #avg size of all sil per quad
        foo <- cover15[is.na(cover15$quant_sil)==F,] #remove NA for if loop
        if (foo$quant_sil[i] > 5) { #for quads with non-measured plants
                cover15$avgsmsize[i] <- 
                        (cover15$sil[i]*100-cover15$sum_meas_sil[i])/
                        (cover15$quant_sil[i]-cover15$meas_sil[i])
                #avg size of non-measured sil per quad
        }
        else {
                cover15$avgsmsize[i] <- NA
                }
}

#calculate avgsize and avgsmsize for NIW
for (i in 1:length(cover15$unique)) {
        foo <- size15[which(size15$id=='niw'),]
        cover15$avgsize[i] <- (cover15$sil[i]*400)/cover15$quant_sil[i]
        foo <- cover15[is.na(cover15$quant_sil)==F,]
        if (foo$quant_sil[i] > 5) {
                cover15$avgsmsize[i] <- 
                        (cover15$sil[i]*400-cover15$sum_meas_sil[i])/
                        (cover15$quant_sil[i]-cover15$meas_sil[i])
        }
        else {
                cover15$avgsmsize[i] <- NA
        }
}

## LOW/HI ELEV AND DIST/UNDIST DATAFRAMES ## -----------------------------------
#make low_elev df 
by_elev <- split(size15, size15$low_elev) #[2] is low elevation
low_elev_df <- data.frame(by_elev[2])
colnames(low_elev_df) <- c('id', 'trans', 'quad', 'maj', 'min', 'miss',
                           'unique', 'trail', 'low_elev', 'area', 'pa')
low_elev_df$pa <- as.numeric(low_elev_df$pa)
hi_elev_df <- data.frame(by_elev[1])
colnames(hi_elev_df) <- c('id', 'trans', 'quad', 'maj', 'min', 'miss',
                           'unique', 'trail', 'low_elev', 'area', 'pa')

#trail vs. no trail df's 
lowtrail <- (split(low_elev_df, low_elev_df$trail))
lowtrail_df <- data.frame(lowtrail[2])
colnames(lowtrail_df) <- c('id', 'trans', 'quad', 'maj', 'min', 'miss',
                           'unique', 'trail', 'low_elev', 'area', 'pa')
lownotrail_df <- data.frame(lowtrail[1])
colnames(lownotrail_df) <- c('id', 'trans', 'quad', 'maj', 'min', 'miss',
                             'unique', 'trail', 'low_elev', 'area', 'pa')
hitrail <- (split(hi_elev_df, hi_elev_df$trail))
hitrail_df <- data.frame(hitrail[2])
colnames(hitrail_df) <- c('id', 'trans', 'quad', 'maj', 'min', 'miss',
                           'unique', 'trail', 'low_elev', 'area', 'pa')
hinotrail_df <- data.frame(hitrail[1])
colnames(hinotrail_df) <- c('id', 'trans', 'quad', 'maj', 'min', 'miss',
                             'unique', 'trail', 'low_elev', 'area', 'pa')

## SAVE DATAFRAMES ## ----------------------------------------------------------
save(lowtrail_df, file= '~/Desktop/Research/silene/r_files/lowtrail.RData')
save(lownotrail_df, file= '~/Desktop/Research/silene/r_files/lownotrail.RData')
save(hitrail_df, file= '~/Desktop/Research/silene/r_files/hitrail.RData')
save(hinotrail_df, file= '~/Desktop/Research/silene/r_files/hinotrail.RData')
save(by_elev, file= '~/Desktop/Research/silene/r_files/by_elev.RData')
save(size15, file='~/Desktop/Research/silene/r_files/size15.RData')
save(cover15, file='~/Desktop/Research/silene/r_files/cover15.RData')
save(id_numb, file='~/Desktop/Research/silene/r_files/id_numb.RData')
