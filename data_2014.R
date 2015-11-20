## N. Chardon
## Start: 18 Nov 2014
## Content: Silene size structure data, KS test, SSD
## Goals: 
## 1) plot size mean and variance by site [DONE],
## 2) and by variables within site (elevation & lat) [DONE];
## 3) examine P/A patterns at margin vs. center areas
## Generated Datasets:
load('~/Desktop/Research/silene/r_files/size.RData')
load('~/Desktop/Research/silene/r_files/cover.RData')
load('~/Desktop/Research/silene/r_files/sites.RData')
load('~/Desktop/Research/silene/r_files/sums14.RData')

## DEFINITIONS
## site = unique geographic site (n = 14), denoted by 3 letter code
## point = unique GPS point, made up of 4 quad placements
## quad = 1m^2 quadrat with % cover data and plant sizes data

## SITE CODES
## wra = Wrangell Mts, AK
## too = Toolik Lake, AK
## pin = Pingo1, AK
## tho = Thompson Pass, AK
## sub = "Sub" (i.e., East) Niwot Ridge, CO
## niw = Niwot Ridge, CO
## bal = Bald Mt, CO
## lat = Latir Pk, NM
## tao = Taos Ski Valley, NM
## whe = Wheeler Pk, NM
## val = Val Bercla, Switzerland
## duc = Ducanfurgga, Switzerland
## ais = Aisa, Spain
## urd = Urdiceto, Spain

# clear memory & set directory
rm(list=ls())
setwd("~/Desktop/Research/silene/data_2014")

# read in data
size <- read.csv('size_data8.csv')
cover <- read.csv('cover_data4.csv')
pa <- read.csv('pa_data2.csv')
gps <- read.csv('allgps.csv')

# remove NAs from dataframe
#size = size[is.na(size$area) == FALSE,]

# check
head(size)
colnames(size)
head(cover)
colnames(cover)
head(pa)
colnames(pa)
head(gps)
colnames(gps)

# add lat, long, & elevation to size dataframe
cover2size <- match(size$id, cover$id)
size$lat <- cover$lat[cover2size]
size$long <- cover$long[cover2size]

gps2size <- match(size$lat, gps$lat)
size$elev <- gps$ele[gps2size]

# add elev to cover df
gps2cover <- match(cover$lat, gps$lat)
cover$elev <- gps$ele[gps2cover]

# make ibut id uppercase for matching in ibut.R
cover$ibutt <- toupper(cover$ibutt)

#calculate cushion area in cm^2 
size$maj <- as.numeric(size$maj)
size$min <- as.numeric(size$min)
size$miss <- as.numeric(size$miss)
size$areacm <- ((3.14*size$maj/2*size$min/2)*(1-(size$miss/100)))

# save cover dataframes
save(cover, file='~/Desktop/Research/silene/r_files/cover.RData')

# adding region and regional lat to each site
# no.ak: Northern Alaska
# so.ak: Southern Alaska
# co: Colorado
# nm: New Mexico
# ch: Switzerland
# es: Spain

# regional lat means
site.lat <- tapply(size$lat, size$site, mean, na.rm = TRUE)
so.ak.lat = mean(site.lat[14], site.lat[9])
no.ak.lat = mean(site.lat[6], site.lat[10])
co.lat = mean(site.lat[7], site.lat[5], site.lat[2])
nm.lat = mean(site.lat[4], site.lat[8], site.lat[13])
ch.lat = mean(site.lat[12], site.lat[3])
es.lat = mean(site.lat[1], site.lat[11])

# add region to 'size' dataframe with individual for loops
size$reg <- rep(NA, nrow(size))
size$reg.lat <- rep(NA, nrow(size))

for(i in 1:nrow(size)) {
        if(size[i,2] == "ais"){
                size$reg[i] = "es" 
                size$reg.lat[i] = es.lat
        } else if(size[i,2] == "bal"){
                size$reg[i] = "co" 
                size$reg.lat[i] = co.lat
        } else if(size[i,2] == "duc"){
                size$reg[i] = "ch" 
                size$reg.lat[i] = ch.lat
        } else if(size[i,2] == "lat"){
                size$reg[i] = "nm" 
                size$reg.lat[i] = nm.lat 
        } else if(size[i,2] == "niw"){
                size$reg[i] = "co" 
                size$reg.lat[i] = co.lat 
        } else if(size[i,2] == "pin"){
                size$reg[i] = "no.ak" 
                size$reg.lat[i] = no.ak.lat
        } else if(size[i,2] == "sub"){
                size$reg[i] = "co" 
                size$reg.lat[i] = co.lat
        } else if(size[i,2] == "tao"){
                size$reg[i] = "nm" 
                size$reg.lat[i] = nm.lat
        } else if(size[i,2] == "tho"){
                size$reg[i] = "so.ak" 
                size$reg.lat[i] = so.ak.lat
        } else if(size[i,2] == "too"){
                size$reg[i] = "no.ak" 
                size$reg.lat[i] = no.ak.lat
        } else if(size[i,2] == "urd"){
                size$reg[i] = "es" 
                size$reg.lat[i] = es.lat
        } else if(size[i,2] == "val"){
                size$reg[i] = "ch" 
                size$reg.lat[i] = ch.lat
        } else if(size[i,2] == "whe"){
                size$reg[i] = "nm" 
                size$reg.lat[i] = nm.lat
        } else {
                size$reg[i] = "so.ak" 
                size$reg.lat[i] = so.ak.lat
        }       
}
# save 'size' dataframe
save(size, file = '~/Desktop/Research/silene/r_files/size.RData')

#####################
# SITE LEVEL ANALYSES
#####################

# site GPS & elev means
site.lat <- tapply(size$lat, size$site, mean, na.rm = TRUE)
site.long <- tapply(size$long, size$site, mean, na.rm = TRUE)
site.elev <- tapply(size$elev, size$site, mean, na.rm = TRUE)

# site: means, var, sd
s.size.means <- tapply(size$area, size$site, mean, na.rm = TRUE)
s.size.var <- tapply(size$area, size$site, var, na.rm = TRUE)
s.size.sd <- tapply(size$area, size$site, sd, na.rm = TRUE)
size.means.na <- tapply(size$area, size$site, mean, na.rm = TRUE)

# North American means
site_lat_NA  <- c(site.lat[2], site.lat[4], site.lat[5], site.lat[6],
                  site.lat[7], site.lat[8], site.lat[9], site.lat[10],
                  site.lat[13], site.lat[14])

# splitting dataframe into individual sites
sites <- split(size, size$site)

# save 'sites' dataframe
save(sites, file = '~/Desktop/Research/silene/r_files/sites.RData')
# call site parameters by sites$CODE[,COLUMN]
# COLUMNS: 1) ID, 2) SITE, 3) POINT, 4) QUAD, 5) ROS, 6) MAJ, 7) MIN, 8) MISS, 
# 9) YEAR, 10) AREA, 11) LAT, 12) LONG, 13) ELEV 14) Region

# split into 'reg' dataframe & save
reg  <- split(size, size$reg)
save(reg, file = '~/Desktop/Research/silene/r_files/reg.RData')

# % of 1ros and >1ros plants per site
sum <- sum(sites$wra[,5], na.rm = TRUE)
perc.wra <- sum/length(sites$wra[,5])
perc.wra
 
sum <- sum(sites$too[,5], na.rm = TRUE)
perc.too <- sum/length(sites$too[,5])
perc.too

sum <- sum(sites$pin[,5], na.rm = TRUE)
perc.pin <- sum/length(sites$pin[,5])
perc.pin

sum <- sum(sites$tho[,5], na.rm = TRUE)
perc.tho <- sum/length(sites$tho[,5])
perc.tho

sum <- sum(sites$sub[,5], na.rm = TRUE)
perc.sub <- sum/length(sites$sub[,5])
perc.sub

sum <- sum(sites$niw[,5], na.rm = TRUE)
perc.niw <- sum/length(sites$niw[,5])
perc.niw

sum <- sum(sites$bal[,5], na.rm = TRUE)
perc.bal <- sum/length(sites$bal[,5])
perc.bal

sum <- sum(sites$lat[,5], na.rm = TRUE)
perc.lat <- sum/length(sites$lat[,5])
perc.lat

sum <- sum(sites$tao[,5], na.rm = TRUE)
perc.tao <- sum/length(sites$tao[,5])
perc.tao

sum <- sum(sites$whe[,5], na.rm = TRUE)
perc.whe <- sum/length(sites$whe[,5])
perc.whe

sum <- sum(sites$val[,5], na.rm = TRUE)
perc.val <- sum/length(sites$val[,5])
perc.val

sum <- sum(sites$val[,5], na.rm = TRUE)
perc.val <- sum/length(sites$val[,5])
perc.val

sum <- sum(sites$duc[,5], na.rm = TRUE)
perc.duc <- sum/length(sites$duc[,5])
perc.duc

sum <- sum(sites$ais[,5], na.rm = TRUE)
perc.ais <- sum/length(sites$ais[,5])
perc.ais

sum <- sum(sites$urd[,5], na.rm = TRUE)
perc.urd <- sum/length(sites$urd[,5])
perc.urd

all.perc <- rbind(perc.wra, perc.too, perc.pin, perc.tho, perc.sub, perc.niw,
                  perc.bal, perc.lat, perc.tao, perc.whe, perc.val, perc.duc,
                  perc.ais, perc.urd)
all.perc <- as.data.frame(all.perc)
colnames(all.perc) <- c('ros')
all.perc$id_names <- c('wra','too','pin','tho','sub','niw','bal','lat','tao','whe','val',
                       'duc','ais','urd')

#ABUNDANCE
#number of plants per point
id_names <- c('wra','too','pin','tho','sub','niw','bal','lat','tao','whe','val',
              'duc','ais','urd')
id_numb14 <- matrix(NA, length(id_names), max(as.numeric(size$point)))
for(j in 1:length(id_names)) {
        sub <- size[which(size$site==id_names[j]),]
        for(i in 1:max(as.numeric(sub$point))) {
                subset <- sub[which(sub$point==i),]
                id_numb14[j,i] <- nrow(subset)
        }
}
id_numb14 <- as.data.frame(id_numb14)
rownames(id_numb14) <- id_names
colnames(id_numb14) <- seq(1,52,1)

#number of plants per site (PIN 0 because didn't estimate cover)
id_names <- c(names(sites))
sums14 <- as.data.frame(id_names)
size$point <- as.numeric(size$point) #make $point numeric for 'max' function
for(i in 1:length(sites)) {
        foo <- size[which(size$site==id_names[i]),]
        sums14$reldens[i] <- sum(foo$areacm, na.rm=T)/
                (max(foo$point, na.rm=T)*40000)
        #point area with plants per site = points*40000cm^2
        sums14$total[i] <- nrow(foo) #total plants
}

cover$site <- substr(cover$id, 1, 3) #make site recognizable
for(i in 1:length(sites)) {
        foo <- cover[which(cover$site==id_names[i]),]
        sums14$estdens[i] <- sum(foo$sil, na.rm=T)/(length(foo)*10000)
        #total cm^2 of silene area/total cm^2 area of all sampled quads
}

all.perc2sums14 <- match(sums14$id_names, all.perc$id_names)
sums14$ros <- all.perc$ros[all.perc2sums14] #% of rosettes at each site
save(sums14, file='~/Desktop/Research/silene/r_files/sums14.RData')

######################
# POINT LEVEL ANALYSES
######################

# point lat, long, elev means; size mean, var, sd; site quantiles
wra.lat <- tapply(sites$wra[,11], sites$wra[,3], mean, na.rm = TRUE)
wra.long <- tapply(sites$wra[,12], sites$wra[,3], mean, na.rm = TRUE)
wra.elev <- tapply(sites$wra[,13], sites$wra[,3], mean, na.rm = TRUE)
wra.size <- tapply(sites$wra[,10], sites$wra[,3], mean, na.rm = TRUE)
wra.var <- tapply(sites$wra[,10], sites$wra[,3], var, na.rm = TRUE)
wra.sd <- tapply(sites$wra[,10], sites$wra[,3], sd, na.rm = TRUE)
wra.quant <- quantile(sites$wra[,10], c(.1, .9), na.rm = TRUE)

too.lat <- tapply(sites$too[,11], sites$too[,3], mean, na.rm = TRUE)
too.long <- tapply(sites$too[,12], sites$too[,3], mean, na.rm = TRUE)
too.elev <- tapply(sites$too[,13], sites$too[,3], mean, na.rm = TRUE)
too.size <- tapply(sites$too[,10], sites$too[,3], mean, na.rm = TRUE)
too.var <- tapply(sites$too[,10], sites$too[,3], var, na.rm = TRUE)
too.sd <- tapply(sites$too[,10], sites$too[,3], sd, na.rm = TRUE)
too.quant <- quantile(sites$too[,10], c(.1, .9), na.rm = TRUE)

pin.lat <- tapply(sites$pin[,11], sites$pin[,3], mean, na.rm = TRUE)
pin.long <- tapply(sites$pin[,12], sites$pin[,3], mean, na.rm = TRUE)
pin.elev <- tapply(sites$pin[,13], sites$pin[,3], mean, na.rm = TRUE)
pin.size <- tapply(sites$pin[,10], sites$pin[,3], mean, na.rm = TRUE)
pin.var <- tapply(sites$pin[,10], sites$pin[,3], var, na.rm = TRUE)
pin.sd <- tapply(sites$pin[,10], sites$pin[,3], sd, na.rm = TRUE)
pin.quant <- quantile(sites$pin[,10], c(.1, .9), na.rm = TRUE)

tho.lat <- tapply(sites$tho[,11], sites$tho[,3], mean, na.rm = TRUE)
tho.long <- tapply(sites$tho[,12], sites$tho[,3], mean, na.rm = TRUE)
tho.elev <- tapply(sites$tho[,13], sites$tho[,3], mean, na.rm = TRUE)
tho.size <- tapply(sites$tho[,10], sites$tho[,3], mean, na.rm = TRUE)
tho.var <- tapply(sites$tho[,10], sites$tho[,3], var, na.rm = TRUE)
tho.sd <- tapply(sites$tho[,10], sites$tho[,3], sd, na.rm = TRUE)
tho.quant <- quantile(sites$tho[,10], c(.1, .9), na.rm = TRUE)

sub.lat <- tapply(sites$sub[,11], sites$sub[,3], mean, na.rm = TRUE)
sub.long <- tapply(sites$sub[,12], sites$sub[,3], mean, na.rm = TRUE)
# no elev for SUB, but need filler:
sub.elev <- tapply(sites$sub[,13], sites$sub[,3], mean, na.rm = TRUE)
sub.size <- tapply(sites$sub[,10], sites$sub[,3], mean, na.rm = TRUE)
sub.var <- tapply(sites$sub[,10], sites$sub[,3], var, na.rm = TRUE)
sub.sd <- tapply(sites$sub[,10], sites$sub[,3], sd, na.rm = TRUE)
sub.quant <- quantile(sites$sub[,10], c(.1, .9), na.rm = TRUE)

niw.lat <- tapply(sites$niw[,11], sites$niw[,3], mean, na.rm = TRUE)
niw.long <- tapply(sites$niw[,12], sites$niw[,3], mean, na.rm = TRUE)
niw.elev <- tapply(sites$niw[,13], sites$niw[,3], mean, na.rm = TRUE)
niw.size <- tapply(sites$niw[,10], sites$niw[,3], mean, na.rm = TRUE)
niw.var <- tapply(sites$niw[,10], sites$niw[,3], var, na.rm = TRUE)
niw.sd <- tapply(sites$niw[,10], sites$niw[,3], sd, na.rm = TRUE)
niw.quant <- quantile(sites$niw[,10], c(.1, .9), na.rm = TRUE)

bal.lat <- tapply(sites$bal[,11], sites$bal[,3], mean, na.rm = TRUE)
bal.long <- tapply(sites$bal[,12], sites$bal[,3], mean, na.rm = TRUE)
bal.elev <- tapply(sites$bal[,13], sites$bal[,3], mean, na.rm = TRUE)
bal.size <- tapply(sites$bal[,10], sites$bal[,3], mean, na.rm = TRUE)
bal.var <- tapply(sites$bal[,10], sites$bal[,3], var, na.rm = TRUE)
bal.sd <- tapply(sites$bal[,10], sites$bal[,3], sd, na.rm = TRUE)
bal.quant <- quantile(sites$bal[,10], c(.1, .9), na.rm = TRUE)

lat.lat <- tapply(sites$lat[,11], sites$lat[,3], mean, na.rm = TRUE)
lat.long <- tapply(sites$lat[,12], sites$lat[,3], mean, na.rm = TRUE)
lat.elev <- tapply(sites$lat[,13], sites$lat[,3], mean, na.rm = TRUE)
lat.size <- tapply(sites$lat[,10], sites$lat[,3], mean, na.rm = TRUE)
lat.var <- tapply(sites$lat[,10], sites$lat[,3], var, na.rm = TRUE)
lat.sd <- tapply(sites$lat[,10], sites$lat[,3], sd, na.rm = TRUE)
lat.quant <- quantile(sites$lat[,10], c(.1, .9), na.rm = TRUE)

tao.lat <- tapply(sites$tao[,11], sites$tao[,3], mean, na.rm = TRUE)
tao.long <- tapply(sites$tao[,12], sites$tao[,3], mean, na.rm = TRUE)
tao.elev <- tapply(sites$tao[,13], sites$tao[,3], mean, na.rm = TRUE)
tao.size <- tapply(sites$tao[,10], sites$tao[,3], mean, na.rm = TRUE)
tao.var <- tapply(sites$tao[,10], sites$tao[,3], var, na.rm = TRUE)
tao.sd <- tapply(sites$tao[,10], sites$tao[,3], sd, na.rm = TRUE)
tao.quant <- quantile(sites$tao[,10], c(.1, .9), na.rm = TRUE)

whe.lat <- tapply(sites$whe[,11], sites$whe[,3], mean, na.rm = TRUE)
whe.long <- tapply(sites$whe[,12], sites$whe[,3], mean, na.rm = TRUE)
whe.elev <- tapply(sites$whe[,13], sites$whe[,3], mean, na.rm = TRUE)
whe.size <- tapply(sites$whe[,10], sites$whe[,3], mean, na.rm = TRUE)
whe.var <- tapply(sites$whe[,10], sites$whe[,3], var, na.rm = TRUE)
whe.sd <- tapply(sites$whe[,10], sites$whe[,3], sd, na.rm = TRUE)
whe.quant <- quantile(sites$whe[,10], c(.1, .9), na.rm = TRUE)

val.lat <- tapply(sites$val[,11], sites$val[,3], mean, na.rm = TRUE)
val.long <- tapply(sites$val[,12], sites$val[,3], mean, na.rm = TRUE)
val.elev <- tapply(sites$val[,13], sites$val[,3], mean, na.rm = TRUE)
val.size <- tapply(sites$val[,10], sites$val[,3], mean, na.rm = TRUE)
val.var <- tapply(sites$val[,10], sites$val[,3], var, na.rm = TRUE)
val.sd <- tapply(sites$val[,10], sites$val[,3], sd, na.rm = TRUE)
val.quant <- quantile(sites$val[,10], c(.1, .9), na.rm = TRUE)

duc.lat <- tapply(sites$duc[,11], sites$duc[,3], mean, na.rm = TRUE)
duc.long <- tapply(sites$duc[,12], sites$duc[,3], mean, na.rm = TRUE)
duc.elev <- tapply(sites$duc[,13], sites$duc[,3], mean, na.rm = TRUE)
duc.size <- tapply(sites$duc[,10], sites$duc[,3], mean, na.rm = TRUE)
duc.var <- tapply(sites$duc[,10], sites$duc[,3], var, na.rm = TRUE)
duc.sd <- tapply(sites$duc[,10], sites$duc[,3], sd, na.rm = TRUE)
duc.quant <- quantile(sites$duc[,10], c(.1, .9), na.rm = TRUE)

ais.lat <- tapply(sites$ais[,11], sites$ais[,3], mean, na.rm = TRUE)
ais.long <- tapply(sites$ais[,12], sites$ais[,3], mean, na.rm = TRUE)
ais.elev <- tapply(sites$ais[,13], sites$ais[,3], mean, na.rm = TRUE)
ais.size <- tapply(sites$ais[,10], sites$ais[,3], mean, na.rm = TRUE)
ais.var <- tapply(sites$ais[,10], sites$ais[,3], var, na.rm = TRUE)
ais.sd <- tapply(sites$ais[,10], sites$ais[,3], sd, na.rm = TRUE)
ais.quant <- quantile(sites$ais[,10], c(.1, .9), na.rm = TRUE)

urd.lat <- tapply(sites$urd[,11], sites$urd[,3], mean, na.rm = TRUE)
urd.long <- tapply(sites$urd[,12], sites$urd[,3], mean, na.rm = TRUE)
urd.elev <- tapply(sites$urd[,13], sites$urd[,3], mean, na.rm = TRUE)
urd.size <- tapply(sites$urd[,10], sites$urd[,3], mean, na.rm = TRUE)
urd.var <- tapply(sites$urd[,10], sites$urd[,3], var, na.rm = TRUE)
urd.sd <- tapply(sites$urd[,10], sites$urd[,3], sd, na.rm = TRUE)
urd.quant <- quantile(sites$urd[,10], c(.1, .9), na.rm = TRUE)

# combining 10% & 90% Quantiles
quant.1 <- rbind(wra.quant[1], too.quant[1], pin.quant[1], tho.quant[1],
                 sub.quant[1], niw.quant[1], bal.quant[1], lat.quant[1],
                 tao.quant[1], whe.quant[1], val.quant[1], duc.quant[1],
                 ais.quant[1], urd.quant[1])

quant.9 <- rbind(wra.quant[2], too.quant[2], pin.quant[2], tho.quant[2],
                 sub.quant[2], niw.quant[2], bal.quant[2], lat.quant[2],
                 tao.quant[2], whe.quant[2], val.quant[2], duc.quant[2],
                 ais.quant[2], urd.quant[2])

# combining point means, var, and sd
# ALL SITES (n = 14)
all.p.lat <- c(wra.lat, too.lat, pin.lat, tho.lat, sub.lat, niw.lat, bal.lat,
               lat.lat, tao.lat, whe.lat, val.lat, duc.lat, ais.lat, urd.lat)
all.p.long <- c(wra.long, too.long, pin.long, tho.long, sub.long, niw.long, 
                bal.long, lat.long, tao.long, whe.long, val.long, duc.long, 
                ais.long, urd.long)
all.p.elev <- c(wra.elev, too.elev, pin.elev, tho.elev, niw.elev, bal.elev,
                lat.elev, tao.elev, whe.elev, val.elev, duc.elev, ais.elev, 
                urd.elev)
all.p.size.mean <- c(wra.size, too.size, pin.size, tho.size, sub.size, niw.size, 
                     bal.size, lat.size, tao.size, whe.size, val.size, duc.size, 
                     ais.size, urd.size)
all.p.size.mean2 <- c(wra.size, too.size, pin.size, tho.size, niw.size, 
                     bal.size, lat.size, tao.size, whe.size, val.size, duc.size, 
                     ais.size, urd.size) # omit SUB
all.p.size.var <- c(wra.var, too.var, pin.var, tho.var, sub.var, niw.var, 
                    bal.var, lat.var, tao.var, whe.var, val.var, duc.var, 
                    ais.var, urd.var)
all.p.size.sd <- c(wra.sd, too.sd, pin.sd, tho.sd, sub.sd, niw.sd, 
                   bal.sd, lat.sd, tao.sd, whe.sd, val.sd, duc.sd, 
                   ais.sd, urd.sd)
all.p.size.sd2 <- c(wra.sd, too.sd, pin.sd, tho.sd, niw.sd, 
                   bal.sd, lat.sd, tao.sd, whe.sd, val.sd, duc.sd, 
                   ais.sd, urd.sd)
all.p.size.quant <- c(wra.quant, too.quant, pin.quant, tho.quant, niw.quant, 
                      bal.quant, lat.quant, tao.quant, whe.quant, val.quant, 
                      duc.quant, ais.quant, urd.quant)

# NORTH AMERICA (n = 10)
# All size data
na_df_sites <- c(sites$pin[,2], sites$too[,2], sites$tho[,2], sites$wra[,2],
                 sites$bal[,2], sites$niw[,2], sites$tao[,2], sites$lat[,2], 
                 sites$whe[,2])
na_df <- data.frame(site=character(length(na_df_sites)))
na_df$site <- c(sites$pin[,2], sites$too[,2], sites$tho[,2], sites$wra[,2],
                sites$bal[,2], sites$niw[,2], sites$tao[,2], sites$lat[,2], 
                sites$whe[,2]) #returning numeric value??
na_df$lat <- c(sites$pin[,11], sites$too[,11], sites$tho[,11], sites$wra[,11],
                sites$bal[,11], sites$niw[,11], sites$tao[,11], sites$lat[,11], 
                sites$whe[,11])
na_df$elev <- c(sites$pin[,13], sites$too[,13], sites$tho[,13], sites$wra[,13],
                sites$bal[,13], sites$niw[,13], sites$tao[,13], sites$lat[,13], 
                sites$whe[,13])
na_df$areacm <- c(sites$pin[,14], sites$too[,14], sites$tho[,14], sites$wra[,14],
               sites$bal[,14], sites$niw[,14], sites$tao[,14], sites$lat[,14], 
               sites$whe[,14])
save(na_df, file='~/Desktop/Research/silene/r_files/na_df.RData')

# Point means
na.p.lat <- c(wra.lat, too.lat, pin.lat, tho.lat, sub.lat, niw.lat, bal.lat,
              lat.lat, tao.lat, whe.lat)
na.p.long <- c(wra.long, too.long, pin.long, tho.long, sub.long, niw.long, 
                bal.long, lat.long, tao.long, whe.long)
na.p.elev <- c(wra.elev, too.elev, pin.elev, tho.elev, niw.elev, bal.elev,
                lat.elev, tao.elev, whe.elev)
na.p.size.mean <- c(wra.size, too.size, pin.size, tho.size, sub.size, niw.size, 
                     bal.size, lat.size, tao.size, whe.size)
na.p.size.mean2 <- c(wra.size, too.size, pin.size, tho.size, niw.size, 
                    bal.size, lat.size, tao.size, whe.size) # omit SUB
na.p.size.var <- c(wra.var, too.var, pin.var, tho.var, sub.var, niw.var, 
                    bal.var, lat.var, tao.var, whe.var)
na.p.size.var2 <- c(wra.var, too.var, pin.var, tho.var, niw.var, 
                    bal.var, lat.var, tao.var, whe.var)
na.p.size.sd <- c(wra.sd, too.sd, pin.sd, tho.sd, sub.sd, niw.sd, 
                   bal.sd, lat.sd, tao.sd, whe.sd)
na.p.size.sd2 <- c(wra.sd, too.sd, pin.sd, tho.sd, niw.sd, 
                   bal.sd, lat.sd, tao.sd, whe.sd)

co.nm.size <- c(sub.size, niw.size, 
                bal.size, lat.size, tao.size, whe.size)
co.nm.lat <- c (sub.lat, niw.lat, bal.lat,
                lat.lat, tao.lat, whe.lat)
co.nm.size2 <- c(niw.size, 
                 bal.size, lat.size, tao.size, whe.size)
co.nm.elev <- c(niw.elev, bal.elev,
                lat.elev, tao.elev, whe.elev)

# EUROPE (n = 4)
e.p.lat <- c(val.lat, duc.lat, ais.lat, urd.lat)
e.p.long <- c(val.long, duc.long, ais.long, urd.long)
e.p.elev <- c(val.elev, duc.elev, ais.elev, urd.elev)
e.p.size.mean <- c(val.size, duc.size, ais.size, urd.size)
e.p.size.var <- c(val.var, duc.var, ais.var, urd.var)
e.p.size.sd <- c(val.sd, duc.sd, ais.sd, urd.sd)

###############################################################################
##################
# SITE-LEVEL PLOTS
##################

plot(s.size.means ~ site.lat) 
plot(s.size.means[2,4,5,7,8,9,10,13,14] ~ site.lat[2,4,5,7,8,9,10,13,14])
## need to plot NA and Europe separately - how to remove those sites?
plot(s.size.means ~ site.long)
plot(s.size.var ~ site.lat) ## no relationship
plot(s.size.means ~ site.elev, main = 'Elevation and Site Size Means',
     xlab = 'Site Mean Elevation (m)', 
     ylab = 'Site Size Means (mm^2)')
plot(s.size.sd ~ site.lat)

plot(quant.1 ~ site.lat, main = '10% Quantiles',
     xlab = 'Site Latitude (deg)',
     ylab = 'Size (mm^2)')
plot(quant.1 ~ site.elev, main = '10% Quantiles',
     xlab = 'Site Elevation (m)',
     ylab = 'Size (mm^2)')

plot(quant.9 ~ site.lat, main = '90% Quantiles',
     xlab = 'Site Latitude (deg)',
     ylab = 'Size (mm^2)')
plot(quant.9 ~ site.elev, main = '90% Quantiles',
     xlab = 'Site Elevation (m)',
     ylab = 'Size (mm^2)')

###################
# SINGLE SITE Plots
###################

plot(niw.size ~ niw.elev)
plot(bal.size ~ bal.elev)

#############
# POINT PLOTS
#############

plot(all.p.size.mean2 ~ all.p.elev, main = 'All Sites', 
     xlab = 'Point Elevation (m)', 
     ylab = 'Point Mean Size (mm^2)')

plot(all.p.size.sd2 ~ all.p.elev, main = 'All Sites', 
     xlab = 'Point Elevation (m)', 
     ylab = 'Point Mean Size SD') ## peak at mid-elevations?
plot(all.p.size.quant, main = '10% & 90% Quantiles',
     xlab = 'Site Index',
     ylab = 'Mean Point Size (mm^2)')
plot(all.p.size.mean ~ all.p.lat)

plot(log(all.p.size.mean2) ~ all.p.elev,
     main = 'All Sites',
     xlab = 'Point Elevation (m)',
     ylab = 'Point Mean Size (log scale)')

# NORTH AMERICA
plot(na.p.size.mean ~ na.p.lat, main = 'North America',
     xlab = 'Point Latitude (deg)', 
     ylab = 'Point Mean Size (mm^2)')
plot(na.p.size.mean2 ~ na.p.elev, main = 'North America',
     xlab = 'Point Elevation (m)', 
     ylab = 'Point Mean Size (mm^2')
plot(na.p.size.var2 ~ na.p.elev)

# CO & NM
plot(co.nm.size ~ co.nm.lat, main = 'New Mexico & Colorado',
     xlab = 'Point Latitude', ylab = 'Mean Point Size (mm^2)')
plot(co.nm.size2 ~ co.nm.elev, main = 'New Mexico & Colorado',
     xlab = 'Point Elevation (m)', ylab = 'Mean Point Size (mm^2)')

anova(lm(co.nm.size2 ~ co.nm.elev))

plot(log(co.nm.size) ~ co.nm.lat)

# NM
plot(whe.size ~ whe.elev)

# AK
plot(tho.size ~ tho.elev)

# EUROPE
plot(e.p.size.mean ~ e.p.lat)
plot(e.p.size.mean ~ e.p.long)
plot(e.p.size.mean ~ e.p.elev, main = 'Europe',
     xlab = 'Point Elevation (m)',
     ylab = 'Mean Point Size (mm^2)') ## max and min values
plot(e.p.size.var ~ e.p.elev)
plot(e.p.size.sd ~ e.p.elev, main = 'Europe',
     xlab = 'Point Elevation',
     ylab = 'Point Size SD')

summary(lm(e.p.size.mean ~ e.p.elev))
cor(e.p.size.mean, e.p.elev, method = c("pearson"))
###############################################################################

#######################
# BELOW: IN PROGRESS...
#######################

######
# GLMs
######

# remove outlier point
max(size$area, na.rm = TRUE)
which.max(size$area)
size$area[!352]
new.size <- size$area[!max(size$area)] ## neither of these work, need to fix

summary(glm(all.p.size.mean2 ~ all.p.elev + (all.p.elev)^2))
summary(glm(all.p.size.mean ~ all.p.lat + (all.p.lat)^2))

## Boxplots for binned elevations
bin.elev <- cut(size$elev, breaks = 15)
bin.lat <- cut(size$lat, breaks = 15)

plot(size$area ~ bin.elev, main = 'All Sizes',
     xlab = 'Binned Elevations (m)',
     ylab = 'Size (mm^2)')
plot(size$area ~ bin.lat)

##########
# USGS DEM
##########

# match lat & long with elevation (USGS DEM)

## ex from P. coulteri
elevation.ras <- raster(read.asciigrid('/Pinus coulteri/prism-CAdomain-annuals & imported files/ca_dem_geo.asc'))

#accurate elev for all points
pcfield$best.elev <- xyValues(elevation.ras, cbind(pcfield$long, pcfield$lat))
head(pcfield$best.elev)

# match lat & long with aspect (USGS DEM)
