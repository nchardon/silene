load('~/Desktop/Research/silene/r_files/size15.RData')
load('~/Desktop/Research/silene/r_files/cover15.RData')

id_names <- c('bel', 'bie', 'dem', 'den', 'eln', 'els', 'eva', 'evw', 'gra', 
              'qua', 'red', 'she', 'squ', 'sun', 'yal', 
              'niw', 'ele', 'han', 'kin', 'mam')
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
        }
}