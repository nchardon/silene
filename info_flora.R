## N. Chardon
## Start: 5 April 2016
## Aim: find field work sites for Summer 2016

# load packages
library(lattice)
library(ggplot2)

# load handpicked sites from SummitFlora (S. Wipf, 3 March 2016)
swizzy <- read.csv('~/Desktop/Research/silene/Summer 2016 Fieldwork/Switzerland/silene_sites_summitflora.csv', stringsAsFactors = F)

# plot potential sites (n=25)
ggplot(data=swizzy, aes(x, y, label=Mountain_name)) + 
    geom_point(aes(col='red')) +
    geom_text() + 
    ggtitle('Potential Sites (Summit Flora data)') +
    xlim(700000,825000) +
    theme(axis.text.x=element_text(size=13), 
          axis.title.x=element_text(size=15,vjust=-0.3), #vjust=moves axis label
          axis.text.y=element_text(size=13),
          axis.title.y=element_text(size=15,vjust=1),
          legend.position='none')

# load InfoFlora DFs
aca <- read.csv('~/Desktop/Research/silene/Summer 2016 Fieldwork/Switzerland/infoflora_Silene_acaulis.csv', stringsAsFactors = F)
exs <- read.csv('~/Desktop/Research/silene/Summer 2016 Fieldwork/Switzerland/infoflora_Silene_exscapa.csv', stringsAsFactors = F)
aca <- aca[is.na(aca$x)==F,] #remove NA values
exs <- exs[is.na(exs$x)==F,]

# plot in 3D
cloud(altitude_inf ~ x * y, data=aca)
cloud(altitude_inf ~ x * y, data=exs)

# subset by xy_precision < 501m
aca500 <- subset(aca, xy_precision < 501)
exs500 <- subset(exs, xy_precision < 501)

# plot S. acaulis and S. excapa occurrences for comparison
pdf('~/Desktop/Research/silene/Summer 2016 Fieldwork/Switzerland/infoflora_map500.pdf')
plot(aca500$x, aca500$y, main='InfoFlora Occurrences (SwissGrid)', 
     xlab='X', ylab='Y', cex=1.4)
points(exs500$x, exs500$y, col='red', cex=0.6)
points(782976.696, 186299.786, col='green', pch=15, cex=2)
legend('topleft', pch=c(1,1,15), col=c('black','red','green'),
       legend=c('Silene acaulis', 'Silene exscapa', 'Davos'), bty='n')
dev.off()

# S. acaulis histograms
pdf('~/Desktop/Research/silene/Summer 2016 Fieldwork/Switzerland/infoflora_silene.pdf')
par(mfrow=c(2,1))
hist(aca$altitude_inf, breaks=20, xlim=c(1000,3500),
     main='Silene acaulis InfoFlora Occurrences', xlab='Altitude (m)')
hist(aca$altitude_sup, breaks=20, add=T, col='grey')
legend('topright', pch=c(0,15), col=c('black', 'grey'), 
       legend=c('lowest observation', 'highest observation'),
       bty='n') #no box around legend
hist(aca500$altitude_inf, breaks=20, xlim=c(1000,3500),
     main='Silene acaulis InfoFlora Occurrences (< 501 m precision)', 
     xlab='Altitude (m)')
hist(aca500$altitude_sup, breaks=20, add=T, col='grey')
dev.off()

# S. exscapa histograms
pdf('~/Desktop/Research/silene/Summer 2016 Fieldwork/Switzerland/infoflora_exscapa.pdf')
par(mfrow=c(2,1))
hist(exs$altitude_inf, breaks=20, xlim=c(1000,3500),
     main='Silene exscapa InfoFlora Occurrences', xlab='Altitude (m)')
hist(exs$altitude_sup, breaks=20, add=T, col='grey')
legend('topleft', pch=c(0,15), col=c('black', 'grey'), 
       legend=c('lowest observation', 'highest observation'),
       bty='n') #no box around legend
hist(exs500$altitude_inf, breaks=20, xlim=c(1000,3500),
     main='Silene exscapa InfoFlora Occurrences (< 501 m precision)', 
     xlab='Altitude (m)')
hist(exs500$altitude_sup, breaks=20, add=T, col='grey')
dev.off()
