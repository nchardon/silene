## N. Chardon
## Start: 18 Sept 2015
## Aim: create iButton Dataframes
## Generated Datasets:
load('~/Desktop/Research/silene/r_files/ibut.RData')
load('~/Desktop/Research/silene/r_files/ibut_table.RData')
load('~/Desktop/Research/silene/r_files/table.RData') 
#ibut_table with correct date format
load(ibut_dat, file='~/Desktop/Research/silene/r_files/ibut_dat.RData') 
#ibutton id matched with lat, long, elev, and site

# load library
library(abind) #package necessary to combine arrays

# read in files with '%d.%m.%y %H:%M:%S' date format
setwd("~/Desktop/Research/silene/ibuttons_2014/nat")
fileList_nat <- list.files(path = "~/Desktop/Research/silene/ibuttons_2014/nat", 
                       pattern = ".csv")
data_nat <- sapply(fileList_nat, read.csv, stringsAsFactors = F) #factors mess!
ibut_nat <- data.frame(data_nat)

# read in files with '%m/%d/%y %I:%M:%S %p' date format
setwd("~/Desktop/Research/silene/ibuttons_2014/em")
fileList_em <- list.files(path = "~/Desktop/Research/silene/ibuttons_2014/em", 
                           pattern = ".csv")
data_em <- sapply(fileList_em, read.csv, stringsAsFactors = F) #factors mess!
ibut_em <- data.frame(data_em)

# eliminate unneccessary info from ibut
ibut_table_nat <- data.frame(ibut_nat[16:nrow(ibut_nat),], row.names = NULL)
ibut_table_em <- data.frame(ibut_em[16:nrow(ibut_em),], row.names = NULL)

# table_em with only time stamp and tbot
datum_em <- seq(1,nrow(ibut_table_em),3) #seq. from 1:nrow by 3's to pick from ibut_table
tbot_em <- datum_em+2 #pick temperature values from ibut_table

table_em <- array(NA, dim=c(length(datum_em), 3, length(ibut_table_em)))

for (i in 1:ncol(ibut_table_em)){ #loop through all ibuttons
        for (j in 1:length(datum_em)){ #loop through each ibutton
                table_em[j,1,i] <- as.character(ibut_table_em[datum_em[j],i])
                table_em[j,2,i] <- as.character(ibut_table_em[tbot_em[j],i])
                table_em[j,1,i] <- as.character(strptime(table_em[j,1,i], 
                                                      format = '%m/%d/%y %I:%M:%S %p'))
                #converts date to recognizable date format
                table_em[j,3,i] <- strftime(table_em[j,1,i], '%m')
                #extract month from date string
        }
}

# table_nat with only time stamp and tbot
datum_nat <- seq(1,nrow(ibut_table_nat),3) #seq. from 1:nrow by 3's to pick from ibut_table
tbot_nat <- datum_nat+2 #pick temperature values from ibut_table

table_nat <- array(NA, dim=c(length(datum_nat), 3, length(ibut_table_nat))) #create container

for (i in 1:ncol(ibut_table_nat)){ #loop through all ibuttons
        for (j in 1:length(datum_nat)){ #loop through each ibutton
                table_nat[j,1,i] <- as.character(ibut_table_nat[datum_nat[j],i])
                table_nat[j,2,i] <- as.character(ibut_table_nat[tbot_nat[j],i])
                table_nat[j,1,i] <- as.character(strptime(table_nat[j,1,i], 
                                                      format = '%d.%m.%y %H:%M:%S'))
                table_nat[j,3,i] <- strftime(table_nat[j,1,i], '%m')
                #extract month from date string
        }
}

# combine separate ibut_table and ibut dataframes and table arrays
ibut_table <- cbind(ibut_table_nat, ibut_table_em)
ibut <- cbind(ibut_nat, ibut_em)
table <- abind(table_nat, table_em) #default binds on last dimension

# convert temperature and month to numeric
table[,2,] <- as.numeric(table[,2,]) 
table[,3,] <- as.numeric(table[,3,])

# isolate reg. number (digits 9-14) for ibut names
substrRight <- function(x,n){
        substr(x, nchar(x)-n+1, nchar(x))
}

ibut_names <- rep(0, length=ncol(ibut))
for (n in 1:ncol(ibut)){
        ibut_names[n] <- substr(substrRight(as.character(ibut[1,n]),8),1,6)
        #take 8 from right, then take first 6
}

# match ibut ID with cover data (lat, long, elev) in new df
load('~/Desktop/Research/silene/r_files/cover.RData')
ibut_dat <- data.frame(id=character(length(ibut_names))) #create empty df
ibut_dat$id <- ibut_names
cover2ibut <- match(ibut_dat$id, cover$ibutt) #match ibut names with cover df
ibut_dat$lat <- cover$lat[cover2ibut] #add lat to ibut_dat
ibut_dat$long <- cover$long[cover2ibut] #add long to ibut_dat
ibut_dat$elev <- cover$elev[cover2ibut] #add elev to ibut_dat
size2ibut <- match(ibut_dat$lat, size$lat) #match ibut lat with size lat
ibut_dat$site <- size$site[size2ibut] #add site to ibut_dat

# save dataframes
save(ibut, file='~/Desktop/Research/silene/r_files/ibut.RData')
#all data from CSV file
save(ibut_table, file='~/Desktop/Research/silene/r_files/ibut_table.RData')
#ibut without random info at start of file
save(table, file='~/Desktop/Research/silene/r_files/table.RData')
#ibut_table with correct date format
save(ibut_dat, file='~/Desktop/Research/silene/r_files/ibut_dat.RData')
#ibutton id matched with lat, long, elev, and site
