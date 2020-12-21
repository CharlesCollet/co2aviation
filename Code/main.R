#Libraries
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

setwd("C:/Users/colle/Documents/R/co2aviation/Code")
#Call function aggregations
source("read_avia_par.R")
source("read_avia_paocc.R")
source("base_avia_comp.R")
source("matrix_orig_dest.R")

###### Load data directory and read files name #####
setwd("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat/avia_par")
files <- list.files(pattern="*.tsv", full.names=TRUE, recursive=FALSE)
PAS_DEP <- read_avia_par_list(files,unit ="PAS",arr_dep ="DEP")
PAS_ARR <- read_avia_par_list(files,unit ="PAS",arr_dep ="ARR")
#Verification with database avia_paocc
avia_paocc_DEP <- read_avia_paocc("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat/avia_paocc/avia_paocc.tsv",
                                  unit ="PAS",arr_dep ="DEP")
avia_paocc_ARR <- read_avia_paocc("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat/avia_paocc/avia_paocc.tsv",
                              unit = "PAS",arr_dep = "ARR")

comparison_DEP <- base_avia_comp(PAS_DEP,avia_paocc_DEP,2017)
comparison_ARR <- base_avia_comp(PAS_ARR,avia_paocc_ARR,2017)
#Quick verification of the percentage of similarities
sum(comparison_DEP[1,])/sum(comparison_DEP[2,])
sum(comparison_ARR[1,])/sum(comparison_ARR[2,])
#### WARNING: in the database avia_paocc for arrivals within the same country
        # Example : For Flights Norway (N0) in 2017,
        #         Departure from Norway = 15 283 971 
        #         whereas Arrivals in Norway = 307 164
        # It makes sense because the flights would be counted twice otherwise

# Tackle czech airport codes problems:
# We get the data on departures in Czech airports (cz database)
# with the arrivals from Czech airports in all other country databases
PAS_DEP <- PAS_DEP[PAS_DEP$orig_count != "CZ",]
czech_PAS_ARR <- PAS_ARR[PAS_ARR$dest_count == "CZ",]
czech_PAS_ARR[,5:8] <- czech_PAS_ARR[,c(7,8,5,6)]
PAS_DEP <- rbind(PAS_DEP,czech_PAS_ARR)

#Origin destination matrix on one year
mat_orig_dest_DEP <- matrix_orig_dest(PAS_DEP,year=2017)

###### Group airports in the same city ######

table(substr(base_par$orig_airp,4,7) %in% airport_city$airport)["TRUE"]
airport_city[!(airport_city$airport %in% substr(base_par$orig_airp,4,7)),]
PAS_DEP[PAS_DEP$dest_airp=="EDHI",]
PAS_DEP[PAS_DEP$orig_airp=="EDHI",]

#Some airport are not in the origin list but only on the destination one
#This is due to the "main declaring airports" and "main partners" selection by Eurostat:
# An airport (ex: Hamburg Finkerwerder - EDHI) can be below the "main declaring airports" threshold of 150 000 passengers (by quarter ?)
# -> It does appear in the base of the declaring country (avia_par_de in this case)
# However this airport can appear in the destination of a "main declaring airports" of another country (if above 10 000 passengers)
#  the number of passengers per quarter is above the threshold (Blagnac/Toulouse CCER - LFBO)
# The consequence is that for these airports, only the flights from the "big" to the "small" airport are accounted in the DEPARTURE base
# 2 solutions :
#    1 - Eliminate all the airports that do not appear both in the origin and destination list
#    2 - Integrate these airports through the use of the ARRIVAL database

#
airport_same_city <- function(){
}

