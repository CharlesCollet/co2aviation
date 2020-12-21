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
source("matrix_origin_dest.R")

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
#### WARNING: there is a problem in the database avia_paocc for arrivals within the same country
# Example : For Flights Norway (N0) in 2017,
#          Departure from Norway = 15 283 971 
#  whereas Arrivals in Norway = 307 164
# It maskes sense because the flights would be counted twice otherwise
#Origin destination matrix on one year

base_par <- matrix_orig_dest(PAS_DEP,year=2017)
levels(factor(PAS_DEP$orig_count))
levels(factor(PAS_DEP$orig_airp))
levels(factor(PAS_DEP$dest_airp))

table(substr(base_par$orig_airp,4,7) %in% airport_city$airport)["TRUE"]
airport_same_city <- function(){
}