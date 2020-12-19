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

###### Load data directory and read files name #####
setwd("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat/avia_par")
files <- list.files(pattern="*.tsv", full.names=TRUE, recursive=FALSE)
PAS_DEP <- read_avia_par_list(files,unit ="PAS",arr_dep ="DEP")

#Verification with database avia_paocc
avia_paocc <- read_avia_paocc("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat/avia_paocc/avia_paocc.tsv")

comparison <- base_avia_comp(PAS_DEP,avia_paocc,2017)
sum(comparison[1,])/sum(comparison[2,])

## Brouillon - data by year
list <- levels(as.factor(avia_paocc$country_2))[c(-12:-14,-25,-26,-33)]
avia_paocc_2017 <- avia_paocc[avia_paocc$country_2=="LV" & 
                                avia_paocc$partner %in% list,c(1:6,18:21)]
#Sum on the 4 trimesters of the yeear
avia_paocc_2017 <- cbind(avia_paocc_2017[,1:6],avia_paocc_2017[,7:10] %>% transmute(y_2017=rowSums(.)))

#Test for a specific country
avia_par_2017 <- PAS_DEP[PAS_DEP$orig_count =="LV" & 
                                           PAS_DEP$dest_count %in% list,c(1:8,19:22)]
avia_par_2017[,9:12] <- sapply(avia_par_2017[,9:12],as.numeric)
avia_par_2017 <- cbind(avia_par_2017[,1:8],avia_par_2017[,9:12] %>% transmute(y_2017=rowSums(.)))

sum(avia_par_2017$y_2017)
sum(avia_paocc_2017$y_2017)

sum(PAS_DEP[(PAS_DEP$orig_airp=="LFPG" & PAS_DEP$dest_airp=="LEBL") |
                              (PAS_DEP$orig_airp=="LEBL" & PAS_DEP$dest_airp=="LFPG") 
                            ,c("2017Q1","2017Q2","2017Q3","2017Q4")])

#Traffic between 31 states 
