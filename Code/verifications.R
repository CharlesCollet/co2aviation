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

#List of original data frames -> for verification
avia_par <- lapply(files, function(x) {
  oldw <- getOption("warn")
  options(warn = -1)
  capture.output(
    avia_par_country <- as.data.frame(read_tsv(x))
    ,file='NUL')
  options(warn = oldw)
  return(avia_par_country)
})

#
avia_par_PAS_DEP <- lapply(files, function(x) {
  read_avia_par(x,unit ="PAS",arr_dep ="DEP")
})
names(avia_par_PAS_DEP) <- lapply(files, function(x) {str_sub(x,-6,-5)})
count_row_list(avia_par_PAS_DEP)
comparison <- count_row_list(avia_par)/count_row_list(avia_par_PAS_DEP)
comparison

### Agregation into a single DF
PAS_DEP <- do.call("bind_rows",avia_par_PAS_DEP)

### Filter extra EU -> From 9384 to 7544 airports relation (one way)
PAS_DEP <- PAS_DEP[
  PAS_DEP$dest_count %in%
    levels(as.factor(PAS_DEP$orig_count)),]

#Verification with database avia_paocc
avia_paocc <- read_avia_paocc("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat_avia_paocc/avia_paocc.tsv")
#Suppress empty rows
avia_paocc[is.na(avia_paocc)] = 0


comparison <- base_avia_comp(PAS_DEP,avia_paocc,2017)
sum(comparison[1,])/sum(comparison[2,])


#Focus sur un pays une année spécifique
list <- levels(as.factor(avia_paocc$orig_count))[c(-12:-14,-25,-26,-33)]

spec_country_year <- function(base_test,year,country){
  # base_test <- avia_paocc
  # year=2017
  # country="NO"
  n_col <- sum(lapply(base_test,class)=="character")
  base_test <- base_test[base_test$orig_count==country & 
                           base_test$dest_count %in% list,]
  base_test <- base_test[,c(rep(TRUE,n_col),
                            sapply(colnames(base_test)[-1:-n_col],
                                   function(x){substr(x,1,4)== year}))]
  #Sum on the 4 trimesters of the yeear
  base_test <- cbind(base_test[,1:n_col],base_test[,(n_col+1):(n_col+4)] %>% transmute(Y=rowSums(.)))
  colnames(base_test)[n_col+1] <- paste("y",year,sep = "_")
  return(base_test)
}
comp_spec_country_year <- function(PAS_DEP,avia_paocc_DEP,PAS_ARR,avia_paocc_ARR,country,year){
  par_DEP_test <- spec_country_year(PAS_DEP,year,country)
  paocc_DEP_test <- spec_country_year(avia_paocc_DEP,year,country)
  par_ARR_test <- spec_country_year(PAS_ARR,year,country)
  paocc_ARR_test <- spec_country_year(avia_paocc_ARR,year,country)
  test <- lapply(list,function(x){sum(par_DEP_test[par_DEP_test$dest_count==x,9])})
  test <- as.data.frame(cbind(list,do.call(rbind,test)))
  test <- cbind(test,paocc_DEP_test[,7])
  test <- cbind(test,do.call(rbind,lapply(list,function(x){sum(par_ARR_test[par_ARR_test$dest_count==x,9])})))
  test <- cbind(test,paocc_ARR_test[,7])
  names(test) <- c("count_dest","par_DEP","paocc_DEP","par_ARR","paocc_ARR")
  return(test)
}
NO_comp_2017 <- comp_spec_country_year(PAS_DEP,avia_paocc_DEP,PAS_ARR,avia_paocc_ARR,country="NO",year=2017)
FR_comp_2017 <- comp_spec_country_year(PAS_DEP,avia_paocc_DEP,PAS_ARR,avia_paocc_ARR,country="FR",year=2017)
#### WARNING: there is a problem in the database avia_paocc for arrivals within the same country
    # Example : For Flights Norway (N0) in 2017,
    #          Departure from Norway = 15 283 971 
    #  whereas Arrivals in Norway = 307 164
# It maskes sense because the flights would be counted twice otherwise


#Test sur certains aéroports
sum(PAS_DEP[(PAS_DEP$orig_airp=="LFPG" & PAS_DEP$dest_airp=="LEBL") |
              (PAS_DEP$orig_airp=="LEBL" & PAS_DEP$dest_airp=="LFPG") 
            ,c("2017Q1","2017Q2","2017Q3","2017Q4")])


### Group airports in the same city
airp