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