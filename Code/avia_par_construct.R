#Libraries
library(readr)
library(tidyr)
#Load data directory
setwd("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat")

#Parameters of the database taken from Eurostat
#With custom data

avia_par_fr <- read_tsv("avia_par_fr__custom_370602_20201217_161620.tsv")
avia_par_fr <- as.data.frame(avia_par_fr )
avia_par_fr <- separate(avia_par_fr,
                        col =1,
                        sep=",",
                        into=unlist(strsplit(colnames(avia_par_fr)[1],",")))
avia_par_fr <- separate(avia_par_fr,
                        col =4,
                        sep="_",
                        into=c("orig_count","orig_airp","dest_count","dest_airp"))
#CAF_PAS = Commercial_Passenger Air Flights
#PAS_CRD = Passenger Carried
avia_par_fr <- separate(avia_par_fr,
                        col =3,
                        sep="_",
                        into=c("data_type","data-type_2","arr_dep"))

