
# 2 naming problems :
#   1. Wrong codes for all destination airports coming from Czech (avia_par_cz database)
#       -> We try to recuperate the airport codes with the ARRIVAL database
#   2. "Weird" UK airport, coded EG40 (Finmere airport) but not found on ICAO site
#       -> We choose to supress this airport (about 300k passengers arrivals/year, all within UK)
# Different "specific airports" that appear only in the destination list, but not in the origin one:
# 

filter_year <- function(base_par,year){
  # base_par=PAS_DEP
  # year=2017
  # small_airports="suppress"
  base_par$orig_airp <- paste(base_par$orig_count,base_par$orig_airp,sep="_")
  base_par$dest_airp <- paste(base_par$dest_count,base_par$dest_airp,sep="_")
  base_par <- base_par[,c(-1:-5,-7)]
  base_par <- base_par[,c(rep(TRUE,2),
                          sapply(colnames(base_par)[-1:-2],function(x){substr(x,1,4) == year}
                          ))]
  base_par <- cbind(base_par[,1:2],base_par[,-1:-2] %>% transmute(rowSums(.)))
  colnames(base_par)[3] <- paste("year",year,sep="_")
  base_par <- arrange(base_par,orig_airp,dest_airp)
  return(base_par)
}

test <- filter_year(PAS_DEP,2017)
test <- test[test$year_2017 !=0,]
n_dest_pas_sup <- sum(test[!(test$dest_airp %in% test$orig_airp),3])
test <- test[test$dest_airp %in% test$orig_airp,]
n_orig_pas_sup <- sum(test[!(test$orig_airp %in% test$dest_airp),3])
test <- test[test$orig_airp %in% test$dest_airp,]
total_pas <- sum(test[,3])
c(n_dest_pas_sup,n_orig_pas_sup,n_dest_pas_sup/total_pas ,n_orig_pas_sup/total_pas)
#We suppress approximatively 0,6% of the passengers
mat_orig_dest_DEP <- matrix_orig_dest(test,year=2017)
# test_2 <- test_2[(substr(test_2$dest_airp,1,2) == "CZ") & test_2$year_2017 !=0,]
sum(test[,3])
sum(test_2[,3])

matrix_orig_dest(test,year = 2017)
matrix_orig_dest <- function(base_par,year,small_airports="suppress"){
  # base_par <- test
  # base_par <- filter_year(base_par,year)
  # if(small_airports=="suppress"){
  #   test <- base_par[!(base_par$dest_airp %in% base_par$orig_airp) & base_par$year_2017 !=0,]
  #   base_par <- base_par[base_par$dest_airp %in% base_par$orig_airp,]
  # }
  # base_par <- base_par[base_par$year_2017!=0,]
  #From long to wide -> Matrix form
  base_par <- reshape(data=base_par,idvar="orig_airp",
                      v.names = "year_2017",
                      timevar = "dest_airp",
                      direction="wide")
  base_par <- base_par[,order(names(base_par))]
}
