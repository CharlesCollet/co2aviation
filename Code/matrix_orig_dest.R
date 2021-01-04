
# 2 naming problems :
#   1. Wrong codes for all destination airports coming from Czech (avia_par_cz database)
#       -> We try to recuperate the airport codes with the ARRIVAL database
#   2. "Weird" UK airport, coded EG40 (Finmere airport) but not found on ICAO site
#       -> We choose to supress this airport (about 300k passengers arrivals/year, all within UK)
# Different "specific airports" that appear only in the destination list, but not in the origin one:
# 

filter_year <- function(base_par,year){
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


matrix_orig_dest <- function(base_par,year,small_airports="supress"){
  base_par <- filter_year(base_par,year)
  base_par <- base_par[base_par[,3] !=0,]
  if(small_airports=="supress"){
    test <- base_par[!(base_par$dest_airp %in% base_par$orig_airp),]
    n_dest_pas_sup <- sum(base_par[!(base_par$dest_airp %in% base_par$orig_airp),3])
    base_par <- base_par[base_par$dest_airp %in% base_par$orig_airp,]
    n_orig_pas_sup <- sum(base_par[!(base_par$orig_airp %in% base_par$dest_airp),3])
    base_par <- base_par[base_par$orig_airp %in% base_par$dest_airp,]
    total_pas <- sum(base_par[,3])
  }
  # option small_airports = keep_all not implemented yet
  #From long to wide -> Matrix form
  base_par <- reshape(data=base_par,idvar="orig_airp",
                      v.names = paste("year",year,sep = "_"),
                      timevar = "dest_airp",
                      direction="wide")
  base_par[is.na(base_par)] <- 0 
  base_par <- base_par[,order(names(base_par))]
  names(base_par)[-1] <- sapply(names(base_par)[-1],function(x){substr(x,11,17)})
  return(list(base_par,share_small_airports=(n_orig_pas_sup+ n_dest_pas_sup)/total_pas))
}
