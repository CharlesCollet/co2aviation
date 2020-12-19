matrix_orig_dest <- function(base_par,year){
  # base_par=avia_par_PAS_DEP_filter
  # year=2017
  base_par$orig_airp <- paste(base_par$orig_count,base_par$orig_airp,sep="_")
  base_par$dest_airp <- paste(base_par$dest_count,base_par$dest_airp,sep="_")
  base_par <- base_par[,c(-1:-5,-7)]
  base_par <- base_par[,c(rep(TRUE,2),
                              sapply(colnames(base_par)[-1:-2],function(x){substr(x,1,4) == year}
                                     ))]
  base_par <- cbind(base_par[,1:2],base_par[,-1:-2] %>% transmute(rowSums(.)))
  colnames(base_par)[3] <- paste("year",year,sep="_")
  base_par <- arrange(base_par,orig_airp,dest_airp)
  base_par <- base_par[base_par$year_2017!=0,]
  #From long to wide -> Matrix form
  base_par <- reshape(data=base_par,idvar="orig_airp",
                      v.names = "year_2017",
                      timevar = "dest_airp",
                      direction="wide")
  base_par <- base_par[,order(names(base_par))]
}
