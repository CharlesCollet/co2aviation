#Number of rows of each DF element within a list 
count_row_list <- function(list){
  # list=avia_par_PAS_DEP
  i=1
  list_row=c()
  while (i<=length(list)) {
    list_row <- c(list_row,nrow(list[[i]]))
    i=i+1
  }
  names(list_row) <- names(list)
  return(list_row)
}
#Number of colums of each DF element within a list 
count_col_list <- function(list){
  # list=avia_par_PAS_DEP
  i=1
  list_col=c()
  while (i<=length(list)) {
    list_col <- c(list_col,ncol(list[[i]]))
    i=i+1
  }
  names(list_col) <- names(list)
  return(list_col)
}

base_avia_comp <- function(base_par,base_paocc,year=2017){
  # 1st base, calculate nb passengers by country for a year
   base_par=PAS_ARR
   base_paocc=avia_paocc_ARR
  base_par <- base_par[,c(rep(TRUE,8),
                          #-1 to keep the first column with variable information
                          sapply(colnames(base_par)[-1:-8],
                                 function(x){
                                   as.numeric(substr(x,1,4), 1, 4) == year
                                 }))]
  list=levels(as.factor(base_par$orig_count))
  S_par <- sapply(list,function(x){
    sum(base_par[base_par$orig_count==x,9:12])
  })
  # 2nd base, calculate nb passengers by country for a year
  base_paocc <- base_paocc[,c(rep(TRUE,6),
                              #-1 to keep the first column with variable information
                              sapply(colnames(base_paocc)[-1:-6],
                                     function(x){
                                       as.numeric(substr(x,1,4), 1, 4) == year
                                     }))]
  list=levels(as.factor(base_paocc$orig_count))[c(-12:-14,-25,-26,-33)]
  S_paocc<- sapply(list,function(x){
    sum(base_paocc[base_paocc$orig_count==x & base_paocc$dest_count %in%  list,7:10])
  })
  # levels(factor(base_paocc$dest_count))
  return(rbind(S_par,S_paocc,S_par/S_paocc))
}
