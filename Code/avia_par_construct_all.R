library(stringr)

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


# List of files names
files <- list.files(#path="C:/Users/colle/Documents/R/co2aviation/Data/Eurostat",
                    pattern="*.tsv", full.names=TRUE, recursive=FALSE)
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


avia_par_PAS_DEP <- lapply(files, function(x) {
  read_avia_par(x,unit ="PAS",arr_dep ="DEP")
})
names(avia_par_PAS_DEP) <- lapply(files, function(x) {str_sub(x,-6,-5)})
count_row_list(avia_par_PAS_DEP)
comparison <- count_row_list(avia_par)/count_row_list(avia_par_PAS_DEP)


# avia_par_FLIGHT_DEP <- lapply(files, function(x) {
#   read_avia_par(x,unit ="FLIGHT",arr_dep ="DEP")
# })
# names(avia_par_FLIGHT_DEP) <- lapply(files, function(x) {str_sub(x,-6,-5)})
# comparison <- count_row_list(avia_par_PAS_DEP)/count_row_list(avia_par_FLIGHT_DEP)


count_row_list(avia_par_PAS_DEP)

### Agregation into a single DF
avia_par_PAS_DEP_filter <- do.call("bind_rows",avia_par_PAS_DEP)

### Filter extra EU -> From 9384 to 7544 airports relation (one way)
avia_par_PAS_DEP_filter <- avia_par_PAS_DEP_filter[
  avia_par_PAS_DEP_filter$dest_count %in%
    levels(as.factor(avia_par_PAS_DEP_filter$orig_count)),]

#Verification with database avia_paocc
avia_paocc <- read_avia_paocc("C:/Users/colle/Documents/R/co2aviation/Data/Eurostat_avia_paocc/avia_paocc.tsv")
avia_paocc[is.na(avia_paocc)] = 0

base_avia_comp <- function(base_par,base_paocc,year=2018){
  # 1st base, calculate nb passengers by country for a year
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
  list=levels(as.factor(base_paocc$country_2))[c(-12:-14,-25,-26,-33)]
  S_paocc<- sapply(list,function(x){
    sum(base_paocc[base_paocc$country_2==x & base_paocc$partner %in%  list,7:10])
  })
  return(rbind(S_par,S_paocc,S_par/S_paocc))
}
comparison <- base_avia_comp(avia_par_PAS_DEP_filter,avia_paocc,2017)
sum(comparison[1,])/sum(comparison[2,])
## Brouillon
list <- levels(as.factor(avia_paocc$country_2))[c(-12:-14,-25,-26,-33)]
avia_paocc_2017 <- avia_paocc[avia_paocc$country_2=="LV" & 
                 avia_paocc$partner %in% list,c(1:6,18:21)]
avia_paocc_2017[,7:10] <- sapply(avia_paocc_2017[,7:10],as.numeric)
avia_paocc_2017 <- cbind(avia_paocc_2017[,1:6],avia_paocc_2017[,7:10] %>% transmute(y_2017=rowSums(.)))

avia_par_2017 <- avia_par_PAS_DEP_filter[avia_par_PAS_DEP_filter$orig_count =="LV" & 
                              avia_par_PAS_DEP_filter$dest_count %in% list,c(1:8,19:22)]
avia_par_2017[,9:12] <- sapply(avia_par_2017[,9:12],as.numeric)
avia_par_2017 <- cbind(avia_par_2017[,1:8],avia_par_2017[,9:12] %>% transmute(y_2017=rowSums(.)))

sum(avia_par_2017$y_2017)
sum(avia_paocc_2017$y_2017)

sum(avia_par_PAS_DEP_filter[(avia_par_PAS_DEP_filter$orig_airp=="LFPG" & avia_par_PAS_DEP_filter$dest_airp=="LEBL") |
                              (avia_par_PAS_DEP_filter$orig_airp=="LEBL" & avia_par_PAS_DEP_filter$dest_airp=="LFPG") 
                            ,c("2017Q1","2017Q2","2017Q3","2017Q4")])

#Traffic between 31 states 