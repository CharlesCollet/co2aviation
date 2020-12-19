####### With full datasets ###########
read_avia_par <- function(data,first_year=2015,freq_data="Q",unit="PAS",arr_dep="DEP"){
  oldw <- getOption("warn")
  options(warn = -1)
  # data="avia_par_rs.tsv"
  #capture.output to make invisible output of the read function
  capture.output(
    avia_par_country <- as.data.frame(read_tsv(data))
    ,file='NUL')
#1 Filter on time
  #First year of data, can be modified
  # first_year=2015
  #Frequency selected (Monthly or Quaterly)
  # freq_data="Q"
  avia_par_country <- avia_par_country[,c(TRUE,
                                #-1 to keep the first column with variable information
                                sapply(colnames(avia_par_country)[-1],
                                       function(x){
                                         #Filter on year
                                         as.numeric(substr(x,1,4), 1, 4) >= first_year &
                                           #Filter on frequency
                                           substr(x,5,5)==freq_data
                                         }))]

#2 Treatment of the first column
  #Separation into 6 colums
  avia_par_country <- separate(avia_par_country,
                          col =1,
                          sep=",",
                          into=unlist(strsplit(colnames(avia_par_country)[1],",")))
  avia_par_country <- separate(avia_par_country,
                          col =3,
                          sep="_",
                          into=c("orig_count","orig_airp","dest_count","dest_airp"))
  #col 1 and 2 are inverted for Serbia database
  if(data=="./avia_par_rs.tsv"){
    avia_par_country[,c(1,2)] <- avia_par_country[,c(2,1)]
    names(avia_par_country)[c(1,2)] <- colnames(avia_par_country)[c(2,1)]
  }
#3 Filter on data type (number of flights / passenger on board, carried)
  #Filter on CAF_PAS (Commercial Passenger Air Flights) & PAS_CRD (Passenger Carried), Arrival and departure
  var_select=c("CAF_PAS_ARR","CAF_PAS_DEP","PAS_CRD_ARR","PAS_CRD_DEP")
  avia_par_country <- avia_par_country[avia_par_country$tra_meas %in% var_select,]
  #Numerical 
  avia_par_country[,-1:-6] <- as.numeric(as.matrix(avia_par_country[,-1:-6]))
  #Remove entirely empty rows -> 
  #avia_par_country <- avia_par_country[rowSums(is.na(avia_par_country[,-1:-6])) != ncol(avia_par_country[,-1:-6]),]

#4 Passage toward list of matrices, with each a different data type (ex:Arrival of Passenger Carried)
  #CAF_PAS = Commercial Passenger Air Flights
  #PAS_CRD = Passenger Carried
  avia_par_country <- separate(avia_par_country,
                          col =2,
                          sep="_",
                          into=c("data_type","data-type_2","arr_dep"))
  avia_par_country <- avia_par_country[avia_par_country$arr_dep==arr_dep & avia_par_country$unit==unit,]
  avia_par_country [is.na(avia_par_country)] = 0
  options(warn = oldw)
  return(avia_par_country)
}

####### Read list of files and aggregate into a dataframe
read_avia_par_list <- function(files,unit ="PAS",arr_dep="DEP"){
  avia_par_PAS_DEP <- lapply(files, function(x) {
    read_avia_par(x,unit = unit,arr_dep=arr_dep)
  })
  names(avia_par_PAS_DEP) <- lapply(files, function(x) {str_sub(x,-6,-5)})
  
  ### Agregation into a single DF
  PAS_DEP <- do.call("bind_rows",avia_par_PAS_DEP)
  
  ### Filter extra EU -> From 9384 to 7544 airports relation (one way)
  PAS_DEP <- PAS_DEP[
    PAS_DEP$dest_count %in%
      levels(as.factor(PAS_DEP$orig_count)),]
  return(PAS_DEP)
}
  