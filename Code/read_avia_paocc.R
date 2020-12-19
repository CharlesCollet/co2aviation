read_avia_paocc <- function(data,first_year=2015,freq_data="Q",unit="PAS",arr_dep="DEP"){
  oldw <- getOption("warn")
  options(warn = -1)
  # data="avia_paocc.tsv"
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
  #3 Filter on data type (number of flights / passenger on board, carried)
  #Filter on CAF_PAS (Commercial Passenger Air Flights) & PAS_CRD (Passenger Carried), Arrival and departure
  var_select=c("CAF_PAS_ARR","CAF_PAS_DEP","PAS_CRD_ARR","PAS_CRD_DEP")
  avia_par_country <- avia_par_country[avia_par_country$tra_meas %in% var_select,]
  #Numerical 
  avia_par_country[,-1:-4] <- as.numeric(as.matrix(avia_par_country[,-1:-4]))
  #Remove entirely empty rows
  # avia_par_country <- avia_par_country[rowSums(is.na(avia_par_country[,-1:-6])) != ncol(avia_par_country[,-1:-6]),]
  
  #4 Passage toward list of matrices, with each a different data type (ex:Arrival of Passenger Carried)
  #CAF_PAS = Commercial Passenger Air Flights
  #PAS_CRD = Passenger Carried
  avia_par_country <- separate(avia_par_country,
                               col =2,
                               sep="_",
                               into=c("data_type","data-type_2","arr_dep"))
  avia_par_country <- avia_par_country[avia_par_country$arr_dep==arr_dep & avia_par_country$unit==unit,]
  colnames(avia_par_country)[6] <-"country_2"
  options(warn = oldw)
  return(avia_par_country)
}


