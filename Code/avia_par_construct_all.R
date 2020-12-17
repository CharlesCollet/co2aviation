avia_par_at_pas_arr <- read_avia_par("avia_par_at.tsv")


files <- list.files(#path="C:/Users/colle/Documents/R/co2aviation/Data/Eurostat",
                    pattern="*.tsv", full.names=TRUE, recursive=FALSE)

files <- lapply(files, function(x) {
  read_avia_par(x,unit ="PAS",arr_dep ="DEP")
})

i=1
test=c()
  while (i<=length(files)) {
    test <- c(test,nrow(files[[i]]))
    i=i+1
  }
test

#Turkey
avia_par_tr <- as.data.frame(read_tsv("avia_par_tr.tsv"))
avia_par_tr <- read_avia_par("avia_par_tr.tsv")
#Serbia
avia_par_rs <- as.data.frame(read_tsv("avia_par_rs.tsv"))
avia_par_rs <- read_avia_par("avia_par_rs.tsv")
