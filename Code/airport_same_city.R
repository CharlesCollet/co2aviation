airport_city <- data.frame(city=c("Brussels","Brussels",
                  "Hamburg","Hamburg",
                  "Berlin","Berlin",
                  "Belfast","Belfast",
                  "Nottingham","Nottingham",
                  "London","London","London","London","London","London",
                  "Gothenburg","Gothenburg",
                  "Stockholm","Stockholm","Stockolm",
                  "Tenerife","Tenerife",
                  "Paris","Paris","Paris",
                  "Milan","Milan","Milan",
                  "Rome","Rome"
                  ),
           airport=c("EBBR","EBCI",
                     "EDDH","EDHI",
                     "EDDT","EDDB",
                     "EGAA","EGAC",
                     "EGBN","EGNX",
                     "EGGW","EGKK","EGLL","EGSS","EGLC","EGMC",
                     "ESGG","ESGP",
                     "ESSA","ESSB","ESOW",
                     "GCXO","GCTS",
                     "LFPG","LFPO","LFOB",
                     "LIMC","LIML","LIME",
                     "LIRF","LIRA"
                     )
           )


base_par <- matrix_orig_dest(avia_par_PAS_DEP_filter,year=2017)
table(substr(base_par$orig_airp,4,7) %in% airport_city$airport)["TRUE"]
airport_same_city <- function(){
  
}