airport_same_city <- function(base_par){
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
  list_airports <- base_par[base_par$orig_airp %in% airport_city[,2],]
  list_airports$orig_airp <- sapply(list_airports$orig_airp,
                 function(x){airport_city[airport_city[,2]==x,1]})
  base_par[base_par$orig_airp %in% airport_city[,2],"orig_airp"] <- list_airports$orig_airp
  list_airports <- base_par[base_par$dest_airp %in% airport_city[,2],]
  list_airports$dest_airp <- sapply(list_airports$dest_airp,
                                    function(x){airport_city[airport_city[,2]==x,1]})
  base_par[base_par$dest_airp %in% airport_city[,2],"dest_airp"] <- list_airports$dest_airp
  return(base_par)
}
